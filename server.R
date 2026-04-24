source('R/api_functions.R')


server <- function(input, output, session) {
  output$Map <- renderLeaflet({
    leaflet(data = stations_df) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addCircleMarkers(
        lng = ~lon, 
        lat = ~lat,
        radius = 7,
        fillColor = ~pal(province), 
        color = "white",            
        weight = 1, 
        fillOpacity = 0.8,
        popup = ~paste0("<b>Station:</b> ", stationName, "<br><b>Province:</b> ", province),
        layerId = ~id
      ) %>%
      addLegend(
        pal = pal, 
        values = ~province, 
        title = "Provinces", 
        position = "bottomright",
        opacity = 0.7
      )
  })
  sensors_data <- eventReactive(input$Map_marker_click, {
    req(input$Map_marker_click$id)
    tab <- fetch_sensors(input$Map_marker_click$id)
    
    
    tab$Wskaźnik <- ifelse(tab$Wskaźnik %in% names(translate_params), 
                           translate_params[tab$Wskaźnik], 
                           tab$Wskaźnik)
    return(tab)
  })
  
  output$sensor_table <- DT::renderDataTable({
    df <- sensors_data()
    req(df)
    
    DT::datatable(
      df[, c("Identyfikator stanowiska", "Wskaźnik")], 
      selection = "single",
      colnames = c("Sensor ID", "Measurement Parameter"), 
      options = list(scrollY = "250px", scrollCollapse = TRUE, paging = FALSE)
    )
  })
  
  output$data_preview <- renderTable({
    df <- metadata_archival() 
    req(df)
    
    preview <- head(df[, c("Data", "Wartość")], input$n_rows)
    colnames(preview) <- c("Date", "Value") 
    
    return(preview)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  metadata_archival <- eventReactive(input$sensor_table_rows_selected, {
    s <- input$sensor_table_rows_selected
    req(s)
    
    sensor_id <- sensors_data()$`Identyfikator stanowiska`[s]
    
    # Używasz wydzielonej funkcji
    raw_list <- fetch_archival_data(sensor_id, input$days_back)
    
    if (is.null(raw_list) || length(raw_list) == 0) {
      showNotification("No data found.", type = "warning")
      return(NULL)
    }
    
    df_val <- as.data.frame(raw_list)
    df_val$date <- as.POSIXct(df_val$Data, format = "%Y-%m-%d %H:%M")
    df_val$value <- as.numeric(df_val$Wartość)
    return(df_val)
  })

  output$sensor_graph <- renderPlotly({
    df <- metadata_archival()
    req(df)
    
    s <- input$sensor_table_rows_selected
    param_name <- sensors_data()$Wskaźnik[s] 
    
    p <- ggplot(df, aes(x = date, y = value)) +
      geom_line(color = "#2c3e50", linewidth = 1) +
      geom_point(color = "#e74c3c", alpha = 0.5) +
      theme_minimal() +
      labs(title = paste("Analysis of", param_name),
           subtitle = paste("Last", input$days_back, "days"),
           x = "Time", y = paste(param_name, "[µg/m³]"))
    
    ggplotly(p) %>%
      layout(hovermode = 'x unified')
    
    
  })
  
  output$dynamic_header <- renderUI({
    click <- input$Map_marker_click
    req(click)

    station_name <- stations_df$stationName[stations_df$id == click$id]

    HTML(paste0(
      "<span style='color: #7f8c8d; font-weight: bold; margin-right: 10px;'>SELECTED STATION:</span>",
      "<span style='color: #2c3e50; font-size: 1.1em;'>", station_name, "</span>"
    ))
  })
  output$summary_boxes <- renderUI({
    df <- metadata_archival()
    req(df)
    
    avg_v <- round(mean(df$value, na.rm = TRUE), 1)
    max_v <- round(max(df$value, na.rm = TRUE), 1)
    
    fluidRow(
      column(6, 
             div(style = "background: #e1f5fe; padding: 10px; text-align: center; border-radius: 5px;",
                 span("Average", style="display:block; font-size: 0.8em; color: #546e7a;"),
                 strong(avg_v, " µg/m³", style="font-size: 1.2em; color: #0288d1;")
             )
      ),
      column(6, 
             div(style = "background: #fff3e0; padding: 10px; text-align: center; border-radius: 5px;",
                 span("Maximum", style="display:block; font-size: 0.8em; color: #6d4c41;"),
                 strong(max_v, " µg/m³", style="font-size: 1.2em; color: #ef6c00;")
             )
      )
    )
  })
  output$aqi_status_box <- renderUI({
    df <- metadata_archival()
    req(df, nrow(df) > 0)
    
    latest_record <- tail(df, 1)
    latest_val <- latest_record$value
    latest_time <- format(latest_record$date, "%Y-%m-%d %H:%M")
    
    s <- input$sensor_table_rows_selected
    param_original <- sensors_data()$Wskaźnik[s]

    thresholds <- c(
      "Particulate Matter (PM10)" = 50,
      "Particulate Matter (PM2.5)" = 25,
      "Nitrogen Dioxide (NO2)" = 200,
      "Sulphur Dioxide (SO2)" = 350,
      "Ozone (O3)" = 120,
      "Carbon Monoxide (CO)" = 10000,
      "Benzene" = 5
    )

    current_threshold <- if(param_original %in% names(thresholds)) thresholds[[param_original]] else 50

    status <- "GOOD"
    color <- "#2ecc71" 
    advice <- "Air quality is healthy."
    
    if(latest_val > current_threshold) {
      status <- "EXCEEDED"
      color <- "#e74c3c" 
      advice <- "Health standards exceeded!"
    } else if (latest_val > current_threshold * 0.7) {
      status <- "MODERATE"
      color <- "#f39c12" 
      advice <- "Air quality is acceptable."
    }
    
    tagList(
      div(style = paste0("background-color:", color, "; color:white; padding:15px; border-radius:10px; text-align:center; box-shadow: 2px 2px 5px rgba(0,0,0,0.1);"),
          h4(style="margin-bottom:0;", "Last Recorded Status"),
          p(style="font-size: 0.8em; opacity: 0.9;", paste("As of:", latest_time)),
          hr(style="margin: 10px 0; border-top: 1px solid rgba(255,255,255,0.3);"),
          h2(style="margin:0; font-weight:bold;", status),
          h3(style="margin:0;", paste(round(latest_val, 1), "µg/m³")),
          p(style="font-size: 0.7em; margin-top:5px;", paste("Threshold for", param_original, ":", current_threshold))
      ),
      br(),
      div(style = "background-color: #fdfefe; border-left: 5px solid #bdc3c7; padding: 10px; font-size: 0.9em;",
          strong("Health Advice:"),
          p(advice)
      )
    )
  })
  
} 