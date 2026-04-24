ui <- fluidPage(
  
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  
  div(style = "text-align: center; padding: 20px;",
      h1("Dynamic map of air quality assessment in Poland", 
         style = "color: #2c3e50; font-weight: bold;")
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h3("Main Panel"),
      hr(),
      numericInput("n_rows", "Number of rows in the preview:",
                   value = 10, min = 1, max = 100),
      hr(),
      numericInput("days_back", "Choose how many back from today to download:",
                   value = 7, min = 1, max = 366),
      hr(),
      
      
      h4('Station metadata:'),
      
      textOutput("selected_station_name"),
      br(),
      
      helpText("Instructions:", br(),
               "1. Choose a station on the map.", br(),
               "2. Choose a sensor with a parameter"),
      hr(),
      uiOutput("aqi_status_box")
    ),
    mainPanel(
      width = 9,
      #MAP
      fluidRow(
        column(12, 
               leafletOutput("Map", height = "400px")
        )
      ),
      
      hr(),
      #Sensor table
      fluidRow(
        column(12, withSpinner(
               DT::dataTableOutput("sensor_table"))
        )
      ),
      hr(),
      fluidRow(
        column(12,
               div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; border: 1px solid #dee2e6;",
                   uiOutput("dynamic_header")
               )
        )
      ),
      
      fluidRow(
        column(12,
               uiOutput("summary_boxes") # Trzy małe statystyki obok siebie
        )
      ),
      br(),
      #Data table and graphs
      fluidRow(
        column(5,
               tableOutput("data_preview")
        ),
        column(7,withSpinner(
               plotlyOutput("sensor_graph"))
        )
      )
    )
  )
)
