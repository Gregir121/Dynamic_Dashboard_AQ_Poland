
fetch_stations <- function() {
  url <- "https://api.gios.gov.pl/pjp-api/v1/rest/station/findAll?size=500"
  res <- fromJSON(content(httr::GET(url), as = "text", encoding = "UTF-8"))
}



fetch_sensors <- function(station_id) {
  url <- paste0("https://api.gios.gov.pl/pjp-api/v1/rest/station/sensors/", station_id)
  res <- fromJSON(content(httr::GET(url), as = "text", encoding = "UTF-8"))
  return(res$`Lista stanowisk pomiarowych dla podanej stacji`)
}

fetch_archival_data <- function(sensor_id, days_back) {
  url <- paste0("https://api.gios.gov.pl/pjp-api/v1/rest/archivalData/getDataBySensor/", 
                sensor_id, "?dayNumber=", days_back, "&size=500")
  resp <- httr::GET(url)
  res <- fromJSON(content(resp, as = "text", encoding = "UTF-8"))
  return(res$`Lista archiwalnych wyników pomiarów`)
}