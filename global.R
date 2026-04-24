library(httr)
library(jsonlite)
library(tidyverse)
library(shiny)
library(leaflet)
library(ggplot2)
library(DT)
library(plotly)
library(shinycssloaders)
source("R/api_functions.R")


stations_raw <- fetch_stations()

stations_df <- stations_raw$`Lista stacji pomiarowych`

colnames(stations_df)[1:5] <- c("id", "stationCode", "stationName",
                                "lat", "lon")

head(stations_df[, c("id","stationCode",
                     "stationName", "lat", "lon")])

library(dplyr)
library(tidyverse)

#Na handling
colSums(is.na(stations_df))

stations_df <- stations_df %>%
  filter(!is.na(lat) & !is.na(lon))

colSums(is.na(stations_df))

#Type conversions
str(stations_df)

stations_df$lon <- as.numeric(stations_df$lon)
stations_df$lat <- as.numeric(stations_df$lat)

str(stations_df)


#Mapping
translate_params <- c(
  "arsen w PM10" = "Arsenic (As) in PM10",
  "kadm w PM10" = "Cadmium (Cd) in PM10",
  "nikiel w PM10" = "Nickel (Ni) in PM10",
  "ołów w PM10" = "Lead (Pb) in PM10",
  "benzo(a)piren w PM10" = "Benzo(a)pyrene in PM10",
  "tlenki azotu" = "Nitrogen Oxides (NOx)",
  "dwutlenek azotu" = "Nitrogen Dioxide (NO2)",
  "ozon" = "Ozone (O3)",
  "pył zawieszony PM10" = "Particulate Matter (PM10)",
  "pył zawieszony PM2.5" = "Particulate Matter (PM2.5)",
  "tlenek węgla" = "Carbon Monoxide (CO)",
  "dwutlenek siarki" = "Sulphur Dioxide (SO2)",
  "benzen" = "Benzene",
  "tlenek azotu" = "Nitrogen Oxide (NO)",
  "rtęć całkowita gazowa (TGM)" = "Total Gaseous Mercury (TGM)"
)

translate_provinces <- c(
  "DOLNOŚLĄSKIE" = "Lower Silesia",
  "KUJAWSKO-POMORSKIE" = "Kuyavia-Pomerania",
  "LUBELSKIE" = "Lublin",
  "LUBUSKIE" = "Lubusz",
  "ŁÓDZKIE" = "Lodz",
  "MAŁOPOLSKIE" = "Lesser Poland",
  "MAZOWIECKIE" = "Mazovia",
  "OPOLSKIE" = "Opole",
  "PODKARPACKIE" = "Subcarpathia",
  "PODLASKIE" = "Podlaskie",
  "POMORSKIE" = "Pomerania",
  "ŚLĄSKIE" = "Silesia",
  "ŚWIĘTOKRZYSKIE" = "Holy Cross",
  "WARMIŃSKO-MAZURSKIE" = "Warmia-Masuria",
  "WIELKOPOLSKIE" = "Greater Poland",
  "ZACHODNIOPOMORSKIE" = "West Pomerania"
)

stations_df$province <- stations_raw$`Lista stacji pomiarowych`$Województwo

stations_df$province <- ifelse(stations_df$province %in% names(translate_provinces),
                               translate_provinces[stations_df$province],
                               stations_df$province)

table(stations_df$province, useNA = 'ifany')

pal <- colorFactor(
  palette = "Set1",
  domain = stations_df$province
)


