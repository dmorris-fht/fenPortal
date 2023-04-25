library(shiny)
library(shinyjs)
library(httr)
library(shinydashboard)
library(shinycssloaders)
library(leaflet)
library(htmlTable)
library(sf)
library(DBI)
library(jsonlite)
library(uuid)
library(DT)
library(rpostgis)
library(ggplot2)
library(scales)
library(lubridate)
library(RColorBrewer)
library(plotly)
library(RPostgres)
library(hexView)
library(readr)
library(magrittr)


fenDb <- function(u, p){
            dbConnect(RPostgreSQL::PostgreSQL(max.con=200000), 
              user = u, 
              password = p,
              host = "data-fht.postgres.database.azure.com", 
              port = 5432, 
              dbname = "fen_database")
}

fenDbTest <- function(u, p){
  dbCanConnect(RPostgreSQL::PostgreSQL(max.con=200000), 
            user = u, 
            password = p,
            host = "data-fht.postgres.database.azure.com", 
            port = 5432, 
            dbname = "fen_database")
}