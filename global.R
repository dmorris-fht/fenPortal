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
library(tidyverse)
library(dplyr)
library(shinyWidgets)
library(shinyFeedback)
library(shinyTime)


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

#Functions for adding row buttons to DT

create_btns <- function(x) {
  x %>%
    purrr::map_chr(~
                     paste0(
                       '<div class = "btn-group">
                   <button class="btn btn-default action-button btn-info action_button cru" id="edit_',
                       .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-edit"></i></button>
                   <button class="btn btn-default action-button btn-danger action_button" id="info_',
                       .x, '" type="button" onclick=get_id(this.id)><i class="fa-solid fa-circle-info"></i></button></div>'
                     ))
}

create_btns_ru <- function(x,n) {
  x %>%
    purrr::map_chr(~
                     paste0(
                       '<div class = "btn-group">
                   <button class="btn btn-default action-button btn-info action_button cru" id="', n ,'_edit_',
                       .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-edit"></i></button>
                   <button class="btn btn-default action-button btn-danger action_button" id="', n ,'_info_',
                       .x, '" type="button" onclick=get_id(this.id)><i class="fa-solid fa-circle-info"></i></button></div>'
                     ))
}

create_btns_r <- function(x,n) {
  x %>%
    purrr::map_chr(~
                     paste0(
                       '<div class = "btn-group">
                   <button class="btn btn-default action-button btn-danger action_button" id="', n ,'_info_',
                       .x, '" type="button" onclick=get_id(this.id)><i class="fa-solid fa-circle-info"></i></button></div>'
                     ))
}

add_btns <- function(d,m,n) {
  #Adds buttons indexed by primary key column of dataframe, which is assumed to be called 'id'
  if(grepl("u",m)){
    x <- create_btns_ru(d[,c("id")],n)
    y <- d %>% dplyr::bind_cols(tibble("Buttons" = x))
    }
  else{
    x <- create_btns_r(d[,c("id")],n)
    y <- d %>% dplyr::bind_cols(tibble("Buttons" = x))
  }
  
  return(y)
}

