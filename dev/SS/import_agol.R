#Libraries ----
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

library(geojsonsf) # ADD THIS TO GLOBAL.R

# Params ----

con <- dbConnect(RPostgreSQL::PostgreSQL(max.con=200000), 
                 user = "dmorris", 
                 password = "Taraxacum1!",
                 host = "data-fht.postgres.database.azure.com", 
                 port = 5432, 
                 dbname = "fen_database")

u <- "https://services9.arcgis.com/J5mi5KNMFx93FsQu/arcgis/rest/services/monitoring_observations/FeatureServer/0"

w <- "OBJECTID > 0"
g <- 'true'
a <- TRUE

s <- "spatial"
t <- "monitoring_observations"

fetch_agol(connection = con, url = "", where = "", geometry = TRUE, attachments = TRUE)

date_correction <- function(x){as.POSIXct(x / 1000, origin="1970-01-01", tz="GMT")}

fetch_agol <- function(connection, url, where, geometry, attachments){
  
  con <- connection
  u <- url
  w <- ifelse(is.null(where) || where == "" || is.na(where),"OBJECTID > 0")
  g <- ifelse(geometry == TRUE,"true","false")
  a <- attachments
  
  
  if (is.null(con) || is.null(u)) {
    stop("'connection' and 'url' must be given")
  }
  if (is.null(geometry) || !geometry %in% c(TRUE, FALSE)) {
    stop("'geometry' argument must be one of: TRUE, FALSE")
  }
  if (is.null(a) || !a %in% c(TRUE, FALSE)) {
    stop("'attachments' argument must be one of: TRUE, FALSE")
  }
  
    #url for querying AGOL
    url <- parse_url(paste(u,"/query",sep=""))
  
    if(g == "true"){
      url$query <- list(
        where = w,
        returnGeometry = g,
        outFields = "*",
        f = "geojson")
    }else{
      url$query <- list(
        where = w,
        returnGeometry = g,
        outFields = "*",
        f = "json")
    }

    #Make the request and parse the data
    request <- build_url(url)
    
    resp <- tryCatch({
      GET(request)
      }
      ,
      error = function(e){
        stop("GET request on URL returned an error")
      }
    )
      
    raw <- rawToChar(resp$content)
    Encoding(raw) <- "UTF-8"
    
    if(g == "true"){
      d <- geojsonsf::geojson_sf(raw)
      dates <- grep("date",colnames(d))
      for(i in 1:length(dates)){
        d[,dates[i]][[1]] <- date_correction(d[,dates[i]][[1]]) # correct dates to postgres format
      }
      
      d$guid <- as.UUID(d$GlobalID)
      d <- d[,-which(colnames(d)== "OBJECTID" | colnames(d) == "GlobalID")] #Drop AGOL id fields
      
      tracking <- which(colnames(d) == "last_edited_user" | colnames(d) == "last_edited_date" | colnames(d) == "created_user" | colnames(d) == "created_date")
      colnames(d)[tracking] <- paste("source_",colnames(d)[tracking],sep="")    #change tracking column names
    
      data <- d
      }else{
      d <- fromJSON(raw)
      
      #Extract data frame and fields
      x <- d$features$attributes
      
      dates <- grep("Date",d$fields$type)
      x[dates] <- lapply(x[dates],date_correction) # correct dates to postgres format
      
      x$guid <- as.UUID(x$GlobalID)
      x <- x[,-which(colnames(x)== "OBJECTID" | colnames(x) == "GlobalID")] #Drop AGOL id fields
      
      tracking <- which(colnames(x) == "last_edited_user" | colnames(x) == "last_edited_date" | colnames(x) == "created_user" | colnames(x) == "created_date")
      colnames(x)[tracking] <- paste("source_",colnames(x)[tracking],sep="")    #change tracking column names
    
      data <- x
      }
    
    if(a == TRUE){
      #url for querying AGOL attachments
      url_attach <- parse_url(paste(u,"/queryAttachments",sep=""))
      url_attach$query <- list(definitionExpression = w,
                               returnUrl = "true",
                               f = "json")
      
      #Make the request and parse the data
      request_attach <- build_url(url_attach)
      resp_attach <- GET(request_attach)
      raw_attach <- rawToChar(resp_attach$content)
      Encoding(raw_attach) <- "UTF-8"
      d_attach <- fromJSON(raw_attach)
      
      g <- d_attach$attachmentGroups[[2]]
      for(i in 1:length(g)){
        a0 <- d_attach$attachmentGroups[[3]][[i]]
        a0$rel_guid <- g[i]
        if(i ==1){
          a <- a0
        }
        else{
          a <- rbind(a,a0)
        }
      }
      a$guid <- as.UUID(a$globalId)
      a <- a[,-c(1,2,6,8)]
      colnames(a) <- c("att_name","att_type","att_size","url","rel_guid","guid")
      
      a$att_type <- ifelse(a$att_type == "application/octet-stream","image/jpeg",a$att_type)
      
      attach <- a
      
    }
    else{
      attach <- NA
    }

  return(list("data" = data, "attach" = attach))
}

import_agol <- function(con, u, w, g, a, s, t){
  
    data <- fetch_agol(con, u, w, g, a)$data
    attach <- fetch_agol(con, u, w, g, a)$attach
    
    if(class(data)[[1]] == "sf"){
      dbGetQuery(con, paste0("ALTER TABLE ",s,".",t," ALTER COLUMN geom TYPE geometry(POINT, 880001) using ST_SETSRID(geom, 880001);"))
      write <- rpostgis::pgWriteGeom(conn = con, 
                                       name = c(s,t),
                                       data.obj = data,
                                       geom = "geom",
                                       overwrite = FALSE,
                                       partial.match = TRUE,
                                       upsert.using = c("guid")
                                     )
      dbGetQuery(con, paste0("ALTER TABLE ",s,".",t," ALTER COLUMN geom TYPE geometry(POINT, 27700) using ST_SETSRID(geom, 27700);"))
    }
    else{
      write <- rpostgis::pgInsert(conn = con, 
                                     name = c(s,t),
                                     data.obj = data,
                                     overwrite = FALSE,
                                     partial.match = TRUE,
                                     upsert.using = c("guid")
                                  )
    }
    
    
    if(a == TRUE){
      for(i in 1:nrow(attach)){
        #Check attachment not already in attachment table
        dl <- dbGetQuery(con, 
                        paste0("SELECT COUNT(guid) FROM (SELECT guid FROM ",s,".",t,"_attach WHERE guid IN ('b98dc6d4-a244-4bbf-9c61-9f8dc8f1b6cb')) AS A")
                        ) 
        if(dl == 0){
          #Download and convert attachment to binary
          tf <- tempfile(fileext=paste0(".",
                                        substr(attach[i,"att_type"],7,10000)
          )
          )
          download.file(attach[i,c("url")], tf, mode = "wb")
          z <- readRaw(tf)
          r <- attach[i,]
          r$att <- paste0("\\x", paste(z$fileRaw, collapse = ""))
          
          #Upsert attachment
          insert_attach <- pgInsert(
            con,
            name = c(s,paste0(t,"_attach")),
            data.obj = r,
            df.mode = FALSE,
            partial.match = TRUE,
            overwrite = FALSE,
            upsert.using = c("guid")
          )
        } 
      }
    }
}
