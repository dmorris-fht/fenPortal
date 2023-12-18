library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinycssloaders)
library(DT)
library(shinyvalidate) # NEEDS INSTALLING ON SERVER 

library(httr)

library(leaflet)
library(sf)

library(htmlTable) #???

library(jsonlite)
library(uuid)

library(ggplot2)
library(scales) #?
library(lubridate) #?
library(RColorBrewer) #? For hydro graphs?
library(plotly)
library(hexView)
library(readr)
library(tidyverse) #?
library(dplyr)
library(shinyWidgets)
library(shinyFeedback) #?
library(shinyTime) #?
library(future) #?
library(promises) #?
library(geojsonsf) # NEEDS INSTALLING ON SERVER

library(RPostgreSQL) # NEEDS INSTALLING ON SERVER
library(RPostgres)
library(rpostgis)
library(DBI) # Don't need this too?
library(pool)


# Misc functions ----

blank <- function(x){ifelse(!isTruthy(x),NA,x)}

concatenate_na.rm <- function(x){
  paste(x[!is.na(x)], collapse = " ")
}

con_sql_string <- function(v){
  return(paste0("('",paste(v,collapse="','"),"')"))
}
con_sql_num <- function(v){
  return(paste0("(",paste(v,collapse=","),")"))
}
sql_in <- function(x,v){
  if(!isTruthy(v)){
    return("1=1")
  }
  else{
    if(class(v)=="character"){
      return(
        paste0(x, " IN ",con_sql_string(v))
             )
    }
    if(class(v)=="numeric"){
      return(
        paste0(x, " IN ",con_sql_num(v))
      )
    }
  }
}
  
date_correction <- function(x){as.POSIXct(x / 1000, origin="1970-01-01", tz="GMT")}
date_format <- function(x){as.Date(x,format="%Y-%m-%d",origin="1970-01-01")} # Do I need?

na <- function(x){
  y <- ifelse(!isTruthy(x),
              "NULL",
                if(class(x) == "character"){
                  paste0("'",x,"'")
                }
              )
  return(y)
}

null_date <- function(x, y){
  ifelse(isTruthy(x), 
          paste0(y, " = TO_DATE('", x ,"','yyyy-mm-dd')"),
          paste0(y,"= NULL")
         )
  }
null_text <- function(con, x, y){ifelse(isTruthy(x),paste0(y,"='",postgresqlEscapeStrings(con,x),"'"),paste0(y,"= NULL"))}
null_num <- function(x, y){ifelse(isTruthy(x),paste0(y,"=",x),paste0(y,"= NULL"))}

null_date_val <- function(x){
  ifelse(isTruthy(x),
         paste0("TO_DATE('", x ,"','yyyy-mm-dd')"),
         "NULL")
}
null_text_val <- function(con, x){ifelse(isTruthy(x) && nchar(x) >0,paste0("'",postgresqlEscapeStrings(con,x),"'"),"NULL")}
null_num_val <- function(x){ifelse(isTruthy(x),x,"NULL")}

year_range <- function(v){
  x <-as.character(v[1])
  y <- as.character(v[2])
  
  if(isTruthy(x) && isTruthy(y)){
    if(x == y){
      return(x)
    }else{
      return(paste(x,y,sep="-"))
    }
  }
  if(isTruthy(x) && !isTruthy(y)){
    return(x)
  }
  if(!isTruthy(x) && isTruthy(y)){
    return(paste0("-",y))
  }
  if(!isTruthy(x) && !isTruthy(y)){
    return(NA)
  }
}

validate_gf <- function(g,s,ss,con){
  g <- toupper(gsub(" ","",g))
  tryCatch(
    if(as.numeric(substr(g,3,nchar(g)))>0){
      t <- "pass"
    }, error = function(e){t <- "fail"}
  )
  
  results <- list("error" = 0, "message"="gridref okay")

  if(!isTruthy(g)){
    results$error <- 0
    results$message <- "gridref null"
    
    return(results)
  }
  if(nchar(g) > 12 || nchar(g) < 4 || nchar(g) %% 2 == 1 || !(substr(g,1,2) %in% squares_100k$gridref) || t == "fail"){
    results$error <- 1
    results$message <- "Invalid grid reference format"
    
    return(results)
  }
  
  res <- nchar(g)*0.5 -1
  r <- 10^(5-res)
  
  e <- squares_100k[which(squares_100k$gridref == substr(g,1,2)),c("x")]
  n <- squares_100k[which(squares_100k$gridref == substr(g,1,2)),c("y")]
  
  x0 <- r*as.numeric(paste0(e,substr(g,3,2+res)))
  y0 <- r*as.numeric(paste0(n,substr(g,3+res,2+2*res)))
  x1 <- x0 + r
  y1 <- y0 + r
  ls <- paste0("LINESTRING(",x0," ",y0,",",x0," ",y1,",",x1," ",y1,",",x1," ",y0,",",x0," ",y0,")")

  if(isTruthy(s)){
    q_s <- dbGetQuery(con,
                      paste0(
                        "SELECT id FROM spatial.fen_sites WHERE ST_Intersects(ST_Polygon('",
                        ls,
                        "'::geometry, 27700),ST_buffer(geom,10)) AND id =",
                        s
                        )
                      )
    if(!(s %in% q_s$id)){
      results$error <- 1
      results$message <- "Grid reference not within 10m of given site"
      
      return(results)
    }
  }
  if(isTruthy(ss)){
    geom_ss <- dbGetQuery(con,paste0("SELECT id FROM spatial.fen_subsites WHERE geom IS NOT NULL AND id =",ss))
    q_ss <- dbGetQuery(con,
                      paste0(
                        "SELECT id FROM spatial.fen_subsites WHERE ST_Intersects(ST_Polygon('",
                        ls,
                        "'::geometry, 27700),ST_buffer(geom,10)) AND id =",
                        ss
                      )
    )
    if(nrow(geom_ss) > 0 &&!(ss %in% q_ss$id)){
      results$error <- 1
      results$message <- "Grid reference not within 10m of given subsite"
      
      return(results)
    }
  }
  
  return(results)
}

gf_string <- function(x,g){
  g <- toupper(gsub(" ","",g))
  r <- 10^(5-0.5*(nchar(g)-2))
  g1 <- substr(g,1,2)
  e <- substr(g,3,2+0.5*(nchar(g)-2))
  n <- substr(g,3+0.5*(nchar(g)-2),nchar(g))
  
  e1 <- as.numeric(paste0(e,paste(rep("0",5-0.5*(nchar(g)-2)),collapse="")))
  n1 <- as.numeric(paste0(n,paste(rep("0",5-0.5*(nchar(g)-2)),collapse="")))
  e2 <- as.numeric(paste0(e,paste(rep("0",5-0.5*(nchar(g)-2)),collapse=""))) + r
  n2 <- as.numeric(paste0(n,paste(rep("0",5-0.5*(nchar(g)-2)),collapse=""))) + r
  
  g0 <- paste0("substring(",x," from 1 for 2)")
  e0 <- paste0("CAST(substring(",x," from 3 for LENGTH(",x,")/2-1) AS INTEGER)")
  n0 <- paste0("CAST(substring(",x," from 2 + LENGTH(",x,") / 2) AS INTEGER)")
  
  paste0("(",g0,"='",g1,"' AND ", 
          e0, " <= ",e2," AND ", e0 , " >= ", e1, " AND ",
          n0, " <= ",n2," AND ", n0 , " >= ", n1, ")")
}

gf_vec <- function(x,v){
  if(!isTruthy(v)){
    return("(1=1)")
  }
  else{
    return(paste0("(",paste(lapply(v,function(y){gf_string(x,y)}),collapse = " OR "),")"))
  }
}

like_string <- function(x,s){
  if(isTruthy(s)){
    return(paste0("UPPER(",x,") LIKE '%",toupper(s),"%'"))
  }
  else{
    return("(1=1)")
  }
}

like_vec <- function(x,v){
  if(isTruthy(v)){
    return(paste("(",lapply(v,function(y){like_string(x,y)}),")",collapse = " OR "))
  }else{
    return("(1=1)")
  }
}


date_check <- function(x,m){
  d <- as.Date(x, "%d/%m/%Y")

  if(as.numeric(x)>0 && nchar(x) == 4){
    if(is.na(d)){
      if(m==0){
        d <- as.Date(ISOdate(x, 1, 1))
      }
      if(m==1){
        d <- as.Date(ISOdate(x, 12, 31))
      }
    }
  }
  
  return(list("error" = (as.numeric(x)>0 && nchar(x) == 4) || !is.na(d), "d" = d))
}

sql_date <- function(y,x0,x1){
  if(shiny::isTruthy(x0) && shiny::isTruthy(x1)){
    w <- paste0(y, " BETWEEN '", date_check(x0,0)$d, "' AND '", date_check(x1,1)$d,"'")
  }
  if(!shiny::isTruthy(x0) && shiny::isTruthy(x1)){
    w <- paste0(y, " <= '", date_check(x1,1)$d,"'")
  }
  if(shiny::isTruthy(x0) && !shiny::isTruthy(x1)){
    w <- paste0(y, " >= '", date_check(x0,0)$d,"'")
  }
  if(!shiny::isTruthy(x0) && !shiny::isTruthy(x1)){
    w <- "(1=1)"
  }
  return(w)
  
}

# Connection function ----
drv <- dbDriver("PostgreSQL")
fenDb <- function(u, p){
            dbPool(drv, 
              user = u, 
              password = p,
              host = "data-fht.postgres.database.azure.com", 
              port = 5432, 
              dbname = "fen_database")
}

fenDbTest <- function(u, p){
  DBI::dbCanConnect(RPostgres::Postgres(), 
            user = u, 
            password = p,
            host = "data-fht.postgres.database.azure.com", 
            port = 5432, 
            dbname = "fen_database")
}

# Functions for adding row buttons to DT ----

create_btns <- function(x) {
  x %>%
    purrr::map_chr(~
                     paste0(
                       '<div class = "btn-group">
                   <button class="btn btn-default action-button row-btn action_button cru" id="edit_',
                       .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-pen"></i></button>
                   <button class="btn btn-default action-button row-btn action_button inf" id="info_',
                       .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-info"></i></button></div>'
                     ))
}

create_btns_ru <- function(x,n) {
  x %>%
    purrr::map_chr(~
                     paste0(
                       '<div class = "btn-group">
                   <button class="btn btn-default action-button row-btn action_button cru" id="', n ,'_edit_',
                       .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-pen"></i></button>
                   <button class="btn btn-default action-button row-btn action_button inf" id="', n ,'_info_',
                       .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-info"></i></button></div>'
                     ))
}

create_btns_r <- function(x,n) {
  x %>%
    purrr::map_chr(~
                     paste0(
                       '<div class = "btn-group">
                   <button class="btn btn-default action-button row-btn action_button inf" id="', n ,'_info_',
                       .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-info"></i></button></div>'
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

del_btns <- function(x,n){
  x %>% purrr::map_chr(~
                   paste0(
                     '<div class = "btn-group">
                   <button title = "Delete record" class="btn btn-default action-button row-btn action_button inf" id="', n ,'_del_',
                     .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-trash-alt"></i></button></div>'
                   ))
}

val_btns <- function(x,n){
  x %>% purrr::map_chr(~
                         paste0(
                           '<div class = "btn-group">
                   <button title="Mark as valid" class="btn btn-default action-button row-btn action_button inf" id="', n ,'_val_',
                           .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-check"></i></button></div>'
                         ))
}

# AGOL integration functions ----

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
  
  #FIX FOR NON-SF CASE
  
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
# Global database tables and lookups ----
con_global <- fenDb("fen_guest","Alkal1n3F3ns!")
  
#uksi
uksi_full <-read.csv("./www/uksi.csv", header = TRUE)

# Taxon names with qualifiers and authorities
uksi <- read.csv("./www/uksi_full_names.csv",header = TRUE)
choices_uksi <- uksi$tvk
names(choices_uksi) <- uksi$full_name

# Taxon names with qualifiers and without authorities
uksi_1 <- read.csv("./www/uksi_names.csv",header = TRUE)
choices_uksi_1 <- uksi_1$tvk
names(choices_uksi_1) <- uksi_1$name

# Taxon groups
tgps <- read.csv("./www/taxon_groups.csv",header= TRUE)
choices_tgps <- tgps$x

# Fen plant spp
choices_fenspp <- c(0,1,2)
names(choices_fenspp) <- c("Select an option","All fen species", "Alkaline fen species")

fspp <- dbGetQuery(con_global, "SELECT nbn_taxon_version_key_for_recommended_name, taxon_latest, alkaline_fen_oxon AS alkaline_fen FROM lookups.fen_spp ORDER BY taxon_latest")
choices_fspp <- fspp$nbn_taxon_version_key_for_recommended_name
names(choices_fspp) <- fspp$taxon_latest
string_fspp <- paste0("('",paste(choices_fspp,collapse="','"),"')")

choices_afspp <- choices_fspp[which(fspp$alkaline_fen == TRUE)]
string_afspp <- paste0("('",paste(choices_afspp,collapse="','"),"')")

# Verification categories
verification <- dbGetQuery(con_global, "SELECT code, description FROM lookups.lookup_verification")
choices_verification <- verification$code
names(choices_verification) <- verification$description

squares_100k <- dbGetQuery(con_global, "SELECT gridref, x , y FROM lookups.squares_100k")

# Global for remembering record entry ----

global_records <- data.frame(
    "verification" = numeric(),
    "site_record" = character(),
    "site" = numeric(),
    "subsite" = numeric(),
    "taxon_nbn" = character(),
    "gridref" = character(),
    "quantity" = character(),
    "status" = character(),
    "sex" = character(),
    "stage" = character(),
    "habitat" = character(),
    "note" = character(),
    "record_date" = Date(),
    "record_date_end" = Date(),
    "recorder" = character(),
    "determiner" = character(),
    "method" = character(),
    "survey" = numeric(),
    "start_year" = numeric(),
    "end_year" = numeric(),
    "start_month" = numeric(),
    "end_month" = numeric(),
    "start_day" = numeric(),
    "end_day" = numeric(),
    "verification_user" = character(),
    "verification_date" = Date(),
    "verification_note" = character(),
    "guid"=character(),
    "site_name" = character(),
    "subsite_name" = character(),
    "taxon_name" = character(),
    "survey_name" = character(),
    "id" = as.numeric(),
    "Buttons" = character(),
    "in_db" = numeric(),
    "user" = character()
  )
global_records$guid <- as.UUID(global_records$guid)
