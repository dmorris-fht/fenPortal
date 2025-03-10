# Libraries ----
library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinycssloaders)
#library(shinydisconnect) # Not in use
library(DT)
library(shinyvalidate)

library(billboarder)

library(httr)

library(leaflet) 
library(leaflet.extras) 
library(sf)

library(htmlTable) # Used in hydro explore module

library(uuid)
library(zip)
library(ggplot2)
library(lubridate)
library(RColorBrewer) # For plots in hydro explore module
library(plotly) # For plots in hydro explore module
library(hexView)
library(tidyverse) #?
library(dplyr)
library(shinyWidgets)
library(shinyFeedback) 
library(shinyTime) #?
library(future) 
library(promises)
library(geojsonsf) #?
library(jsonlite)
library(magick) # seems to have installed

#library(slickR) # Not in use
library(base64enc)

library(RPostgreSQL) 
library(rpostgis)
library(DBI) # Don't need this too?
library(pool)

# No longer in use
library(scales) #?
library(readr)
library(RPostgres)

# Shiny options ----
options(shiny.maxRequestSize = 30*1024^2) # 30 MB limit

# Functions to handle missing values ----
blank <- function(x){ifelse(!isTruthy(x),NA,x)}

na <- function(x){
  y <- ifelse(!isTruthy(x),
              "NULL",
              if(class(x) == "character"){
                paste0("'",x,"'")
              }
  )
  return(y)
}

null <- function(x){
  ifelse(!isTruthy(x),NULL,x)
}

compareNA <- function(v1,v2) {
  same <- (v1 == v2) | (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  return(same)
}

coalesce.list <- function(x){
  return(x[min(which(!is.na(x)))])
  }

concatenate_na.rm <- function(x){
  paste(x[!is.na(x)], collapse = " ")
}

paste_na.rm <- function(x,s){
  paste(x[!is.na(x)], collapse = s)
}

# Functions for sql strings ----

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

date_correction <- function(x){as.POSIXct(x / 1000, origin="1970-01-01", tz="GMT")}
date_format <- function(x){as.Date(x,format="%Y-%m-%d",origin="1970-01-01")} # Do I need?

null_date <- function(x, y){
  ifelse(isTruthy(x), 
          paste0(y, " = TO_DATE('", x ,"','yyyy-mm-dd')"),
          paste0(y,"= NULL")
         )
  }
null_text <- function(con, x, y){ifelse(isTruthy(x),paste0(y,"='",postgresqlEscapeStrings(con,x),"'"),paste0(y,"= NULL"))}
null_num <- function(x, y){ifelse(isTruthy(x),paste0(y,"=",x),paste0(y,"= NULL"))}

null_date_val <- function(x){
  
  ifelse(isTruthy(x) && length(x) > 0,
         paste0("TO_DATE('", format(x,"%Y-%m-%d") ,"','yyyy-mm-dd')"),
         "NULL")
}
null_timestamp_val <- function(x){
  ifelse(isTruthy(x),
         paste0("TO_TIMESTAMP('", x ,"','YYYY-MM-DD HH24:MI:SS')"),
         "NULL")
}
null_text_val <- function(con, x){ifelse(isTruthy(x) && nchar(x) >0,paste0("'",postgresqlEscapeStrings(con,x),"'"),"NULL")}
null_num_val <- function(x){ifelse(isTruthy(x),x,"NULL")}

# Date functions ----

year_check <- function(x){
  suppressWarnings(
    (!is.na(as.numeric(x)) && as.numeric(x) %% 1 == 0 && as.numeric(x) > 1600 && as.numeric(x) < 2100) || !isTruthy(x)
  )
  }

month_check <- function(x){
  suppressWarnings(
    (!is.na(as.numeric(x)) && as.numeric(x) %% 1 == 0 && as.numeric(x) > 0 && as.numeric(x) < 13) || !isTruthy(x)
  )
}

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

start_end <- function(s,e){
  if(!is.na(s) && !is.na(e)){
    return(paste_na.rm(c(s," - ",e),s=""))
  }
  if(!is.na(s) && is.na(e)){
    return(paste_na.rm(c(s," - "),s=""))
  }
  if(is.na(s) && !is.na(e)){
    return(paste_na.rm(c(" - ",e),s=""))
  }
}

date_range <- function(x){
  d <- x[1] # date
  m <- as.numeric(x[13]) # added later, so must be specified last in argument
  y <- as.numeric(x[14]) # added later, so must be specified last in argument
  ds <- x[2] # start date
  de <- x[3] # end date
  ms <- as.numeric(x[4]) # start month
  me <- as.numeric(x[5]) # end month
  ys <- x[6] # start year
  ye <- x[7] # end year
  ss <- x[8] # survey start date
  se <- x[9] # survey start date
  sys <- x[10] # survey start year
  sye <- x[11] # survey end year
  if(!is.na(d)){
    #return(format(d,"%Y-%m-%d"))
    return(d)
  }
  if(!is.na(m) || !is.na(y)){
    if(is.na(m)){
      return(y)
    }else{
      paste_na.rm(c(as.character(as.Date(ISOdate(y, m, 1)))," - ",as.character(as.Date(ISOdate(y, m+1, 1))-1)),s="")
    }
  }
  if(!is.na(ds) || !is.na(de)){
    #return(start_end(format(ds,"%Y-%m-%d"),format(de,"%Y-%m-%d")))
    return(start_end(ds,de))
  }
  if(!is.na(ms) || !is.na(me)){
    if(is.na(me)){
      return(
        paste_na.rm(c(as.character(as.Date(ISOdate(ys, ms, 1)))," - ",as.character(as.Date(ISOdate(ys, ms+1, 1))-1)),s="")
        )
    }
    if(is.na(ms)){
      return(
        paste_na.rm(c(as.Date(ISOdate(ye, me, 1))," - ",as.character(as.Date(ISOdate(ye, me+1, 1))-1)),s="")
        )
    }
    return(
      paste_na.rm(c(as.character(as.Date(ISOdate(ys, ms, 1)))," - ",as.character(as.Date(ISOdate(ye, me+1, 1))-1)),s="")
      )
  }
  if(!is.na(ys) || !is.na(ye)){
    return(start_end(ys,ye))
  }
  if(!is.na(ss) || !is.na(se)){
    #return(start_end(format(ss,"%Y-%m-%d"),format(se,"%Y-%m-%d")))
    return(start_end(ss,se))
  }
  if(!is.na(sys) || !is.na(sye)){
    return(start_end(sys,sye))
  }
}

# Validation functions ----

isNumeric <- function(x){
  tryCatch({
    !grepl("\\D", x) 
  },
  error = function(err){FALSE})
}

isGridref <-function(g){
  g <- toupper(gsub(" ","",g))

  if(!isTruthy(g)){
    return(NA)
  }
  if(
    nchar(g) < 13 && nchar(g) > 3 && nchar(g) %% 2 == 0 && substr(g,1,2) %in% squares_100k$gridref && isNumeric(substr(g,3,nchar(g)))
    ){
    return(TRUE)
  } # 100km, 10km, 1km, 100m, 10m, 1m grid ref
  if(
    nchar(g) == 5 && isNumeric(substr(g,3,4)) && substr(g,1,2) %in% squares_100k$gridref && substr(g,5,5) %in% toupper(letters[c(1:14,16:25)])
  ){
    return(TRUE)
  } # tetrad
  return(FALSE)
}

validate_gf <- function(g,s,ss,sites0,subsites0){
  g <- toupper(gsub(" ","",g))

  results <- list("error" = 0, "message"="gridref okay")

  if(!isTruthy(g)){
    results$error <- 0
    results$message <- "gridref null"
    
    return(results)
  }
  if(!isGridref(g)){
    results$error <- 1
    results$message <- "Invalid grid reference format"
    
    return(results)
  }
  
  if(isTruthy(s) && !(s %in% sites0[st_is_empty(sites0),]$id)){
    if(nchar(g) != 5){ # Normal gridref
      res <- nchar(g)*0.5 -1
      r <- 10^(5-res)
      e <- squares_100k[which(squares_100k$gridref == substr(g,1,2)),c("x")]
      n <- squares_100k[which(squares_100k$gridref == substr(g,1,2)),c("y")]
      x0 <- r*as.numeric(paste0(e,substr(g,3,2+res)))
      y0 <- r*as.numeric(paste0(n,substr(g,3+res,2+2*res)))
      x1 <- x0 + r
      y1 <- y0 + r
    }
    if(nchar(g) == 5){ # Tetrads
      r <- 2000
      e <- squares_100k[which(squares_100k$gridref == substr(g,1,2)),c("x")]
      n <- squares_100k[which(squares_100k$gridref == substr(g,1,2)),c("y")]
      x0 <- as.numeric(paste0(e,substr(g,3,3),colnames(dinty)[which(dinty == substr(g,5,5),arr.ind = TRUE)[2]],"000"))
      y0 <- as.numeric(paste0(n,substr(g,4,4),rownames(dinty)[which(dinty == substr(g,5,5),arr.ind = TRUE)[1]],"000"))
      x1 <- x0 + 2000
      y1 <- y0 + 2000
    }
  
    #ls <- paste0("LINESTRING(",x0," ",y0,",",x0," ",y1,",",x1," ",y1,",",x1," ",y0,",",x0," ",y0,")")
    
    l <- matrix(c(x0,y0,x1,y0,x1,y1,x0,y1,x0,y0),ncol = 2, byrow = TRUE)
    gf <- st_polygon(list(l))
  
    q_s <- sites0[unlist(st_intersects(st_buffer(gf,10),sites0)),c("id")]
    
    # q_s <- dbGetQuery(con,
    #                   paste0(
    #                     "SELECT id FROM spatial.fen_sites WHERE ST_Intersects(ST_Polygon('",
    #                     ls,
    #                     "'::geometry, 27700),ST_buffer(geom,10)) AND id =",
    #                     s
    #                     )
    #                   )
    if(!(s %in% q_s$id)){
      results$error <- 1
      results$message <- "Grid reference not within 10m of given site"
    }
  
    if(isTruthy(ss) && !(ss %in% subsites0[st_is_empty(subsites0),]$id)){
        q_ss <- subsites0[unlist(st_intersects(st_buffer(gf,10),subsites0)),c("id")]
        
        if(!(ss %in% q_ss$id)){
          results$error <- 1
          results$message <- "Grid reference not within 10m of given subsite"
        }
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
  v <- toupper(gsub(" ","",v))
  if(isTruthy(v)){
    return(paste("(",lapply(v,function(y){like_string(x,y)}),")",collapse = " OR "))
  }else{
    return("(1=1)")
  }
}

IsDate <- function(x, date.format = "%d/%m/%y") {
tryCatch(
  {!is.na(as.Date(x, date.format))
    },  
         error = function(err) {FALSE})  
}
IsYear <- function(x){
  tryCatch({
    !grepl("\\D", x) && nchar(x) == 4
    },
           error = function(err){FALSE})
}

date_check <- function(x,m){
  d <- NA
  error <- FALSE
  if(IsDate(x,date.format = "%d/%m/%Y")){
    d <- as.Date(x, "%d/%m/%Y")
    error <- TRUE
  }else{
    if(IsYear(x)){
      error <- TRUE
      if(is.na(d)){
        if(m==0){
          d <- as.Date(ISOdate(x, 1, 1))
        }
        if(m==1){
          d <- as.Date(ISOdate(x, 12, 31))
        }
      }
    }
  }
  
  return(list("error" = error, "d" = d))
}

dinty <- data.frame(c("A","B","C","D","E"),
                    c("F","G","H","I","J"),
                    c("K","L","M","N","P"),
                    c("Q","R","S","T","U"),
                    c("V","W","X","Y","Z"))
colnames(dinty) <- c("0","2","4","6","8")
rownames(dinty) <- c("0","2","4","6","8")

gf_sf <- function(g){
    g <- toupper(gsub(" ","",g))
    
    if(isGridref(g) == FALSE || !isTruthy(g)){
      return(NA)
    }
    else{
      if(nchar(g) != 5){
        res <- nchar(g)*0.5 -1
        r <- 10^(5-res)
        e <- squares_100k[which(squares_100k$gridref == substr(g,1,2)),c("x")]
        n <- squares_100k[which(squares_100k$gridref == substr(g,1,2)),c("y")]
        x0 <- r*as.numeric(paste0(e,substr(g,3,2+res)))
        y0 <- r*as.numeric(paste0(n,substr(g,3+res,2+2*res)))
        x1 <- x0 + r
        y1 <- y0 + r
        l <- matrix(c(x0,y0,x1,y0,x1,y1,x0,y1,x0,y0),ncol = 2, byrow = TRUE)
        gf <- st_polygon(list(l))
        return(gf)
      }
      if(nchar(g) == 5){
        r <- 2000
        e <- squares_100k[which(squares_100k$gridref == substr(g,1,2)),c("x")]
        n <- squares_100k[which(squares_100k$gridref == substr(g,1,2)),c("y")]
        x0 <- as.numeric(paste0(e,substr(g,3,3),colnames(dinty)[which(dinty == substr(g,5,5),arr.ind = TRUE)[2]],"000"))
        y0 <- as.numeric(paste0(n,substr(g,4,4),rownames(dinty)[which(dinty == substr(g,5,5),arr.ind = TRUE)[1]],"000"))
        x1 <- x0 + 2000
        y1 <- y0 + 2000
        l <- matrix(c(x0,y0,x1,y0,x1,y1,x0,y1,x0,y0),ncol = 2, byrow = TRUE)
        gf <- st_polygon(list(l))
        return(gf)
      }
      }
}

taxon_lookup <- function(t){
  
}


import_validation <- function(x){
  prod(
    isGridref(x[[1]]) || !isTruthy(x[[1]]), # gridref
    IsDate(x[[2]]) || !isTruthy(x[[2]]), # date
    IsDate(x[[3]]) || !isTruthy(x[[3]]), # start date
    IsDate(x[[4]]) || !isTruthy(x[[4]]), # end date
    (IsDate(x[[3]]) && IsDate(x[[4]]) && (as.Date(x[[3]], "%d/%m/%Y") <= as.Date(x[[4]], "%d/%m/%Y"))) || !isTruthy(x[[3]]) || !isTruthy(x[[4]]), # date range
    year_check(x[[5]]), # start year
    year_check(x[[6]]), # end year
    (year_check(x[[5]]) && year_check(x[[6]]) && as.numeric(x[[5]]) <= as.numeric(x[[6]])) || !isTruthy(x[[5]]) || !isTruthy(x[[6]]), # year range
    month_check(x[[7]]), # start month
    month_check(x[[8]]), # end month
    (month_check(x[[7]]) && month_check(x[[8]]) && ( (x[[5]] == x[[6]] && as.numeric(x[[7]]) <= as.numeric(x[[8]])) ||  x[[5]] != x[[6]] ) ) || !isTruthy(x[[7]]) || !isTruthy(x[[8]]), # month range
    isTruthy(x[[1]]) || isTruthy(x[[9]]), # gridref and site not both null
    isTruthy(x[[10]]) || isTruthy(x[[11]]) # nbn tvk or taxon name given
    )
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


edit_del_btns <- function(x,n){
  x %>% purrr::map_chr(~
                         paste0(
                           '<div class = "btn-group">
                           <button class="btn btn-default action-button row-btn action_button cru" id="', n ,'_edit_',
                           .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-pen"></i></button>
                   <button title = "Delete record" class="btn btn-default action-button row-btn action_button inf" id="', n ,'_del_',
                           .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-trash-alt"></i></button></div>'
                         ))
}

add_edit_del_btns <- function(d,n){
  x <- edit_del_btns(d[,c("id")],n)
  y <- d %>% dplyr::bind_cols(tibble("Buttons" = x))
}

val_btns <- function(x,n){
  x %>% purrr::map_chr(~
                         paste0(
                           '<div class = "btn-group">
                   <button title="Mark as valid" class="btn btn-default action-button row-btn action_button inf" id="', n ,'_val_',
                           .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-check"></i></button></div>'
                         ))
}

# AGOL fetch function ----

fetch_agol <- function( url, where, geometry, attachments){
  
  u <- url
  w <- ifelse(!isTruthy(where) || where == "*","OBJECTID > 0",where)
  a <- attachments
  
  r <- list("error" = NA, "data" = NA, "attach" = NA)
  
  if (is.null(u)) {
    stop("'url' must be given")
  }
  if (is.null(geometry) || !geometry %in% c(TRUE, FALSE)) {
    stop("'geometry' argument must be one of: TRUE, FALSE")
  }
  if (is.null(a) || !a %in% c(TRUE, FALSE)) {
    stop("'attachments' argument must be one of: TRUE, FALSE")
  }
  
  #url for querying AGOL
  url <- parse_url(paste0(u,"/query"))
  
  url$query <- list(
    where = w,
    returnGeometry = "false",
    outFields = "*",
    f = "json")
  
  #Make the request and parse the data
  request <- build_url(url)
  
  resp <- tryCatch({
    GET(request)
  }
  ,
  error = function(e){
    NA
  }
  )
  
  if(!isTruthy(resp)){
    r$error <- "Invalid URL"
    return(r)
  }
  
  raw <- rawToChar(resp$content)
  Encoding(raw) <- "UTF-8"
  
  d <- fromJSON(raw)
  
  if(isTruthy(d$error$code)){
    r$error <- d$error$message
    return(r)
  }
  
  #Extract data frame and fields
  x <- d$features$attributes
  dates <- grep("Date",d$fields$type)
  x[dates] <- lapply(x[dates],date_correction) # correct dates to postgres format
  
  x$guid <- x$GlobalID #as.UUID(x$GlobalID)
  x <- x[,-which(colnames(x)== "OBJECTID" | colnames(x) == "GlobalID")] #Drop AGOL id fields
  
  tracking <- which(colnames(x) == "last_edited_user" | colnames(x) == "last_edited_date" | colnames(x) == "created_user" | colnames(x) == "created_date")
  if(length(tracking) > 0){
    colnames(x)[tracking] <- paste("source_",colnames(x)[tracking],sep="")    #change tracking column names
  }
  
  if(geometry){
    url2 <- parse_url(paste(u,"/query",sep=""))
    url2$query <- list(
      where = w,
      returnGeometry = "true",
      outFields = "*",
      outSR = "27700",
      f = "geoJSON")
    
    request <- build_url(url2)
    resp <- GET(request)
    
    raw <- rawToChar(resp$content)
    Encoding(raw) <- "UTF-8"
    
    x$geom <- apply(geojson_wkt(raw)["geometry"],1,unlist) # geojson to WKT
    }
  
  data <- x
  
  # Original version for processing geojson
  # if(g == "true"){
  #   d <- geojsonsf::geojson_sf(raw)
  #   # correct dates to postgres format
  #   dates <- grep("date",colnames(d))
  #   for(i in 1:length(dates)){
  #     eval(parse(
  #       text = paste0("d$",colnames(d)[dates[i]],"<- lapply(d$",colnames(d)[dates[i]],",date_correction)")
  #     )) # Can't get this to work any other way - other column selectors bring sf geom column along too
  #   }
  #   
  #   d$guid <- as.UUID(d$GlobalID)
  #   d <- d[,-which(colnames(d)== "OBJECTID" | colnames(d) == "GlobalID")] #Drop AGOL id fields
  #   
  #   tracking <- which(colnames(d) == "last_edited_user" | colnames(d) == "last_edited_date" | colnames(d) == "created_user" | colnames(d) == "created_date")
  #   colnames(d)[tracking] <- paste("source_",colnames(d)[tracking],sep="")    #change tracking column names
  #   
  #   dates <- grep("Date",fromJSON(raw)$fields$type)
  #   
  #   data <- d
  # }else{
  #   d <- fromJSON(raw)
  #   
  #   #Extract data frame and fields
  #   
  #   dates <- grep("Date",d$fields$type)
  #   x[dates] <- lapply(x[dates],date_correction) # correct dates to postgres format
  #   
  #   x$guid <- as.UUID(x$GlobalID)
  #   x <- x[,-which(colnames(x)== "OBJECTID" | colnames(x) == "GlobalID")] #Drop AGOL id fields
  #   
  #   tracking <- which(colnames(x) == "last_edited_user" | colnames(x) == "last_edited_date" | colnames(x) == "created_user" | colnames(x) == "created_date")
  #   colnames(x)[tracking] <- paste("source_",colnames(x)[tracking],sep="")    #change tracking column names
  #   
  #   data <- x
  # }
  
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
  
  r$data <- data
  r$attach <- attach
  return(r)
}

# Import to table function ----

null_val <- function(con,x){
  if(!isTruthy(x)){
    return('NULL')
  }
  if(is.POSIXct(x)){
    return(as.character(x))
    #return(null_timestamp_val(x))
  }
  if(is.UUID(x)){
    return(paste0("'",as.UUID(x),"'"))
  }
  if(is.numeric(x)){
    return(null_num_val(x))
  }
  if(is.character(x)){
    return(null_text_val(con,x))
  }
}

import_table <- function(con,
                         schema,
                         table,
                         import_type,
                         import_source,
                         import_notes,
                         import_geometry,
                         import_attach,
                         data,
                         attach,
                         f,
                         sz){

  r <- list("error"=NULL,"output"=NULL)
  
  if(nrow(data) == 0 || !isTruthy(data)){
    r$error <- "No data to import"
    print(r$error)
    return(r)
  }
  
  # Get matching columns in target table
  q_col <- paste0("SELECT column_name FROM information_schema.columns WHERE table_schema = '",schema,"' AND table_name  = '",table,"'")
  cols <- dbGetQuery(con,q_col)
  import_cols <- colnames(data)[which(colnames(data) %in% unlist(cols))]
  
  if(sum(colnames(data) %in% unlist(cols)) == 0){
    r$error <- "No columns match"
    print(r$error)
    return(r)
  }
  
  # Import insert string
  q0 <- paste0("INSERT INTO records.imports (source_type,source,notes,source_attachments,schema,table_name) VALUES (",
               import_type,",",
               null_text_val(con,import_source),",",
               null_text_val(con,import_notes),",",
               import_attach,",",
               paste0("'",schema,"'"),",",
               paste0("'",table,"'"),
               ") RETURNING id")
  
  imp <- dbGetQuery(con,q0)$id
  
  # Handle geometry column
  if(import_geometry){
    # If importing geometry, move geom column to beginning
    geom_ind <- which(import_cols == "geom")
    import_cols <- c("geom",import_cols[-c(geom_ind)])
  }else{
    # Remove geom column if in dataframe for import
    if("geom" %in% import_cols){
      import_cols <- import_cols[-which(import_cols == "geom")]
    }
  }
  
  # Target table values for insert - without geom
  if(!import_geometry){
    # Parse rows of data into tuples
    d_parse <-  paste(
      "(",
      apply(
        apply(data[import_cols],c(1,2),function(x) null_val(con,x))
        ,1,function(x) paste(x,collapse=","))
      ,
      ",",imp,")"
    )
  }
  
  # Target table values for insert - with geom
  if(import_geometry){
    # Parse rows of data into tuples
    d_parse <- 
      paste(
        paste("(",
               "ST_GeomFromText('", data$geom, "',27700),",sep=""),
        apply(apply(data[import_cols[2:length(import_cols)]],c(1,2),function(x) null_val(con,x)),1,function(x) paste(x,collapse=",")),
        ",",imp,")"
               )
      }
  

  # Form the insert string with and without attachments
  if(import_attach & isTruthy(attach)){
    
    q_att <- paste0("INSERT INTO ",schema,".",table,"_attach (guid,rel_guid,att_type,att_name,att_size,att) VALUES \n")

    Q_att <- NULL
    for(i in 1:nrow(attach)){
      
      # Download attachments and convert to binary
      if(attach[i,c("att_type")] == "image/jpeg"){
        tf <- tempfile(fileext=".jpeg")
        download.file(attach[i,c("url")], tf, mode = "wb")
        
        # Compress images
        if(attach[i,c("att_size")] > f * 1000){
          img <- magick::image_read(tf)
          
          g <- magick::geometry_size_pixels(width = sz, height = sz, preserve_aspect = TRUE)
          image_write(image_resize(img,geometry =g),tf,quality=20, compression = "JPEG")
          attach[i,c("att_size")] <- image_info(image_read(tf))$filesize
        }
        
        z <- readRaw(tf)
        attach[i,c("att")] <- paste0("\\x", paste(z$fileRaw, collapse = ""))
        
        # construct attachment values string
        Q_att <- c(Q_att,
                paste0("('",
                       attach[i,c("guid")],
                       
                       "', (SELECT guid FROM tab WHERE guid = '",attach[i,c("rel_guid")],"'),'",
                       attach[i,c("att_type")],"','",
                       attach[i,c("att_name")],"',",
                       attach[i,c("att_size")],",'",
                       attach[i,c("att")] ,"')")
        )
      }
    }
    
    # Final query string for import with attachments
    q3 <- paste0(
      "WITH imp AS (",q0,"), \n",
      "tab AS (
                    INSERT INTO ",schema,".",table," (",paste(import_cols,collapse = ","),",import) VALUES \n "
      , paste(d_parse,collapse = ", \n "),
      " ON CONFLICT (guid) DO NOTHING \n 
                 RETURNING guid",
      ") \n ",
      q_att, 
      paste(Q_att,collapse = ", \n "),
      " \n ON CONFLICT (guid) DO NOTHING; "
    )
  }else{
    # Target table insert string - 1st chunk
    q1 <- paste0("INSERT INTO ", schema,".",table ," (",paste(import_cols,collapse = ","),",import) VALUES \n ")
    
    if("guid" %in% colnames(data)){
      q1 <- pasteo(q1," \n ON CONFLICT (guid) DO NOTHING;")
    }

    # BATCH THE INSERT INTO BATCHES OF SIZE N = 2000
    n<-2000
    max <- ceiling(nrow(d) / n)
    q3 <- NULL
    
    for(i in 1:max){
      d_parse_0 <- d_parse[((i-1)*n):(min(nrow(d),(i*n)))]
    
      Q <- paste(d_parse_0,collapse = ", \n ")
      
      q1 <- paste0(
        "INSERT INTO ", schema,".",table ," (",paste(import_cols,collapse = ","),",import) VALUES \n "
      ) 
      
      q3 <- paste0(q3,"BEGIN; \n ",q1,paste(Q,collapse = ", \n "),
                   "; \n COMMIT; \n ")
      }
    }
  
      r$output <- dbGetQuery(con,q3)
      return(r)
    }



import_agol <- function(con, 
                        u, # service url
                        w, # where clause
                        g, # include geometry?
                        a, # include attachments ?
                        f, # max attachment size in kb
                        d, # max width / height to resize to
                        s, # schema to import to
                        t # table to import to
                        ){
  result <- list(error = NA, d = NA, a = 0)
  
  f <- fetch_agol(con, u, w, g, a)
  
  if(!is.na(f$error)){
    result$error <- f$error # What about invalid where clause?
  }
  if(!is.na(f$data) && nrow(f$data) == 0){
    result$error <- "Inputs valid but no data to import!"
  }
  if(!is.na(f$data) && nrow(f$data) > 0){
    data <- f$data
    attach <- f$attach
    
    
    # Problem here for changing SRS when not table owner, or service not same SRS as 27700?
    if(class(data)[[1]] == "sf"){
      
      # Need to write to temporary table then insert into desired table
      write <- rpostgis::pgWriteGeom(conn = con, 
                                     name = c(s,paste0(t,"_temp")),
                                     data.obj = data,
                                     geom = "geometry",
                                     overwrite = FALSE,
                                     partial.match = TRUE,
                                     upsert.using = c("guid")
                                     )
      
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
    result$d <- write
    
    if(a == TRUE){
      for(i in 1:nrow(attach)){
        #Check attachment not already in attachment table
        dl <- dbGetQuery(con, 
                         paste0("SELECT COUNT(guid) FROM (SELECT guid FROM ",s,".",t,"_attach WHERE guid IN ('b98dc6d4-a244-4bbf-9c61-9f8dc8f1b6cb')) AS A")
        ) 
        if(dl == 0){
          #Download and convert attachment to binary if jpeg
          
          if(attach[i,c("att_type")] == "image/jpeg"){
            tf <- tempfile(fileext=paste0(".",
                                          substr(attach[i,"att_type"],7,10000)
            )
            )
            download.file(attach[i,c("url")], tf, mode = "wb")
            if(attach[i,c("att_size")] > f * 1000){
              img <- magick::image_read(tf)
              
              g <- magick::geometry_size_pixels(width = d, height = d, preserve_aspect = TRUE)
              image_write(image_resize(img,geometry =g),tf,quality=20, compression = "JPEG")
            }
            
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
            result$a <- result$a + insert_attach
          }
        } 
      }
    }
  }
  return(result)
}
# Database connection function  ----
drv <- dbDriver("PostgreSQL", max.con = 100)
fenDb <- function(u, p){
  dbPool(drv, 
         user = u, 
         password = p,
         host = "data-fht.postgres.database.azure.com", 
         port = 5432, 
         dbname = "fen_database")
}

fenDb0 <- function(u, p){
  dbConnect(drv, 
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

# App database connect pool ----
con_global <- fenDb("fenportal","Alkal1n3F3ns!")

onStop(function() {
  pool::poolClose(con_global)
})

# Database tables loading function ----

app_tables <- function(tables,t){
  # Query definitions ----
  q_sites0 <- "SELECT id, site, county, geom AS geom FROM spatial.fen_sites ORDER BY site"
  q_subsites0 <- "SELECT 
                            ss.id, 
                            ss.site, 
                            s.site AS site_name, 
                            ss.subsite, 
                            ss.geom AS geom 
                            FROM 
                              spatial.fen_subsites ss,
                              spatial.fen_sites s
                            WHERE 
                              ss.site = s.id
                            ORDER BY site, subsite"
  q_surveys <- "SELECT
                            s.id AS id,
                            s.survey,
                            s.survey_type AS survey_type,
                            s.start_date,
                            s.end_date,
                            s.start_year,
                            s.end_year,
                            s.source,
                            s.project AS projectid,
                            s.sharing AS sharing_code,
                            s.copyright,
                            s.description AS description,
                            s.url,
                            s.created_user,
                            s.last_edited_user,
                            s.created_date,
                            s.last_edited_date,
                            s.status,
                            st.description AS survey_type_description,
                            sh.description AS sharing,
                            p.project AS project
                            FROM
                              records.surveys s,
                              records.projects p,
                              lookups.lookup_sharing sh,
                              lookups.lookup_survey_types st
                            WHERE
                              s.project = p.id AND
                              s.sharing = sh.code AND
                              s.survey_type = st.code
                            ORDER BY s.id"
  q_projects <- "SELECT * FROM records.projects ORDER BY project"
  q_plots <- "SELECT p.id,
                      ST_TRANSFORM(p.geom, 4326) AS geom,
                      p.site AS site,
                      p.subsite AS subsite,
                      p.group,
                      p.plot,
                      p.plot_reference,
                      p.gridref,
                      p.type,
                      p.transect_side,
                      p.dim,
                      p.note,
                      p.created_user,
                      p.created_date,
                      p.last_edited_user,
                      p.last_edited_date,
                      s.site AS site_name,
                      ss.subsite AS subsite_name
                      FROM (spatial.monitoring_vegetation p
                            LEFT JOIN spatial.fen_sites s ON p.site = s.id) LEFT JOIN
                            spatial.fen_subsites ss ON ss.id = NULLIF(p.subsite,0)
                      ORDER BY site_name, plot_reference"
  q_hydro_installs0 <- "SELECT 
                         A.id AS id,
                         A.install_name,
                         B.id AS site, B.site AS site_name,
                         A.install_type AS install_type,
                         C.description AS install_type_description,
                         A.install_date,
                         A.install_location,
                         A.install_reason,
                         A.installed_by,
                         A.install_depth,
                         A.install_protrusion,
                         A.install_geology,
                         A.install_hydrogeo,
                         A.install_hydroeco,
                         ST_TRANSFORM(A.geom, 4326) AS geom
                     FROM 
                        spatial.monitoring_hydro_installs A, 
                        spatial.fen_sites B, 
                        lookups.lookup_hydro_install C
                     WHERE 
                      A.site = B.id AND 
                      A.install_type = C.code 
                    ORDER BY B.site, A.install_name"
  # Download tables ----
  future_promise({
    con <- poolCheckout(con_global)
    if("sites" %in% t && !isTruthy(tables$sites0)){
      sites0 <- st_read(dsn = con, query = q_sites0, geometry_column = "geom")
    }else{
      sites0 <-NA
    }
    if("subsites" %in% t && !isTruthy(tables$subsites0)){
      subsites0 <- st_read(dsn = con, query = q_subsites0, geometry_column = "geom")
    }else{
      subsites0 <- NA
    }
    if("surveys" %in% t && !isTruthy(tables$surveys)){
      surveys <- dbGetQuery(con, q_surveys)
    }else{
      surveys <- NA
    }
    if("projects" %in% t && !isTruthy(tables$projects)){
      projects <- dbGetQuery(con, q_projects)
    }else{
      projects <- NA
    }
    if("plots" %in% t && !isTruthy(tables$plots)){
      plots <- st_read(dsn = con, query = q_plots, geometry_column = "geom")
    }else{
      plots <- NA
    }
    if("hydro_installs" %in% t && !isTruthy(tables$hydro_installs0)){
      hydro_installs0 <- st_read(dsn = con, query = q_hydro_installs0, geometry_column = "geom")
    }else{
      hydro_installs0 <- NA
    }
    poolReturn(con)
    return(list(
      "sites0" = sites0,
      "subsites0" = subsites0,
      "surveys" = surveys,
      "projects" = projects,
      "plots" = plots,
      "hydro_installs0" = hydro_installs0
    ))
  })%...>% (function(r){
    a <- 0
    
    if("sites" %in% t && !isTruthy(tables$sites)){
      tables$sites0 <- r$sites0
      tables$sites <- st_drop_geometry(r$sites0)
      a <- a + 1
    }
    if("subsites" %in% t && !isTruthy(tables$subsites)){
      tables$subsites0 <- r$subsites0
      tables$subsites <- st_drop_geometry(r$subsites0)
      a <- a + 1
    }
    if("surveys" %in% t && !isTruthy(tables$surveys)){
      tables$surveys <- r$surveys
      a <- a + 1
    }
    if("projects" %in% t && !isTruthy(tables$projects)){
      tables$projects <- r$projects
      a <- a + 1
    }
    if("plots" %in% t && !isTruthy(tables$plots)){
      tables$plots <- r$plots
      a <- a + 1
    }
    if("hydro_installs" %in% t && !isTruthy(tables$hydro_installs)){
      tables$hydro_installs0 <- r$hydro_installs0
      tables$hydro_installs <- st_drop_geometry(r$hydro_installs0)
      a <- a + 1
    }
  })
}

# App database tables and lookups ----

uksi_full <- NULL
uksi_pl_rec <- NULL
choices_uksi <- NULL
choices_uksi_1 <- NULL
choices_uksi_plants <- NULL
string_fspp <- NULL
string_afspp <- NULL
fspp <- NULL

uksi_load <- function(x){
  if(0 %in% x && !isTruthy(choices_uksi)){
    if(!isTruthy(uksi_full)){
      uksi_full <<- read.csv("./www/uksi.csv", header = TRUE, encoding = "UTF-8")
    }
    # Taxon names with qualifiers and authorities
    choices_uksi <<- uksi_full$nbn_taxon_version_key
    names(choices_uksi) <<- uksi_full$full_name
  }
  if(1 %in% x && !isTruthy(choices_uksi_1)){
    if(!isTruthy(uksi_full)){
      uksi_full <<- read.csv("./www/uksi.csv", header = TRUE, encoding = "UTF-8")
    }
    # Taxon names with qualifiers and without authorities
    choices_uksi_1 <<- uksi_full$nbn_taxon_version_key
    names(choices_uksi_1) <<- uksi_full$name
  }
  if(2 %in% x && !isTruthy(choices_uksi_plants)){
    if(!isTruthy(uksi_pl_rec)){
      uksi_pl_rec <<- read.csv("./www/uksi_pl_rec.csv",header = TRUE, encoding = "UTF-8")
    }
    # Taxon plant choices
    choices_uksi_plants <<- uksi_pl_rec$nbn_taxon_version_key
    names(choices_uksi_plants) <<- uksi_pl_rec$full_name
  }
  if(3 %in% x){
    if(!isTruthy(fspp)){
      fspp <<- read.csv("./www/fen_spp.csv",header = TRUE, encoding = "UTF-8")
    }
    
    string_fspp <<- paste0("('",paste(fspp$nbn_taxon_version_key_for_recommended_name,collapse="','"),"')")
    
    string_afspp <<- fspp[which(fspp$alkaline_fen == TRUE),"nbn_taxon_version_key_for_recommended_name"]
    string_afspp <<- paste0("('",paste(string_afspp,collapse="','"),"')")
  }
}

con_global0 <- poolCheckout(con_global)

# Record status
choices_status <-c(NULL,dbGetQuery(con_global0,"SELECT DISTINCT(status) AS status FROM records.records WHERE status IS NOT NULL ORDER BY status"))

# Verification categories
verification <- dbGetQuery(con_global0, "SELECT code, description FROM lookups.lookup_verification")
choices_verification <- verification$code
names(choices_verification) <- verification$description

# Data source sharing 
sh <- dbGetQuery(con_global0, "SELECT code, description FROM lookups.lookup_sharing")
choices_sh <- sh$code
names(choices_sh) <- sh$description 

#Data source types
st <- dbGetQuery(con_global0, "SELECT code, description FROM lookups.lookup_survey_types")
choices_st <- st$code
names(choices_st) <- st$description

squares_100k <- dbGetQuery(con_global0, "SELECT gridref, x , y FROM lookups.squares_100k")

poolReturn(con_global0)

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
    "record_year" = numeric(),
    "record_month" = numeric(),
    "record_date_start" = Date(),
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
