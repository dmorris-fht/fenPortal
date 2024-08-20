importObsUI <- function(id){
  ns <- NS(id)
  tagList(
    withSpinner(
      tagList(
        div(style="height:80vh",
        column(12,
               column(12,
                 h3("Import target notes"),
                 column(6,
                        HTML("<p>To import target notes and photos recorded using ArcGIS Online, enter the REST URL for the feature service 
                        containing the data and click 'Import data'.</p>
                        <p>You can find the URL for a service on its details page - you just need to add the layer number to the end. 
                        The layer must follow the template available <a href='https://fht.maps.arcgis.com/home/item.html?id=c513fe6f65494b8ca4ca671f60773729', target='_blank'>here</a>, and the service must be shared publicly (e.g. via a view layer) or the URL should include an authentication key.</p>
                        <p>Target notes already imported will be skipped - these should be edited in the database.</p><br>"),
                        textAreaInput(ns("agol_url"), label="Enter ArcGIS Online REST service URL", width = "100%", resize = "vertical", placeholder ="ArcGIS REST service URL"),
                        actionButton(ns("importAGOL"), label = "Import data")
                        )
               ,)
               
               )
      )
        ),
      id = ns("module"),
      type = 4,
      size = 2,
      proxy.height = "100%",
      hide.ui = TRUE,
      caption = "Loading module"),
    tags$script(src ="script.js"),
    tags$script(
      HTML(
        paste0("$('#",id,"-module').parent().removeClass('shiny-spinner-hidden')")
        )
      )
    )
  }

importObsServer <- function(id, login) {
  moduleServer(
    id,
    function(input, output, session) {
      role <- login$role
      user <- login$username
      password <- login$password
      
      # Module initialisation ---- 
      observe({
        runjs(
          paste0(
          "$('#",id,"-module').parent().addClass('shiny-spinner-hidden');
                 $('div[data-spinner-id=\\'",id,"-module\\']').css('display','inline')"
                 )
        )
      })
      
      # Reactives ----
      
      import <- reactiveValues(data = NA, attach = NA, agol = 0) # Reactive for data import
      
      # Form controls ----
      
      observe({
        if(isTruthy(input$agol_url)){
          shinyjs::enable("importAGOL")
        }else{
          shinyjs::disable("importAGOL")
        }
      })
      
      # AGOL import ----
      
      observeEvent(input$importAGOL,{
        req(input$agol_url)
        import_modal()
      })
      
      # Don't import
      observeEvent(input$import0,{
        removeModal()
      })
      
      # Make query to AGOL and import
      observeEvent(input$import1,{
        # Fetch AGOL data
        
        f <- fetch_agol(input$agol_url,NULL,TRUE,TRUE)
        
        if(isTruthy(f$data) && nrow(f$data)>0){
          con0 <- poolCheckout(con_global)
          guid <- dbGetQuery(con0,paste0("SELECT guid FROM spatial.monitoring_observations WHERE guid IN ",con_sql_string(f$data$guid)))
          cols <- dbGetQuery(con0,"SELECT column_name FROM information_schema.columns WHERE table_schema = 'spatial' AND table_name = 'monitoring_observations'; ")
          poolReturn(con0)
          
          # Check column names and then subset by new observations only
          if(all(colnames(f$data) %in% unlist(cols))){
            import$data <- f$data[!(f$data$guid %in% guid$guid),]
            import$attach <- f$attach[!(f$attach$rel_guid %in% guid$guid),]
            import$agol <- 1 #agol verifier
          }else{
            import$agol <- 0
            import_agol_error()
            return("Error")
            }
          }else{
            import$agol <- 0
            import_agol_error()
            return("Error")
          }
        
        # No data
        if(nrow(import$data) == 0 || !isTruthy(import$data)){
          import_NA_modal()
          return("No data to import")
        }
        
        # Start import
        
        req(import$agol == 1)
        req(import$data)
        
        removeModal()
        import_progress_modal()
        
        future_promise({
          con0 <- fenDb0(user,password)
          
          # Construct monitoring observations insert string
          Q1 <- NULL
          for(i in 1:nrow(import$data)){
            d <- import$data[i,]
            Q1 <- c(Q1,
                    paste0("(",
                           "ST_GeomFromText('", d$geom, "',27700),",
                           null_num_val(d$site),",",
                           null_num_val(d$survey),",",
                           null_text_val(con0,d$obs_type),",",
                           null_timestamp_val(d$date),",",
                           null_num_val(d$direction),",",
                           null_text_val(con0,d$observer),",",
                           null_text_val(con0,d$label),",",
                           null_text_val(con0,d$notes),",",
                           null_text_val(con0,d$source_created_user),",",
                           null_timestamp_val(d$source_created_date),",",
                           null_text_val(con0,d$source_last_edited_user),",",
                           null_timestamp_val(d$source_last_edited_date),",'",
                           d$guid
                           ,"')"
                           )
                    )
            }
          
          q1 <- paste0("INSERT INTO spatial.monitoring_observations (geom,site,survey,obs_type,date,
                                                                    direction,observer,label,notes,
                                                                    source_created_user,source_created_date,
                                                                    source_last_edited_user,source_last_edited_date,guid)  VALUES \n",
                       paste(Q1,collapse = ", \n "),
                       " \n ON CONFLICT (guid) DO NOTHING"
                       )
          
          # Download + compress attachments and construct insert string
          if(isTruthy(import$attach) && nrow(import$attach) > 0){
            q2 <- "INSERT INTO spatial.monitoring_observations_attach (guid,rel_guid,att_type,att_name,att_size,att) VALUES \n"
            f <- 500 # Max file size in kB
            sz <- 2000 # Resize image max dim in px
            
            Q2 <- NULL
            for(i in 1:nrow(import$attach)){
              
              # Download attachments and convert to binary
              if(import$attach[i,c("att_type")] == "image/jpeg"){
                tf <- tempfile(fileext=".jpeg")
                download.file(import$attach[i,c("url")], tf, mode = "wb")
                
                # Compress images
                if(import$attach[i,c("att_size")] > f * 1000){
                  img <- magick::image_read(tf)
                  
                  g <- magick::geometry_size_pixels(width = sz, height = sz, preserve_aspect = TRUE)
                  image_write(image_resize(img,geometry =g),tf,quality=20, compression = "JPEG")
                  import$attach[i,c("att_size")] <- image_info(image_read(tf))$filesize
                }
                
                z <- readRaw(tf)
                import$attach[i,c("att")] <- paste0("\\x", paste(z$fileRaw, collapse = ""))
                
                # construct attachment values string
                Q2 <- c(Q2,
                        paste0("('",
                               import$attach[i,c("guid")],
                               
                               "', (SELECT guid FROM obs WHERE guid = '",import$attach[i,c("rel_guid")],"'),'",
                               
                               # "','", import$attach[i,c("rel_guid")],"','",
                               
                               import$attach[i,c("att_type")],"','",
                               import$attach[i,c("att_name")],"',",
                               import$attach[i,c("att_size")],",'",
                               import$attach[i,c("att")] ,"')")
                        )
                }
              }
            # construct final query string
            q3 <- paste0(
              "WITH obs AS (",q1,"\n RETURNING guid", ") \n",
              q2, 
              paste(Q2,collapse = ", \n "),
              " \n ON CONFLICT (guid) DO NOTHING"
            )
          }else{
            q3 <- q1
          }
          
          r <- dbGetQuery(con0,q3)
          
          dbDisconnect(con0)
          return(r)
          
        })%...>%(function(r){
          if(isTruthy(r)){
            removeModal()
            import_success_modal()
          }else{
            import_error_modal()
          }
        })
        })
      
      # Modal functions ----
      
      #Modal for verifying upload
      import_modal <- function() {
        ns <- session$ns
        modalDialog(
          div(
            p("Are you sure you want to import the data to the database?"),
            actionButton(ns("import1"),label = "Yes"),
            actionButton(ns("import0"),label= "No")
            ,style="width:100%;text-align:center")
          , footer=NULL,size="s",easyClose=FALSE,fade=TRUE) %>% showModal()
      }
      
      # Modal for AGOL error
      import_agol_error <- function() {
        ns <- session$ns
        modalDialog(
          div(
            p("No response from ArcGIS Online source or no data found")
            ,style="width:100%;text-align:center")
          , footer=NULL,size="s",easyClose=TRUE,fade=TRUE) %>% showModal()
      }
      
      #Modal for upload success
      import_success_modal <- function() {
        ns <- session$ns
        modalDialog(
          div(
            h4("Data successfully imported")
            ,style="width:100%; text-align:center")
          , footer=NULL,size="s",easyClose=TRUE,fade=TRUE) %>% showModal()
      }
      
      #Modal for no data to import
      import_NA_modal <- function() {
        ns <- session$ns
        modalDialog(
          div(
            h4("No data to import")
            ,style="width:100%; text-align:center")
          , footer=NULL,size="s",easyClose=TRUE,fade=TRUE) %>% showModal()
      }
      
      #Modal for upload error
      import_error_modal <- function() {
        ns <- session$ns
        modalDialog(
          div(
            h4("Import error")
            ,style="width:100%; text-align:center")
          , footer=NULL,size="s",easyClose=TRUE,fade=TRUE) %>% showModal()
      }
      
      #Modal for upload progress
      import_progress_modal <- function(){
        ns <- session$ns
        modalDialog(
          div(
            tags$h4("Importing",class="loading")
            ,style="width:100%; text-align:left")
          , footer=NULL,size="s",easyClose=FALSE,fade=TRUE) %>% showModal()
      }
      
    }
  )
}