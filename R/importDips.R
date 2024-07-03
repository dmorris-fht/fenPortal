dipsImportUI <- function(id) {
  ns <- NS(id)
  tagList(
    withSpinner(
          tagList(
            column(12,
              column(12,
                     column(6,
                     h3("Import manual dipwell measurements"),
                     HTML("<p>Use this page to import manual dipwell measurements to the database. First, specify one of the following two types of data source:</p> 
                       <ul>
                          <li><b>ArcGIS Online feature service:</b> Specify the REST URL for the layer containing the table of dipwell measurements. 
                          You can find the URL for a service on its details page - you just need to add the layer number to the end. To import from a feature service, 
                          it must be shared publicly (e.g. via a view layer) and follow the template available <a href='https://fht.maps.arcgis.com/home/item.html?id=804697e8efb843e7adf9ca3badcd56e3', target='_blank'>here</a>.</li>
                          <li><b>Comma-separated value (.csv) file:</b> Upload a .csv file using the template available <a href='./templates/dips_import_template.csv'>here</a>. Using this method, data to import must be from the same site.</li>
                          </ul>")
                     )
                     ),
              column(6,
                     column(12,
                            h4("Data to import"),
                            selectizeInput(ns("importMode"),label = "Choose import method:", 
                                           choices = c("ArcGIS Online","Import .csv file"),
                                           options = list(
                                             placeholder = 'Select an import option',
                                             onInitialize = I('function() { this.setValue(""); }')
                                              )
                                           )
                     ),
                     hidden(div(id = ns("AGOL"), style = "",
                     #Form to define the query to AGOL
                     column(12,
                            textAreaInput(ns("agol_url"), label="Enter REST URL of table of dipwell measurements", width = "100%", resize = "vertical", placeholder ="https://services9.arcgis.com/J5mi5KNMFx93FsQu/arcgis/rest/services/hydro_monitoring_template/FeatureServer/1")
                            ),
                     column(12,
                            h5("Optional query parameters:")
                            ),
                     column(6, 
                            selectInput(ns("choose_site_agol"),label="Choose site(s):",choices=NULL,multiple=TRUE),
                            selectInput(ns("choose_install"),label="Choose install(s):",choices=NULL,multiple=TRUE)
                            ),
                    column(6,
                          dateInput(ns("date0"),label="Start date:",min="2000-01-01",value=NA,format="dd/mm/yyyy")
                          ),
                    column(6,
                          dateInput(ns("date1"),label="End date:",max="2100-12-31",value=NA,format="dd/mm/yyyy")
                          ),
                   column(12,
                          br(),
                          actionButton(ns("queryDips"), label = "Query data"),
                          actionButton(ns("queryClear"), label = "Clear query")
                          )
              )),
              hidden(div(id = ns("csv"),
                         column(12, 
                                p("Choose a .csv file to import. The date-time column must be formated as yyyy-mm-dd hh:mm, e.g. '2023-04-14 13:00'."),
                                fileInput(ns("choose_csv"),label="Choose .csv file to import:", 
                                          buttonLabel = "Browse...",
                                          placeholder = "No file selected",
                                          accept = ".csv"),
                                selectInput(ns("choose_site_csv"), label = "Choose site", choices = c())
                                )
                  )),
              
              #Form to set import parameters
              column(12,
                            hidden(div(id = ns("importAGOLtext") , 
                                       h4("Import data"),
                                       HTML("<p>Click the button below to import the query results into the database. Note that:</p>
                               <ul>
                               <li>If a subset of data to import is already present in the database 
                               (identified by the universal unique identifier column), then this will be skipped.</li>
                               <li>Checking 'Import attachments' will import files attached to dip records. Only images in jpeg format will be imported,
                               and files more than 500 kB will frst be compressed. Importing attachments will make the import slower.</li>
                               </ul>
                               "),
                                              textAreaInput(ns("import_notes_agol"), label = "Import notes", width = "100%", resize = "vertical"),
                                              checkboxInput(ns("import_attachments"), label = "Import attachments",value = FALSE),
                                              actionButton(ns("importAGOL"), label = "Import data")
                            )),
                            hidden(div(id = ns("importCSVtext"),
                                       h4("Import data"),
                                       HTML("<p>Click the button below to import the .csv file to the database. If the database already contains a record of a measurement for a given installation and date/time
                                          then that record will be updated rather than a new record added.</p>"),
                                              textAreaInput(ns("import_notes_csv"), label = "Import notes", width = "100%", resize = "vertical"),
                                              actionButton(ns("importCSV"), label = "Import data")
                            ))
              )
              
                  ),
              
              # DT preview
              column(6,
                     h4("Data preview"),
                       withSpinner(DT::DTOutput(ns("previewTable")),type=4,caption="Loading preview"),
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

dipsImportServer <- function(id,login,tables) {
  moduleServer(
    id,
    function(input, output, session) {
      role <- login$role
      user <- login$username
      password <- login$password
      
      # Module initialisation ---- 
      isolate({
        app_tables(tables, c("hydro_installs"))
      })
      
      observe({
        req(tables$hydro_installs)
        
        runjs(
          paste0(
            "$('#",id,"-module').parent().addClass('shiny-spinner-hidden');
                 $('div[data-spinner-id=\\'",id,"-module\\']').css('display','inline')"
          )
        )
      })

      choices_site <- reactive({
        if(isTruthy(tables$hydro_installs)){
          c <- unique(tables$hydro_installs$site)
          names(c) <- unique(tables$hydro_installs$site_name)
          return(c)
        }
        else{
          return(c(""))
        }
      })
      
      choices_install <- reactive({
        if(isTruthy(tables$hydro_installs)){
          if(isTruthy(input$choose_site_agol)){
            inst <- tables$hydro_installs[tables$hydro_installs$site == as.numeric(input$choose_site_agol),]
            c <- inst$id
            names(c) <- paste(inst$site_name, inst$install_name,sep = " - ")
            return(c)
          }else{
            return(c(""))
          }
        }
        else{
          return(c(""))
        }
      })
      
      # Reactives ----
      
      import <- reactiveValues(data = NA, attach = NA, agol = 0, csv = 0) # Reactive for data import
      
      # Form controls ----
      
      #Set initial selectInput choices
      observeEvent(input$importMode,{
        shinyjs::hide("importAGOLtext")
        shinyjs::hide("importCSVtext")

        if(input$importMode == "ArcGIS Online"){  
          shinyjs::show("AGOL")
          shinyjs::hide("csv")
            
          updateSelectInput(session, inputId = "choose_site_agol", choices = choices_site())
          }
        
        if(input$importMode == "Import .csv file"){
          shinyjs::hide("AGOL")
          shinyjs::show("csv")
            
          updateSelectInput(session, inputId = "choose_site_csv", choices = choices_site())
          }
          })
      
      #update installs input choices when site is chosen
      observe({
        updateSelectInput(session, inputId = "choose_install", choices = choices_install())
      })
      
      # DT preview ----
      
      output$previewTable <- DT::renderDT({
        data.frame(
          install_name = character(),dip_date_time = character(), dip_measurer = character(), dip_depth_top = numeric()
          )
        }
        ,
        server = TRUE,
        rownames = FALSE,
        colnames = c("Install","Dip date / time", "Recorder", "Dip (cm)"),
        options = list(
          language = list(
            infoEmpty = "No data",
            emptyTable = "No data to display"
          ),
          pageLength = 25,
          dom = 'tpli',
          extensions = c("FixedHeader", "Scroller"),
          fixedHeader = TRUE,scrollY = "50vh",
          columnDefs = list(
            list(className = 'dt-center', targets = c(3))
            )
        )
        )
      
      proxy_DT <- DT::dataTableProxy("previewTable")
      
      observe({
        req(import$data)
        req(import$csv == 1 || import$agol == 1)
        x <- import$data[,c("install_name","dip_date_time","dip_measurer","dip_depth_top")]
        proxy_DT %>% DT::replaceData(data = x,resetPaging = TRUE, rownames = FALSE)
      })
      
      # AGOL import ----
      
      #Modal for empty AGOL request
      import_agol_error <- function() {
        ns <- session$ns
        modalDialog(
          div(
          p("No response from ArcGIS Online source or no data found")
          ,style="width:100%;text-align:center")
          , footer=NULL,size="s",easyClose=TRUE,fade=TRUE)
        }
      
      #Make query to AGOL
      observeEvent(input$queryDips,{
        req(input$importMode == "ArcGIS Online")
        req(input$agol_url)
        req(tables$hydro_installs)
        
        showSpinner("previewTable")
        
        #Where clause for date range
        q_d <- ""
        
        if(isTruthy(input$date1) && isTruthy(input$date0)){
          q_d <- paste("dip_date_time >= DATE '", input$date0, " 00:00:00' AND dip_date_time <= DATE '", input$date1, " 00:00:00'", sep="")
        }else{
          if(isTruthy(input$date1)){
            q_d <- paste("dip_date_time <= DATE '", input$date1, " 00:00:00'", sep="")
          }
          if(isTruthy(input$date0)){
            q_d <- paste("dip_date_time >= DATE '", input$date0, input$date1, " 00:00:00'", sep="")
          }
        }
        
        #Where clause for installs
        q_i <- ""
        
        if(isTruthy(input$choose_install)){
          q_i <- paste("install IN (",paste(input$choose_install,sep=",",collapse=","),")",sep="",collapse="")
        }
        
        if(isTruthy(input$choose_site_agol)){
          q_i <- paste("install IN (",paste(installs$table[installs$table$siteid %in% input$choose_site_agol,c("installid")],sep=",",collapse=","),")",sep="",collapse="")
        }

        #Where clause to use in url
        if(q_d != "" && q_i != ""){
          w <- paste(q_d, " AND ", q_i,sep="")
        }else{
          if(q_d != ""){
            w <- q_d
          }
          if(q_i != ""){
            w <- q_i
          }
          if(q_d == "" && q_i == ""){
            w <- "*"
          }
        }

        f <- fetch_agol(input$agol_url,w,FALSE,TRUE)

          if(isTruthy(f$data) && nrow(f$data)>0){
            shinyjs::hide("noPreview")
            shinyjs::show("dipsPreview")
            
            t <- merge(f$data, tables$hydro_installs, 
                       by.x = "install", 
                       by.y = "id") 
            t$install_name <- apply(t[c("site_name","install_name")],1,function(x){paste(x[1],x[2],sep=" - ")}) # Add site to install name

            import$data <- t
            import$attach <- f$attach
            import$agol <- 1 #agol verifier

            shinyjs::show("importAGOLtext")
            shinyjs::hide("importCSVtext")
        }
        else{
          import$agol <- 0
          showModal(import_agol_error())
        }
        
        hideSpinner("previewTable")
      })
      
      #Clear AGOL query
      observeEvent(input$queryClear,{
        req(input$importMode == "ArcGIS Online")
        req(import$data)
        import$data <- import$data[0,]
        
        updateTextInput(session,"agol_url",value = NA)
        updateSelectInput(session,"choose_site_agol",selected = "")
        updateSelectInput(session,"choose_install",selected = "")
        updateDateInput(session,"date0",value = NA)
        updateDateInput(session,"date1",value = NA)
        
        shinyjs::hide("importAGOLtext")
      })
      
      #Import AGOL data to database
      observeEvent(input$importAGOL,{
        req(import$agol == 1)
          #Yes/no popup to verify import
          showModal(import_modal())
        })

          #Yes - Insert new row into dips install table
          # observeEvent(input$import1,{
          #   removeModal()
          #   
          #   shiny::validate(need(input$agol_url, "Please include an ArcGIS Online REST URL"))
          #   req(input$agol_url)
          #   
          #   showModal(import_progress_modal())
            
            # #Connect to database
            # 
            # #Insert dip import row
            # query <- paste("INSERT INTO hydro_monitoring.dips_imports (source_type,source,notes,source_attachments) VALUES (",
            #                1,",",
            #                "'", input$agol_url,"',",
            #                "'", input$import_notes_agol,"',",
            #                input$import_attachments,
            #                ") RETURNING id", sep="")
            # 
            # import_info <- dbGetQuery(con, query)
            # import$data$dips_import <- import_info$id
            # 
            # if(!is.na(import_info$id)){
            #   insert <- pgInsert(
            #     con,
            #     name = c("hydro_monitoring","dips"),
            #     data.obj = import$data,
            #     df.mode = FALSE,
            #     partial.match = TRUE,
            #     overwrite = FALSE,
            #     upsert.using = c("guid")
            #   )
            #   
            #   #Import attachments
            #   if(isTruthy(input$import_attachments) && nrow(import$attach)>0){
            #     import$attach$att <- NA
            #     
            #     #Download and convert attachments to binary
            #     for(i in 1:nrow(import$attach)){
            #       tf <- tempfile(fileext=paste0(".",
            #                                     substr(import$attach[i,"att_type"],7,10000)
            #       )
            #       )
            #       download.file(import$attach[i,c("url")], tf, mode = "wb")
            #       z <- readRaw(tf)
            #       r <- import$attach[i,]
            #       r$att <- paste0("\\x", paste(z$fileRaw, collapse = ""))
            #       
            #       # dbExecute(con, "INSERT INTO hydro_monitoring.dips_attach (rel_guid,att_name,att_type,att_size,att) VALUES ($1,$2,$3,$4,$5)",
            #       #           list(r$rel_guid,r$name,r$contentType, r$size ,r$att))
            #     }
            #     insert_attach <- pgInsert(
            #       con,
            #       name = c("hydro_monitoring","dips_attach"),
            #       data.obj = import$attach,
            #       df.mode = FALSE,
            #       partial.match = TRUE,
            #       overwrite = FALSE,
            #       upsert.using = c("guid")
            #     )
            #   }
            
      #       
      #       
      #       
      #       
      #       
      #       
      #       
      #         
      #         if(insert == TRUE){
      #           removeModal()
      #           showModal(import_success_modal())
      #         }
      #         else{
      #           showModal(import_error_modal())
      #         }
      #         
      #         
      #       }
      #       else{
      #         #Delete the import info record if the import fails
      #         dbGetQuery(con,paste("DELETE FROM hydro_monitoring.dips_imports WHERE id = ",import_info$id,sep=""))
      #         #Import fail modal
      #         removeModal()
      #         showModal(import_error_modal())
      #       }
      #     })
      #   }
      #   else{
      #     showModal(no_data_error_modal())
      #   }
      #   
      #   
      # })
      
      # CSV import ----
      
      #Import csv file
      observe({
        req(input$importMode == "Import .csv file")
        req(input$choose_csv)
        
        #Import csv and load data table
        file <- input$choose_csv
        ext <- file$type
        
        req(file)
        shiny::validate(need(ext == "text/csv", "Please upload a csv file"))
        
        req(tables$hydro_installs)
        req(input$choose_site_csv)
        isolate({
          d <- read.csv(file$datapath, header = TRUE, encoding = "UTF-8")

          colnames(d)[1] <- "install_name"
          d$install_name <- apply(d[c("install_name")],1,trimws)
          d$dip_null <- as.integer(d$dip_null)
          d$site <- as.integer(input$choose_site_csv)
          
          i <- merge(d, tables$hydro_installs, 
                     by.x = c("site","install_name"), 
                     by.y = c("site","install_name"))
          
          import$data <- i[,c("id","install_name","dip_date_time","dip_measurer","dip_depth_top","dip_notes","dip_null")]
          colnames(import$data)[1] <- c("install")

          # Check cols contain valid data
          if(
            isTruthy(import$data) &&
            (nrow(import$data) > 0) &&
            sum(apply(import$data[c("dip_date_time")],1,function(x){!IsDate(x,"%Y-%m-%d %H:%M")})) == 0 &&
            sum(apply(import$data[c("dip_depth_top")],1,
                      function(x){
                        !(is.numeric(x) && floor(as.numeric(x)) == as.numeric(x) ||
                        is.na(x))
                        })) == 0 &&
            sum(!(import$data$dip_null %in% c(NA,0,1,2,3,4,5,6))) == 0
            ){
            import$csv <- 1
            
            shinyjs::hide("importAGOLtext")
            shinyjs::show("importCSVtext") 
            }else{
              import$csv <- 0
              invalid_data()
              }
          })

        
      })

      #Import csv data to database
      observeEvent(input$importCSV,{
        if(import$csv == 1){
          showModal(import_modal())
        }
        else{
          showModal(no_data_error_modal())
        }
      })
      
      # Upload ----
      
      # Upload event
      observeEvent(input$import1,{
        removeModal()
        showModal(import_progress_modal())
        
        # CSV import
        if(import$csv == 1 && import$agol == 0){
          future_promise({
            con0 <- fenDb0(user,password)
            
            # Construct query
            
            q0 <- paste0("INSERT INTO hydro_monitoring.dips_imports (source_type,source,notes) VALUES (",
                         2,",",
                         null_text_val(con0,input$choose_csv),",",
                         null_text_val(con0,input$import_notes_csv),
                         ") RETURNING id")
            q1 <- paste0("WITH imp AS (",q0,") \n ",
                         "INSERT INTO hydro_monitoring.dips (install,dip_date_time,dip_measurer,dip_depth_top,dip_notes,dip_null,dips_import) VALUES "
            )
            
            ## Construct dips values string
            Q <- NULL
            for(i in 1:nrow(import$data)){
              d <- import$data[i,]
              Q <- c(Q,
                     paste0("(",
                            d$install,",",
                            null_timestamp_val(d$dip_date_time),",",
                            null_text_val(con0,d$dip_measurer),",",
                            null_num_val(d$dip_depth_top),",",
                            null_text_val(con0,d$dip_notes),",",
                            null_num_val(d$import_null),
                            ",(SELECT id FROM imp))"
                     )
              )
            }
            
            q2 <- paste0(q1,paste(Q,collapse = ", \n ")," RETURNING (SELECT id FROM imp)")
            
            r <- dbGetQuery(con0,q2)
            
            dbDisconnect(con0)
            return(r)
          })%...>%(function(r){
            if(isTruthy(r)){
              removeModal()
              showModal(import_success_modal())
            }else{
              showModal(import_error_modal())
            }
          })
        }
        
        # AGOL import
        if(import$agol == 1 && import$csv == 0){
          future_promise({
            con0 <- fenDb0(user,password)
            
            # Construct query
            
            ## Construct dips import string
            q0 <- paste0("INSERT INTO hydro_monitoring.dips_imports (source_type,source,notes,source_attachments) VALUES (",
                         1,",",
                         null_text_val(con0,input$agol_url),",",
                         null_text_val(con0,input$import_notes_csv),",",
                         input$import_attachments,
                         ") RETURNING id")
            
            ## Construct dips values string
            Q1 <- NULL
            for(i in 1:nrow(import$data)){
              d <- import$data[i,]
              Q1 <- c(Q1,
                     paste0("(",
                            d$install,",",
                            null_timestamp_val(d$dip_date_time),",",
                            null_text_val(con0,d$dip_measurer),",",
                            null_num_val(d$dip_depth_top),",",
                            null_text_val(con0,d$dip_notes),",",
                            null_num_val(d$import_null),
                            ",(SELECT id FROM imp),'",
                            d$guid,"')"
                     )
              )
            }
            
            if(input$import_attachments == TRUE && isTruthy(import$attach) && nrow(import$attach) > 0){
              
              q2 <- "INSERT INTO hydro_monitoring.dips_attach (guid,rel_guid,att_type,att_name,att_size,att) VALUES \n"
              f <- 500 # Max file size in kB
              d <- 2000 # Resize image max dim in px
              
              Q2 <- NULL
              for(i in 1:nrow(import$attach)){
                
                # Download attachments and convert to binary
                if(import$attach[i,c("att_type")] == "image/jpeg"){
                  tf <- tempfile(fileext=".jpeg")
                  download.file(import$attach[i,c("url")], tf, mode = "wb")
                  
                  # Compress images
                  if(import$attach[i,c("att_size")] > f * 1000){
                    img <- magick::image_read(tf)
                    
                    g <- magick::geometry_size_pixels(width = d, height = d, preserve_aspect = TRUE)
                    image_write(image_resize(img,geometry =g),tf,quality=20, compression = "JPEG")
                    import$attach[i,c("att_size")] <- image_info(image_read(tf))$filesize
                  }
                  
                  z <- readRaw(tf)
                  import$attach[i,c("att")] <- paste0("\\x", paste(z$fileRaw, collapse = ""))
                  
                  # construct attachment values string
                  Q2 <- c(Q2,
                          paste0("('",
                                 import$attach[i,c("guid")],
                                 
                                 "', (SELECT guid FROM dips WHERE guid = '",import$attach[i,c("rel_guid")],"'),'",
                                 
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
                "WITH imp AS (",q0,"), \n",
                "dips AS (
                    INSERT INTO hydro_monitoring.dips (install,dip_date_time,dip_measurer,dip_depth_top,dip_notes,dip_null,dips_import,guid) VALUES \n "
                    , paste(Q1,collapse = ", \n "),
                " ON CONFLICT (guid) DO NOTHING \n 
                 RETURNING guid",
                ") \n ",
                q2, 
                paste(Q2,collapse = ", \n "),
                " \n ON CONFLICT (guid) DO NOTHING"
                )
              
              
            }else{
              # construct final query string
              q1 <- paste0("WITH imp AS (",q0,") \n ",
                           "INSERT INTO hydro_monitoring.dips (install,dip_date_time,dip_measurer,dip_depth_top,dip_notes,dip_null,dips_import) VALUES "
                           )
              q3 <- paste0(q1,paste(Q1,collapse = ", \n "),
                           "ON CONFLICT (guid) DO NOTHING \n 
                            RETURNING (SELECT id FROM imp);")
            }
            
            r <- dbGetQuery(con0,q3)
            
            dbDisconnect(con0)
            return(r)
          })%...>%(function(r){
            if(isTruthy(r)){
              removeModal()
              showModal(import_success_modal())
            }else{
              showModal(import_error_modal())
            }
          })
        }
        
        
      })
      
      # Don't upload
      observeEvent(input$import0,{
        removeModal()
      })
      
      # Invalid data modal
      invalid_data <- function() {
        ns <- session$ns
        modalDialog(
          div(
            p("Data format invalid")
            ,style="width:100%;text-align:center")
          , footer=NULL,size="s",easyClose=TRUE,fade=TRUE) %>% showModal()
      }
      
      #Modal for verifying upload
      import_modal <- function() {
          ns <- session$ns
          modalDialog(
            div(
                        p("Are you sure you want to import the data to the database?"),
          actionButton(ns("import1"),label = "Yes"),
          actionButton(ns("import0"),label= "No")
          ,style="width:100%;text-align:center")
          , footer=NULL,size="s",easyClose=FALSE,fade=TRUE)
      }
      
      #Modal for upload progress
      import_progress_modal <- function() {
        ns <- session$ns
        modalDialog(
          div(
            tags$h4("Importing",class="loading")
            ,style="width:100%; text-align:left")
          , footer=NULL,size="s",easyClose=FALSE,fade=TRUE)
      }
      
      #Modal for no data error
      no_data_error_modal <- function() {
        ns <- session$ns
        modalDialog(
          div(
            h4("There are no data to import.")
            ,style="width:100%; text-align:center")
          , footer=NULL,size="s",easyClose=TRUE,fade=TRUE)
      }
      
      #Modal for upload success
      import_success_modal <- function() {
        ns <- session$ns
        modalDialog(
          div(
            h4("Data successfully imported")
          ,style="width:100%; text-align:center")
          , footer=NULL,size="s",easyClose=TRUE,fade=TRUE)
      }
      
      #Modal for upload error
      import_error_modal <- function() {
        ns <- session$ns
        modalDialog(
          div(
            h4("Import error")
          ,style="width:100%; text-align:center")
          , footer=NULL,size="s",easyClose=TRUE,fade=TRUE)
      }
      
      
      
      
})}