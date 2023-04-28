dips_importUI <- function(id, label = "dips import") {
  ns <- NS(id)
    tagList(
      fluidRow(
        column(12,column(6,
               h1("Import manual dipwell measurements"),
               HTML("<p>Use this page to import manual dipwell measurements to the database. First, specify one of the following two types of data source:</p> 
                 <ul>
                    <li><b>ArcGIS Online feature service:</b> Specify the REST URL for the layer containing the table of dipwell measurements. 
                    You can find the URL for a service on its details page - you just need to add the layer number to the end. To import from a feature service, 
                    it must be shared publicly and follow the template available <a href='https://fht.maps.arcgis.com/home/item.html?id=804697e8efb843e7adf9ca3badcd56e3', target='_blank'>here</a>.</li>
                    <li><b>Comma-separated value (.csv) file:</b> Upload a .csv file using the template available here. Using this method, data to import must be from the same site.</li>
                    </ul>")
               )),
        column(6,
               column(12,
                      h3("Data to import"),
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
                      h4("Optional query parameters:")
                      ),
               column(6, 
                      selectInput(ns("choose_site_agol"),label="Choose site(s):",choices=NULL,multiple=TRUE),
                      selectInput(ns("choose_install"),label="Choose install(s):",choices=NULL,multiple=TRUE)
                      ),
              column(6,
                    dateInput(ns("date0"),label="Start date:",value="2000-01-01",format="dd/mm/yyyy")
                    ),
              column(6,
                    dateInput(ns("date1"),label="End date:",value="2100-12-31",format="dd/mm/yyyy")
                    ),
             column(12,
                    br(),
                    actionButton(ns("queryDips"), label = "Query data"),
                    actionButton(ns("queryClear"), label = "Clear query")
                    )
        )),
        hidden(div(id = ns("csv"),
                   column(12, 
                          p("Choose a .csv file to import. The date-time column must be formated as yyyy-mm-dd hh:mm, e.g. '2023-04-14 00:00'."),
                          fileInput(ns("choose_csv"),label="Choose .csv file to import:", 
                                    buttonLabel = "Browse...",
                                    placeholder = "No file selected",
                                    accept = ".csv"),
                          selectInput(ns("choose_site_csv"), label = "Choose site", choices = c())
                          )
            ))
            ),
        #DataTable for the query results
        column(6,
               h3("Data preview"),
               div(id = ns("noPreview"), p("No data to preview")),
               hidden(div(id = ns("dipsPreview"), 
                          withSpinner(
                            DT::dataTableOutput(ns("query_results"))
                            )
                          , style = "font-size:70%;width:80%"))
               )
      ),
    fluidRow(
      #Form to set import parameters
      column(12,
             column(8,
                    h3("Import data"),
                    div(id = ns("importNA"), "Choose data to import."),
                    hidden(div(id = ns("importAGOLtext") , 
                    HTML("<p>Click the button below to import the query results into the database. For import from an ArcGIS Online feature service, note that:</p>
                         <ul>
                         <li>If the query results include dip measurements already present in the database 
                         (identified by the universal unique identifier column 'guid'), 
                         then those data in the database will be updated rather than new data inserted.</li>
                         <li>Checking 'Import attachments?' will also import images attached to dip records. This will make the import much slower.</li>
                         </ul>
                         "),
                    column(6,
                           textAreaInput(ns("import_notes_agol"), label = "Import notes", width = "100%", resize = "vertical"),
                           checkboxInput(ns("import_attachments"), label = "Import attachments?",value = FALSE),
                           actionButton(ns("importAGOL"), label = "Import data")
                           )
                    )),
                    hidden(div(id = ns("importCSVtext"),
                               HTML("<p>Click the button below to import the .csv file to the database. If the database already contains a record of a measurement for a given installation and date/time
                                    then that record will be updated rather than a new record added.</p>"),
                               column(6,
                                      textAreaInput(ns("import_notes_csv"), label = "Import notes", width = "100%", resize = "vertical"),
                                      actionButton(ns("importCSV"), label = "Import data")
                               )
                               ))
             )
             )
    )
    )
      
  }

dips_importServer <- function(id,con) {
  moduleServer(
    id,
    function(input, output, session) {

      installs <- reactiveValues(table=NA)
      
      #Set initial selectInput choices for AGOL
      observeEvent(input$importMode,{
          
        shinyjs::show("importNA")
        shinyjs::hide("importAGOLtext")
        shinyjs::hide("importCSVtext")

          #Get list of sites
          sites <- dbGetQuery(con, "SELECT B.id AS siteid, B.site AS sitename  FROM spatial.monitoring_hydro_installs A, spatial.fen_sites B WHERE A.site = B.id ORDER BY B.site")
          inst <- dbGetQuery(con, "SELECT A.id AS installid, A.install_name AS installname, B.id AS siteid, B.site AS sitename  FROM spatial.monitoring_hydro_installs A, spatial.fen_sites B WHERE (A.install_type <> 4) AND A.site = B.id ORDER BY B.site, A.install_name")
          installs$table <- inst

                    if(input$importMode == "ArcGIS Online"){  
            shinyjs::show("AGOL")
            shinyjs::hide("csv")
            
            #update sites input choices with installs  
            sites_choices <- sites$siteid
            names(sites_choices) <- sites$sitename
            updateSelectInput(session, inputId = "choose_site_agol", choices = sites_choices)
            
            installs_choices <- inst$installid
            names(installs_choices) <- paste(inst$sitename, inst$installname,sep = " - ")
            updateSelectInput(session, inputId = "choose_install", choices = installs_choices)
          }
        
          if(input$importMode == "Import .csv file"){
            shinyjs::hide("AGOL")
            shinyjs::show("csv")
            
            #update sites input choices with installs  
            sites_choices <- sites$siteid
            names(sites_choices) <- sites$sitename
            updateSelectInput(session, inputId = "choose_site_csv", choices = sites_choices)
          }
      })
      
      #update installs input choices when site is chosen
      observeEvent(input$choose_site_agol,{
        inst <- installs$table
        inst <- inst[inst$siteid %in% input$choose_site_agol,]
        installs_choices <- inst$installid
        names(installs_choices) <- paste(inst$sitename, inst$installname,sep = " - ")
        updateSelectInput(session, inputId = "choose_install", choices = installs_choices)
      })
      
      #Reactive for import data
      import <- reactiveValues(data = NA, attach = NA, agol = 0, csv = 0)
      
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
        
        #Where clause for date range
        q_d <- ""
        
        if(isTruthy(input$date1) && isTruthy(input$date0)){
          q_d <- paste("dip_date_time >= DATE '", input$date0, " 00:00:00' AND dip_date_time <= DATE '", input$date1, " 00:00:00'", sep="")
        }
        else{
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
        }
        else{
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
        
        #url for querying AGOL
        url <- parse_url(paste(input$agol_url,"/query",sep=""))
        url$query <- list(where = w,
                          outFields = "*",
                          f = "JSON")
        
        #Make the request and parse the data
        request <- build_url(url)
        resp <- GET(request)
        raw <- rawToChar(resp$content)
        Encoding(raw) <- "UTF-8"
        d <- fromJSON(raw)
        
        #url for querying AGOL attachments
        url_attach <- parse_url(paste(input$agol_url,"/queryAttachments",sep=""))
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
        import$attach <- a

          #Extract the table and format data and column names
          x <- d$features$attributes
          x$dip_date_time <- as.POSIXct(x$dip_date_time / 1000, origin="1970-01-01", tz="GMT")
          x$created_date <- as.POSIXct(x$created_date / 1000, origin="1970-01-01", tz="GMT")
          x$last_edited_date <- as.POSIXct(x$last_edited_date / 1000, origin="1970-01-01", tz="GMT")
          x$guid <- as.UUID(x$GlobalID)
          
          x<-x[-c(1)] #Drop AGOL objectid field
          colnames(x)[7:10] <- paste("source_",colnames(x)[7:10],sep="")

          if(length(x)>0){
            shinyjs::hide("noPreview")
            shinyjs::show("dipsPreview")
            
            #Store the table in reactive
            import$data <- x
            import$agol <- 1 #agol verifier
            
            #Format the query results for display
            t <- merge(x,installs$table, by.x = "install",by.y="installid")
            t$install_ref <- paste(t$sitename, t$installname,sep = " - ")
            t <- t[,c("install_ref","dip_date_time","dip_measurer","dip_depth_top","dip_notes","guid")]
            
            #Render data table of query results
            output$query_results <- DT::renderDataTable(
              datatable(t, 
                        options=list(bFilter=0, bLengthChange=0, columnDefs = list(list(visible=FALSE,targets=c(0,6)),list(className = 'dt-center', targets = 4)))
                        , colnames = c("Install","Dip date / time", "Recorder", "Dip (cm below well top)", "Notes","")
              )
            )
            
            shinyjs::hide("importNA")
            shinyjs::show("importAGOLtext")
            shinyjs::hide("importCSVtext")
        }
        else{
          import$agol <- 0
          showModal(import_agol_error())
        }

    })
      
      #Clear AGOL query
      observeEvent(input$queryClear,{
        import$data <- NA
        
        shinyjs::show("noPreview")
        shinyjs::hide("dipsPreview")
        
        shinyjs::hide("importAGOLtext")
        shinyjs::show("importNA")

      })
      
      #Import csv file
      observe({
        
        #Import csv and load data table
        req(input$choose_csv)
        file <- input$choose_csv
        ext <- file$type
        
        req(file)
        shiny::validate(need(ext == "text/csv", "Please upload a csv file"))
        
        d <- read.csv(file$datapath, header = TRUE, encoding = "UTF-8")
        d[,2] <- as.POSIXct(d[,2],tz="GMT")
        
        i <- d
        colnames(i)[1] <- "install_name"

        i <- merge(i, installs$table, by.x = "install_name", by.y = "installname")
        import$data <- i[,c("installid","dip_date_time","dip_measurer","dip_depth_top","dip_notes")]
        import$csv <- 1

        shinyjs::hide("noPreview")
        shinyjs::show("dipsPreview")        
        output$query_results <- DT::renderDataTable(
          datatable(d, 
                    options=list(bFilter=0, bLengthChange=0, columnDefs = list(list(visible=FALSE,targets=c(0)),list(className = 'dt-center', targets = c(4))))
                    , colnames = c("Install","Dip date / time", "Recorder", "Dip (cm below well top)", "Notes")
          )
        )
        
        shinyjs::hide("importNA")
        shinyjs::hide("importAGOLtext")
        shinyjs::show("importCSVtext") 
      })

      #Modal for verifying import
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
      #Modal for import progress
      import_progress_modal <- function() {
        ns <- session$ns
        modalDialog(
          div(
            p("Import in progress")
            ,style="width:100%; text-align:center")
          , footer=NULL,size="s",easyClose=FALSE,fade=TRUE)
      }
      #Modal for no data error
      no_data_error_modal <- function() {
        ns <- session$ns
        modalDialog(
          div(
            p("There are no data to import.")
            ,style="width:100%; text-align:center")
          , footer=NULL,size="s",easyClose=TRUE,fade=TRUE)
      }
      #Modal for import success
      import_success_modal <- function() {
        ns <- session$ns
        modalDialog(
          div(
            p("Data successfully imported")
          ,style="width:100%; text-align:center")
          , footer=NULL,size="s",easyClose=TRUE,fade=TRUE)
      }
      #Modal for import error
      import_error_modal <- function() {
        ns <- session$ns
        modalDialog(
          div(
            p("Import error")
          ,style="width:100%; text-align:center")
          , footer=NULL,size="s",easyClose=TRUE,fade=TRUE)
      }
      
      #Import AGOL data to database
      observeEvent(input$importAGOL,{
        if(import$agol == 1){

        #Import table to database
        #Yes/no popup to verify import
          
        showModal(import_modal())
        
        #No - close modal
        observeEvent(input$import0,{
          removeModal()
        })
        
        #Yes - Insert new row into dips install table
        observeEvent(input$import1,{
          removeModal()
          
          shiny::validate(need(input$agol_url, "Please include an ArcGIS Online REST URL"))
          req(input$agol_url)
          
          showModal(import_progress_modal())
          
          #Connect to database

            #Insert dip import row
            query <- paste("INSERT INTO hydro_monitoring.dips_imports (source_type,source,notes,source_attachments) VALUES (",
                           1,",",
                           "'", input$agol_url,"',",
                           "'", input$import_notes_agol,"',",
                           input$import_attachments,
                           ") RETURNING id", sep="")
            
            import_info <- dbGetQuery(con, query)
            import$data$dips_import <- import_info$id
            
            if(!is.na(import_info$id)){
              insert <- pgInsert(
                con,
                name = c("hydro_monitoring","dips"),
                data.obj = import$data,
                df.mode = FALSE,
                partial.match = TRUE,
                overwrite = FALSE,
                upsert.using = c("guid")
              )
              
              #Import attachements
              if(isTruthy(input$import_attachments) && nrow(import$attach)>0){
                import$attach$att <- NA
                
                #Download and convert attachments to binary
                for(i in 1:nrow(import$attach)){
                  tf <- tempfile(fileext=paste0(".",
                                                substr(import$attach[i,"att_type"],7,10000)
                                                )
                                 )
                  download.file(import$attach[i,c("url")], tf, mode = "wb")
                  z <- readRaw(tf)
                  r <- import$attach[i,]
                    r$att <- paste0("\\x", paste(z$fileRaw, collapse = ""))

                  # dbExecute(con, "INSERT INTO hydro_monitoring.dips_attach (rel_guid,att_name,att_type,att_size,att) VALUES ($1,$2,$3,$4,$5)",
                  #           list(r$rel_guid,r$name,r$contentType, r$size ,r$att))
                }
                insert_attach <- pgInsert(
                  con,
                  name = c("hydro_monitoring","dips_attach"),
                  data.obj = import$attach,
                  df.mode = FALSE,
                  partial.match = TRUE,
                  overwrite = FALSE,
                  upsert.using = c("guid")
                )
              }
              
              if(insert == TRUE){
                removeModal()
                showModal(import_success_modal())
              }
              else{
                showModal(import_error_modal())
              }
              
              
            }
            else{
              #Delete the import info record if the import fails
              dbGetQuery(con,paste("DELETE FROM hydro_monitoring.dips_imports WHERE id = ",import_info$id,sep=""))
              #Import fail modal
              removeModal()
              showModal(import_error_modal())
            }
          })
        }
        else{
          showModal(no_data_error_modal())
        }
          
        
      })
      
      #Import csv data to database
      observeEvent(input$importCSV,{
        if(import$csv == 1){
          
          #Import table to database
          #Yes/no popup to verify import
          
          showModal(import_modal())
          
          #No - close modal
          observeEvent(input$import0,{
            removeModal()
          })
          
          #Yes - Insert new row into dips install table
          observeEvent(input$import1,{
            removeModal()
            showModal(import_progress_modal())
            
            #Insert dip import row
            query <- paste("INSERT INTO hydro_monitoring.dips_imports (source_type,source,notes) VALUES (",
                           2,",",
                           "'", input$choose_csv$file,"',",
                           "'", input$import_notes_csv,"'",
                           ") RETURNING id", sep="")
            
            colnames(import$data) <- c("install","dip_date_time","dip_measurer","dip_depth_top","dip_notes")
            import_info <- dbGetQuery(con, query)
            import$data$dips_import <- import_info$id
            
            if(!is.na(import_info$id)){
              insert <- pgInsert(
                con,
                name = c("hydro_monitoring","dips"),
                data.obj = import$data,
                df.mode = FALSE,
                partial.match = TRUE,
                overwrite = FALSE,
                upsert.using = c("install","dip_date_time")
              )
              
              if(insert == TRUE){
                removeModal()
                showModal(import_success_modal())
              }
              else{
                showModal(import_error_modal())
              }
            }
            else{
              #Delete the import info record if the import fails
              dbGetQuery(con,paste("DELETE FROM hydro_monitoring.dips_imports WHERE id = ",import_info$id,sep=""))
              #Import fail modal
              removeModal()
              showModal(import_error_modal())
            }
            
          })
        }
        else{
          showModal(no_data_error_modal())
        }
        
        
      })
      
})}