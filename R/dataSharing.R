dataSharingUI <- function(id){
  ns <- NS(id)
  tagList(
    withSpinner(
  tagList(
    column(12,
                  column(8,
                         h4("Data sharing"),
                         p("The dropdown below shows the queries defined for sharing data with
                         external organisations. Choose a data sharing definition to view properties. The table to the right shows
                           the exports generated using that query."),
                         p("Click the 'Create new export' button to run the query. A download will be automatically generated
                           and a new record added to the table."),br()
                         ),
           column(12,
                  column(5,
                         selectizeInput(
                           inputId = ns("data_sharing"),
                           label = "Data sharing definitions",
                           choices = c(""),
                           multiple = TRUE,
                           options = list(placeholder = 'Select from list')
                         ),
                         div(style="float:left;width:33%",
                             div(style="width:85%",
                                 textInput(
                                   inputId = ns("name"),
                                   label = "Name",
                                   value = NULL
                                 ) 
                             )
                         ),
                         div(style="float:left;width:33%",
                             div(style="width:85%",
                                 numericInput(
                                   inputId = ns("start_year"),
                                   label = "Start year",
                                   value = NULL
                                 ) 
                             )
                         ),
                         div(style="float:left;width:33%",
                             div(style="width:85%",
                                 numericInput(
                                   inputId = ns("end_year"),
                                   label = "End year",
                                   value = NULL
                                 )
                             )
                         ),
                         
                         textInput(
                           inputId = ns("organisation"),
                           label = "Organistion",
                           value = NULL
                         ),
                         
                         div(style="float:left;width:50%",
                             div(style="width:85%",
                                 textInput(
                                   inputId = ns("contact_name"),
                                   label = "Contact name",
                                   value = NULL
                                 )  
                             )
                         ),
                         div(style="float:left;width:50%",
                             div(style="width:85%",
                                 textInput(
                                   inputId = ns("contact_email"),
                                   label = "Contact email",
                                   value = NULL
                                 )   
                             )
                         ),
                         textAreaInput(
                           inputId = ns("description"),
                           label = "Description",
                           value = NULL,
                           resize = "vertical"
                         )
                  ),
                  column(7,
                         div(
                           style = "border: 1px solid black; border-radius: 5px; padding: 10px ",
                           withSpinner(DT::DTOutput(outputId = ns("exportsTable")),type = 7)
                         ),
                         div(style = "margin-top: 10px;",
                           actionButton(ns("export"), label = "Create new export", icon = icon("plus")),br(),br(),
                         )
                  )
                  ),
           div(downloadButton(ns("dlExport")),class="buttonHidden")
           )
    ,
    tags$script(src ="script.js")
  )
  ,
  id = ns("module"),
  type = 4,
  size = 2,
  proxy.height = "100%",
  hide.ui = TRUE,
  caption = "Loading module"),
  tags$script(src ="script.js"),
  tags$script(
    HTML("$('#dataSharing-module').parent().removeClass('shiny-spinner-hidden')")
  )
  )
}

dataSharingServer <- function(id, login){
  moduleServer(
    id,
    function(input, output, session) {
      
      ## Initialisation ----
      
      user <- login$username
      password <- login$password
      
      #Load modals ----
      source("./R/modals/export_modal.R")
      # Disable fields ----
        shinyjs::disable("name")
        shinyjs::disable("start_year")
        shinyjs::disable("end_year")
        shinyjs::disable("organisation")
        shinyjs::disable("contact_name")
        shinyjs::disable("contact_email")
        shinyjs::disable("description")
        shinyjs::disable("export")
        
      # Reactive vals ----
        rv <- reactiveValues(
          sh = NA, 
          exp = NA, 
          exp0 = data.frame(
            id = numeric(),
            sharing = numeric(),
            export_user = character(),
            export_date = Date(),
            export_note = character(),
            created_date = Date(),
            last_edited_date = Date(),
            created_user = character(),
            last_edited_user = character()
            ),
          choices = NA,
          dl = NA,
          t = NA,
          dl_ready = FALSE,
          timestamp = Sys.time(),
          dt_row = NA
          )
        
        
      # Get data sharing info ----
        future_promise({
          con0 <- poolCheckout(con_global)
          tryCatch({
            s <- dbGetQuery(con0, "SELECT * FROM records.data_sharing ORDER BY id")
            e <- dbGetQuery(con0,"SELECT * FROM records.data_sharing_exports ORDER BY id")
            poolReturn(con0)
            return(list("sh" = s, "exp" =e))
            },
            error=function(err){
              poolReturn(con0)
              return(err)
              }
            )
          })%...>% (function(result) {
            rv$sh <- result$sh
            choices <- result$sh$id
            names(choices) <- result$sh$name
            updateSelectizeInput(
              session,
              inputId = "data_sharing",
              choices = choices,
              selected = "",
              server = FALSE,
              options = list(
                placeholder = 'Select from list',
                maxItems = 1)
              )
            rv$exp <- result$exp
            
            runjs(
              paste0(
                "$('#",id,"-module').parent().addClass('shiny-spinner-hidden');
                 $('div[data-spinner-id=\\'",id,"-module\\']').css('display','inline')"
              )
            )
            })

      # Data sharing name selection populate fields and DT ----
      
        observeEvent(input$data_sharing,{
          sh <- rv$sh
          updateTextInput(session,"name", value = sh[sh$id == input$data_sharing,c("name")])
          updateNumericInput(session,"start_year", value = sh[sh$id == input$data_sharing,c("start_year")])
          updateNumericInput(session,"end_year", value = sh[sh$id == input$data_sharing,c("end_year")])
          updateTextInput(session,"organisation", value = sh[sh$id == input$data_sharing,c("organisation")])
          updateTextInput(session,"contact_name", value = sh[sh$id == input$data_sharing,c("contact_name")])
          updateTextInput(session,"contact_email", value = sh[sh$id == input$data_sharing,c("contact_email")])
          updateTextAreaInput(session,"description", value = sh[sh$id == input$data_sharing,c("description")])
          shinyjs::enable("export")
          })
        
        observe({
          req(input$data_sharing)
          rv$exp0 <- rv$exp[rv$exp$sharing == input$data_sharing,]
        })
        
      # Data table ----
        
        observe({
          req(nrow(rv$exp0) == 0 )
          x <- rv$exp0[,c("export_date","export_user")]
          x$Buttons <- character()
          output$exportsTable <- DT::renderDT({
            x
            },
            server = TRUE,
            escape = F,
            rownames = FALSE,
            selection = 'single',
            colnames = c("Export date / time", "Export user",""),
            options = list(
              columnDefs = list(
                list(orderable = FALSE, targets = c(2)),
                list(width = '15px',targets=c(2))
              ),
              extensions = c("FixedHeader"),#, "Scroller")
              fixedHeader = TRUE,
              scrollY = "30vh",
              language = list(
                infoEmpty = "No exports", 
                emptyTable = "No exports to display"))
          )
        })
        
        proxy_DT <- DT::dataTableProxy("exportsTable")
        
        observe({
          req(nrow(rv$exp0) > 0 )
          d <- add_btns(rv$exp0,"r","export")
          x <- d[,c("export_date","export_user","Buttons")]
          x$export_date <- format(x$export_date,format="%Y-%m-%d %H:%M:%S")
          DT::replaceData(proxy_DT, x, resetPaging = FALSE, rownames = FALSE)
        })
        
      # DT buttons ----
      
        observeEvent(input$current_id, {
          if(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "export_info")){
            rv$dt_row <- as.numeric(sub("export_info_","",input$current_id))
            
            e <- rv$exp0[rv$exp0$id == rv$dt_row,c("export_user","export_date","export_note","created_user","created_date","last_edited_user","last_edited_date")] 
            survey_modal_dialog(session = session, rv$sh[rv$sh$id == input$data_sharing,c("name")], e)
            
            shinyjs::disable("modal_sharing")
            shinyjs::disable("modal_export_user")
            shinyjs::disable("modal_export_date")
            shinyjs::disable("modal_export_note")
            shinyjs::disable("modal_created_user")
            shinyjs::disable("modal_created_date")
            shinyjs::disable("modal_last_edited_user")
            shinyjs::disable("modal_last_edited_date")
          }
        })
          
      # Generate new export ----
      
        output$dlExport <- downloadHandler(
          filename = function() {
            paste0("blank", ".csv")
          },
          content = function(file) {
            write.csv(NA, file)
          }
        )
        
        observeEvent(input$export,{
          req(isTruthy(input$data_sharing))
          export_modal(rv$sh[rv$sh$id == input$data_sharing,c("description")])
        })
        
        export_modal <- function(d){
          ns <- session$ns
          modalDialog(
            h4("Export data"),
            p("This will download the following data - do you want to continue?"),
            HTML(paste0("<div style='width:100%;padding:10px'><i>",d,"</i></div><br>")),
            actionButton(ns("yes"),"Yes"), actionButton(ns("no"),"No")
            , footer=NULL,size="s",easyClose=FALSE,fade=TRUE
          ) %>% showModal()
        }
        
        observeEvent(input$yes,{
          export_note_modal()
        })
        
        export_note_modal <- function(){
          ns <- session$ns
          modalDialog(
            h4("Export data"),
            textAreaInput(
              ns("export_note"),
              "Add a note",
              resize = "vertical"
            ),
            actionButton(ns("submit_export"),"Export"), actionButton(ns("cancel_export"),"Cancel")
            , footer=NULL,size="s",easyClose=FALSE,fade=TRUE
          ) %>% showModal()
          
          shinyjs::disable("submit_export")
          
          iv <- InputValidator$new()
          iv$add_rule("export_note",sv_required())
          iv$enable()
          
          observeEvent(input$export_note,{
            if(isTruthy(input$export_note)){
              shinyjs::enable("submit_export")
            }
            else{
              shinyjs::disable("submit_export")
            }
          })
        }
        
        observeEvent(input$no,{
          removeModal()
        })        
        
        observeEvent(input$submit_export,{
          req(input$data_sharing)
          req(input$export_note)
          
          rv$t <- rv$sh[rv$sh$id == as.numeric(input$data_sharing),c("type")] # Data type to export
          q <- rv$sh[rv$sh$id == input$data_sharing,c("query")] # Query to run
          rv$timestamp <- Sys.time() # Need to change to client's local time #
          
          removeModal()
          showModal(
            modalDialog(
              div(style="width:60%; text-align:left",
                  tags$h4("Export in progress",class="loading"),
              )
              ,footer=NULL, size="s", easyClose=FALSE, fade=TRUE
            )
          )
          
          #Table export
            future_promise({
              con0 <- fenDb0(user,password)
                q_exp <- paste0(
                  "INSERT INTO records.data_sharing_exports
                  (sharing, export_date, export_note, export_user)
                  VALUES
                  (",as.numeric(input$data_sharing),
                  ", TO_TIMESTAMP('",rv$timestamp,"','YYYY-MM-DD HH24:MI:SS'), '",
                  postgresqlEscapeStrings(con0,input$export_note),"', '",
                  user,"') RETURNING id, created_date, created_user, guid"
                  )
                if(rv$t == 1){
                  d1 <- dbGetQuery(con0, q)
                }
                if(rv$t == 2){
                  d1 <- st_read(dsn = con0, query = q, geometry_column = "geom")
                }

                i1 <- dbGetQuery(con0, q_exp)
 
                dbDisconnect(con0)
                return(list("err" = NA, "d" = d1, "i" = i1))
              })%...>% (function(result) {
                if(isTruthy(result$d)){
                  rv$exp[nrow(rv$exp) + 1,] <- c(result$i$id,
                                                  input$data_sharing,
                                                  format(rv$timestamp,format="%Y-%m-%d %H:%M:%S"),
                                                  input$export_note,
                                                  format(result$i$created_date,format="%Y-%m-%d %H:%M:%S"),
                                                  NA,
                                                  result$i$created_user,
                                                  NA,
                                                  user,
                                                 result$i$guid #guid column
                                              )
                  rv$dl <- result$d

                  name <- paste0(input$name, " ", format(rv$timestamp,format="%Y%m%d%H%M%S"))
                  
                  if(rv$t == 1){
                    req(nrow(rv$dl) > 0)
                    output$dlExport <- downloadHandler(
                      filename = function() {
                        paste0(name, ".csv")
                      },
                      content = function(file) {
                        tmp.path <- dirname(file)
                        name.base <- file.path(tmp.path, name)
                        name.glob <- paste0(name.base, ".*")
                        name.csv  <- paste0(name.base, ".csv")
                        
                        if (length(Sys.glob(name.glob)) > 0) file.remove(Sys.glob(name.glob))
                        
                        write.csv(rv$dl, name.csv)
                        req(file.copy(name.csv, file))
                        
                        if (length(Sys.glob(name.glob)) > 0) file.remove(Sys.glob(name.glob))
                      }
                    )
                  }
                  if(rv$t == 2){
                    output$dlExport <- downloadHandler(
                      filename = function() {
                        paste0(name, ".zip")
                      },
                      content = function(file) {
                        
                        tmp.path <- dirname(file)
                        name.base <- file.path(tmp.path, name)
                        name.glob <- paste0(name.base, ".*")
                        name.zip  <- paste0(name.base, ".zip")
                        
                        if (length(Sys.glob(name.glob)) > 0) file.remove(Sys.glob(name.glob))
                        
                        write_sf(rv$dl, name, driver = "ESRI Shapefile")
                        zip::zip(zipfile=name.zip,files=name)
                        unlink("temp_fens", recursive=TRUE)
                        
                        req(file.copy(name.zip, file))
                        
                        if (length(Sys.glob(name.glob)) > 0) file.remove(Sys.glob(name.glob))
                      }
                    )
                  }
                  Sys.sleep(1)
                  rv$dl_ready <- TRUE
                  
                }
                else{print(result$err)}
                removeModal()
              })
          
        })
        
        observeEvent(input$cancel_export,{
          removeModal()
        })
        
        observeEvent(rv$dl_ready,{
          req(rv$dl_ready == TRUE)
          runjs("document.getElementById('dataSharing-dlExport').click();")
          rv$dl_ready <- FALSE
        })
        
        
        outputOptions(output, "dlExport")
    }
  )
}