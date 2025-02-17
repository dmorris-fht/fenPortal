importVegUI <- function(id){
  ns <- NS(id)
  tagList(
    withSpinner(
      tagList(
        # tags$head(
        #   tags$script(src = "//cdn.datatables.net/plug-ins/2.1.8/sorting/absolute.js")
        # ),
        useShinydashboard(),
        column(12,
               column(3,
                      h3("Import vegetation data"),
                      HTML("<p>Import a comma-separated value (.csv) file of plot data. 
                                         Download the import template from 
                                         <a href='./templates/records_import_template.csv'>here</a>.</p>"),
                          p("To import a file, first carry out the following validation steps. You can also filter by date."),
                          
                      div(style="padding-bottom:5px",
                        div(style="float:left;padding-right:5px","- Plots"),
                        uiOutput(ns("plotTick"))
                        ),br(),
                      div(style="padding-bottom:5px",
                        div(style="float:left;padding-right:5px","- Dates"),
                        uiOutput(ns("dateTick"))
                      ),br(),  
                      div(style="padding-bottom:5px",
                        div(style="float:left;padding-right:5px","- Environmental variables"),
                        uiOutput(ns("envTick"))
                      ),br(),
                      div(style="padding-bottom:5px",
                        div(style="float:left;padding-right:5px","- Species names"),
                        uiOutput(ns("speciesTick"))
                      ),br(),
                      div(style="padding-bottom:5px",
                        div(style="float:left;padding-right:5px","- Species abundances"),
                        uiOutput(ns("abundanceTick"))
                      ),br(),
                         
                      
                      box(title = "Load & validate data", width = 12, solidHeader = T,collapsible = T, collapsed = F,
                          column(12,
                                 selectizeInput(ns("site"), label = "Choose site", choices = c(), multiple = FALSE),
                                 conditionalPanel(ns = NS(id),condition = 'input.site !=""',
                                                  
                                                  fileInput(ns("csvFile"),label="Choose .csv file to import:", 
                                                            buttonLabel = "Browse...",
                                                            placeholder = "No file selected",
                                                            accept = ".csv"
                                                  ),
                                                  selectizeInput(ns("abundance"), label = "Abundance measure:",
                                                                 multiple = FALSE,
                                                                 choices = c("Domin","Percentage cover","Frequency","Presence")
                                                  ),
                                                  actionButton(ns("loadCSV"),label = "Validate"),
                                                  
                                                  ),
                          
                          
                          
                              
                          )
                      ),
                      
                      box(title = "Import data", width = 12, solidHeader = T,collapsible = T, collapsed = T,
                          column(12,
                                 selectizeInput(ns("survey"),label = "Dataset:",multiple=F,choices=c(""),selected=""),
                                 textAreaInput(ns("importNote"),label = "Import notes:",resize="vertical",value=NULL),
                                 actionButton(ns("import"),label = "Import data")
                                 )
                          )
                      
                      
                      
                      ),
               column(9,
                      h4("Preview data"),
                      div(
                          style = "width:100%;overflow-y:scroll;min-height:600px;padding: 10px ",
                          uiOutput(ns("previewData"))
                      )
                      
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

importVegServer <- function(id, login, tables) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Module initialization ---- 
       shinyjs::hide("options")
      
      observe({
        role <- login$role
        user <- login$username
        password <- login$password
      })
      
      isolate({
        app_tables(tables, c("sites","surveys","plots"))
        
        uksi_load(c(1,2,3))
      })
      
      observe({
        req(tables$sites0)
        req(tables$sites)
        req(tables$surveys)
        req(tables$plots)
        
        req(choices_uksi_plants)
        
        shinyjs::runjs(
          paste0(
          "$('#",id,"-module').parent().addClass('shiny-spinner-hidden');
                 $('div[data-spinner-id=\\'",id,"-module\\']').css('display','inline');
          "
                 )
        )
       
        if(isTruthy(tables$sites)){
          choices_sites <- tables$sites$id
          names(choices_sites) <- paste0(tables$sites$site, " [",tables$sites$county, "]")
        }
        else{
          choices_sites <- c("")
        }

        updateSelectizeInput(session, inputId = "site", 
                         choices = choices_sites,
                         selected = 1,
                         options = list(
                           placeholder = 'Choose a site',
                           onInitialize = I('function() { this.setValue(""); }')
                         ))
        
        if(isTruthy(tables$sites)){
          choices_surveys <- tables$surveys$id
          names(choices_surveys) <- tables$surveys$survey
        }
        else{
          choices_surveys <- c("")
        }
      
        updateSelectizeInput(session, inputId = "survey", 
                             choices = choices_surveys,
                             options = list(
                               placeholder = 'Choose a dataset',
                               onInitialize = I('function() { this.setValue(""); }')
                             ))
        
        })
      
      output$previewData <- renderUI({
        tagList(
          HTML("<p style='font-size:14px'>No data to show</p>")
        )
      })
      
      shinyjs::disable("loadCSV")
      shinyjs::disable("import")
      
      # Symbol
      tick <- "<div style='float:left;padding-right:5px'><i class='fa-solid fa-check' style='color:green;font-size:16px'></i></div>"
      cross <- "<div style='float:left;padding-right:5px'><i class='fa-solid fa-xmark' style='color:green;font-size:16px'></i></div>"

      # Reactives ----
      
      rv <- reactiveValues(df0 = NA, # CSV file
                           df1 = NA, df1_validation = NA, # Filtered CSV
                           t = NA, t_validation = NA, # DF of species from df1
                           v = 0 # Validation counter, value is 5 when validation complete (dates,env,plots,abundance,taxa)
                           )
      
      # Data tables ----
      
      renderCsvTable <- function(){
        renderDT({ 
          x <- cbind(rv$df1[1:nrow(rv$df1),c(1,3:ncol(rv$df1))],rv$df1_validation)
          
          n1 <- ncol(rv$df1)
          n2 <- ncol(rv$df1_validation)
          n3 <- ncol(x)
          
          x$col <- NA
          x[1:8,"col"] <- 1
          x[8:nrow(x),"col"] <- 2
          
          ns <- session$ns

          x[11:nrow(x),1] <- unlist(lapply(1:(nrow(rv$df1)-10),function(i){
            as.character(selectizeInput(
              ns(paste0("taxon_",i)),
              selected = rv$df1[10 + i,2],
              label=NULL,
              choices=c(rv$df1[10 + i,2]),
              multiple = TRUE,
              options = list(maxItems = 1,
              
              # Callbacks so that drop down overflows table cell
                             
              onFocus = I("function(){
                  var id = this['$control_input']['0']['id'];
                  var td = $('#' + id).closest('td');
                      td.addClass('show');
                                    }"),
              
              onBlur = I("function(){
                  var id = this['$control_input']['0']['id'];
                  var td = $('#' + id).closest('td');
                      td.removeClass('show');
                                    }")
              )
            ))
          }))
            
          # UPDATESELECTIZE DOESN'T WORK
          # DATE ROW TO DATE PICKER
          
          DT::datatable(
            x
            ,
            colnames = c("",as.vector(unlist(rv$df1[1,3:ncol(rv$df1)])),rep("",ncol(rv$df1_validation)),"col"),
            escape = FALSE,
            rownames = FALSE,
            selection = 'none',
            extensions = c("FixedColumns","FixedHeader"),
            fillContainer = TRUE,
            editable = list(target= "cell",disable=list(columns=c(0))),
            options = list(
              dom = "tpli",
              ordering=F,
              pageLength = 25,
              processing = TRUE,
              language = list(zeroRecords = "No data"),
              scrollY = "70vh",
              scrollX = TRUE,
              autoWidth = TRUE,
              fixedColumns = TRUE,
              columnDefs = list(
                list( targets = "_all", width = '200px'),
                list(visible=FALSE,targets = (n1-1):(n3))
                ),
              preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
              drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
            )) %>% formatStyle(1,fontWeight="bold") %>% 
            formatStyle(1,backgroundColor="#7af478") %>%
            formatStyle("col",target="row",backgroundColor = styleEqual(c(1,2),c("#c7f5c6",""))) %>%
            formatStyle(2:ncol(x),textAlign = "center") %>%
            formatStyle(2:(n1-1),(n1):(n3),backgroundColor = styleEqual(c(0,1),c("orange","")))
        },server=T) 
        
        
      }

      updateCsvTable <- function(){
        proxy <- DT::dataTableProxy("csvTable",deferUntilFlush=TRUE)
        
        x <- cbind(rv$df1[,c(1,3:ncol(rv$df1))],rv$df1_validation)
     
        x$col <- NA
        x[1:8,"col"] <- 1
        x[8:nrow(x),"col"] <- 2
        
        ns <- session$ns
        
        x[11:nrow(x),1] <- unlist(lapply(1:(nrow(rv$df1)-10),function(i){
          as.character(selectizeInput(
            ns(paste0("taxon_",i)), 
            selected = rv$df1[10 + i,2],
            label=NULL, 
            choices=c(rv$df1[10 + i,2]),
            multiple = TRUE,
            options = list(maxItems = 1,
                           load = I("function(query,callback){
                                  $(this).click(function(){
                                    console.log('hi');
                                    $(this).closest('td').addClass('show')
                                  })
                           }")
                           
                           ),
            )
            
          )
        }))
        
        DT::replaceData(proxy,data = x , resetPaging = FALSE, rownames = FALSE)
        
        for(i in 1:nrow(rv$t)){
          updateSelectizeInput(session,paste0("taxon_",i),selected = rv$t[i,"match"],choices= choices_uksi_plants,server=T)
        }
        
        }
      
      # Colour in heading where error in column
      # Tick validation
      # Fix selectize update
      # Get rid of validation button and updateCSVTable, make updates to style on cell edit via JS
      
      
      ## Update with cell edits ----
      observeEvent(input$csvTable_cell_edit, {
        r  <- input$csvTable_cell_edit$row
        c <- input$csvTable_cell_edit$col + 2
        rv$df1[r, c] <- input$csvTable_cell_edit$value
        
        # if(c>1){
        #   validate()
        #   updateCsvTable()
        # }
      })
      
      # Csv loading ----
      
      observe({
        if(isTruthy(input$csvFile) & isTruthy(input$abundance)){
          shinyjs::enable("loadCSV")
        }else{
          shinyjs::disable("loadCSV")
        }
      })
      
      observeEvent(input$csvFile,{
        # Check file format ---
        
        # DO THIS HERE
        
        # Read in CSV file ----
        
        file <- input$csvFile
        ext <- file$type
        
        req(file)
        shiny::validate(need(ext == "text/csv", "Please upload a csv file"))
        
        showSpinner("csvTable")
        
        d <- read.csv(file$datapath, header = FALSE,fileEncoding = "UTF-8-BOM")
        
        rv$df0 <-d
        rv$df1 <- d
        
        d_validation <- d[,3:ncol(d)]
        d_validation[] <- 1
        rv$df1_validation <- d_validation
        
        
        ### Load table ----
        
        output$previewData <- renderUI({
          ns <- NS(id)
          tagList(
            withSpinner(
              DT::DTOutput(outputId = ns("csvTable"))
              ,type = 7, caption = "Loading")
          )
        })
        
        output$csvTable <- renderCsvTable()
      })
      
      ## Validate CSV ----
      
      observeEvent(input$loadCSV,{
        req(input$csvFile)
        req(input$abundance)
        
        d <- rv$df1
        
        ### Validate ----
        
        validate()
        
        ### Check taxa ----
        
        t <- data.frame(t0 = trimws(d[11:nrow(d),2]))
        t$match <- lapply(d[11:nrow(d),1],function(s){ifelse(isTruthy(s),s,NA)})
        no_match <- which(is.na(t$match))
        match <- t[no_match,]  %>% left_join(uksi_pl_rec, join_by(t0 == name), relationship = "one-to-one")
        t[no_match,"match"] <- match$nbn_taxon_version_key
        rv$t <- t[,c("t0","match")]
        
        # ns <- session$ns
        # 
        # rv$t$select <- NA
        # rv$t$select <- lapply(1:nrow(rv$t),function(i){
        #   as.character(selectizeInput(
        #     ns(paste0("taxon_",i)), 
        #     label=NULL, 
        #     choices=c(""),
        #     multiple = TRUE,
        #     options = list(maxItems = 1, placeholder = rv$t[i,"t0"])
        #     ))
        # })
        
        updateCsvTable()
        
        
        
        
        
        
        
        ### Validation message ----
        
        
        
          })
      
      # Validation function ----
      
      validate <- function(){
        d <- rv$df1
        
        ### Check plot names ----
        
        invalid_plots <- which(!(d[1,3:ncol(d)] %in% tables$plots[tables$plots$site == as.numeric(input$site),]$plot))
        rv$df1_validation[1,invalid_plots] <- 0
        
        ### Check dates ----
        
        not_dates <- lapply(d[3,3:ncol(d)],function(y){
          ifelse(isTruthy(as.Date(y,format="%d/%m/%Y")),1,0)
        })
        rv$df1_validation[3,] <- not_dates
        
        ### Check structure attributes ----
        
        rv$df1_validation[4:7,] <- apply(d[4:7,3:ncol(d)],c(1,2),function(y){
          y <- trimws(y)
          suppressWarnings(
            ifelse(isTruthy(as.numeric(y)) || !isTruthy(y),1,0)
          )})
        
        rv$df1[4:7,3:ncol(d)] <- apply(d[4:7,3:ncol(d)],c(1,2),function(y){
          y <- trimws(y)
          suppressWarnings(
            ifelse(isTruthy(as.numeric(y)),as.numeric(y),y)
          )})
        
        ### Check abundance ----
        
        if(input$abundance == "Domin"){
          
          rv$df1_validation[11:nrow(d),] <- apply(d[11:nrow(d),3:ncol(d)],c(1,2),function(y){
            y <- trimws(y)
            ifelse(!isTruthy(y) || (isTruthy(as.integer(y)) & as.numeric(y) == as.integer(y) & as.integer(y) <= 10 & as.integer(y)) >= 1,1,0)
          })
          
          rv$df1[11:nrow(d),3:ncol(d)] <- apply(d[11:nrow(d),3:ncol(d)],c(1,2),function(y){
            y <- trimws(y)
            ifelse(isTruthy(as.integer(y)) & as.numeric(y) == as.integer(y) & as.integer(y) <= 10 & as.integer(y) >= 1,
                   as.integer(y),y)
          })
        }
        
        if(input$abundance == "Percentage cover"){
          rv$df1_validation[11:nrow(d),] <- apply(d[11:nrow(d),],c(1,2),function(y){
            y <- trimws(y)
            ifelse(!isTruthy(y) || (isTruthy(as.integer(y)) & !is.na(as.numeric(y)) & as.numeric(y) <= 100 & as.numeric(y) >= 0.01),1,0)
          })
          
          rv$df1[11:nrow(d),3:ncol(d)] <- apply(d[11:nrow(d),3:ncol(d)],c(1,2),function(y){
            y <- trimws(y)
            ifelse(isTruthy(as.integer(y)) & !is.na(as.numeric(y)) & as.numeric(y) <= 100 & as.numeric(y) >= 0.01,
                   as.integer(y),y)
          }) 
        }
        
        if(input$abundance == "Frequency"){
          rv$df1_validation[11:nrow(d),] <- apply(d[11:nrow(d),],c(1,2),function(y){
            y <- trimws(y)
            ifelse(!isTruthy(y) || (!is.na(as.numeric(y)) & as.numeric(y) <= 1 & as.numeric(y) >= 0.0000001),1,0)
          })
          
          rv$df1[11:nrow(d),3:ncol(d)] <- apply(d[11:nrow(d),3:ncol(d)],c(1,2),function(y){
            y <- trimws(y)
            ifelse(!is.na(as.numeric(y)) & as.numeric(y) <= 1 & as.numeric(y) >= 0.0000001,
                   as.integer(y),y)
          })
        }
        if(input$abundance == "Presence"){
          rv$df1_validation[11:nrow(d),] <- apply(d[11:nrow(d),],c(1,2),function(y){
            y <- trimws(y)
            ifelse(isTruthy(y),1,0)
          })
          
          rv$df1[11:nrow(d),3:ncol(d)] <- apply(d[11:nrow(d),3:ncol(d)],c(1,2),function(y){
            y <- trimws(y)
            ifelse(isTruthy(y),1,NA)
          })
        }
        
      }
      
      
      
      # Filter dates ----
      
      observeEvent(input$filterDates,{
        req(rv$df0)
        
        # Check date formatting
        not_dates <- which(!(unlist(lapply(x[3,],function(y){IsDate(y)}))))
        
        if(length(not_dates) > 0){
          dates_modal(session,not_dates)
        }else{
          rv$v <- rv$v + 1
          
          if(isTruthy(rv$df1)){
            x <- rv$df1
          }else{
            x <- rv$df0
          }
          filter_dates(x)
        }
        
        
      })
      
      dates_modal <- function(session,d){
        ns <- session$ns
        modalDialog(
          tagList(
            div(style="text-align:center",
                h4("Dates checked"),
                div(style="text-align:center;height:100%;overflow-y:auto;overflow-x:hidden",
                    p("The following columns do not contain valid dates in the format 'dd/mm/yyyy'. Click 'Continue' to remove
                      these columns and filter columns with valid dates."),
                    p(paste(d,collapse = ",")),
                    actionButton(ns("continueDates"),"Continue"),
                    actionButton(ns("cancelDates"),"Cancel")
                )
                
            )
          ),
          footer=NULL,easyClose = F, size = "s"
        )
      }
      
      observeEvent(input$continueDates,{
        if(isTruthy(rv$df1)){
          x <- rv$df1
        }else{
          x <- rv$df0
        }
        filter_dates(x)
      })
      
      observeEvent(input$cancelDates,{
        output$plotTick <- renderText({cross})
        removeModal()
      })
      
      filter_dates <- function(x){
        showSpinner("csvTable")
        
        which_dates <- which(unlist(lapply(x[3,],
                                           function(y){
                                             IsDate(y) *
                                               (as.Date(y, format = "%d/%m/%Y") <= max) * 
                                               (as.Date(y, format = "%d/%m/%Y") >= min)
                                           }
        )
        ) >0
        )
        
        min <- as.Date(input$dateRange[1])
        max <- as.Date(input$dateRange[2])
        
        if(length(which_dates) < ncol(x)){
          x <- x[,which_dates]
          which_taxa <- which(rowSums(!is.na(x[11:nrow(x),]))>0)
          rv$df1 <- x[which_taxa,]
          
          output$csvTable <- DT::renderDataTable({ 
            
            x <- rv$df1[2:nrow(rv$df1),]
            
            DT::datatable(
              x
              ,
              colnames = as.vector(unlist(rv$df1[1,],"Hidden")),
              escape = FALSE,
              rownames = TRUE,
              selection = 'single',
              extensions = c("FixedColumns","FixedHeader","Scroller"),
              fillContainer = TRUE,
              options = list(
                columnDefs = list(list(width = '200px', targets = 2)),
                dom = "t",
                ordering=F,
                pageLength = nrow(x),
                processing = FALSE,
                language = list(zeroRecords = "No data"),
                scrollY = "60vh",
                fixedColumns = list(leftColumns = 1)
              ))%>% formatStyle(0,fontWeight="bold")
          }) 
        }
        
        hideSpinner("csvTable")
      }
      

      
      observeEvent(input$refresh,{
        rv$df1 <- rv$df0
        d_validation <- d[,3:ncol(d)]
        d_validation[] <- 1
        rv$df1_validation <- d_validation
        
        output$csvTable <- renderCsvTable()
        
        output$plotTick <- renderText({""})
        output$dateTick <- renderText({""})
        output$envTick <- renderText({""})
        output$speciesTick <- renderText({""})
        output$abundanceTick <- renderText({""})
      })
      

      
      
      
      # Import ----
      
      observeEvent(input$import,{
        req(rv$v == 5)
      })
    }
  )
}