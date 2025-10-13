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
                                         <a href='./templates/plots_import_template.csv'>here</a>.</p>"),
                          
                         
                      
                      box(title = "Load & validate data", width = 12, solidHeader = T,collapsible = T, collapsed = F,
                          column(12,
                                 p("Choose a site and file to import. Then choose the species abundance measure and click
                                   the 'validate' button to check the following."),
                                 
                                 div(style="padding-bottom:5px",
                                     div(style="float:left;padding-right:5px",HTML("<li>Plots names are all present in the database and not duplicated</li>")),
                                     uiOutput(ns("plotTick"))
                                 ),br(),
                                 div(style="padding-bottom:5px",
                                     div(style="float:left;padding-right:5px",HTML("<li>Dates are in the format DD/MM/YYYY</li>")),
                                     uiOutput(ns("dateTick"))
                                 ),br(),  
                                 div(style="padding-bottom:5px",
                                     div(style="float:left;padding-right:5px",HTML("<li>Structural variables are numeric values</li>")),
                                     uiOutput(ns("envTick"))
                                 ),br(),
                                 div(style="padding-bottom:5px",
                                     div(style="float:left;padding-right:5px",HTML("<li>Species names are matched to the taxon dictionary</li>")),
                                     uiOutput(ns("speciesTick"))
                                 ),br(),
                                 div(style="padding-bottom:5px",
                                     div(style="float:left;padding-right:5px",HTML("<li>Species abundances match the abundance type specified</li>")),
                                     uiOutput(ns("abundanceTick"))
                                 ),br(),br(),
                                 p("When the above are all are ticked, the data are ready to import."),
                                 
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
      # Spinner on table has no height on table load

      # number formatting
      # DATE ROW TO DATE PICKER
      
      # Module initialization ---- 
       shinyjs::hide("options")
      
      role <- login$role
      user <- login$username
      password <- login$password
      
      isolate({
        app_tables(tables, c("sites","surveys","plots"))
        
        uksi_load(c(0,1,2,3))
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
          open <- tables$surveys[tables$surveys$status == "open",]
          choices_surveys <- open$id
          names(choices_surveys) <- open$survey
          }else{
          choices_surveys <- c("")
          }
      
        updateSelectizeInput(session, inputId = "survey", server = F,
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
      tick <- "<div style='float:left;padding-right:5px'><i class='fa-solid fa-check' style='color:green;font-size:18px'></i></div>"
      cross <- "<div style='float:left;padding-right:5px'><i class='fa-solid fa-xmark' style='color:red;font-size:18px'></i></div>"
      
      # Reactives ----
      
      rv <- reactiveValues(df0 = NA, # CSV file
                           df1 = NA,  # Table to upload 
                           df1_validation = NA, # Validation tracker
                           df1_validation_col = NA, # column validation tracker
                           t = NA, t_validation = NA, # DF of species from df1
                           v = 0 # Validation counter, value is 5 when validation complete (dates,env,plots,abundance,taxa)
                           )
      
      # Data tables ----
      
      renderCsvTable <- function(){
        renderDT({ 
          isolate({
            x <- rv$df1[1:nrow(rv$df1),c(1,3:ncol(rv$df1))]
            
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
            
            
            
            d <- DT::datatable(
              x
              ,
              colnames = c("",as.vector(unlist(rv$df1[1,3:ncol(rv$df1)]))),
              escape = FALSE,
              rownames = FALSE,
              selection = 'none',
              extensions = c("FixedColumns","FixedHeader"),
              fillContainer = TRUE,
              editable = list(target= "cell",disable=list(columns=c(0))),
              plugins = "ellipsis",
              options = list(
                dom = "tpli",
                ordering=F,
                pageLength = 200,
                processing = TRUE,
                language = list(zeroRecords = "No data"),
                scrollY = "70vh",
                scrollX = TRUE,
                autoWidth = TRUE,
                columnDefs = list(
                  list(
                    targets = 1:(ncol(x)-1),
                      render = JS("$.fn.dataTable.render.ellipsis( 50, false )")
                      ),
                  
                  list(width="200px",targets = 0:(ncol(x)-1)),
                  list(width="50px",targets = 1:(ncol(x)-1))
                  )
                ,
                fixedColumns = TRUE,
                preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
              ))%>% formatStyle(1,fontWeight="bold") %>% 
              formatStyle(1,backgroundColor="#7af478") %>%
              formatStyle(2:ncol(x),textAlign = "center")
            
            
          })
          
          d
          
        },server=T) 
      }

      # Csv loading ----
      
      observe({
        if(isTruthy(input$csvFile) & isTruthy(input$abundance)){
          shinyjs::enable("loadCSV")
        }else{
          shinyjs::disable("loadCSV")
        }
      })
      
      invalidCsv <- function(err) {
        ns <- session$ns
        modalDialog(
          div(
            h4("Invalid csv file"),
            HTML(err)
            ,style="width:100%; text-align:center")
          , footer=NULL,size="s",easyClose=TRUE,fade=TRUE)
      }
      
      observeEvent(input$csvFile,{

        ## Read in CSV file ----
        
        file <- input$csvFile
        ext <- file$type
        
        req(file)
        shiny::validate(need(ext == "text/csv", "Please upload a csv file"))
        
        showSpinner("csvTable")
        
        d <- read.csv(file$datapath, header = FALSE,fileEncoding = "UTF-8-BOM")
        d[d == ""] <- NA
        d[is.null(d)] <- NA

        err <- NULL
        if(!identical(d[1:10,1],c("plot",
                          "visit",
                          "record_date",
                          "height",
                          "bare_ground",
                          "bryophyte_cover",
                          "litter_cover",
                          "nvc",
                          "note",
                          "recorder"))){
          err <- c(err,"Invalid row names")
        }
        if(length(unique(d[11:nrow(d),2])) != nrow(d) - 10){
          err <- c(err,"Species names repeated")
        }
        if(sum(rowSums(!is.na(d[11:nrow(d),3:ncol(d)]))==0)>0){
          err <- c(err,"Species with null rows")
        }
        
        if(isTruthy(err)){
          err <- paste0("<p>Please fix the following errors in the source file and reimport:</p>","<ul><li>",paste(err,collapse="</li><li>"),"</li></ul>")
          showModal(invalidCsv(err))
          }else{
            rv$df0 <-d
            rv$df1 <- d
            
          ### Load data table ----
          
          output$previewData <- renderUI({
            ns <- NS(id)
            tagList(
              withSpinner(
                DT::DTOutput(outputId = ns("csvTable"),height="75vh")
                ,type = 7, caption = "Loading",hide.ui = FALSE)
              )
          })
          output$csvTable <- renderCsvTable()
        }
      })
      
      # Data validation ----
      
      mode <- reactiveVal("read")
      
      ## Table Validation function ----
      validate_table <- function(data,ab,s){
        suppressWarnings({
          d <- data[,c(3:ncol(data))]
          v <- d
          v[] <- 1
          v[1,] <- d[1,] %in% tables$plots[tables$plots$site == as.numeric(s),]$plot # Check plot names
          v[1,] <- v[1,] * (sapply(d[1,],function(x) sum(x == d[1,]))==1) # check if Plot names duplicated
          
          v[3,] <- sapply(d[3,],function(x) ifelse(isTruthy(as.Date(x,format="%d/%m/%Y")),1,0))
          v[4:7,] <- as.data.frame(t(apply(d[4:7,],1, function(x) ifelse(!is.na(as.numeric(x)) | x == "" | is.na(x),1,0))))
          
          if(ab == "Domin"){
            v[11:nrow(v),] <- apply(d[11:nrow(d),],2,
                                    function(x) ifelse(x %in% 1:10 | is.na(x) | x == "",1,0))
          }
          if(ab == "Percentage cover"){
            v[11:nrow(v),] <- apply(d[11:nrow(d),],2,
                                    function(x) ifelse((is.na(x) | x == "") | (!is.na(as.numeric(x)) & as.numeric(x) <= 100 & as.numeric(x) >= 0),1,0))
          }
          if(ab == "Frequency"){
            v[11:nrow(v),] <- apply(d[11:nrow(d),],2,
                                    function(x) ifelse((is.na(x) | x == "") | (!is.na(as.numeric(x)) & as.numeric(x) <= 1 & as.numeric(x) >= 0),1,0))
          }
          if(ab == "Presence"){
            v[11:nrow(v),] <- apply(d[11:nrow(d),],2,
                                    function(x) ifelse((is.na(x) | x == "") | (!is.na(as.numeric(x)) & as.numeric(x) %in% c(0,1)),1,0))
          }
          
          return(v)
        })
      }
      
      ## Table cell validation function ----
      validate_cell <- function(i = NULL,j = NULL,ab){
        suppressWarnings({
          v <- 1
          i0 <- i
          j0 <- j
          d <- rv$df1[,c(3:ncol(rv$df1))]
         
          if(i0 %in% c(2,8,9,10)){
            return(list(v=1,i0=i0,j0=j0))
          }
          
          # Plots
          if(i0 == 1 &
             !(
               d[i0,j0] %in% tables$plots[tables$plots$site == as.numeric(input$site),]$plot &&
               sum(d[1,] == d[i0,j0]) == 1 # check if Plot names duplicated
               
               )){
            v <- 0
          }
          # Dates
          if(i0 == 3 &
             !isTruthy(as.Date(d[i0,j0],format="%d/%m/%Y"))){
            v <- 0
          }
          
          y <- trimws(d[i0,j0])
          # Structure attributes
          if(i0>3 & i0 <8 & !(suppressWarnings(isTruthy(as.numeric(y)) || !isTruthy(y)))){
            v <- 0
          }
          # Abundance
          if(i0 > 10 &
             (
               (ab == "Domin" & !(!isTruthy(y) || (isTruthy(as.integer(y)) & as.numeric(y) == as.integer(y) & as.integer(y) <= 10 & as.integer(y) >= 1))) ||
               (ab == "Percentage cover" & !(!isTruthy(y) || (isTruthy(as.numeric(y)) & as.numeric(y) <= 100 & as.numeric(y) >= 0))) ||
               (ab == "Frequency" & !(!isTruthy(y) || (isTruthy(as.numeric(y)) & as.numeric(y) <= 1 & as.numeric(y) >= 0))) ||
               (ab == "Presence" & !(!isTruthy(y) || (isTruthy(as.numeric(y)) & as.numeric(y) %in% c(0,1))))
             )
          ){
            v <- 0
          }
          
          
          r <- list(v=v,i0=i0,j0=j0)
          return(r)
        })
      }
      
      # Validation mode ----  
      
      observeEvent(input$loadCSV,{
        showSpinner("csvTable")
        
        mode("validate")
        shinyjs::disable("loadCSV")
        
        # VALIDATE VISIT 
        rv$df1_validation <- validate_table(rv$df1,input$abundance,input$site)
        
        future_promise({
          # VALIDATE SPECIES 
          t <- data.frame(t0 = trimws(rv$df1[11:nrow(rv$df1),2]))
          t$match <- lapply(rv$df1[11:nrow(rv$df1),1],function(s){ifelse(isTruthy(s),s,NA)})
          no_match <- which(is.na(t$match))
          match <- t[no_match,]  %>% left_join(uksi_pl_rec, join_by(t0 == name), relationship = "one-to-one")
          t[no_match,"match"] <- match$nbn_taxon_version_key
          t <- t[,c("t0","match")]
          
          ## NEED TO MAKE THIS FASTER
          
          for(i in 1:nrow(t)){
            c <- c("",choices_uksi_plants)
            names(c) <- c(t[i,1],names(choices_uksi_plants))

            if(!is.na(t[i,"match"])){
              updateSelectizeInput(session,
                                   paste0("taxon_",i),
                                   selected = t[i,"match"],
                                   choices= c,
                                   options = list(placeholder = t[i,1],maxOptions = 25),
                                   server=T)
              rv$df1[10+i,1] <- t[i,"match"]
              paste0("runjs(\"$('#importVeg-csvTable tr').eq(",11 + i,").find('td').eq(0).addClass('error')\");")
            }else{
              updateSelectizeInput(session,
                                   paste0("taxon_",i),
                                   selected = "",
                                   options = list(placeholder = t[i,1],maxOptions = 25),
                                   choices= c,
                                   server=T)
              paste0("runjs(\"$('#importVeg-csvTable tr').eq(",11 + i,").find('td').eq(0).removeClass('error')\");")
            }
          }
          
          return(t)
        })%...>% (function(t){
          rv$t <- t[,c("t0","match")]
          rv$t_validation <- 0
          
          hideSpinner("csvTable")
          shinyjs::enable("loadCSV")
        })
      })
      
      observeEvent(rv$t,{
        req(rv$t)
        for(i in 1:nrow(rv$t)){
          eval(parse(text=paste0("observeEvent(input$taxon_",i,", ignoreNULL = FALSE,{
                   if(!isTruthy(input$taxon_",i,")){
                   rv$t_validation <- rv$t_validation - 1
                    runjs(\"$('#importVeg-csvTable tr').eq(",11 + i,").find('td').eq(0).addClass('error')\");
                   }else{
                   rv$t_validation <- rv$t_validation + 1
                   rv$df1[10+i,1] <- input$taxon_",i,"
                    runjs(\"$('#importVeg-csvTable tr').eq(",11 + i,").find('td').eq(0).removeClass('error')\");
                    }
            })")))
        }
      })

      ## Validation tracker ----
      observeEvent(rv$df1_validation,{
        req(rv$df1_validation)
        req(mode() == "validate") 
        
        showSpinner("csvTable")
        
        # Update reactive to track columns with errors
        rv$df1_validation_col <- as.vector(colSums(rv$df1_validation)-nrow(rv$df1_validation))
        
        # Highlight cells with errors
        cells <- as.data.frame(which(rv$df1_validation == 0,arr.ind =  T))
        cells <- cells[!(cells$row %in% c(2,8,9,10)),]
        if(isTruthy(cells) & nrow(cells) > 0){
          apply(cells,1,function(x){
            i <- x[1]
            j <- x[2]
              runjs(paste0('
                $("#importVeg-csvTable tr").eq(',i+1,').find("td").eq(',j,').addClass("error");
               '))
          })
          }

        # Validation ticks
        ## Plot names tick
        if(sum(rv$df1_validation[1,2:ncol(rv$df1_validation)]) == ncol(rv$df1_validation)-1){
          output$plotTick <- renderUI({HTML(tick)})
        }else{
          output$plotTick <- renderUI({HTML(cross)})
        }
        ## Dates tick THIS IS NOT CHANGING IF CELLS ARE CORRECTED
        if(sum(rv$df1_validation[3,2:ncol(rv$df1_validation)]) == ncol(rv$df1_validation)-1){
          output$dateTick <- renderUI({HTML(tick)})
        }else{
          output$dateTick <- renderUI({HTML(cross)})
        }
        ## Structural variables tick
        if(sum(rv$df1_validation[4:7,2:ncol(rv$df1_validation)]) == 4 * (ncol(rv$df1_validation)-1)){
          output$envTick <- renderUI({HTML(tick)})
        }else{
          output$envTick <- renderUI({HTML(cross)})
        }
        ## Abundance tick
        if(sum(rv$df1_validation[11:nrow(rv$df1_validation),2:ncol(rv$df1_validation)]) == (nrow(rv$df1_validation)-10) * (ncol(rv$df1_validation)-1) ){
          output$abundanceTick <- renderUI({HTML(tick)})
        }else{
          output$abundanceTick <- renderUI({HTML(cross)})
        }
        hideSpinner("csvTable")
      })
      
      observeEvent(rv$df1_validation_col,{
        # Colour column headings for columns with errors

        err <- paste0("[",paste0(which(rv$df1_validation_col < 0),collapse=","),"]")
        val <- paste0("[",paste0(which(rv$df1_validation_col == 0),collapse=","),"]")
        
        runjs(
          paste0('var err = ',err,';
                 $("#importVeg-csvTable").find("th").filter(function(i) {
                                                                        return $.inArray(i, err) > -1;
                                                                        }).addClass("error");')
          )
        runjs(
          paste0('var val = ',val,';
                 $("#importVeg-csvTable").find("th").filter(function(i) {
                                                                        return $.inArray(i, val) > -1;
                                                                        }).removeClass("error");')
          )
        })
      
      # Species validation tick
      
      observeEvent(rv$t_validation,{
        req(rv$t_validation)
        req(rv$df1)
        ## Species tick
        if(sum(rv$t_validation) == nrow(rv$df1)-10){
          output$speciesTick <- renderUI({HTML(tick)})
        }else{
          output$speciesTick <- renderUI({HTML(cross)})
        }
      })
      
    # Update with cell edits ----
    observeEvent(input$csvTable_cell_edit, {
      r  <- input$csvTable_cell_edit$row
      c <- input$csvTable_cell_edit$col + 2
      rv$df1[r, c] <- input$csvTable_cell_edit$value
      
      if(mode() == "validate"){
        v_cell <- validate_cell(r,c-2,input$abundance)
        rv$df1_validation[v_cell$i0,v_cell$j0] <- v_cell$v
        
        if(v_cell$v == 1){
          runjs(paste0('
                $("#importVeg-csvTable tr").eq(',v_cell$i0+1,').find("td").eq(',v_cell$j0,').removeClass("error");
               '))
        }
      }
      
      if(r == 1){
        runjs(
        paste0('$("#importVeg-csvTable").find("th").eq(',input$csvTable_cell_edit$col,').html("',input$csvTable_cell_edit$value,'");')          
        )
      }
      })
      
    # IMPORT DATA ----
      
      observe({
        req(rv$df1)
        req(rv$df1_validation)
        
        if(!isTruthy(rv$df1_validation == 0) & isTruthy(input$survey) & sum(rv$t_validation) == nrow(rv$df1)-10){
          shinyjs::enable("import")
        }else{
          shinyjs::disable("import")
        }
      })
      
      observeEvent(input$import,{
        req(rv$df1)
        req(rv$df1_validation)
        req(!isTruthy(rv$df1_validation == 0) & isTruthy(input$survey) & sum(rv$t_validation) == nrow(rv$df1)-10)
        
        removeModal()
        showModal(import_progress_modal())
        
        # split out visits table
        v <- rv$df1[c(1,3,4,5,6,7,8,9,10),3:ncol(rv$df1)]
        v <- data.frame(t(v))
        colnames(v) <- c("plot_reference","record_date","height","bare_ground","bryophyte_cover",
                         "litter_cover","nvc","note","recorder")
        
      future_promise({
        con0 <- fenDb0(user,password)
        
        # RETRIEVE PLOT REFERENCES FROM SITE AND PLOT NAMES
        plotrefs <- dbGetQuery(con0,
                   paste0("SELECT plot, plot_reference FROM spatial.monitoring_vegetation WHERE site = ",as.numeric(input$site),
                          " AND ",
                          sql_in("plot",v$plot_reference)
                          )
                   )
        plotrefs <- plotrefs[order(match(plotrefs$plot,v$plot_reference)),]
        v$plot_reference <- plotrefs$plot_reference
        
        # NEED TO THROW AN ERROR HERE JUST IN CASE, THOUGH ALL THE PLOTS SHOULD MATCH !!!
        
        # INSERT IMPORT & VISITS
        q0 <- paste0("INSERT INTO records.imports (source_type,source,notes,source_attachments,schema,table_name) VALUES (",
                     2,",",
                     null_text_val(con0,input$csvFile),",",
                     null_text_val(con0,input$importNote),",",
                     FALSE,",",
                     "'records'",",",
                     "'plot_visits'",
                     ") RETURNING id")
        
          # INSERT VISITS
          
          q1<-"INSERT INTO records.plot_visits (plot_reference,record_date,height,bare_ground,bryophyte_cover,litter_cover,nvc,note,recorder,survey,import) VALUES \n"
          Q <- apply(v,1,function(x){
            paste0("(",paste(
              c(null_text_val(con0,x[1]),
                null_date_val(as.Date(x[2],"%d/%m/%Y")),
                null_num_val(x[3]),
                null_num_val(x[4]),
                null_num_val(x[5]),
                null_num_val(x[6]),
                null_text_val(con0,x[7]),
                null_text_val(con0,x[8]),
                null_text_val(con0,x[9]),
                as.numeric(input$survey),
                "(SELECT id FROM imp)")
              ,collapse=","),")")
          })
          
          q <- paste0(
                      "WITH imp AS (",q0,") \n",
                      q1,
                      paste(Q,collapse=", \n"),"\n RETURNING plot_reference_visit")
          plot_reference_visits <- dbGetQuery(con0,q)        
          
          if(isTruthy(plot_reference_visits)){
            
            # INSERT SPECIS DATA
            # HANDLE DUPLICATED SPECIES ENTRIES !!!
            # MAKE SURE NUMBER OF COLUMNS MATCH NUMBER OF VISITS !!!
            # Are the plots refs definitely going to be in the right order?!
            spp <- rv$df1[11:nrow(rv$df1),-c(2)]
            rownames(spp) <- spp[,1]
            spp <- spp[,-1]
            colnames(spp) <- plot_reference_visits$plot_reference_visit
            spp <- dematrify(as.data.frame(t(spp)) )
            ab <- c("domin","cover","frequency","presence")[which(c("Domin","Percentage cover","Frequency","Presence") == input$abundance)] # choose name for abundance column following plot_data column names
            colnames(spp)<-c("plot_reference_visit","taxon_nbn",paste0("abundance_",ab)) # Column names of dataframe to match plot_data table column names
            
            pgWriteGeom(con0,name=c("records","plot_data"),data.obj = spp, partial.match = T, overwrite = F)
            dbDisconnect(con0)
            return(TRUE)
          }else{
            dbDisconnect(con0)
            return(FALSE)
          }
            
        })%...>% (function(r){
          if(r){
            showModal(import_success_modal())
          }else{
            showModal(import_error_modal())
          }

        })
        })
      
      #Modal for upload progress
      import_progress_modal <- function() {
        ns <- session$ns
        modalDialog(
          div(
            tags$h4("Importing",class="loading")
            ,style="width:100%; text-align:left")
          , footer=NULL,size="s",easyClose=FALSE,fade=TRUE)
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
      
    }
  )
}



