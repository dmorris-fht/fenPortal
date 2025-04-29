# Choices for match type

match_choices_sites <- c(0,1,2,3,4)
names(match_choices_sites) <- c("All","No matches","Uniquely matched","Ambiguously matched","No grid reference")

match_choices_taxa <- match_choices_sites[1:4]

# Import UI

importRecordsUI <- function(id){
  ns <- NS(id)
  tagList(
    withSpinner(
    column(12,
           tabsetPanel(id = ns("importTabset"),
                       tabPanel("1 - Import file",value = "importPanel",
                                column(12,
                                       h3("Import records"),
                                       HTML("<p>Import a comma-separated value (.csv) file of records. 
                                         Download the import template from <a href='./templates/records_import_template.csv'>here</a>.</p>
                                            <p>Follow the workflow to validate the imported file and then upload the records to the database.</p>
                                            "),
                                      column(3,
                                             fileInput(ns("import"),label="Choose .csv file to import:", 
                                                  buttonLabel = "Browse...",
                                                  placeholder = "No file selected",
                                                  accept = ".csv")
                                                )
                                       ,
                                      column(3,
                                                 checkboxInput(ns("invalid"),label="Show invalid rows",value = FALSE),
                                                 actionButton(ns("match"),label = "Match sites and taxa")
                                             )
                                       ),
                                column(12,
                                    div(id = ns("csvSpinner"),
                                        style = "width:100%;overflow-y:scroll;min-height:600px;font-size:10px;border: 1px solid black; border-radius: 5px; padding: 10px ",
                                        withSpinner(
                                            DT::DTOutput(outputId = ns("csvTable"))
                                          ,type = 7, caption = "Loading")
                                      )
                                    )
                                ),
                       tabPanel("2 - Match sites",value = "matchSitesPanel",
                                column(5,
                                       column(12,
                                              h3("Match sites"),
                                              p("Use the table to match each combination of site name and grid reference to a site in the database."),
                                              tags$ul(
                                                tags$li("If a grid reference is given then it will be automatically matched against sites within 10 metres."),
                                                tags$li("If there is one overlapping site, then the site is automatically matched. For multiple matches, you must choose one."),
                                                tags$li("If there is no overlap, then you must edit the grid reference until there is one, or leave it unmatched."),
                                                tags$li("If no grid reference is given then you'll need to choose a site.")
                                              ),
                                              p("If sites are left unmatched, then the corresponding records will not be imported.")
                                              
                                              ),
                                       column(3,
                                              selectInput(
                                                ns("unmatched1"),
                                                "Match type",
                                                choices = match_choices_sites,
                                                selected = 0
                                                )                                              
                                              )
                                       ),
                                column(7,
                                       div(
                                         style = "font-size:12px;border: 1px solid black; border-radius: 5px; padding: 10px ",
                                         withSpinner(DT::DTOutput(outputId = ns("matchSitesTable")),type = 7)
                                       )
                                       )
                                ),
                       tabPanel("3 - Match taxa",value = "matchTaxaPanel",
                                column(5,
                                       column(12,
                                              h3("Match taxa"),
                                              p("Use the table to match each taxon name with a taxon from the UK Species Inventory (UKSI)."),
                                              tags$ul(
                                                tags$li("If a taxon version key was given in the record, it will be automatically matched."),
                                                tags$li("If there is more than one taxon in the UKSI with a matching name, then choose a taxon to use."),
                                              ),
                                              p("If taxa are left unmatched, then the corresponding records will not be imported.")
                                              ),
                                       column(3,
                                              selectInput(
                                                ns("unmatched2"),
                                                "Match type",
                                                choices = match_choices_taxa,
                                                selected = 0
                                              )                                              
                                       )
                                ),
                                column(7,
                                       div(
                                         style = "font-size:12px;border: 1px solid black; border-radius: 5px; padding: 10px ",
                                         withSpinner(DT::DTOutput(outputId = ns("matchTaxaTable")),type = 7)
                                       )
                                )
                                ),
                       tabPanel("4 - Upload records",id = ns("uploadPanel"),
                                column(5,
                                       h4("Upload records"),
                                       p("Once sites and taxa have been matched, choose a data source and click 'Upload records' to upload 
                                         records to the database. Records corresponding to unmatched sites or taxa will not be imported.")
                                       ),
                                column(12,
                                       column(3,
                                              selectizeInput(ns("survey"),
                                                             "Data source",
                                                             choices=c(""),
                                                             selected="",
                                                             multiple = TRUE, 
                                                             options = list(maxItems = 1,
                                                                            placeholder = "Choose a data source")
                                              ),
                                              textAreaInput(inputId = ns("import_notes"),
                                                            label = "Import notes",
                                                            value = NULL,
                                                            resize = "vertical"),
                                              actionButton(ns("upload"),"Upload records")
                                              )
                                       )
                                )
                       )
           )
    ,
    id = ns("module"),
    type = 4,
    size = 2,
    proxy.height = "100%",
    hide.ui = TRUE,
    caption = "Loading module")
    ,
    tags$script(src ="script.js"),
    tags$script(
      HTML("$('#importRecords-module').parent().removeClass('shiny-spinner-hidden')")
    )
  )
}

# Import server

importRecordsServer <- function(id, login, tables) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Module initialisation ----
        
        source("./R/modals/record_import_modals.R")
      
        user <- login$username
        password <- login$password
        
        isolate({
          app_tables(tables, c("sites","subsites","surveys"))
          
          uksi_load(c(0))
        })
        
        observe({
          req(tables$sites)
          req(tables$surveys)
          req(tables$subsites)
          req(choices_uksi)
          
          runjs(
            "$('#importRecords-module').parent().addClass('shiny-spinner-hidden');
          $('div[data-spinner-id=\"import-module\"]').css('display','inline')
          "
          )
        })
        
        sites_choices <- reactive({
          if(isTruthy(tables$sites)){
            c <- tables$sites$id
            names(c) <- tables$sites$site
          }
          else{
            c <- c("")
          }
          return(c)
        })

        rv <- reactiveValues(df = NULL, dt_row = NULL, 
                             df_s = NULL, dt_row_s = NULL, 
                             df_t = NULL, dt_row_t = NULL, 
                             mode = NA)
       
      #Data tables ----
      ## Csv table  ----
      x0 <- read.csv("./www/templates/records_import_template.csv", header = T)
      x1 <- x0
      x1[1,] <- NA
      x1$Buttons <- NA
      x1$valid <- NA
      x1 <- x1[0,]
      
      cols <- c("site_record",
                "gridref",
                "taxon_record",
                "quantity","status","sex","stage","habitat","note",
                "recorder","determiner","method","sample",
                "record_date","record_year","record_date_start","record_date_end","start_year","end_year","start_month","end_month",
                "origin_key"
                )
      
      output$csvTable <- DT::renderDataTable(
        {
          DT::datatable(x1[,c(cols,"Buttons","valid")]
                        ,
                        escape = FALSE,
                        rownames = FALSE,
                        selection = 'single',
                        filter = list(position='top'),
                        colnames <- c(cols, "","valid"),
                        options = list(processing = FALSE,
                                       dom = 'tlpi',
                                       language = list(zeroRecords = "No records"),
                                       columnDefs = list(
                                         list(visible = FALSE, targets = c(length(cols)+1)),
                                         list(orderable = FALSE, targets = c(length(cols))),
                                         #list(targets = c(ncol(x1)-2,ncol(x1)-1),searchable = FALSE),
                                         list(width = '60px',targets=c(length(cols)))
                                       )
                                       ,
                                       extensions = c("FixedHeader", "Scroller"),
                                       fixedHeader = TRUE,
                                       scrollY = "500"
                        )
                        ) %>% formatStyle("valid",target='row',backgroundColor = styleEqual(0,"rgb(255, 77, 0,0.3)"))
        }
      )
      
      proxy1 <- DT::dataTableProxy("csvTable")
      
      observe({
        req(isTruthy(rv$df))
        if(input$invalid == TRUE){
          x1 <- rv$df[rv$df$valid == 0,c(cols,"Buttons","valid")]
        }
        if(input$invalid == FALSE){
          x1 <- rv$df[,c(cols,"Buttons","valid")]
        }
        
        DT::replaceData(proxy1, x1, resetPaging = FALSE, rownames = FALSE) %>%
          updateFilters(data = x1)
      })
      
      ## Site table ----
      x2 <- data.frame(site_record = character(), 
                       gridref = character(),
                       site_match = character(),
                       subsite = character(), 
                       Buttons = character()
                       )
      
      output$matchSitesTable <- DT::renderDataTable(
        {
          datatable(x2,escape = FALSE,
                    rownames = FALSE,
                    selection = 'single',
                    filter = list(position='top'),
                    colnames = c(colnames(x2)[1:(ncol(x2)-1)],""),
                    options = list(processing = FALSE,
                                   dom = 'tlpi',
                                   language = list(zeroRecords = "No sites"),
                                   columnDefs = list(
                                     list(orderable = FALSE, targets = c(ncol(x2)-1)),
                                     list(width = '60px',targets=c(ncol(x2)-1))
                                   ),
                                   extensions = c("FixedHeader"),#, "Scroller")
                                   fixedHeader = TRUE,
                                   scrollY = "50vh"
                    )
                    )
        }
      )
      
      proxy2 <- DT::dataTableProxy("matchSitesTable")
      
      observe({
        req(isTruthy(rv$df_s))
        if(input$unmatched1 == 0){
          x2 <- rv$df_s[,c("site_record","gridref","site_match","subsite","Buttons")]
        }
        if(input$unmatched1 == 1){
          x2 <- rv$df_s[is.na(rv$df_s$site_match),c("site_record","gridref","site_match","subsite","Buttons")]
        }
        if(input$unmatched1 == 2){
          x2 <- rv$df_s[which(rv$df_s$length_match == 1),c("site_record","gridref","site_match","subsite","Buttons")]
        }
        if(input$unmatched1 == 3){
          x2 <- rv$df_s[which(rv$df_s$length_match > 1),c("site_record","gridref","site_match","subsite","Buttons")]
        }
        if(input$unmatched1 == 4){
          x2 <- rv$df_s[is.na(rv$df_s$gridref) ,c("site_record","gridref","site_match","subsite","Buttons")]
        }
        DT::replaceData(proxy2, x2, resetPaging = FALSE, rownames = FALSE) %>%
          updateFilters(data = x2)
      })
      
      ## Taxon table ----
      x3 <- data.frame(taxon_record = character(), taxon_match = character(), Buttons = character(), match = character()
      )
      
      output$matchTaxaTable <- DT::renderDataTable(
        {
          datatable(x3,
                    escape = FALSE,
                    rownames = FALSE,
                    selection = 'single',
                    filter = list(position='top'),
                    colnames = c(c("taxon_record","taxon_match"),"","match"),
                    options = list(processing = FALSE,
                                   dom = 'tlpi',
                                   language = list(zeroRecords = "No taxa"),
                                   columnDefs = list(
                                     list(orderable = FALSE, targets = 2),
                                     list(visible = FALSE, targets = 3),
                                     list(width = '60px',targets=c(2))
                                   ),
                                   extensions = c("FixedHeader"),#, "Scroller")
                                   fixedHeader = TRUE,
                                   scrollY = "50vh"
                                   )
                    ) %>% 
            formatStyle("match",target='row',backgroundColor = styleEqual(0,"rgb(255, 77, 0,0.3)"))  %>% 
            formatStyle("match",target='row',backgroundColor = styleEqual(2,"rgb(255, 200, 0,0.3)")) 
        }
      )
      
      proxy3 <- DT::dataTableProxy("matchTaxaTable")
      
      observe({
        req(isTruthy(rv$df_t))
        
        x3 <- rv$df_t[,c("taxon_record","taxon_match","Buttons")]
        x3$match <- apply(rv$df_t[c("tvks_match","length_match")],1,function(x) ifelse(is.na(x[1]),0,ifelse(x[2]==1,1,2)))
          
        if(input$unmatched2 == 0){
          x3 <- x3
          }
        if(input$unmatched2 == 1){
          x3 <- x3[is.na(rv$df_t$tvks_match),c("taxon_record","taxon_match","Buttons","match")]
        }
        if(input$unmatched2 == 2){
          x3 <- x3[which(rv$df_t$length_match == 1),c("taxon_record","taxon_match","Buttons","match")]
        }
        if(input$unmatched2 == 3){
          x3 <- x3[which(rv$df_t$length_match > 1),c("taxon_record","taxon_match","Buttons","match")]
        }
        DT::replaceData(proxy3, x3, resetPaging = FALSE, rownames = FALSE) %>%
          updateFilters(data = x3)
      })

      # Import csv ----
      observe({
        req(input$import)
        
        file <- input$import
        ext <- file$type

        req(file)
        shiny::validate(need(ext == "text/csv", "Please upload a csv file"))
        
        showSpinner("csvTable")
        d <- read.csv(file$datapath, header = TRUE)
        
        if(identical(colnames(d), colnames(x0)) == TRUE){
          # validate input
          d[d==""] <- NA
          d$gridref <- as.character(d$gridref)
          d$gridref <- apply(d[c("gridref")],1,function(g)toupper(gsub(" ","",blank(g))))
          d$site_record <- as.character(d$site_record)
          d$taxon_nbn <- as.character(d$taxon_nbn)
          d$taxon_nbn <- apply(d["taxon_nbn"],1,trimws)
          d$taxon_record <- as.character(d$taxon_record)
          d$taxon_record <- apply(d["taxon_record"],1,trimws)
          d[apply(d[c("taxon_nbn")],1,function(x) !isTruthy(x)),c("taxon_nbn")] <- NA
          d$valid <- 
            apply(d[c("gridref",
                               "record_date",
                                "record_year",
                               "record_date_start",
                               "record_date_end",
                               "start_year",
                               "end_year",
                               "start_month",
                               "end_month",
                               "site_record",
                               "taxon_record",
                               "taxon_nbn"
                               )],1,import_validation) *
            (!is.na(match(d$taxon_nbn , uksi_full$nbn_taxon_version_key)) | is.na(d$taxon_nbn))
            # checks all columns

          # add DT rows
          d$id <- seq.int(nrow(d)) # add id row
          d <- add_edit_del_btns(d,"csv") # add buttons

          updateCheckboxInput(session, "invalid", value = FALSE)
          
          if(sum(d$valid) != nrow(d)){
            showModal(
              modalDialog(
                div(style="text-align:center",
                    h4("Invalid records"),
                    p("One or more records have invalid fields. Correct the highlighted rows before proceeding.")
                )
                ,footer=NULL,size="s",easyClose=TRUE,fade=TRUE
              )
            )
          }
          
        }else{
          d <- x1
          modalDialog(
            div(style="text-align:center",h4("Invalid table structure")),
            size = "m",
            easyClose = TRUE,
            footer = NULL
          ) %>% showModal()
        }
  
        rv$df <- d
        hideSpinner("csvTable")
        })
      
      # Match sites and taxa ---- 
      
        ## Enable / disable controls ----
      
      shinyjs::disable("invalid")
      shinyjs::disable("match")
      
      observe({
        req(rv$df)
        if(sum(rv$df$valid) == nrow(rv$df)){
          # Only show this modal if on the import tab - otherwise it appears when 
          # sites / taxa are edited in the match tables on subsequent tabs
          if(input$importTabset == "importPanel"){
            showModal(
              modalDialog(
                div(style="text-align:center",
                    h4("All records valid"),
                    p("Click the 'Match sites and taxa' button to proceed.")
                )
                ,footer=NULL,size="s",easyClose=TRUE,fade=TRUE
              )
            )          }
          
          # Controls on import tab
          updateCheckboxInput(session,"invalid",value = FALSE)
          
          shinyjs::disable("invalid")
          shinyjs::enable("match")
        }else{
          # Controls on import tab
          shinyjs::enable("invalid")
          shinyjs::disable("match")
        }
      })
        
        ## Run the matching ----
      observeEvent(input$match,{
        req(sum(rv$df$valid) == nrow(rv$df))
        
        showSpinner("matchSitesTable")
        ## Match site ----
        
          req(tables$sites0)
          
          site_match <- unique(rv$df[,c("site_record","gridref")])
          d <- site_match
          d$geometry <- st_sfc(d$geometry)
          d[which(!is.na(d$gridref)),]$geometry <- apply(d[which(!is.na(d$gridref)),][c("gridref")],1,gf_sf)
          d <- st_as_sf(d)
          
          st_crs(d) <- st_crs(27700)
          int <- st_intersects(st_buffer(d,10),tables$sites0) # sites within 10m of grid squares
          
          site_match$ids_match <- lapply(int,function(x) tables$sites[unlist(x),c("id")]) # ids of matched sites
          site_match$sites_match <- lapply(int,function(x) tables$sites[unlist(x),c("site")]) # names of matched sites
          
          site_match$length_match <- apply(site_match[c("ids_match")],1,function(x) length(unlist(x)))
          u <- which(site_match$length_match == 1) # unique matches
          site_match$site_match <- NA
          site_match[u,c("site_match")] <- apply(site_match[u,][c("sites_match")],1,unlist)
          site_match$id_match <- NA
          site_match[u,c("id_match")] <- apply(site_match[u,][c("ids_match")],1,unlist)
          
          site_match$subsite <- NA
          site_match$id <- seq.int(nrow(site_match))
          site_match <- add_edit_del_btns(site_match,"sites")
          
          rv$df_s <- site_match
          
          hideSpinner("matchSitesTable")
        
        ## Match taxa ----
        showSpinner("matchTaxaTable")
        future_promise({
          taxon_match <- unique(rv$df[,c("taxon_record","taxon_nbn")])

          taxon_match$tvk_match <- NA
          taxon_match$taxon_match <- NA
          taxon_match$tvks_match <- NA
          taxon_match$taxa_match <- NA
          
          m <- which((taxon_match$taxon_nbn %in% uksi_full$nbn_taxon_version_key) == TRUE) # NBN keys match
          n <- which(is.na(taxon_match$taxon_nbn) | !(taxon_match$taxon_nbn %in% uksi_full$nbn_taxon_version_key)) # no valid tvk
          
          # Add names to valid tvk
          taxon_match[m,c("tvks_match")] <- taxon_match[m,c("taxon_nbn")] 
          match <- (taxon_match[m,] %>% left_join(uksi_full, join_by(taxon_nbn == nbn_taxon_version_key), relationship = "one-to-one"))
          taxon_match[m,c("taxa_match")] <- match[,c("full_name")]
            
          # Attempt to match record taxon with a uksi taxon, using the uksi taxon name
          no_tvk <- taxon_match[n,][c("taxon_record")] %>% left_join(uksi_full, join_by(taxon_record == name), relationship = "one-to-many")
          no_tvk <- no_tvk %>% group_by(taxon_record) %>% summarise(tvks_match = list(nbn_taxon_version_key), taxa_match = list(full_name))
          no_tvk$tvk_match <- NA
          no_tvk$taxon_match <- NA
          no_tvk$taxon_nbn <- NA
          
          taxon_match[n,] <- no_tvk[c("taxon_record","taxon_nbn","tvk_match","taxon_match","tvks_match","taxa_match")]
          
          taxon_match$length_match <- apply(taxon_match[c("tvks_match")],1,function(x) length(unlist(x)))
          
          # u <- which(taxon_match$length_match == 1)
          # taxon_match[u,c("tvk_match")] <- apply(taxon_match[u,][c("tvks_match")],1,unlist)
          # taxon_match[u,c("taxon_match")] <- apply(taxon_match[u,][c("taxa_match")],1,unlist)
          
          taxon_match[c("tvk_match")] <- apply(taxon_match[c("tvks_match")],1,function(x) unlist(x)[1])
          taxon_match[c("taxon_match")] <- apply(taxon_match[c("taxa_match")],1,function(x) unlist(x)[1])
          
          taxon_match$id <- seq.int(nrow(taxon_match))
          taxon_match <- add_edit_del_btns(taxon_match,"taxa")
          
          return(taxon_match)
          })%...>% (function(taxon_match) {
            rv$df_t <- taxon_match
            hideSpinner("matchTaxaTable")
          })
      })
      
      # DT row buttons ----
      
      # Reactive to hold matched choices of site / taxa
      match_choices <- reactive({
            req(rv$dt_row)
            if(rv$mode == "site"){
              c <- c("")
              if(rv$df_s[rv$dt_row,c("length_match")] > 0){
                c <- unlist(rv$df_s[rv$dt_row,c("ids_match")])
                names(c) <- unlist(rv$df_s[rv$dt_row,c("sites_match")])
                
              }
              if(is.na(rv$df_s[rv$dt_row,c("gridref")])){
                c <- sites_choices()
              }
              
            }
            if(rv$mode == "taxon"){
              c <- c("")
              if(rv$df_t[rv$dt_row,c("length_match")] > 0){
                c <- unlist(rv$df_t[rv$dt_row,c("tvks_match")])
                names(c) <- unlist(rv$df_t[rv$dt_row,c("taxa_match")])
                
                c <- c(c, choices_uksi)
              }
              if(is.na(rv$df_t[rv$dt_row,c("tvks_match")])){
                c <- choices_uksi
              }
            }
          
        c
      })
      
      # Update site choices when gridref changes
      observeEvent(input$match_gridref,{
        req(isGridref(input$match_gridref))
        req(rv$dt_row)
        req(rv$mode == "site")
        req(input$match_gridref != rv$df_s[rv$dt_row,c("gridref")])
        
        if(isTruthy(input$match_gridref)){
          g <- st_sfc(gf_sf(input$match_gridref))
          st_crs(g) <- st_crs(27700)
          int <- st_intersects(st_buffer(g,10),tables$sites0) # sites within 10m of grid squares
          
          c <- tables$sites[unlist(int),c("id")]
          names(c) <- tables$sites[unlist(int),c("site")]
        }else{
          c <- sites_choices()
        }
        
        updateSelectizeInput(session, "site_match", choices = c, selected = "")
      })
      
      # Subsite choices
      match_subsites <- reactive({
        req(input$site_match)
        req(tables$subsites)
        
        ss <- tables$subsites[tables$subsites$site == as.numeric(input$site_match),c("id","subsite")]
        
        if(nrow(ss) > 0){
          c <- ss$id
        names(c) <- ss$subsite
        }else{
          c<-c("")
        }
        
        c
      })
      
      observeEvent(input$site_match,{
        updateSelectizeInput(session, "subsite", choices = match_subsites(), selected = "")
      })
      
      # Row click event
      observeEvent(input$current_id, {
        # Delete row from df
        if(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "csv_del")){
          rv$dt_row <- which(stringr::str_detect(rv$df$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
          rv$mode <- "record"
          delete_row(session)
        }
        # Edit df row
        if(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "csv_edit")){
          rv$dt_row <- which(stringr::str_detect(rv$df$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
          edit_df_row(session,rv$df[rv$dt_row,])

          # Modal validation

          iv <- InputValidator$new()
          iv$add_rule("gridref",function(value){
            if(isTruthy(input$gridref) && !isGridref(input$gridref)){
              return("Invalid grid reference format")
            }
          })
          iv$add_rule("gridref",function(value){
            if(!isTruthy(input$site_record) && !isTruthy(input$gridref)){
              return("'site_record' and 'gridref' cannot both be blank")
            }
          })
          iv$add_rule("site_record",function(value){
            if(!isTruthy(input$site_record) && !isTruthy(input$gridref)){
              return("'site_record' and 'gridref' cannot both be blank")
            }
          })
          
          iv$add_rule("taxon_nbn",function(value){
            if(!isTruthy(input$taxon_nbn) && !isTruthy(input$taxon_record)){
              return("'taxon_record' and 'taxon_nbn' cannot both be blank")
            }
          })
          iv$add_rule("taxon_record",function(value){
            if(!isTruthy(input$taxon_nbn) && !isTruthy(input$taxon_record)){
              return("'taxon_record' and 'taxon_nbn' cannot both be blank")
            }
          })
          iv$add_rule("taxon_nbn",function(value){
            if(isTruthy(input$taxon_nbn) && !(input$taxon_nbn %in% uksi_full$nbn_taxon_version_key)){
              return("Invalid NBN taxon version key")
            }
          })
          
          
          iv$add_rule("record_date",function(value){
            if(!(IsDate(input$record_date) || !isTruthy(input$record_date))){
              return("Invalid date format")
            }
          })
          
          iv$add_rule("record_year",function(value){
            if(!year_check(input$record_year)){
              return("Invalid year")
            }
          })
          
          iv$add_rule("record_date_start",function(value){
            if(!(IsDate(input$record_date_start) || !isTruthy(input$record_date_start))){
              return("Invalid date format")
            }
          })
          iv$add_rule("record_date_end",function(value){
            if(!(IsDate(input$record_date_end) || !isTruthy(input$record_date_end))){
              return("Invalid date format")
            }
          })
          iv$add_rule("record_date_start",function(value){
            if(
              !((IsDate(input$record_date_start) && IsDate(input$record_date_end) && (as.Date(input$record_date_start, "%d/%m/%Y") <= as.Date(input$record_date_end, "%d/%m/%Y"))) || !isTruthy(input$record_date_start) || !isTruthy(input$record_date_end))
            ){
              return("Invalid date range")
            }
          })
          iv$add_rule("record_date_end",function(value){
            if(
              !((IsDate(input$record_date_start) && IsDate(input$record_date_end) && (as.Date(input$record_date_start, "%d/%m/%Y") <= as.Date(input$record_date_end, "%d/%m/%Y"))) || !isTruthy(input$record_date_start) || !isTruthy(input$record_date_end))
            ){
              return("Invalid date range")
            }
          })
          
          iv$add_rule("start_year",function(value){
            if(!year_check(input$start_year)){
              return("Invalid year")
            }
          })
          iv$add_rule("end_year",function(value){
            if(!year_check(input$end_year)){
              return("Invalid year")
            }
          })
          iv$add_rule("start_year",function(value){
            if(
              !((year_check(input$start_year) && year_check(input$end_year) && as.numeric(input$start_year) <= as.numeric(input$end_year) || !isTruthy(input$start_year) || !isTruthy(input$end_year)))
            ){
              return("Invalid year range")
            }
          })
          iv$add_rule("end_year",function(value){
            if(
              !((year_check(input$start_year) && year_check(input$end_year) && as.numeric(input$start_year) <= as.numeric(input$end_year) || !isTruthy(input$start_year) || !isTruthy(input$end_year)))
            ){
              return("Invalid year range")
            }
          })
          
          iv$add_rule("start_month",function(value){
            if(!month_check(input$start_month)){
              return("Invalid month")
            }
          })
          iv$add_rule("end_month",function(value){
            if(!month_check(input$end_month)){
              return("Invalid month")
            }
          })
          iv$add_rule("start_month",function(value){
            if(
              !(
                (month_check(input$start_month) && month_check(input$end_month) && ( (input$start_year == input$end_year && as.numeric(input$start_month) <= as.numeric(input$end_month)) ||  input$start_year != input$start_year ) ) || !isTruthy(input$start_month) || !isTruthy(input$end_month) # month range
              )
            ){
              return("Invalid month range")
            }
          })
          iv$add_rule("end_month",function(value){
            if(
              !(
                (month_check(input$start_month) && month_check(input$end_month) && ( (input$start_year == input$end_year && as.numeric(input$start_month) <= as.numeric(input$end_month)) ||  input$start_year != input$start_year ) ) || !isTruthy(input$start_month) || !isTruthy(input$end_month) # month range
              )
            ){
              return("Invalid month range")
            }
          })
          
          iv$enable()
          
          shinyjs::disable("final_edit_r")
          
          observe({
            req(
              isTruthy(
              import_validation(c(
              blank(input$gridref),
              blank(input$record_date),
              blank(input$record_year),
              blank(input$record_date_start),
              blank(input$record_date_end),
              blank(input$start_year),
              blank(input$end_year),
              blank(input$start_month),
              blank(input$end_month),
              blank(input$site_record),
              blank(input$taxon_record),
              blank(input$taxon_nbn)
            ))) 
            )
            
            if(
              import_validation(c(
                blank(input$gridref),
                blank(input$record_date),
                blank(input$record_year),
                blank(input$record_date_start),
                blank(input$record_date_end),
                blank(input$start_year),
                blank(input$end_year),
                blank(input$start_month),
                blank(input$end_month),
                blank(input$site_record),
                blank(input$taxon_record),
                blank(input$taxon_nbn)
              )) == 1
            ){
              shinyjs::enable("final_edit_r")
            }else{
              shinyjs::disable("final_edit_r")
            }
          })
        }
        
        # Delete row from df_s
        if(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "sites_del")){
          rv$dt_row <- which(stringr::str_detect(rv$df_s$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
          rv$mode <- "site"
          delete_row(session)
        }
        
        # Edit df_s row
        if(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "sites_edit")){
          rv$dt_row <- which(stringr::str_detect(rv$df_s$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
          rv$mode <- "site"
          
          s <- ifelse(rv$df_s[rv$dt_row,c("length_match")] == 1,rv$df_s[rv$dt_row,c("id_match")],"")
          edit_df_s_row(session,rv$df_s[rv$dt_row,c("site_record","gridref")],match_choices(), s)
          updateSelectizeInput(session, "subsite", choices = match_subsites(), selected = "")
          
          # validation
          iv <- InputValidator$new()
          iv$add_rule("site_match",sv_required())
          iv$add_rule("match_gridref",function(value){
            if(isTruthy(input$match_gridref) && !isGridref(input$match_gridref)){
              return("Invalid grid reference format")
            }
          })
          iv$enable()
          
          shinyjs::disable("final_edit_s")
          
          observe({
            if(isTruthy(input$site_match) && nchar(input$site_match) > 0){
              shinyjs::enable("final_edit_s")
            }else{
              shinyjs::disable("final_edit_s")
            }
          })
        }
      
        # Delete row from df_t
        if(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "taxa_del")){
          rv$dt_row <- which(stringr::str_detect(rv$df_t$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
          rv$mode <- "taxon"
          delete_row(session)
        }
        
        # Edit df_t row
        if(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "taxa_edit")){
          rv$dt_row <- which(stringr::str_detect(rv$df_t$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
          rv$mode <- "taxon"
          
          edit_df_t_row(session,rv$df_t[rv$dt_row,c("taxon_record")])
          
          if(!is.na(rv$df_t[rv$dt_row,c("tvk_match")])){
            updateSelectizeInput(session, "tvk_match", 
                                 choices = match_choices(), 
                                 server = TRUE,
                                 selected = rv$df_t[rv$dt_row,c("tvk_match")])
          }else{
            updateSelectizeInput(session, "tvk_match", 
                                 choices = choices_uksi, 
                                 server = TRUE,
                                 selected = "")
          }

          shinyjs::disable("match_taxon_record")
          
          # Validation
          iv <- InputValidator$new()
          iv$add_rule("tvk_match",sv_required())
          iv$enable()
          
          shinyjs::disable("final_edit_t")
          observe({
            if(isTruthy(input$tvk_match) && nchar(input$tvk_match) > 0){
              shinyjs::enable("final_edit_t")
            }else{
              shinyjs::disable("final_edit_t")
            }
          })
        }
        })
      
      # Delete rows ----
      observeEvent(input$del_0,{removeModal()})
      
      observeEvent(input$del_1,{
        if(rv$mode == "record"){
          rv$df <- rv$df[-c(rv$dt_row),]
          removeModal()
        }
        if(rv$mode == "site"){
          rv$df_s <- rv$df_s[-c(rv$dt_row),]
          removeModal()
        }
        if(rv$mode == "taxon"){
          rv$df_t <- rv$df_t[-c(rv$dt_row),]
          removeModal()
        }
        })
      
      # Record edit submit button ----
      
      observeEvent(input$final_edit_r,{
        req(import_validation(c(
          input$gridref,
          input$record_date,
          input$record_year,
          input$record_date_start,
          input$record_date_end,
          input$start_year,
          input$end_year,
          input$start_month,
          input$end_month,
          input$site_record,
          input$taxon_record,
          input$taxon_nbn
        )) == 1)
        
        rv$df[rv$dt_row,
              c(
                "site_record",
                "gridref",
                "taxon_record",
                "taxon_nbn",
                "quantity",
                "status",
                "sex",
                "stage",
                "habitat",
                "note",
                "recorder",
                "determiner",
                "method",
                "sample",
                "record_date",
                "record_year",
                "record_date_start",
                "record_date_end",
                "start_year",
                "end_year",
                "start_month",
                "end_month",
                "origin_key"
                )] <- c(
                  blank(input$site_record),
                  blank(input$gridref),
                  blank(input$taxon_record),
                  blank(input$taxon_nbn),
                  blank(input$quantity),
                  blank(input$status),
                  blank(input$sex),
                  blank(input$stage),
                  blank(input$habitat),
                  blank(input$note),
                  blank(input$recorder),
                  blank(input$determiner),
                  blank(input$method),
                  blank(input$sample),
                  blank(input$record_date),
                  blank(input$record_year),
                  blank(input$record_date_start),
                  blank(input$record_date_end),
                  blank(input$start_year),
                  blank(input$end_year),
                  blank(input$start_month),
                  blank(input$end_month),
                  blank(input$origin_key)
              )
        rv$df[rv$dt_row,c("valid")] <- 1
        removeModal()
      })
      
      # Site edit submit button ----
      observeEvent(input$final_edit_s,{
        req(input$site_match)
        
        # Update records if site_record or gridref edited
        if(!identical(rv$df_s[rv$dt_row,c("site_record","gridref")],
                      c(input$match_site_record, input$match_gridref))){
          r <- rv$df_s[rv$dt_row,]
          s <- which(
            (rv$df$site_record == r[,c("site_record")] & rv$df$gridref == r[,c("gridref")]) |
              (rv$df$site_record == r[,c("site_record")] & is.na(rv$df$gridref) & is.na(r[,c("gridref")])) |
              (is.na(rv$df$site_record) & is.na(r[,c("site_record")]) & rv$df$gridref == r[,c("gridref")])
          ) # records in data where sites and gridrefs match
          rv$df[s, c("site_record")] <- input$match_site_record
          rv$df[s, c("gridref")] <- input$match_gridref
        }
        
        # Update site match table
        rv$df_s[rv$dt_row,
              c("site_record",
                "gridref",
                "id_match",
                "site_match",
                "subsite"
              )] <- c(
                blank(input$match_site_record),
                blank(input$match_gridref),
                as.numeric(input$site_match),
                tables$sites[tables$sites$id == input$site_match,c("site")],
                blank(input$subsite)
              )
        removeModal()
      })
      
      # Taxon edit submit button ----
      
      observeEvent(input$final_edit_t,{
        req(input$tvk_match)
        
        rv$df_t[rv$dt_row,c("tvk_match","taxon_match")] <- c(input$tvk_match, names(choices_uksi)[which(choices_uksi == input$tvk_match)])
        
        removeModal()
      })
      # Submit validated records ----
      
      choices_surveys <- reactive({
        if(isTruthy(tables$surveys)){
          open <- tables$surveys[tables$surveys$status == "open",]
          c <- open$id
          names(c) <- open$survey
        }
        else{
          c <- c("")
        }
        return(c)
      })
      
      
      observe({
        updateSelectizeInput(session, "survey", choices = choices_surveys(), selected = "")
      })
      
      shinyjs::disable("upload")
      
      observe({
        req(rv$df)
        req(input$survey)
        if(sum(rv$df$valid) == nrow(rv$df)
           ){
          shinyjs::enable("upload")
        }else{
          shinyjs::disable("upload")
        }
      })
      
      # Upload button
      observeEvent(input$upload,{
        req(input$survey)
        req(sum(rv$df$valid) == nrow(rv$df))
        
        upload_modal(session)
      })
      
      # Cancel upload
      observeEvent(input$up_0,{
        removeModal()
      })
      
      # Submit upload
      observeEvent(input$up_1,{
        showModal(
          modalDialog(
            div(style="text-align:left;width:60%",
                tags$h4("Uploading",class="loading"),
            )
            ,footer=NULL,size="s",easyClose=FALSE,fade=TRUE
          )
        )
        
        cols_upload <- c("site_record","gridref",
                         "taxon_nbn",
                         "quantity","status","sex","stage","habitat","note",
                         "recorder","determiner","method","sample",
                         "record_date","record_year","record_date_start","record_date_end",
                         "start_year","end_year","start_month","end_month",
                         "origin_key")
        
        d <- rv$df[,c(cols_upload,"taxon_record")]
        
       
          # Format data for upload
          
          d$record_date <- as.Date(d$record_date, "%d/%m/%y")
          d$recordyear <- as.numeric(d$record_year)
          d$record_date_start <- as.Date(d$record_date_start, "%d/%m/%y")
          d$record_date_end <- as.Date(d$record_date_end, "%d/%m/%y")
          d$start_year <- as.numeric(d$start_year)
          d$end_year <- as.numeric(d$end_year)
          d$start_month <- as.numeric(d$start_month)
          d$end_month <- as.numeric(d$end_month)
          d[d == ""] <- NA
          
          # Set data source
          d$survey <- as.numeric(input$survey)
          
          # Lookup sites and taxa
          d <- d %>% left_join(rv$df_s, 
                               join_by(site_record, gridref), na_matches = "na")
          
          d$site <- as.numeric(d$id_match)
          
          d <- d %>% left_join(rv$df_t,
                               join_by(taxon_record,taxon_nbn), na_matches = "na")        
          d$taxon_nbn <- d$tvk_match
          
          d <- d[,c(cols_upload,"site","survey")]
          
          d <- d[!is.na(d$site) | !is.na(d$taxon_nbn),] # Drop unmatched sites and taxa
         
          future_promise({
          
          con <- fenDb0(user,password)

          # IMPORT RECORDS
          i <- import_table(con,
                       "records",
                       "records",
                       2,
                       postgresqlEscapeStrings(con,input$import), # file name imported from
                       postgresqlEscapeStrings(con,input$import_notes),
                       FALSE,
                       FALSE,
                       d,
                       NULL, # don't need geometry or attachment parameters
                       NULL,
                       NULL)
          dbDisconnect(con)
          return(i)
          })%...>%(function(i){
          
          if(isTruthy(i$error) || !isTruthy(i$output)){
            showModal(
              modalDialog(
                div(style="text-align:center",
                    tags$h4("Fail!"),
                    p("Record upload failed. No records uploaded.")
                )
                ,footer=NULL,size="s",easyClose=TRUE,fade=TRUE
              )
            )
          }else{
            showModal(
              modalDialog(
                div(style="text-align:center",
                    tags$h4("Success!"),
                    p("Records successfully uploaded to database")
                )
                ,footer=NULL,size="s",easyClose=TRUE,fade=TRUE
              )
            )
          }
          })
        
      })
      
      # Options ----
      
      outputOptions(output, "matchSitesTable", suspendWhenHidden = FALSE)
      outputOptions(output, "matchTaxaTable", suspendWhenHidden = FALSE)
    }
  ) 
}