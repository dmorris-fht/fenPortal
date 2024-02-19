queryRecordsUI <- function(id){
  ns <- NS(id)
  tagList(
    column(12,
           #Query form ----
           tabsetPanel(id = "tabset",  
             ##Form fields ----
          
             tabPanel("Search",id = ns("query"),
                      column(8,
                             h3("Search records")
                             ),
                      ## Action buttons ----
                      column(12,
                             div(style="width:100%; display:inline-block; padding-bottom: 10px; margin-bottom:5px", 
                                 div(
                                   style = "float:left",
                                   actionButton(ns("info"),label = "Instructions",icon = icon("info"))
                                 ),
                                 div(style="float:left",
                                     actionButton(ns("runQuery"),label = "Search",icon=icon("search"))
                                     ),
                                 div(style="float:left",id = ns("dlDiv"),class="buttonHidden",
                                     downloadButton(ns("dlQuery"),label = "Download results", icon = icon("download"))
                                     )
                                )   
                             ),
                      br(),br(),
                      ### Sites fields ----
                    box(title = "Sites", width = 6, solidHeader = T,collapsible = T, collapsed = F,
                      column(12,
                             div(style="width:100%; display:inline-block; padding-bottom: 0; margin-bottom:0", 
                                 column(6,
                                        div(style="float:left;width:100%",
                                            selectizeInput(
                                              inputId = ns("county"),
                                              label = "County(ies)",
                                              choices = c(""),
                                              multiple = TRUE,
                                              options = list(placeholder = 'Select one or more counties')
                                            ) 
                                        )
                                 ),
                                 
                                 column(6,
                                           div(style="float:left;width:100%",
                                               selectizeInput(
                                                 inputId = ns("site"),
                                                 label = "Site(s)",
                                                 choices = c(""),
                                                 multiple = TRUE,
                                                 options = list(placeholder = 'Select one or more sites')
                                               ) 
                                           )
                                           ),
                                    column(6,
                                           div(style="float:left;width:100%",
                                               selectizeInput(
                                                 inputId = ns("subsite"),
                                                 label = "Subsite(s)",
                                                 choices = c(""),
                                                 multiple = TRUE,
                                                 options = list(placeholder = 'No subsites')
                                               ) 
                                           )
                                    ),
                                    column(6,
                                           div(style="float:left;width:100%",
                                               textInput(
                                                 inputId = ns("gridref"),
                                                 label = "Grid reference(s)",
                                                 value = NULL,
                                                 placeholder = 'List of grid references separated by semi-colons'
                                               ) 
                                           )
                                    ),
                                    column(12,
                                           div(style="float:left;width:100%",
                                               textInput(
                                                 inputId = ns("site_record"),
                                                 label = "Site name(s) as given in original record",
                                                 value = NULL,
                                                 placeholder = 'Semicolon-separated list of names'
                                               ) 
                                           )
                                    )
                                    )
                      )
                      ),
                      ### Taxon fields ----
                      box(title = "Species", width = 6, solidHeader = T,collapsible = T, collapsed = F,
                           column(12,
                                    div(style="Width:100%; display:inline-block; padding-bottom: 0; margin-bottom:0", 
                                    column(6,
                                           div(style="float:left;width:100%",
                                               selectizeInput(
                                                 inputId = ns("taxon_group"),
                                                 label = "Taxon group(s)",
                                                 choices = c(""),
                                                 multiple = TRUE,
                                                 options = list(placeholder = 'Select one or more groups')
                                               ) 
                                           )
                                           ),
                                    column(6,
                                           div(style="float:left;width:100%",
                                              selectInput(
                                                inputId = ns("fenspp"),
                                                label = "Fen plant species",
                                                choices = choices_fenspp,
                                                selected = ""
                                              ) 
                                           )
                                         ),
                                    column(12,
                                          div(style="float:left;width:100%",
                                              selectizeInput(
                                                inputId = ns("taxon"),
                                                label = "Taxa",
                                                choices = c(""),
                                                multiple = TRUE,
                                                options = list(placeholder = 'Select one or more taxa')
                                              ) 
                                          )
                                    )
                             )
                             )
                      )
                        ,

                        
                      ### Date fields ----
                      box(title = "Dates", width = 6, solidHeader = T,collapsible = T, collapsed = T,
                      column(12,
                             column(6,
                                    div(style="float:left;width:100%",
                                        textInput(
                                          inputId = ns("date"),
                                          label = "Record date",
                                          value = NULL,
                                          placeholder = "Year or date in format dd/mm/yyyy"
                                        ) 
                                    )
                             ),
                             column(6,
                                    div(style="float:left;width:100%",
                                        textInput(
                                          inputId = ns("start_date"),
                                          label = "Start date",
                                          value = NULL,
                                          placeholder = "Start year or date in format dd/mm/yyyy"
                                        ) 
                                    )
                             ),
                             column(6,
                                    div(style="float:left;width:100%",
                                        textInput(
                                          inputId = ns("end_date"),
                                          label = "End date",
                                          value = NULL,
                                          placeholder = "End year or date in format dd/mm/yyyy"
                                        ) 
                                    )
                             )
                             )
                      ),
                      ## Data sources fields ----
                    box(title = "Data source", width = 6, solidHeader = T,collapsible = T, collapsed = T,
                      column(12,
                             column(6,
                                    div(style="float:left;width:100%",
                                        selectizeInput(
                                          inputId = ns("survey"),
                                          label = "Data source(s)",
                                          choices = c(""),
                                          multiple = TRUE,
                                          options = list(placeholder = 'Select a survey')
                                        ) 
                                    )
                                    ),
                             column(6,
                                    div(style="float:left;width:100%",
                                        selectizeInput(
                                          inputId = ns("survey_type"),
                                          label = "Data source type(s)",
                                          choices = c(""),
                                          options = list(placeholder = 'Select one or more data source types')
                                        ) 
                                    )
                             ),
                             column(6,
                                    div(style="float:left;width:100%",
                                        selectizeInput(
                                          inputId = ns("project"),
                                          label = "Project(s)",
                                          choices = c(""),
                                          options = list(placeholder = 'Select one or more projects')
                                        ) 
                                    )
                             ),
                             column(6,
                                    div(style="float:left;width:100%",
                                        selectizeInput(
                                          inputId = ns("sharing"),
                                          label = "Sharing type(s)",
                                          choices = c(""),
                                          options = list(placeholder = 'Select one or more sharing types')
                                        ) 
                                    )
                             ),
                             column(12,
                                    div(style="float:left;width:100%",
                                        textInput(
                                          inputId = ns("recorder"),
                                          label = "Recorder",
                                          value = NULL,
                                          placeholder = "List of recorder names separated by semicolons"
                                        ) 
                                    )
                             )
                             )
                      ),
                      
                      ## Admin fields ----
                    box(title = "Admin", width = 12, solidHeader = T,collapsible = T, collapsed = T,
                      column(12,
                             column(4,
                                    textInput(
                                      inputId = ns("created_date"),
                                      label = "Record created on",
                                      value = NULL,
                                      placeholder = "Year or date in format dd/mm/yyyy"
                                    ) 
                                    ),
                             column(4,
                                    textInput(
                                      inputId = ns("created_date_start"),
                                      label = "Record created from",
                                      value = NULL,
                                      placeholder = "Start year or date in format dd/mm/yyyy"
                                      ) 
                                    ),
                             column(4,
                                    textInput(
                                      inputId = ns("created_date_end"),
                                      label = "Record created before",
                                      value = NULL,
                                      placeholder = "End year or date in format dd/mm/yyyy"
                                      ) 
                                    ),
                             column(4,
                                    textInput(
                                      inputId = ns("edited_date"),
                                      label = "Record last edited on",
                                      value = NULL,
                                      placeholder = "Year or date in format dd/mm/yyyy"
                                    ) 
                             ),
                             column(4,
                                    textInput(
                                      inputId = ns("edited_date_start"),
                                      label = "Record last edited from",
                                      value = NULL,
                                      placeholder = "Start year or date in format dd/mm/yyyy"
                                    ) 
                             ),
                             column(4,
                                    textInput(
                                      inputId = ns("edited_date_end"),
                                      label = "Record last edited before",
                                      value = NULL,
                                      placeholder = "End year or date in format dd/mm/yyyy"
                                    ) 
                             ),
                             column(4,
                                    div(style="float:left;width:100%",
                                        textInput(
                                          inputId = ns("guid"),
                                          label = "GUID",
                                          value = NULL,
                                          placeholder = "Global unique identifier"
                                        ) 
                                    )
                                    )
                             )
             )
                  ),
           #Results ----

              tabPanel("Results",
                       column(12,
                              h3("Search results")
                              ),
                      column(12,
                        div(
                          style = "font-size:12px;padding: 0 ",
                          withSpinner(DT::DTOutput(outputId = ns("resultsTable")),type = 7)
                        )                             )
                    ),
              tabPanel("Map",
                       column(12,
                              h3("Map of search results"),
                              column(7,
                                     withSpinner(leafletOutput(ns("resultsMap")),type = 7)
                                     ),
                              column(5,
                                     div(
                                       style = "font-size:12px;padding: 0 ",
                                       withSpinner(DT::DTOutput(outputId = ns("mapTable")),type = 7)
                                     )
                                     )
                                  )    
                    )
           )
           )
    ,
    tags$script(src ="script.js")
  )
}

queryRecordsServer <- function(id, login, tables) {
  moduleServer(
    id,
    function(input, output, session) {
      
      role <- login$role
      user <- login$username
      password <- login$password
      
      # Site boundary reactive ----
      sites1 <- reactive({
        req(tables$sites)
        return(
          tables$sites0 %>% filter(!st_is_empty(.)) %>% st_transform(crs = 4326) # Filter out empty geoms and transform to 4326
        )
      })
      
      # Form controls ----
      ## Set up selectize inputs ----
      
      choices_site <- reactive({
        if(isTruthy(tables$sites)){
          if(isTruthy(input$county)){
            s <- tables$sites[tables$sites$county %in% input$county,]
            c <- s$id
            names(c) <- paste0(s$site, " [",s$county, "]")
          }
          else{
            c <- tables$sites$id
            names(c) <- paste0(tables$sites$site, " [",tables$sites$county, "]")
          }
          return(c)
        }
        else{
          return(c(""))
        }
      })
      #   
      # choices_site_1 <- reactive({
      #   if(isTruthy(tables$sites)){
      #     c <- tables$sites$id
      #     names(c) <- tables$sites$site
      #   }
      #   else{
      #     return(c(""))
      #   }
      # })
      
      choices_county <- reactive({
        if(isTruthy(tables$sites)){
          return(sort(unique(tables$sites$county)))
        }
        else{
          return(c(""))
        }
        })
      
      observe({
        updateSelectizeInput(session,
                             "site",
                             choices=choices_site(), 
                             selected = "",
                             server = FALSE,
                             options = list(
                               placeholder = 'Select one or more sites'
                             )
        )
        updateSelectizeInput(session,
                             "county",
                             choices=choices_county(), 
                             selected = "",
                             server = FALSE,
                             options = list(
                               placeholder = 'Select one or more counties'
                             )
        )
      })
      
      choices_subsite_0 <- reactive({
        if(isTruthy(tables$subsites)){
          c <- tables$subsites$id
          names(c) <- tables$subsites$subsite
          return(c)
          }
        else{
          return(c(""))
          }
        })

      choices_subsite <- reactive({
        if(isTruthy(input$site)){
          ss <- tables$subsites[tables$subsites$site %in% input$site,]
          if(nrow(ss) > 0){
            c <- ss[,c("id")]
            names(c) <- paste0(ss$subsite," [",ss$site_name,"]")
          }else{
            c <- c("")
          }
        }
        else{
          c <- c("")
        }
        return(c)
      })
      
      observe({
        if(isTruthy(choices_subsite()) && length(choices_subsite()) > 0){
          updateSelectizeInput(
            session,
            "subsite",
            choices=choices_subsite(),
            selected = "",
            server = FALSE,
            options = list(
              placeholder = 'Select one or more subsites',
              onInitialize = I('function() { this.setValue(""); }')
            )
          )
        }
        else{
          updateSelectizeInput(
            session,
            "subsite",
            choices=choices_subsite(),
            selected = "",
            server = FALSE,
            options = list(
              placeholder = 'No subsites',
              onInitialize = I('function() { this.setValue(""); }')
            )
          )        }
        
      })
      
      updateSelectizeInput(session,
                           "taxon_group",
                           choices = choices_tgps, 
                           selected = "",
                           server = TRUE,
                           options = list(
                             onInitialize = I('function() { this.setValue(""); }')
                           )
      )
      
      choices_uksi_rv <- reactive({
        if(isTruthy(input$taxon_group)){
          t <- uksi_full[uksi_full$informal_group %in% input$taxon_group,]
          c <- t[,c("nbn_taxon_version_key")]
          names(c) <- t$full_name
        }
        else{
          c <- choices_uksi
        }
      })

      observe({
        if(length(input$taxon_group) > 0){
          updateSelectizeInput(session,
                               "taxon",
                               choices = choices_uksi_rv(), 
                               selected = "",
                               server = TRUE,
                               options = list(
                                 onInitialize = I('function() { this.setValue(""); }')
                               )
          )
        }
        else{
          updateSelectizeInput(session,
                               "taxon",
                               choices = choices_uksi, 
                               selected = "",
                               server = TRUE,
                               options = list(
                                 onInitialize = I('function() { this.setValue(""); }')
                               )
          )
        }
      })
      
      choices_p <- reactive({
        if(isTruthy(tables$projects)){
          c <- tables$projects$id
          names(c) <- tables$projects$project
          return(c)
        }
        else{
          return(c(""))
        }
        })
      
      observe({
        updateSelectizeInput(session,
                             "project",
                             choices=choices_p(),
                             selected = "",
                             server = FALSE,
                             options = list(
                               placeholder = 'Select one or more projects',
                               onInitialize = I('function() { this.setValue(""); }')
                             )
        )
      })

      updateSelectizeInput(session,
                           "survey_type",
                           choices=choices_st,
                           selected = "",
                           server = FALSE,
                           options = list(
                             placeholder = 'Select one or more data source types',
                             onInitialize = I('function() { this.setValue(""); }')
                            )
                           )
      
      updateSelectizeInput(session,
                           "sharing",
                           choices=choices_st,
                           selected = "",
                           server = FALSE,
                           options = list(
                             placeholder = 'Select one or more sharing types',
                             onInitialize = I('function() { this.setValue(""); }')
                            )
                           )
      
      survey <- tables$surveys
      choices_survey_0 <- reactive({
        if(isTruthy(tables$surveys)){
          c <- tables$surveys$id
          names(c) <- tables$surveys$survey
          return(c)
          }
        else{
          return(c(""))
          }
        })

      choices_survey_rv <- reactive({
        if(isTruthy(tables$surveys) && isTruthy(tables$projects)){
          if(isTruthy(input$project)){
            p_test <- as.numeric(input$project)
          }
          else{
            p_test <- tables$surveys$project
          }
          if(isTruthy(input$survey_type)){
            st_test <- as.numeric(input$survey_type)
          }
          else{
            st_test <- tables$surveys$survey_type
          }
          if(isTruthy(input$sharing)){
            sh_test <- as.numeric(input$sharing)
          }
          else{
            sh_test <- tables$surveys$sharing
          }
          return(
            choices_survey_0()[which(
              tables$surveys$project %in% p_test &
                tables$surveys$survey_type %in% st_test &
                tables$surveys$sharing %in% sh_test
              )]          
            )
        }
        else{
          return(c(""))
        }
      })
      
      observe({
        updateSelectizeInput(session,
                             "survey",
                             choices=choices_survey_rv(),
                             selected = "",
                             server = FALSE,
                             options = list(
                               placeholder = "Select one or more data sources",
                               onInitialize = I('function() { this.setValue(""); }')
                             )
        )
      })

      ## Validation ----
      
      ### Validation reactives
      gf_check <- reactive({
        v <- lapply(strsplit(input$gridref,","),isGridref)
        return(prod(unlist(v)) == TRUE || !isTruthy(input$gridref))
      })
      date_check_r <- reactive({
        date_check(input$date,0)$error
      })
      date_check_0 <- reactive({
        date_check(input$start_date,0)$error
      })
      date_check_1 <- reactive({
        date_check(input$end_date,1)$error
      })
      date_check_c <- reactiveValues(d=NA,s=NA,e=NA)
      observe({
        date_check_c$d <- date_check(input$created_date,0)$error
        date_check_c$s <- date_check(input$created_date_start,0)$error
        date_check_c$e <- date_check(input$created_date_end,1)$error
      })
      date_check_e <- reactiveValues(d=NA,s=NA,e=NA)
      observe({
        date_check_e$d <- date_check(input$edited_date,0)$error
        date_check_e$s <- date_check(input$edited_date_start,0)$error
        date_check_e$e <- date_check(input$edited_date_end,1)$error
      })
      
      ### Input validator
      iv <- InputValidator$new()
      iv$add_rule("gridref",function(value){
        v <- lapply(unlist(strsplit(input$gridref,",")),function(x){
          validate_gf(x, s = NULL, ss = NULL, sites0 = NA, subsites0 = NA)})
        if(grepl("error = 1",paste(v,collapse="")) && isTruthy(input$gridref)){
          return("Invalid grid reference(s)")
        }
      })
      # Check dates
      iv$add_rule("date",function(value){
        if(isTruthy(input$date) && date_check_r() == FALSE){
          return("Invalid date format")
        }
      })
      iv$add_rule("start_date",function(value){
        if(isTruthy(input$start_date) && date_check_0() == FALSE){
          return("Invalid date format")
        }
      })
      iv$add_rule("end_date",function(value){
        if(isTruthy(input$end_date) && date_check_1() == FALSE){
          return("Invalid date format")
        }
      })
      # Check admin date fields
      
      iv$add_rule("created_date",function(value){
        if(isTruthy(input$created_date) && date_check_c$d == FALSE){
          return("Invalid date format")
        }
      })
      iv$add_rule("created_date_start",function(value){
        if(isTruthy(input$created_date_start) && date_check_c$s == FALSE){
          return("Invalid date format")
        }
      })
      iv$add_rule("created_date_end",function(value){
        if(isTruthy(input$created_date_end) && date_check_c$e == FALSE){
          return("Invalid date format")
        }
      })
      
      iv$add_rule("edited_date",function(value){
        if(isTruthy(input$edited_date) && date_check_e$d == FALSE){
          return("Invalid date format")
        }
      })
      iv$add_rule("edited_date_start",function(value){
        if(isTruthy(input$edited_date_start) && date_check_e$s == FALSE){
          return("Invalid date format")
        }
      })
      iv$add_rule("edited_date_end",function(value){
        if(isTruthy(input$edited_date_end) && date_check_e$e == FALSE){
          return("Invalid date format")
        }
      })
      iv$add_rule("guid",function(value){
        if(isTruthy(input$guid) && is.na(as.UUID(input$guid))){
          return("Invalid guid")
        }
      })
      
      iv$enable()
      
      observe({
        if(
          gf_check() == TRUE &&
          !isTruthy(input$date) || date_check_r() == TRUE &&
          !isTruthy(input$start_date) || date_check_0() == TRUE &&
          !isTruthy(input$end_date) || date_check_1() == TRUE &&
          !isTruthy(input$created_date) || date_check_c$d == TRUE &&
          !isTruthy(input$created_date_start) || date_check_c$s == TRUE &&
          !isTruthy(input$created_date_end) || date_check_c$e == TRUE &&
          !isTruthy(input$edited_date) || date_check_e$d == TRUE &&
          !isTruthy(input$edited_date_start) || date_check_e$s == TRUE &&
          !isTruthy(input$edited_date_end) || date_check_e$e == TRUE &&
          !is.na(as.UUID(input$guid))
        ){
          shinyjs::enable("runQuery")
        }
        else{
          shinyjs::disable("runQuery")
        }
      })
      
      # Reactive to hold records ----
      # Reactive for query results
      rv <- reactiveValues(
        sf = NA,
        df = data.frame(
          id = numeric(),
          site_record = character(),
          site = numeric(),
          subsite = numeric(),
          gridref = character(),
          taxon_nbn = character(),
          quantity = character(),
          status = character(),
          sex = character(),
          stage = character(),
          note = character(),
          record_date = Date(),
          record_date_end = Date(),
          start_year = numeric(),
          end_year = numeric(),
          start_month = numeric(),
          end_month = numeric(),
          recorder = character(),
          determiner = character(),
          method = character(),
          survey = numeric(),
          created_user = character(),
          created_date = Date(),
          last_edited_user = character(),
          last_edited_date = Date(),
          guid = character(),
          verification = numeric(),
          verification_user = character(),
          verification_date = Date(),
          verification_note = character(),
          site_name = character(),
          subsite_name = character(),
          taxon_name = character(),
          taxon_authority = character(),
          taxon_qualifier = character(),
          survey_name = character(),
          survey_start_date = Date(),
          survey_end_date = Date(),
          survey_start_year = numeric(),
          survey_end_year = numeric(),
          verification_description = character(),
          record_date_start = Date(),
          Buttons = character()
        ),
        dt_row = NULL, add_or_edit = 0,
        edit_button = NULL)
      
      # Reactive for record from map
      rv2 <- reactiveValues(
        df = data.frame(
          id = numeric(),
          site_record = character(),
          site = numeric(),
          subsite = numeric(),
          gridref = character(),
          taxon_nbn = character(),
          quantity = character(),
          status = character(),
          sex = character(),
          stage = character(),
          note = character(),
          record_date = Date(),
          record_date_end = Date(),
          start_year = numeric(),
          end_year = numeric(),
          start_month = numeric(),
          end_month = numeric(),
          recorder = character(),
          determiner = character(),
          method = character(),
          survey = numeric(),
          created_user = character(),
          created_date = Date(),
          last_edited_user = character(),
          last_edited_date = Date(),
          guid = character(),
          verification = numeric(),
          verification_user = character(),
          verification_date = Date(),
          verification_note = character(),
          site_name = character(),
          subsite_name = character(),
          taxon_name = character(),
          taxon_authority = character(),
          taxon_qualifier = character(),
          survey_name = character(),
          survey_start_date = Date(),
          survey_end_date = Date(),
          survey_start_year = numeric(),
          survey_end_year = numeric(),
          verification_description = character(),
          record_date_start = Date(),
          Buttons = character()
        ),
        dt_row = NULL, add_or_edit = 0,
        edit_button = NULL)
      
      # Data table definitions ----
      output$resultsTable <- DT::renderDT(
        {
          shiny::isolate(rv$df)
          
          x <- rv$df[,c("taxon_name","site_name","subsite_name","gridref","record_date","survey_name","Buttons")]

          return(x)
        }
        ,
        server = TRUE,
        escape = F,
        rownames = FALSE,
        selection = 'single',
        colnames =  c("Taxon","Site","Subsite","Gridref","Date","Date source",""),
        options = list(processing = TRUE,
                       columnDefs = list(
                         list(orderable = FALSE, targets = c(6)),
                         list(width = '60px',targets=c(6))
                       ),
                       extensions = c("FixedHeader"),#, "Scroller")
                       fixedHeader = TRUE,
                       scrollY = "50vh"
        )
      ) 
      
      proxy_DT <- DT::dataTableProxy("resultsTable")
      
      output$mapTable <- DT::renderDT(
        {
          shiny::isolate(rv2$df)
          
          x <- rv2$df[,c("taxon_name","site_name","subsite_name","record_date","Buttons")]

          return(x)
        }
        ,
        server = TRUE,
        escape = F,
        rownames = FALSE,
        selection = 'single',
        colnames = c("Taxon","Site","Subsite","Date",""),
        options = list(processing = TRUE,
                       searching=FALSE,
                       lengthChange = FALSE,
                       language = list(
                         infoEmpty = "No records",
                         emptyTable = "No records selected in map"
                         ),
                       columnDefs = list(
                         list(orderable = FALSE, targets = c(4)),
                         list(width = '60px',targets=c(4))
                       ),
                       extensions = c("FixedHeader"),#, "Scroller")
                       fixedHeader = TRUE,
                       scrollY = "55vh"
        )
      ) 
      
      proxy_DT2 <- DT::dataTableProxy("mapTable")
      
      # DT row buttons ----
      #Load modals
      source("./R/modals/record_modal.R")
      #Row button click event 

      gfv <- reactive({
        req(input$modal_site)
        return(validate_gf(input$modal_gridref, 
                           s = as.numeric(input$modal_site), 
                           ss = as.numeric(input$modal_subsite),
                           sites0 = tables$sites0,
                           subsites0 = tables$subsites0))
      })
      
      observeEvent(input$current_id, {
        
        #Edit button
        if(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "query_edit")){      
          rv$dt_row <- which(stringr::str_detect(rv$df$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
          
          d <- rv$df[rv$dt_row,]
          
          record_modal_dialog(session = session, d, edit = TRUE,
                              c1=choices_site(),
                              c2=choices_subsite_0(),
                              c3=choices_uksi,
                              c4=choices_survey_0()
                              )

          shinyjs::disable("modal_created_user")
          shinyjs::disable("modal_created_date")
          shinyjs::disable("modal_last_edited_user")
          shinyjs::disable("modal_last_edited_date")
          shinyjs::disable("modal_guid")
          shinyjs::enable("modal_site")
          shinyjs::enable("modal_subsite")
          shinyjs::enable("modal_gridref")
          shinyjs::enable("modal_site_record")
          shinyjs::enable("modal_taxon_nbn")
          shinyjs::enable("modal_quantity")
          shinyjs::enable("modal_status")
          shinyjs::enable("modal_sex")
          shinyjs::enable("modal_stage")
          shinyjs::enable("modal_recorder")
          shinyjs::enable("modal_determiner")
          shinyjs::enable("modal_method")
          shinyjs::enable("modal_survey")
          shinyjs::enable("modal_record_date")
          shinyjs::enable("modal_start_year")
          shinyjs::enable("modal_start_month")
          shinyjs::enable("modal_record_date_end")
          shinyjs::enable("modal_end_year")
          shinyjs::enable("modal_end_month")
          shinyjs::enable("modal_note")
          shinyjs::enable("modal_verification")
          shinyjs::disable("modal_verification_user")
          shinyjs::disable("modal_verification_date")
          shinyjs::enable("modal_verification_note")

          shinyjs::disable("final_edit")
          
          # Reactive to check for changes in form
          changes <- reactive({
            v1 <- replace_na(as.vector(unlist(d[1,c(2:10,18:21,12,14,16,13,15,17,11,27,30)])),"")
            v2 <- replace_na(c(
              blank(input$modal_site_record),
              input$modal_site,
              blank(input$modal_subsite),
              blank(input$modal_gridref),
              input$modal_taxon_nbn,
              blank(input$modal_quantity),
              blank(input$modal_status),
              blank(input$modal_sex),
              blank(input$modal_stage),
              blank(input$modal_recorder),
              blank(input$modal_determiner),
              blank(input$modal_method),
              blank(input$modal_survey),
              blank(input$modal_record_date),
              blank(input$modal_start_year),
              blank(input$modal_start_month),
              blank(input$modal_record_date_end),
              blank(input$modal_end_year),
              blank(input$modal_end_month),
              blank(input$modal_note),
              blank(input$modal_verification),
              blank(input$modal_verification_note)
            ),"")
            return(sum(compareNA(v1,v2)) < length(v2))
          })
          
          # Reactive to force validation note
          validation_check <- reactive({
            v3 <- unlist(d[1,c(27,30)])
            v4 <- c(blank(input$modal_verification),
                    blank(input$modal_verification_note))
            return(sum(compareNA(v3,v4)) == 2 || sum(compareNA(v3,v4)) == 0)
          })
          
          observe({
            c <- subsite[subsite$site == input$modal_site,c("id")]
            names(c) <- subsite[subsite$site == input$modal_site,c("subsite")]

            updateSelectizeInput(
                session,
                "modal_subsite",
                choices = c,
                selected = d[4]
              )
          })

          ## Modal validation
          iv_modal <- InputValidator$new()
          iv_modal$add_rule("modal_site",sv_required())
          iv_modal$add_rule("modal_taxon_nbn",sv_required())
          iv_modal$add_rule("modal_survey",sv_required())
          iv_modal$add_rule("modal_gridref",function(value){
            v <- validate_gf(input$modal_gridref, 
                             s = as.numeric(input$modal_site), 
                             ss = as.numeric(input$modal_subsite),
                             sites0 = tables$sites0,
                             subsites0 = tables$subsites0)
            if(v$error == 1 && isTruthy(input$modal_gridref)){
              v$message
            }
          })
          iv_modal$add_rule("modal_verification_note",function(value){
            if(!validation_check()){
              return("To change validation state, please add note")
            }
          })
          iv_modal$enable()
          
          ## Modal edit button activation

          observe({
            req(input$modal_site)
            req(input$modal_taxon_nbn)
            req(input$modal_survey)
            req(gfv()$error == 0)
            
            if(changes() && validation_check()){
              shinyjs::enable("final_edit")
            }
            else{
              shinyjs::disable("final_edit")
            }
          })
        }
        
        #Info button  
        if(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "query_info")){
          rv$dt_row <- which(stringr::str_detect(rv$df$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
          
          d <- rv$df[rv$dt_row,] 
          record_modal_dialog(session = session, d, edit = FALSE,
                              c1=choices_site(),
                              c2=choices_subsite_0(),
                              c3=choices_uksi,
                              c4=choices_survey_0()
                              )
          
          shinyjs::disable("modal_created_user")
          shinyjs::disable("modal_created_date")
          shinyjs::disable("modal_last_edited_user")
          shinyjs::disable("modal_last_edited_date")
          shinyjs::disable("modal_guid")
          shinyjs::disable("modal_site")
          shinyjs::disable("modal_subsite")
          shinyjs::disable("modal_gridref")
          shinyjs::disable("modal_site_record")
          shinyjs::disable("modal_taxon_nbn")
          shinyjs::disable("modal_quantity")
          shinyjs::disable("modal_status")
          shinyjs::disable("modal_sex")
          shinyjs::disable("modal_stage")
          shinyjs::disable("modal_recorder")
          shinyjs::disable("modal_determiner")
          shinyjs::disable("modal_method")
          shinyjs::disable("modal_survey")
          shinyjs::disable("modal_record_date")
          shinyjs::disable("modal_start_year")
          shinyjs::disable("modal_start_month")
          shinyjs::disable("modal_record_date_end")
          shinyjs::disable("modal_end_year")
          shinyjs::disable("modal_end_month")
          shinyjs::disable("modal_note")
          shinyjs::disable("modal_verification")
          shinyjs::disable("modal_verification_user")
          shinyjs::disable("modal_verification_date")
          shinyjs::disable("modal_verification_note")
          
          shinyjs::hide("final_edit")
        }
        
        rv$add_or_edit <- 0
      })
      
      observeEvent(input$close_modal,{
        removeModal()
      })
      # Submit edits button ----

      observeEvent(input$final_edit, {
        id <- rv$df[rv$dt_row, c("id")]
        #validate input
        req(input$modal_site)
        req(input$modal_taxon_nbn)
        req(input$modal_survey)
        req(gfv()$error == 0)
        
       future_promise({
          con0 <- fenDb0(user,password)
          sql <- sqlInterpolate(con0, "UPDATE records.records SET
                                site_record = ?site_record,
                                site = ?site,
                                subsite = ?subsite,
                                gridref = ?gridref,
                                taxon_nbn = ?taxon_nbn,
                                quantity = ?quantity,
                                status = ?status,
                                sex = ?sex,
                                stage = ?stage,
                                note = ?note,
                                record_date = ?record_date,
                                record_date_end = ?record_date_end,
                                start_year = ?start_year,
                                end_year = ?end_year,
                                start_month = ?start_month,
                                end_month = ?end_month,
                                recorder = ?recorder,
                                determiner = ?determiner,
                                method = ?method,
                                survey = ?survey,
                                verification = ?verification,
                                verification_note = ?verification_note
                                WHERE id = ?id
                                RETURNING 
                                site_record,
                                site,
                                subsite,
                                gridref,
                                taxon_nbn,
                                quantity,
                                status,
                                sex,
                                stage,
                                note,
                                record_date,
                                record_date_end,
                                start_year,
                                end_year,
                                start_month,
                                end_month,
                                recorder,
                                determiner,
                                method,
                                survey,
                                created_user,
                                created_date,
                                last_edited_user,
                                last_edited_date,
                                guid,
                                verification,
                                verification_user,
                                verification_date,
                                verification_note
                                "
                                ,
                                id = id,
                                site_record = blank(input$modal_site_record),
                                site = as.numeric(blank(input$modal_site)),
                                subsite = as.numeric(blank(input$modal_subsite)),
                                gridref = blank(input$modal_gridref),
                                taxon_nbn = blank(input$modal_taxon_nbn),
                                quantity = blank(input$modal_quantity),
                                status = blank(input$modal_status),
                                sex = blank(input$modal_sex),
                                stage = blank(input$modal_stage),
                                note = blank(input$modal_note),
                                record_date = as.Date(blank(input$modal_record_date)),
                                record_date_end = as.Date(blank(input$modal_record_date_end)),
                                start_year = as.numeric(blank(input$modal_start_year)),
                                end_year = as.numeric(blank(input$modal_end_year)),
                                start_month = as.numeric(blank(input$modal_start_month)),
                                end_month = as.numeric(blank(input$modal_end_month)),
                                recorder = blank(input$modal_recorder),
                                determiner = blank(input$modal_determiner),
                                method = blank(input$modal_method),
                                survey = as.numeric(blank(input$modal_survey)),
                                verification = as.numeric(blank(input$modal_verification)),
                                verification_note = blank(input$modal_verification_note)
                                )

          tryCatch({
            r0 <- postgresqlExecStatement(con0, sql)
            r1 <- postgresqlFetch(r0)
            dbDisconnect(con0)
            return(r1)
          },
          error=function(err){
            dbDisconnect(con0)
            return(err)
            }
          )
          
          })%...>% (function(result) {
            showModal(submitModal())

          if(length(result) == 1){
            output$submitMessage <- renderUI({
              tagList(
                h4("An error occured"),
                p(result)
              )
            })
          }else{
            r <- c(result[1,],
                   site[site$id == result$site,c("site")],
                   ifelse(is.na(result$subsite),NA,subsite[subsite$id == result$subsite,c("subsite")]),
                   uksi_full[uksi_full$nbn_taxon_version_key == result$taxon_nbn,c("taxon_name")],
                   uksi_full[uksi_full$nbn_taxon_version_key == result$taxon_nbn,c("taxon_authority")],
                   uksi_full[uksi_full$nbn_taxon_version_key == result$taxon_nbn,c("taxon_qualifier")],
                   survey[survey$id == result$survey,c("survey")],
                   ifelse(is.na(result$verification),NA,verification[verification$code == result$verification,c("description")])
            )
            rv$df[rv$dt_row,c(2:37)] <- r
            
            output$submitMessage <- renderUI({
              tagList(
                h4("Record updated successfully")
              )
            })
          }
        })
      })
      
      # Modal for submit message
      submitModal <- function(){
        ns <- session$ns
        modalDialog(
          div(style="text-align:left",
              uiOutput(ns("submitMessage")),
          )
          ,footer=NULL,size="s",easyClose=TRUE,fade=TRUE
        )
      }
      
      # Map definition ----
      output$resultsMap <- renderLeaflet({
        map <- leaflet() %>%
          addTiles(group="OpenStreetMap.Mapnik") %>% 
          addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
          addProviderTiles("OpenStreetMap.HOT", group = "OpenStreetMap.HOT") %>%
          # addProviderTiles("Stamen.TerrainBackground", group = "Stamen.TerrainBackground", options = providerTileOptions(minzoom = 1, maxzoom = 13)) %>%
          addMiniMap(tiles = "OpenStreetMap.HOT", toggleDisplay = TRUE) %>%
          addSearchOSM() %>%
          addEasyButton(easyButton(icon="fa-home", title="Home view", onClick=JS("function(btn, map){ map.fitBounds([[-5.515982,50.024189],[1.35876,55.948577]]); }"))) %>%
          addEasyButton(easyButton(icon="fa-crosshairs", title="Locate Me", onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
          addMeasure() %>%
          addFullscreenControl() %>%
          addScaleBar(position = c("bottomleft")) %>%
          addLayersControl(baseGroups = c("OpenStreetMap.Mapnik","Satellite","OpenStreetMap.HOT","Stamen.TerrainBackground"),
                           options = layersControlOptions(collapsed = TRUE)) %>%
          fitBounds(-5.515982,50.024189,1.35876,55.948577) %>%
          addMapPane("10km", zIndex = 410) %>%
          addMapPane("2km", zIndex = 411) %>%
          addMapPane("1km", zIndex = 412) %>%
          addMapPane("100m", zIndex = 413) %>%
          addMapPane("10m", zIndex = 414) %>%
          addMapPane("1m", zIndex = 415) %>%
          addDrawToolbar(
            position = "bottomleft",
            polylineOptions = FALSE,
            polygonOptions = drawPolygonOptions(),
            circleOptions = FALSE,
            rectangleOptions = drawRectangleOptions(),
            markerOptions = FALSE,
            circleMarkerOptions = FALSE, editOptions = FALSE,
            singleFeature = TRUE
          )
        
        map
      })
      
      proxy_map <- leafletProxy("resultsMap")
      
      # Update results DT and map with results ----
      
      observe({
        x <- rv$df[,c("taxon_name","site_name","subsite_name","gridref","record_date","survey_name","Buttons")]
        
        DT::replaceData(proxy_DT, x, resetPaging = FALSE, rownames = FALSE)
        
        req(isTruthy(rv$sf))
        site_q <- sites1()[sites1()$id %in% rv$df[!is.na(rv$df$gridref),c("site")],]
        r <- rv$sf
        bbox <- st_bbox(r)
        r$res <- nchar(r$gridref)
        r1 <- r[r$res == 4,]
        rt <- r[r$res == 5,]
        r2 <- r[r$res == 6,]
        r3 <- r[r$res == 8,]
        r4 <- r[r$res == 10,]
        r5 <- r[r$res == 12,]
        
        proxy_map %>% 
          fitBounds(bbox[[1]],bbox[[2]],bbox[[3]],bbox[[4]])
        
        if(isTruthy(site_q) && nrow(site_q)>0){
          proxy_map %>% 
            addPolygons(data = site_q, 
                        group = "sites", 
                        color = "red", weight = 1, opacity = 1, fillOpacity = 0, label = ~site)
        }
        
          proxy_map %>% 
            addPolygons(data = r1, 
                        group = "records",
                        layerId = ~gridref, label = ~paste0(gridref,''),
                        opacity = 1, fillOpacity = 0,
                        options = pathOptions(pane = "10km"),
                        highlightOptions = highlightOptions(color = "white", weight = 2,
                                                            bringToFront = TRUE)
                        ) %>%
            addPolygons(data = rt, 
                        group = "records",
                        layerId = ~gridref,label = ~paste0(gridref,''),
                        opacity = 1, fillOpacity = 0,
                        options = pathOptions(pane = "2km"),
                        highlightOptions = highlightOptions(color = "white", weight = 2,
                                                            bringToFront = TRUE)
            ) %>%
            addPolygons(data = r2, 
                        group = "records",
                        layerId = ~gridref, label = ~paste0(gridref,''),
                        opacity = 1, fillOpacity = 0,
                        options = pathOptions(pane = "1km"),
                        highlightOptions = highlightOptions(color = "white", weight = 2,
                                                            bringToFront = TRUE)
            ) %>%
            addPolygons(data = r3, 
                        group = "records",
                        layerId = ~gridref, label = ~paste0(gridref,''),
                        opacity = 1, fillOpacity = 0,
                        options = pathOptions(pane = "100m"),
                        highlightOptions = highlightOptions(color = "white", weight = 2,
                                                            bringToFront = TRUE)
            ) %>%
            addPolygons(data = r4, 
                        group = "records",
                        layerId = ~gridref,label = ~paste0(gridref,''),
                        color = "purple", weight = 0.5, 
                        opacity = 1, fillOpacity = 0.3,
                        options = pathOptions(pane = "10m"),
                        highlightOptions = highlightOptions(color = "white", weight = 2,
                                                            bringToFront = TRUE)
            ) %>%
            addPolygons(data = r5, 
                        group = "records",
                        layerId = ~gridref, label = ~paste0(gridref,''),
                        color = "black", weight = 0.8, fillColor = "white",
                        opacity = 1, fillOpacity = 1,
                        options = pathOptions(pane = "1m"),
                        highlightOptions = highlightOptions(color = "white", weight = 2,
                                                            bringToFront = TRUE)
            ) 

      })
      
      # Update map results DT ----
      
      observeEvent(input$resultsMap_shape_click,{
        g <- input$resultsMap_shape_click$id
        rv2$df <- rv$df[rv$df$gridref == g & !is.na(rv$df$gridref),]
        x <- rv2$df[,c("taxon_name","site_name","subsite_name","record_date","Buttons")]
        DT::replaceData(proxy_DT2, x, resetPaging = FALSE, rownames = FALSE)
      })
      
      data <- reactiveValues(record = list())
      
      observeEvent(input$resultsMap_draw_new_feature, {
        
        polygon_coordinates <- input$resultsMap_draw_new_feature$geometry$coordinates[[1]]
        l <- do.call(rbind,lapply(polygon_coordinates,function(x){c(x[[1]][1],x[[2]][1])}))
        drawn_polygon <- st_sfc(st_polygon(list(l)))
        st_crs(drawn_polygon) <- st_crs(4326) 

        if(isTruthy(rv$sf)){
          selected <- rv$sf[unlist(st_intersects(drawn_polygon,rv$sf)),]
          rv2$df <- rv$df[rv$df$gridref %in% selected$gridref & !is.na(rv$df$gridref),]
          x <- rv2$df[,c("taxon_name","site_name","subsite_name","record_date","Buttons")]
          DT::replaceData(proxy_DT2, x, resetPaging = FALSE, rownames = FALSE)
        }
        proxy_map %>% removeDrawToolbar(clearFeatures=TRUE) %>%
          addDrawToolbar(
            position = "bottomleft",
            polylineOptions = FALSE,
            polygonOptions = drawPolygonOptions(),
            circleOptions = FALSE,
            rectangleOptions = drawRectangleOptions(),
            markerOptions = FALSE,
            circleMarkerOptions = FALSE, editOptions = FALSE,
            singleFeature = TRUE
          )
        
        })
      
      
      
      # Run query ----
      observeEvent(input$runQuery,{
        req(gf_check() == TRUE)
        req(!isTruthy(input$date) || date_check_r() == TRUE)
        req(!isTruthy(input$start_date) || date_check_0() == TRUE)
        req(!isTruthy(input$end_date) || date_check_1() == TRUE)
        req(!isTruthy(input$created_date) || date_check_c$d == TRUE)
        req(!isTruthy(input$created_date_start) || date_check_c$s == TRUE)
        req(!isTruthy(input$created_date_end) || date_check_c$e == TRUE)
        req(!isTruthy(input$edited_date) || date_check_e$d == TRUE)
        req(!isTruthy(input$edited_date_start) || date_check_e$s == TRUE)
        req(!isTruthy(input$edited_date_end) || date_check_e$e == TRUE)
        req(!isTruthy(input$guid) || (!is.na(as.UUID(input$guid)) && isTruthy(input$guid)))
        
        if(input$fenspp > 0){
          if(input$fenspp == 1){
            w_fenspp <- paste0("u.nbn_taxon_version_key_for_recommended_name IN ", string_fspp)
          }
          if(input$fenspp == 2){
            w_fenspp <- paste0("u.nbn_taxon_version_key_for_recommended_name IN ", string_afspp)
          }
        }else{
          w_fenspp <- "(1=1)"
        }
        
        # define sql where string
        w <- paste(
          # Site fields
          sql_in("f.county",input$county),
          sql_in("r.site",as.numeric(input$site)),
          sql_in("r.subsite",as.numeric(input$subsite)),
          gf_vec("r.gridref",unlist(strsplit(input$gridref,";"))),
          like_vec("r.site_record",unlist(strsplit(input$site_record,";"))),
          
          # taxon fields
          sql_in("u.informal_group",input$taxon_group),
          w_fenspp,
          sql_in("u.nbn_taxon_version_key_for_recommended_name",uksi_full[unique(which(uksi_full$nbn_taxon_version_key %in% input$taxon)),c("nbn_taxon_version_key_for_recommended_name")]),
          
          # data source fields
          sql_in("r.survey",as.numeric(input$survey)),
          sql_in("s.project",as.numeric(input$project)),
          sql_in("s.survey_type",as.numeric(input$survey_type)),
          sql_in("s.sharing",as.numeric(input$sharing)),
          like_vec("r.recorder",unlist(strsplit(input$recorder,";"))),
          
          # date fields
          sql_date("r.record_date",input$date,input$date), #WHAT ABOUT RECORDS WHERE DATE IS IN END_DATE OR SURVEYS?
          sql_date("r.record_date",input$start_date,input$end_date), #WHAT ABOUT DATE RANGES AND RECORDS WHERE DATE COMES FROM SURVEY?
          
          # admin fields
          sql_in("r.guid",input$guid),
          sql_date("r.created_date",input$created_date,input$created_date),
          sql_date("r.created_date",input$created_date_start,input$created_date_end),
          sql_date("r.last_edited_date",input$edited_date,input$edited_date),
          sql_date("r.last_edited_date",input$edited_date_start,input$edited_date_end)
          , sep = " AND "
        )
        #define sql string
        sql <- paste0(
          "SELECT 
          r.id,
          r.site_record,
          r.site,
          r.subsite,
          r.gridref,
          r.taxon_nbn,
          r.quantity,
          r.status,
          r.sex,
          r.stage,
          r.note,
          r.record_date,
          r.record_date_end,
          r.start_year,
          r.end_year,
          r.start_month,
          r.end_month,
          r.recorder,
          r.determiner,
          r.method,
          r.survey,
          r.created_user,
          r.created_date,
          r.last_edited_user,
          r.last_edited_date,
          r.guid,
          r.verification,
          r.verification_user,
          r.verification_date,
          r.verification_note,
          f.site AS site_name,
          fs.subsite AS subsite_name,
          u.taxon_name,
          u.taxon_authority,
          u.taxon_qualifier,
          s.survey AS survey_name,
          s.start_date AS survey_start_date,
          s.end_date AS survey_end_date,
          s.start_year AS survey_start_year,
          s.end_year AS survey_end_year,
          v.description AS verification_description,
          r.record_date_start,
          ST_TRANSFORM(gridref_square(r.gridref),4326) AS geom
          FROM 
          records.records r LEFT OUTER JOIN records.surveys s ON r.survey = s.id
		      LEFT OUTER JOIN spatial.fen_sites f ON r.site = f.id
		      LEFT OUTER JOIN spatial.fen_subsites fs ON r.subsite = fs.id 
		      LEFT OUTER JOIN lookups.uksi u ON r.taxon_nbn = u.nbn_taxon_version_key
          LEFT OUTER JOIN lookups.lookup_verification v ON r.verification = v.code 
          WHERE 
          "
          ,w
        )
        
        showModal(
          modalDialog(
            div(style="text-align:left",
                tags$h4("Search in progress",class="loading"),
            )
            ,footer=NULL,size="s",easyClose=TRUE,fade=TRUE
          )
        )
        
        future_promise({
          con0 <- fenDb0(user,password)
          result <- st_read(dsn = con0, 
                            query = sql, 
                            geometry_column = "geom")
          dbDisconnect(con0)
          return(result)
        })%...>% (function(result) {
          proxy_map %>% clearGroup("sites")
          proxy_map %>% clearGroup("records")
          
          rv$df <- add_btns(st_drop_geometry(result),role,"query")
          rv$sf <- result[which(!st_is_empty(result)),]
          
          # Results modal
          n <- nrow(result)
          if(n > 0){
            t <- paste0("Search returned <b>",format(n,big.mark=","),"</b> records. <br>
            Go to 'Results' tab for table of results, or 'Map' to interrogate them on a map.")

            # Activate download
            #shinyjs::show("dlQuery")
            shinyjs::removeClass(id = "dlDiv",class="buttonHidden")
            
          }
          else{
            t <- "<b>No records were found.</b>"
            #shinyjs::hide("dlQuery")
            shinyjs::addClass(id = "dlDiv",class="buttonHidden")
            
          }
          showModal(results_modal(t))
        })
      })
      
      # Results modal
      results_modal <- function(t){
        ns <- session$ns
        modalDialog(
          div(style="text-align:center",
              h4("Search complete"),
              HTML(t)
          )
          ,footer=NULL,size="s",easyClose=TRUE,fade=TRUE
        )
      }
      
      # dl Query ----
      output$dlQuery <- downloadHandler(
        filename = function() {
          paste("records-", format(Sys.time(),format="%Y%m%d%H%M%S"), ".csv", sep="")
        },
        content = function(file) {
          x <- rv$df[,
                     c("taxon_nbn","taxon_name","taxon_authority","taxon_qualifier",
                     "site_name","site_record","subsite_name","gridref",
                     "quantity","status","sex","stage","note",
                     "record_date","record_date_end","start_year","end_year","start_month","end_month",
                     "recorder","determiner","method","survey_name",
                     "verification_description","verification_user","verification_date","verification_note",
                     "guid",
                     "created_user","created_date","last_edited_user","last_edited_date"
                     )]
          write.csv(x, file)
        }
      )
    
      outputOptions(output, 'resultsMap', suspendWhenHidden = FALSE)
      outputOptions(output, 'resultsTable', suspendWhenHidden = FALSE)
      # Info button ----
      
      observeEvent(input$info,{
        showModal(info())
      })
      
      info <- function(){
        ns <- session$id
        modalDialog(
          div(h4("Instructions"),
              tags$ul(
                tags$li("Use the form to define a search query and press the 'Search' button to return a set of records."),
                tags$li("Vague queries may take a while to return the results. If you leave the form blank then it will return all records in the database."),
                tags$li("Results can be downloaded as a comma-separated value (.csv) file by clicking the 'Download results' button that appears once the results have been returned."),
                tags$li("Except for dates, all fields accept several search criteria. If several criteria are entered into one field, then records matching at least one are returned."),
                tags$li("The 'Results' tab shows the search results in table form."),
                tags$li("Under the 'Map' tab, records with grid references are plotted on an interactive map.")
                )
              ,style="width:100%;padding:20px"),
          , footer=NULL,size="m",easyClose=TRUE,fade=TRUE)
      }
    }
  )
}