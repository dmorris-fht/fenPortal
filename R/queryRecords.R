# Date search - improve
# Progress wheel when query fired
# Format of downloaded results (columns)
# DT row buttons NEXT 1
# Leaflet map NEXT 2

queryRecordsUI <- function(id){
  ns <- NS(id)
  tagList(
    column(12,
           #Query form ----
           tabsetPanel(  
             ##Form fields ----
             tabPanel("Search",id = ns("query"),
                      column(6,
                             h3("Search records"),
                             p("Use the form below to define a search query and press the 'Search' button to return a set of records. 
                             Results can be downloaded as a comma-separated value (.csv) file by clicking the 'Download results' button."),
                             p("Except for dates, all fields accept several search criteria. If several criteria are entered, then all records matching at least one are returned."),  
                             p("The 'Results' tab shows the search results in table form. Under the 'Map' tab, you can see the records plotted on an interactive map."),
                               p("Vague queries may take a while to return the results. If you leave the form blank then it will return all records in the database.")
                             ),
                      ## Action buttons ----
                      column(12,
                             div(style="width:100%; display:inline-block; padding-bottom: 10px; margin-bottom:5px", 
                                actionButton(ns("runQuery"),label = "Search",icon=icon("search")),
                                downloadButton(ns("dlQuery"),label = "Download results", icon = icon("file-download"))
                                )   
                             ),
                      br(),br(),
                      ### Sites fields ----
                      column(12,
                             div(style="width:100%; display:inline-block; padding-bottom: 0; margin-bottom:0", 
                                    column(3,
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
                                    column(3,
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
                                    column(3,
                                           div(style="float:left;width:100%",
                                               textInput(
                                                 inputId = ns("gridref"),
                                                 label = "Grid reference(s)",
                                                 value = NULL,
                                                 placeholder = 'List of grid references separated by semi-colons'
                                               ) 
                                           )
                                    ),
                                    column(3,
                                           div(style="float:left;width:100%",
                                               textInput(
                                                 inputId = ns("site_record"),
                                                 label = "Site name as given in original record",
                                                 value = NULL,
                                                 placeholder = 'Comma-separated list of names'
                                               ) 
                                           )
                                    )
                                    )
                      ),
                      ### Taxon fields ----
                             column(12,
                                    div(style="Width:100%; display:inline-block; padding-bottom: 0; margin-bottom:0", 
                                    column(3,
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
                                    column(2,
                                           div(style="float:left;width:100%",
                                              selectInput(
                                                inputId = ns("fenspp"),
                                                label = "Fen plant species",
                                                choices = choices_fenspp,
                                                selected = ""
                                              ) 
                                           )
                                         ),
                                    column(7,
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
                        ,

                        
                      ### Other fields ----
                        
                      column(12,
                             column(3,
                                    div(style="float:left;width:100%",
                                        textInput(
                                          inputId = ns("recorder"),
                                          label = "Recorder",
                                          value = NULL,
                                          placeholder = "List of recorder names separated by semi-colons"
                                        ) 
                                    )
                                    ),
                             column(3,
                                    div(style="float:left;width:100%",
                                        textInput(
                                          inputId = ns("date"),
                                          label = "Date",
                                          value = NULL,
                                          placeholder = "Record date - year or date in format dd/mm/yyyy"
                                        ) 
                                    )
                             ),
                             column(3,
                                    div(style="float:left;width:100%",
                                        textInput(
                                          inputId = ns("start_date"),
                                          label = "Start date",
                                          value = NULL,
                                          placeholder = "Date range - start year or date in format dd/mm/yyyy"
                                        ) 
                                    )
                             ),
                             column(3,
                                    div(style="float:left;width:100%",
                                        textInput(
                                          inputId = ns("end_date"),
                                          label = "End date",
                                          value = NULL,
                                          placeholder = "Date range - end year or date in format dd/mm/yyyy"
                                        ) 
                                    )
                             )
                             ),

                      column(12,
                             column(3,
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
                             column(3,
                                    div(style="float:left;width:100%",
                                        selectizeInput(
                                          inputId = ns("survey_type"),
                                          label = "Data source type(s)",
                                          choices = c(""),
                                          options = list(placeholder = 'Select one or more data source types')
                                        ) 
                                    )
                             ),
                             column(3,
                                    div(style="float:left;width:100%",
                                        selectizeInput(
                                          inputId = ns("project"),
                                          label = "Project(s)",
                                          choices = c(""),
                                          options = list(placeholder = 'Select one or more projects')
                                        ) 
                                    )
                             ),
                             column(3,
                                    div(style="float:left;width:100%",
                                        selectizeInput(
                                          inputId = ns("sharing"),
                                          label = "Sharing type(s)",
                                          choices = c(""),
                                          options = list(placeholder = 'Select one or more sharing types')
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
                              h3("Map of search results")
                       )    
                    )
    
           )
           )
    ,
    tags$script(src ="script.js")
  )
}

queryRecordsServer <- function(id, con, role, user, password) {
  moduleServer(
    id,
    function(input, output, session) {
      # Form controls ----
      ## Initialisation ----
      
      shinyjs::hide("dlQuery")
      
      site <- dbGetQuery(con, "SELECT id, site, county FROM spatial.fen_sites ORDER BY site")
      choices_site <- site$id
      names(choices_site) <- paste0(site$site, " [",site$county, "]")
      
      choices_site_1 <- site$id
      names(choices_site_1) <- site$site
      
      updateSelectizeInput(session,
                           "site",
                           choices=choices_site, 
                           selected = "",
                           server = FALSE,
                           options = list(
                             placeholder = 'Select one or more sites',
                             onInitialize = I('function() { this.setValue(""); }')
                           )
      )
      
      subsite <- dbGetQuery(con, "SELECT ss.id, ss.site, s.site AS site_name, ss.subsite FROM 
                            spatial.fen_subsites ss,
                            spatial.fen_sites s
                            WHERE ss.site = s.id
                            ORDER BY site, subsite")
      choices_subsite <- reactive({
        if(isTruthy(input$site)){
          ss <- subsite[subsite$site %in% input$site,]
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
        if(length(choices_subsite()) > 0){
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
      
      p <- dbGetQuery(con, "SELECT id, project FROM records.projects ORDER BY project")
      choices_p <- p$id
      names(choices_p) <- p$project
      
      updateSelectizeInput(session,
                           "project",
                           choices=choices_p,
                           selected = "",
                           server = FALSE,
                           options = list(
                             placeholder = 'Select one or more projects',
                             onInitialize = I('function() { this.setValue(""); }')
                           )
      )
      
      st <- dbGetQuery(con, "SELECT code, description FROM lookups.lookup_survey_types")
      choices_st <- st$code
      names(choices_st) <- st$description
      
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
      
      sh <- dbGetQuery(con, "SELECT code, description FROM lookups.lookup_sharing")
      choices_st <- sh$code
      names(choices_st) <- sh$description 
      
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
      
      survey <- dbGetQuery(con, "SELECT id, survey, project, survey_type, sharing FROM records.surveys ORDER BY survey")
      choices_survey_0 <- survey$id
      names(choices_survey_0) <- survey$survey
      
      choices_survey_rv <- reactive({
        if(isTruthy(input$project)){
          p_test <- as.numeric(input$project)
        }
        else{
          p_test <- survey$project
        }
        if(isTruthy(input$survey_type)){
          st_test <- as.numeric(input$survey_type)
        }
        else{
          st_test <- survey$survey_type
        }
        if(isTruthy(input$sharing)){
          sh_test <- as.numeric(input$sharing)
        }
        else{
          sh_test <- survey$sharing
        }

        choices_survey_0[which(
          survey$project %in% p_test &
          survey$survey_type %in% st_test &
          survey$sharing %in% sh_test
        )]
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
      
      gf_check <- reactive({
        v <- lapply(unlist(strsplit(input$gridref,",")),function(x){
          validate_gf(x, s = NULL, ss = NULL, con = NULL)})
        return(grepl("error = 1",paste(v,collapse="")) || !isTruthy(input$gridref))
      })
      date_check_0 <- reactive({
        date_check(input$start_date,0)$error
      })
      date_check_1 <- reactive({
        date_check(input$end_date,1)$error
      })
      
      iv <- InputValidator$new()
      iv$add_rule("gridref",function(value){
        v <- lapply(unlist(strsplit(input$gridref,",")),function(x){
          validate_gf(x, s = NULL, ss = NULL, con = NULL)})
        if(grepl("error = 1",paste(v,collapse="")) && isTruthy(input$gridref)){
          return("Invalid grid reference(s)")
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
      iv$enable()
      
      ## Reactive to hold records ----
      rv <- reactiveValues(
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
          geom = character(),
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
         verification_description = character(),
         Buttons = character()
        ),
        df = NA,
        dt_row = NULL, add_or_edit = 0,
        edit_button = NULL)
      
      # Data table definition ----
      
      output$resultsTable <- DT::renderDT(
        {
          shiny::isolate(rv$df)
          
          x <- rv$df[,c("taxon_name","site_name","subsite_name","gridref","record_date","survey_name","Buttons")]
          colnames(x) <-  c("Taxon","Site","Subsite","Gridref","Date","Date source","")

          return(x)
        }
        ,
        server = TRUE,
        escape = F,
        rownames = FALSE,
        selection = 'single',
        options = list(processing = TRUE,
                       columnDefs = list(
                         list(orderable = FALSE, targets = c(6)),
                         list(width = '30px',targets=c(6))
                       ),
                       extensions = c("FixedHeader"),#, "Scroller")
                       fixedHeader = TRUE,
                       scrollY = "50vh"
        )
      ) 
      
      proxy <- DT::dataTableProxy("resultsTable")
      
      observe({
        x <- rv$df[,c("taxon_name","site_name","subsite_name","gridref","record_date","survey_name","Buttons")]

        DT::replaceData(proxy, x, resetPaging = FALSE, rownames = FALSE)
      })
      
      
      ## Run query ----
      observeEvent(input$runQuery,{
        req(gf_check() == TRUE)
        req(date_check_0() == FALSE)
        req(date_check_1() == FALSE)
        
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
          sql_in("r.site",as.numeric(input$site)),
          sql_in("r.subsite",as.numeric(input$subsite)),
          gf_vec("r.gridref",unlist(strsplit(input$gridref,";"))),
          sql_in("u.informal_group",input$taxon_group),
          sql_in("u.nbn_taxon_version_key_for_recommended_name",input$taxon),
          sql_in("r.survey",as.numeric(input$survey)),
          sql_in("s.project",as.numeric(input$project)),
          sql_in("s.survey_type",as.numeric(input$survey_type)),
          sql_in("s.sharing",as.numeric(input$sharing)),
          like_vec("r.recorder",unlist(strsplit(input$recorder,";"))),
          like_vec("r.site_record",unlist(strsplit(input$site_record,";"))),
          sql_date("r.record_date",input$date,input$date), #WHAT ABOUT RECORDS WHERE DATE IS IN END_DATE OR SURVEYS?
          sql_date("r.record_date",input$start_date,input$end_date), #WHAT ABOUT DATE RANGES AND RECORDS WHERE DATE COMES FROM SURVEY?
          w_fenspp
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
          v.description AS verification_description,
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
        
        future_promise({
          con0 <- DBI::dbConnect(RPostgres::Postgres(), 
                        user = user, 
                        password = password,
                        host = "data-fht.postgres.database.azure.com", 
                        port = 5432, 
                        dbname = "fen_database")
          result <- st_read(dsn = con0, query = sql, geometry_column = "geom")
          DBI::dbDisconnect(con0)
          return(result)
        })%...>% (function(result) {
          rv$df <- add_btns(st_drop_geometry(result),role,"query")
          rv$sf <- result

          # Results modal
          n <- nrow(rv$df)
          showModal(results_modal())
          if(n > 0){
            output$results_text <- renderText({
            paste0("Search returned <b>",n,"</b> records. <br>
            Go to 'Results' tab for table of results, or 'Map' to interrogate them on a map.")
              })
          
          # Activate download
            shinyjs::show("dlQuery")
          }
          else{
            output$results_text <- renderText({"<b>No records were found.</b>"})
          }
        })
      })
      
      # Results modal
      results_modal <- function(){
        ns <- session$ns
        modalDialog(
          div(style="text-align:center",
              h4("Search complete"),
              uiOutput(ns("results_text"))
          )
          ,footer=NULL,size="s",easyClose=TRUE,fade=TRUE
        )
      }
      
      ## dl Query
      output$dlQuery <- downloadHandler(
        filename = function() {
          paste("records-", format(Sys.time(),format="%Y%m%d%H%M%S"), ".csv", sep="")
        },
        content = function(file) {
          write.csv(rv$df, file)
        }
      )
    }
  )
}