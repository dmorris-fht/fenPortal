weather_importUI <- function(id, label = "weather import") {
  ns <- NS(id)
  tagList(
    fluidRow(
      
      column(12,
             column(8,
                    h1("Import weather station data"),
                    HTML("<p>Use this page to import data from weather stations. 
                            Import data as a .csv using the template available from here. 
                            The date-time column must be formated as yyyy-mm-dd hh:mm, e.g. '2023-04-14 00:00'.
                            Data in each import must be from the same weather station.
                            </p>")
                    )
      ),
      column(12,
             column(8,
                    fileInput(ns("weatherCsv"),label="Choose .csv file to import:", 
                              buttonLabel = "Browse...",
                              placeholder = "No file selected",
                              accept = ".csv"),
                    selectInput(ns("choose_station"), label = "Choose weather station", choices = c()),
                    textInput(ns("provider"), label = "Name of person or organisation who provided the data:"),
                    dateInput(ns("provide_date"), label = "Date data provided:", format = "dd/mm/yyyy"),
                    textAreaInput(ns("notes"), label = "Import notes:"),
                    actionButton(ns("import"), label = "Import")
             ),
             column(4,
                    withSpinner(DT::dataTableOutput(ns("weatherTable")))
             )
             )
      )
    )
}

weather_importServer <- function(id,con) {
  moduleServer(
    id,
    function(input, output, session) {
      import <- reactiveValues(data = data.frame(NA,NA))
      
      observe({
        #Import csv and load data table
        req(input$weatherCsv)
        file <- input$weatherCsv
        ext <- file$type
        
        req(file)
        shiny::validate(need(ext == "text/csv", "Please upload a csv file"))
        
        w <- read.csv(file$datapath, header = TRUE)
        w[,1] <- as.POSIXct(w[,1],tz="GMT")
        
        import$data <- w
      })
      
      output$weatherTable <- DT::renderDataTable(
        datatable(import$data, 
                  options=list(
                    bFilter=0, 
                    bLengthChange=0, 
                    columnDefs = list(list(visible=FALSE,targets=c(0)),list(className = 'dt-center', targets = 2))),
                  colnames = c("","Date / time", "Total rainfall (cm)")
        )
      )

      #Add weather station choices
      weather <- reactiveValues(stations = NA)
      
      observe({
        
        #Get list of sites
        stations <- dbGetQuery(con, "SELECT * FROM hydro_monitoring.weather_stations ORDER BY station_name")
        weather$stations <- stations

        #update sites input choices with installs  
        station_choices <- stations$id
        names(station_choices) <- stations$station_name
        updateSelectInput(session, inputId = "choose_station", choices = station_choices)
        
      })
      
      #Modal for import progress
      import_progress_modal <- function() {
        ns <- session$ns
        modalDialog(
          div("Import in progress",style="width:100%; text-align:center")
          , footer=NULL,size="s",easyClose=FALSE,fade=TRUE)
      }
      #Modal for import success
      import_success_modal <- function() {
        ns <- session$ns
        modalDialog(
          div("Data successfully imported",style="width:100%; text-align:center")
          , footer=NULL,size="s",easyClose=TRUE,fade=TRUE)
      }
      #Modal for import error
      import_error_modal <- function() {
        ns <- session$ns
        modalDialog(
          div("Import error",style="width:100%; text-align:center")
          , footer=NULL,size="s",easyClose=TRUE,fade=TRUE)
      }
      
      observeEvent(input$import,{
        req(input$weatherCsv)
        file <- input$weatherCsv
        ext <- file$type
        
        shiny::validate(need(ext == "text/csv", "Please upload a csv file"))
        shiny::validate(need(input$choose_station, "Please choose a weather station"))
        shiny::validate(need(input$provider, "Please give name of data provider"))
        shiny::validate(need(input$provide_date, "Please give date data provided"))
        
        showModal(import_progress_modal())
        
          #Insert weather data import row
          query <- paste("INSERT INTO hydro_monitoring.weather_imports (provider,notes,provided_date) VALUES (",
                         "'", input$provider, "',",
                         "'", input$notes, "',",
                         "'", input$provide_date, "'",
                         ") RETURNING id", sep="")
          
          import_info <- dbGetQuery(con, query)

          if(!is.na(import_info$id)){
            i <- import$data
            colnames(i) <- c("measure_date_time","measure_rainfall_daily")
            i$weather_import <- import_info$id
            i$station <- input$choose_station
            insert <- pgInsert(
              conn = con,
              name = c(schema = "hydro_monitoring", table = "weather_data"),
              data.obj = i,
              partial.match = TRUE,
              upsert.using = c("station","measure_date_time")
            )
            if(insert == TRUE){
              removeModal()
              showModal(import_success_modal())
            }
            else{
              #Delete the import info record if the import fails
              dbGetQuery(con,paste("DELETE FROM hydro_monitoring.weather_imports WHERE id = ",import_info$id,sep=""))
              removeModal()
              showModal(import_error_modal())
            }
            }      
        })
    }
  )}