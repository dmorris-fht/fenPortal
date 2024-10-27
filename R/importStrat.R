stratImportUI <- function(id){
  ns <- NS(id)
  tagList(
    withSpinner(
      tagList(
        column(12,
               column(12,
                      column(6,
                             h3("Import stratigraphy data"),
                             HTML("<p>Use this module to import stratigraphic data from coring from ArcGIS Online to the database.</p>
                                  <p> The ArcGIS feature service must follow the template follow the template available <a href='https://services9.arcgis.com/J5mi5KNMFx93FsQu/arcgis/rest/services/fen_stratigraphy_template/FeatureServer', target='_blank'>here</a>")
                      )
               ),
               column(6,
                      column(12,
                             textAreaInput(ns("agol_url"), label="Enter ArcGIS Online REST URL of stratigraphy data", width = "100%", resize = "vertical"),
                      ),
                      textAreaInput(ns("import_notes_agol"), label = "Import notes", width = "100%", resize = "vertical"),
                      checkboxInput(ns("import_attachments"), label = "Import attachments",value = FALSE),
                      actionButton(ns("importAGOL"), label = "Import data")
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

stratImportServer <- function(id, login, tables) {
  moduleServer(
    id,
    function(input, output, session) {
      role <- login$role
      user <- login$username
      password <- login$password
      
      # Module initialisation ---- 
      isolate({
        app_tables(tables, c("sites","hydro_installs"))
      })
      
      observe({
        req(tables$sites0)
        req(tables$sites)
        req(tables$hydro_installs0)
        req(tables$hydro_installs)
        
        runjs(
          paste0(
          "$('#",id,"-module').parent().addClass('shiny-spinner-hidden');
                 $('div[data-spinner-id=\\'",id,"-module\\']').css('display','inline')"
                 )
        )
      })
      
      # Reactives ----
      
      import <- reactiveValues(data = NULL, attach = NULL, agol = 0) # Reactive for data import
      
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
      
      #Import AGOL data to database
      observeEvent(input$importAGOL,{
        req(input$agol_url)
        
        # Show request modal
        # Make request
        # Error or modal to verify import
        
        #Yes/no popup to verify import
        showModal(import_modal())
      })
      
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
      
      # Upload event
      observeEvent(input$import1,{
        removeModal()
        showModal(import_progress_modal())
        
        # IMPORT HERE
      })
      
      # Don't upload
      observeEvent(input$import0,{
        removeModal()
        import$data <- NULL
        import$attach <- NULL
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
    }
  )
}