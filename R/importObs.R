importObsUI <- function(id){
  ns <- NS(id)
  tagList(
    withSpinner(
      tagList(
        div(style="height:80vh",
        column(12,
               column(12,
                 h3("Import target notes"),
                 column(6,
                        HTML("<p>To import target notes and photos recorded using ArcGIS Online, enter the REST URL for the feature service 
                        containing the data and click 'Import data'.</p>
                        <p>You can find the URL for a service on its details page - you just need to add the layer number to the end. 
                        The layer must follow the template available <a href='https://fht.maps.arcgis.com/home/item.html?id=c513fe6f65494b8ca4ca671f60773729', target='_blank'>here</a>, and the service must be shared publicly (e.g. via a view layer) or the URL should include an authentication key.</p>
                        <p>Target notes already imported will be skipped - these should be edited in the database.</p><br>"),
                        textAreaInput(ns("agol_url"), label="Enter ArcGIS Online REST service URL", width = "100%", resize = "vertical", placeholder ="ArcGIS REST service URL"),
                        selectizeInput(ns("sites"), 
                                       label = "Sites to import (optional)",
                                       multiple = TRUE,
                                       choices = c(""),
                                       options = list(placeholder = 'Select a site')
                                       ),
                        actionButton(ns("importAGOL"), label = "Import data")
                        )
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

importObsServer <- function(id, login, tables) {
  moduleServer(
    id,
    function(input, output, session) {
      role <- login$role
      user <- login$username
      password <- login$password
      
      # Module initialisation ---- 
      observe({
        req(tables$sites)
        runjs(
          paste0(
          "$('#",id,"-module').parent().addClass('shiny-spinner-hidden');
                 $('div[data-spinner-id=\\'",id,"-module\\']').css('display','inline')"
                 )
        )
      })
      
      user <- login$username
      password <- login$password
      
      isolate({
        app_tables(tables, c("sites","subsites","surveys"))
        
        uksi_load(c(0))
      })
      
      choices_site <- reactive({
        if(isTruthy(tables$sites)){
          c <- tables$sites$id
          names(c) <- paste0(tables$sites$site, " [",tables$sites$county, "]")
        }
        else{
          c <- c("")
        }
        return(c)
      })
      
      # Reactives ----
      
      import <- reactiveValues(data = NA, attach = NA, agol = 0) # Reactive for data import
      
      # Form controls ----
      
      observe({
        if(isTruthy(input$agol_url)){
          shinyjs::enable("importAGOL")
        }else{
          shinyjs::disable("importAGOL")
        }
      })
      
      iv <- InputValidator$new()
      iv$add_rule("agol_url",sv_required())
      iv$enable()
      
      observe({
        updateSelectizeInput(session,
                             "sites",
                             choices=choices_site(), 
                             selected = "",
                             server = FALSE,
                             options = list(
                               onInitialize = I('function() { this.setValue(""); }')
                               )
                             )
        })
      
      # AGOL import ----
      
      observeEvent(input$importAGOL,{
        req(input$agol_url)
        import_modal()
      })
      
      # Don't import
      observeEvent(input$import0,{
        removeModal()
      })
      
      # Make query to AGOL and import
      observeEvent(input$import1,{
        req(input$agol_url)
        showModal(import_progress_modal())
        where <- paste0("(site IN (",paste(input$sites,collapse=","),"))")

        future_promise({
          showModal(
            modalDialog(
              div(style="text-align:left;width:60%",
                  tags$h4("Uploading",class="loading")
              )
              ,footer=NULL,size="s",easyClose=FALSE,fade=TRUE
            )
          )
          
          # Fetch AGOL data and import
          
          f <- fetch_agol( input$agol_url, where, TRUE, TRUE)
          
          con0 <- fenDb0(user,password)
          
          r <- import_table(con0,
                            "spatial",
                            "monitoring_observations",
                            1,
                            input$agol_url,
                            NULL,
                            TRUE,
                            TRUE,
                            f$data,
                            f$attach,
                            500,
                            2000
                            )
          dbDisconnect(con0)
        return(r)
        })%...>%(function(r){
          if(!isTruthy(r$error)){
            removeModal()
            import_success_modal()
          }else{
            import_error_modal(r$error)
          }
        })

        })
      
      # Modal functions ----
      
      #Modal for verifying upload
      import_modal <- function() {
        ns <- session$ns
        modalDialog(
          div(
            p("Are you sure you want to import the data to the database?"),
            actionButton(ns("import1"),label = "Yes"),
            actionButton(ns("import0"),label= "No")
            ,style="width:100%;text-align:center")
          , footer=NULL,size="s",easyClose=FALSE,fade=TRUE) %>% showModal()
      }
      
      # Modal for AGOL error
      import_agol_error <- function() {
        ns <- session$ns
        modalDialog(
          div(
            p("No response from ArcGIS Online source or no data found")
            ,style="width:100%;text-align:center")
          , footer=NULL,size="s",easyClose=TRUE,fade=TRUE) %>% showModal()
      }
      
      #Modal for upload success
      import_success_modal <- function() {
        ns <- session$ns
        modalDialog(
          div(
            h4("Data successfully imported")
            ,style="width:100%; text-align:center")
          , footer=NULL,size="s",easyClose=TRUE,fade=TRUE) %>% showModal()
      }
      
      #Modal for no data to import
      import_NA_modal <- function() {
        ns <- session$ns
        modalDialog(
          div(
            h4("No data to import")
            ,style="width:100%; text-align:center")
          , footer=NULL,size="s",easyClose=TRUE,fade=TRUE) %>% showModal()
      }
      
      #Modal for upload error
      import_error_modal <- function(err) {
        ns <- session$ns
        modalDialog(
          div(
            h4("Import error"),
            p(err)
            ,style="width:100%; text-align:center")
          , footer=NULL,size="s",easyClose=TRUE,fade=TRUE) %>% showModal()
      }
      
      #Modal for upload progress
      import_progress_modal <- function(){
        ns <- session$ns
        modalDialog(
          div(
            tags$h4("Importing",class="loading")
            ,style="width:100%; text-align:left")
          , footer=NULL,size="s",easyClose=FALSE,fade=TRUE) %>% showModal()
      }
      
    }
  )
}