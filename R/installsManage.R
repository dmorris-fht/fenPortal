installsManageUI <- function(id){
  ns <- NS(id)
  tagList(
    withSpinner(
      tagList(
        
        column(12,
                  column(12,
                         h3("Manage hydrological monitoring installations")
                  ),
                  # Map
                  column(7,
                         div(style="padding:5px;height:50vh",
                           withSpinner(leafletOutput(ns("installsMap")),type = 7, caption = "Loading map")
                         )
                         ),
                  # Datatable
                  column(5,
                         div(
                           style = "font-size:12px;padding: 0 ",
                           withSpinner(DT::DTOutput(outputId = ns("installsTable")),type = 7, caption = "Loading installs")
                           ),
                         actionButton(ns("newInstall"),"Add install", icon = icon("add")),
                         actionButton(ns("importInstalls"),"Import installs", icon = icon("file-import"))
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

installsManageServer <- function(id, login, tables) {
  moduleServer(
    id,
    function(input, output, session) {
      role <- login$role
      user <- login$username
      password <- login$password
      
      source("./R/modals/installs_modal.R")
      
      # Module initialisation ----
      app_tables(con_global, tables, c("sites","hydro_installs"))
      
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
      
      # Reactive to hold installs data ----
      
      rv <- reactiveValues(
        df = NA,
        dt_row = NULL, add_or_edit = 0,
        edit_button = NULL)
      
      observe({
        req(tables$hydro_installs)
        rv$df <- add_btns(tables$hydro_installs,role,"installs")
      })
      
      # Site boundary reactive ----
      sites1 <- reactive({
        req(tables$sites0)
        req(tables$hydro_installs)
        
        s <- tables$sites0 %>% filter(!st_is_empty(.)) %>% st_transform(crs = 4326) # Filter out empty geoms and transform to 4326
        s <- s[s$id %in% tables$hydro_installs$site,]
        return(s)
      })
      
      # DT definition ----
      
      cols <- c("site_name","install_name","install_type_description","install_date","Buttons")
      output$installsTable <- DT::renderDT(
        {
          req(rv$df)
          shiny::isolate(rv$df)
          x <- rv$df[,cols]
          x$install_date <- as.Date(x$install_date)
          return(x)
        }
        ,
        server = TRUE,
        escape = F,
        rownames = FALSE,
        selection = 'single',
        filter = 'top',
        colnames =  c("Site","Install name","Type","Date",""),
        options = list(processing = TRUE,
                       columnDefs = list(
                         list(orderable = FALSE, targets = length(cols)-1),
                         list(width = '60px',targets = length(cols)-1)
                       ),
                       extensions = c("FixedHeader"),#, "Scroller")
                       fixedHeader = TRUE,
                       scrollY = "50vh"
        )
      ) 
      
      proxy_DT <- DT::dataTableProxy("installsTable")
      
      observe({
        req(rv$df)
        x <- rv$df[,cols]
        x$install_date <- as.Date(x$install_date)
        DT::replaceData(proxy_DT, x, resetPaging = FALSE, rownames = FALSE)
      })
      
      # Map definition ----
      
      output$installsMap <- renderLeaflet({
        map <- leaflet() %>%
          addTiles(group="OpenStreetMap.Mapnik") %>% 
          addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
          addProviderTiles("OpenStreetMap.HOT", group = "OpenStreetMap.HOT") %>%
          addMiniMap(tiles = "OpenStreetMap.HOT", toggleDisplay = TRUE) %>%
          addSearchOSM() %>%
          addEasyButton(easyButton(icon="fa-home", title="Home view", onClick=JS("function(btn, map){ map.fitBounds([[-5.515982,50.024189],[1.35876,55.948577]]); }"))) %>%
          addEasyButton(easyButton(icon="fa-crosshairs", title="Locate Me", onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
          addMeasure() %>%
          addFullscreenControl() %>%
          addScaleBar(position = c("bottomleft")) %>%
          addLayersControl(baseGroups = c("OpenStreetMap.Mapnik","Satellite","OpenStreetMap.HOT","Stamen.TerrainBackground"),
                           options = layersControlOptions(collapsed = TRUE)) %>%
          fitBounds(-5.515982,50.024189,1.35876,55.948577)
        
        map
      })
      
      proxy_map <- leafletProxy("installsMap")
      
      observe({
        req(tables$sites0)
        req(tables$hydro_installs0)
        bbox <- st_bbox(tables$hydro_installs0)
        
        proxy_map %>% 
          fitBounds(bbox[[1]],bbox[[2]],bbox[[3]],bbox[[4]]) %>%
          addPolygons(data = sites1(),
                      group = "sites", 
                      color = "red", weight = 1, opacity = 1, fillOpacity = 0, label = ~site) %>%
          addCircleMarkers(data = tables$hydro_installs0,
                           radius = 3,
                           stroke = TRUE,
                           color = "black",
                           weight = 0.5,
                           opacity = 1,
                           fill = TRUE,
                           fillColor =  ifelse(tables$hydro_installs$install_type_description == "Barometric logger","blue","white"),
                           fillOpacity = 1,
                           popup = NA,
                           layerId = tables$hydro_installs$install_name,
                           label = tables$hydro_installs$install_name,
                           group = "installs")
      })
      
      # New install button controls ----
      
      observeEvent(input$newInstall,{
        new_install_modal(session)
      })
      
      observeEvent(input$importInstalls,{
        import_installs_modal(session)
      })
      
      observeEvent(input$importAGOL,{
        future_promise({
          import_agol(
            fenDb0(user,password),
            input$agol_url,
            w = NA, g = TRUE, a = TRUE,
            f = 500, d = 2000, 
            s = "spatial", t = "monitoring_hydro_installs"
          )
        })%...>%(function(result){
          
        })
      })
    }
  )
}