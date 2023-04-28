# dips_manageUI <- function(id){
#   ns <- NS(id)
#   tagList(
#   )
# }
# 
# dips_manageServer <- function(id, login) {
#   moduleServer(
#     id,
#     function(input, output, session) {
#       
#     }
#   )
# }
source("../global.R")

con <- fenDb("dmorris","Taraxacum1!")

dips_manageUI <- fluidPage(
  
  
  column(12,
         column(6,
                withSpinner(leafletOutput("installsMap"), type = 7)
         ),
         column(6,
                withSpinner(DT::dataTableOutput("installsTable"), type = 7)
         )
  )
)

dips_manageServer <- function(input, output){
  installs <- reactiveValues(data=NA,sites=NA,topo=NA)

  #Get list of sites
  sites <- pgGetGeom(con, query = "SELECT ST_Transform(B.geom,4326) AS geom, B.id AS siteid, B.site AS sitename  FROM spatial.monitoring_hydro_installs A, spatial.fen_sites B WHERE A.site = B.id ORDER BY B.site")
  inst <- pgGetGeom(con, query = "SELECT ST_Transform(A.geom,4326) AS geom, A.id AS installid, A.install_name AS installname, B.id AS siteid, B.site AS sitename, C.description AS install_type, A.install_location, A.install_reason, A.installed_by, A.install_depth, A.install_protrusion, A.install_geology, A.install_hydrogeo, A.install_hydroeco  FROM spatial.monitoring_hydro_installs A, spatial.fen_sites B, lookups.lookup_hydro_install C WHERE A.site = B.id AND A.install_type = C.code ORDER BY B.site, A.install_name")
  topo <- dbGetQuery(con, "SELECT * FROM hydro_monitoring.installs_topo")
  
  installs$data <- inst
  installs$sites <- sites
  installs$topo <- topo

  output$installsMap <- renderLeaflet ({
    data <- installs$data
    map <- leaflet() %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
      addMiniMap(tiles = "OpenStreetMap", toggleDisplay = TRUE, minimized = TRUE) %>%
      addEasyButton(easyButton(icon="fa-home", title="Home view", onClick=JS("function(btn, map){ map.fitBounds([[49.959999905, -7.57216793459 ],[58.6350001085, 1.68153079591 ]]); }"))) %>%
      addEasyButton(easyButton(icon="fa-crosshairs", title="Locate Me", onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
      addPolygons(data = installs$sites,
                  stroke = TRUE,
                  weight = 2,
                  color = "red",
                  fill = FALSE,
                  popup = NA
      ) %>%
      addCircleMarkers(data = data,
                       radius = 3,
                       stroke = TRUE,
                       color = "black",
                       weight = 0.5,
                       opacity = 1,
                       fill = TRUE,
                       fillColor =  ifelse(data$install_type == "Barometric logger","blue","white"),
                       fillOpacity = 1,
                       popup = NA,
                       layerId = unlist(as.data.frame(installs$data[installs$data$siteid == input$choose_site,])$installid),
                       group = "installs"
                       # ,label = unlist(as.data.frame(installs$data[installs$data$siteid == input$choose_site,])$installname)
      ) %>%
      addMeasure() %>%
      addScaleBar(position = c("bottomleft")) %>%
      addLayersControl(baseGroups = c("Satellite","OpenStreetMap"),
                       options = layersControlOptions(collapsed = TRUE))
    map
  })
  
  
  output$installsTable <- DT::renderDataTable({
    d <- as.data.frame(installs$data)
    d <- d[,c("sitename","installname","install_type")]
    datatable(d, 
              options=list(bLengthChange=0, columnDefs = list(list(visible=FALSE,targets=c(0)))),
                            colnames = c("Site", "Install name", "Install type")
              )
  }
  )
}

shinyApp(ui = dips_manageUI, server = dips_manageServer)
