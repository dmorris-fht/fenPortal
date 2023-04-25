# loggers_manageUI <- function(id){
#   ns <- NS(id)
#   tagList(
#   )
# }
# 
# loggers_manageServer <- function(id, login) {
#   moduleServer(
#     id,
#     function(input, output, session) {
#       
#     }
#   )
# }
source("../global.R")

con <- fenDb("dmorris","Taraxacum1!")

loggers_manageUI <- fluidPage(
  
  
  column(12,
         column(6,
                withSpinner(leafletOutput("installsMap"), type = 7)
         ),
         column(6,
                withSpinner(DT::dataTableOutput("installsTable"), type = 7)
         )
  )
)

loggers_manageServer <- function(input, output){
  installs <- reactiveValues(data=NA,sites=NA,topo=NA)
  
  #Get list of sites
  sites <- dbGetQuery(con, "SELECT B.id AS siteid, B.site AS sitename  FROM spatial.monitoring_hydro_installs A, spatial.fen_sites B WHERE A.site = B.id ORDER BY B.site")
  inst <- dbGetQuery(con, "SELECT A.id AS installid, A.install_name AS installname, B.id AS siteid, B.site AS sitename, C.description AS install_type, A.install_location, A.install_reason, A.installed_by, A.install_depth, A.install_protrusion, A.install_geology, A.install_hydrogeo, A.install_hydroeco  FROM spatial.monitoring_hydro_installs A, spatial.fen_sites B, lookups.lookup_hydro_install C WHERE A.site = B.id AND A.install_type = C.code ORDER BY B.site, A.install_name")
  topo <- dbGetQuery(con, "SELECT * FROM hydro_monitoring.installs_topo")
  
  installs$data <- inst
  installs$sites <- sites
  installs$topo <- topo
  
}

shinyApp(ui = loggers_manageUI, server = loggers_manageServer)
