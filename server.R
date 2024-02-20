
function(input, output, session) {
  #Session reactives----

  # Login reactive
  login <- reactiveValues(username = NA, password = NA, test = NA, role = NA, con = NA)

  # Session copies of smaller db tables ----
  tables <- reactiveValues(sites0 = NA,
                           sites = NA,
                           subsites0 = NA,
                           subsites = NA,
                           projects = NA,
                           surveys = NA,
                           plots = NA,
                           hydro_installs = NA)

  # Counter to catch first click on menu items ----
  n <- reactiveValues(
    fenmap = 0,
    sites = 0,
    surveys = 0, 
    queryRecords = 0, 
    enterRecords = 0, 
    importRecords = 0,
    plantLists = 0,
    vegLists = 0,
    dataSharing = 0,
    loggersManage = 0
    )
  
  #Login----
  loginServer("loginForm", login)
  
  #Initialise on successful login ----
  observe({
    if(isTruthy(login$test)){

      # logout button ----
      observeEvent(input$logout,{
        session$reload()
      })
      
      # Initialise tables for session ----
      future_promise({
        con0 <- poolCheckout(con_global)
        sites0 <- st_read(dsn = con0,
                         query = "SELECT id, site, county, geom AS geom FROM spatial.fen_sites ORDER BY site",
                         geometry_column = "geom")
        sites <- st_drop_geometry(sites0)
        projects <- dbGetQuery(con0, "SELECT * FROM records.projects ORDER BY project")
        #surveys <- dbGetQuery(con0, "SELECT id, survey, project, survey_type, sharing FROM records.surveys ORDER BY survey") # simpler version used for modules other than query module
        surveys <- dbGetQuery(con0,"SELECT
                            s.id AS id,
                            s.survey,
                            s.survey_type AS survey_type,
                            s.start_date,
                            s.end_date,
                            s.start_year,
                            s.end_year,
                            s.source,
                            s.project AS projectid,
                            s.sharing AS sharing_code,
                            s.copyright,
                            s.description AS description,
                            s.url,
                            s.created_user,
                            s.last_edited_user,
                            s.created_date,
                            s.last_edited_date,
                            st.description AS survey_type_description,
                            sh.description AS sharing,
                            p.project AS project
                            FROM
                              records.surveys s,
                              records.projects p,
                              lookups.lookup_sharing sh,
                              lookups.lookup_survey_types st
                            WHERE
                              s.project = p.id AND
                              s.sharing = sh.code AND
                              s.survey_type = st.code
                            ORDER BY s.id") #This query needed for query module
        subsites0 <- st_read(dsn = con0, query = "SELECT ss.id, ss.site, s.site AS site_name, ss.subsite, ST_TRANSFORM(ss.geom,4326) AS geom FROM 
                            spatial.fen_subsites ss,
                            spatial.fen_sites s
                            WHERE ss.site = s.id
                            ORDER BY site, subsite",
                            geometry_column = "geom")
        subsites <- st_drop_geometry(subsites0)
        plots <- dbGetQuery(con0, "SELECT * FROM spatial.monitoring_vegetation")
        hydro_installs <- dbGetQuery(con0, "SELECT A.id AS installid,
                     A.install_name AS installname,
                     B.id AS siteid, B.site AS sitename,
                     C.description AS install_type,
                     A.install_location,
                     A.install_reason,
                     A.installed_by,
                     A.install_depth,
                     A.install_protrusion,
                     A.install_geology,
                     A.install_hydrogeo,
                     A.install_hydroeco
                     FROM spatial.monitoring_hydro_installs A, spatial.fen_sites B, lookups.lookup_hydro_install C
                     WHERE A.site = B.id AND A.install_type = C.code ORDER BY B.site, A.install_name")
        
        poolReturn(con0)
        return(list("sites0" = sites0,
                  "sites" = sites,
                  "subsites0" = subsites0,
                  "subsites" = subsites,
                  "projects" = projects,
                  "surveys" = surveys,
                  "plots" = plots,
                  "hydro_installs" = hydro_installs))
      })%...>% (function(result) {
        tables$sites0 <- result$sites0
        tables$sites <- result$sites
        tables$subsites0 <- result$subsites0
        tables$subsites <- result$subsites
        tables$projects <- result$projects
        tables$surveys <- result$surveys
        tables$plots <- result$plots
        tables$hydro_installs <- result$hydro_installs
      })
      
      # Home tab ----
      homeServer("home",login)
    
      # Handle tab loading ----
      observeEvent(input$menu,{
        if(input$menu == "spatial"){
          n$fenmap <- n$fenmap + 1
        }
        if(input$menu == "sites"){
          n$sites <- n$sites + 1
        }
        if(input$menu == "surveys"){
          n$surveys <- n$surveys + 1
        }
        if(input$menu == "queryRecords"){
          n$queryRecords <- n$queryRecords + 1
        }
        if(input$menu == "enterRecords"){
          n$enterRecords <- n$enterRecords + 1
        }
        if(input$menu == "importRecords"){
          n$importRecords <- n$importRecords + 1
        }
        if(input$menu == "dataSharing"){
          n$dataSharing <- n$dataSharing + 1
        }
        if(input$menu == "plantLists"){
          n$plantLists <- n$plantLists + 1
        }
        if(input$menu == "vegLists"){
          n$vegLists <- n$vegLists + 1
        }
        if(input$menu == "loggersManage"){
          n$loggersManage <- n$loggersManage + 1
        }
      })
   
      # Initialise fen map module ----
      observe({
        if(n$fenmap == 1){
          fenMapServer("fenmap")
        }
      })
      # Initialise sites module ----
      observe({
        if(n$sites == 1){
          sitesServer("sites")
        }
      })
    
      # Initialise surveys module ----  
      observe({
        if(n$surveys == 1){
          surveyServer("surveys",login, tables)
        }
      })

      # Initialise queryRecords  ----
      observe({
        if(n$queryRecords == 1){
          queryRecordsServer("queryRecords", login, tables)
        }
      })

      # Initialise enterRecords  ----
      observe({
        if(login$role == "cru" || login$role == "cr"){
          if(n$enterRecords == 1){
            enterRecordsServer("enterRecords", login, tables)
          }
        }else{
        shinyjs::hide(selector = "a[data-value='enterRecords']" )
        }
      })
  
      # Initialise enterRecords  ----
      observe({
        if(login$role == "cru" || login$role == "cr"){
          if(n$importRecords == 1){
            importRecordsServer("importRecords", login, tables)
          }
        }else{
          shinyjs::hide(selector = "a[data-value='importRecords']" )
        }
      })
      
      # Initialise plantLists  ----
      observe({
        if(n$plantLists == 1){
          plantListsServer("plantLists", tables)
        }
      })
      # Initialise vegLists  ----
      observe({
        if(n$vegLists == 1){
          vegListsServer("vegLists", tables)
        }
      })
      # Initialise dataSharing  ----
      observe({
        if(login$role == "cru"){
          if(n$dataSharing == 1){
            dataSharingServer("dataSharing", login)
          }
        }else{
          shinyjs::hide(selector = "a[data-value='dataSharing']" )
        }
      })
    
      # Initialise loggersManage ----
      observe({
          if(n$loggersManage == 1){
            loggersManageServer("loggersManage", login, tables)
          }
      })
      
      # Other modules ----
  
      exploreServer("exploreHydro")
  
  
      if(login$role == "cru" || login$role == "cr"){

        #dips_importServer("importDips",login$con)
        #logger_importServer("importLoggers",login$con)
        #weather_importServer("weatherImport",login$con)      
      }
      
      
    }
  })
  
  # Close user's connection at end of session ----
  session$onSessionEnded(function() {
    reactive({
      poolClose(login$con)
    })
  })
  
}
