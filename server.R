
function(input, output, session) {
  addResourcePath("tempdir", tempdir())
  
  #Session reactives----

  login <- reactiveValues(username = NA, password = NA, test = NA, role = NA, con = NA)
  
  # Session copies of smaller db tables ----
  tables <- reactiveValues(sites0 = NA,
                           sites = NA,
                           subsites0 = NA,
                           subsites = NA,
                           projects = NA,
                           surveys = NA,
                           plots = NA,
                           hydro_installs0 = NA,
                           hydro_installs = NA)

  # Counter to catch first click on menu items ----
  n <- reactiveValues(
    fenmap = 0,
    sites = 0,
    surveys = 0, 
    queryRecords = 0, 
    enterRecords = 0, 
    importRecords = 0,
    vegManage = 0,
    plantLists = 0,
    vegLists = 0,
    dataSharing = 0,
    installsManage = 0,
    loggersManage = 0,
    dipsImport = 0
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
        if(input$menu == "vegManage"){
          n$vegManage <- n$vegManage + 1
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
        if(input$menu == "installsManage"){
          n$installsManage <- n$installsManage + 1
        }
        if(input$menu == "loggersManage"){
          n$loggersManage <- n$loggersManage + 1
        }
        if(input$menu == "dipsImport"){
          n$dipsImport <- n$dipsImport + 1
        }
      })
   
      # Initialise fen map module ----
      observe({
        if(n$fenmap == 1){
          #fenMapServer("fenmap")
        }
      })
      # Initialise sites module ----
      observe({
        if(n$sites == 1){
          #sitesServer("sites")
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
      
      # Hide add data modules for read only roles ----
      
      if(!(login$role == "cru" || login$role == "cr")){
        runjs(
          "$('ul[data-value=\\'enterRecords\\']').closest('li').hide();
            $('ul[data-value=\\'importRecords\\']').closest('li').hide();
          "
          
          )
        }
      
      # Initialise enterRecords  ----
      observe({
          if(n$enterRecords == 1){
            enterRecordsServer("enterRecords", login, tables)
          }
      })
  
      # Initialise importRecords  ----
      observe({
          if(n$importRecords == 1){
            importRecordsServer("importRecords", login, tables)
          }
      })
      
      # Initialise vegManage  ----
      observe({
        if(n$vegManage == 1){
          vegManageServer("vegManage", login, tables)
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
    
      # Initialise installsManage ----
      observe({
        if(n$installsManage == 1){
          installsManageServer("installsManage", login, tables)
        } 
      })
      
      # Initialise loggersManage ----
      observe({
          if(n$loggersManage == 1){
            loggersManageServer("loggersManage", login, tables)
            }
        })
      
      # Initialise dipsImport ----
      observe({
        if(n$dipsImport == 1 && (login$role == "cru" || login$role == "cr")){
          dipsImportServer("dipsImport",login,tables)
        }
      })
      
      # Other modules ----
  
      #exploreServer("exploreHydro")
  
        #logger_importServer("importLoggers",login$con)
        #weather_importServer("weatherImport",login$con)      
      
      
      
    }
  })
  
}
