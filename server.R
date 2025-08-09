options(shiny.maxRequestSize=30*1024^2)

function(input, output, session) {
  addResourcePath("tempdir", tempdir())
  
  #Session reactives----

  # Login reactive
  login <- reactiveValues(username = NA, password = NA, test = NA, role = NA, admin = NA)
  
  
  
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
    importVeg = 0,
    plantLists = 0,
    vegLists = 0,
    importObs = 0,
    spIntro = 0,
    dataSharing = 0,
    installsManage = 0,
    loggersManage = 0,
    dipsImport = 0,
    stratImport = 0
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
        if(input$menu == "importVeg"){
          n$importVeg <- n$importVeg + 1
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
        if(input$menu == "importObs"){
          n$importObs <- n$importObs + 1
        }
        if(input$menu == "spIntro"){
          n$spIntro <- n$spIntro + 1
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
        if(input$menu == "stratImport"){
          n$stratImport <- n$stratImport + 1
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
            $('ul[data-value=\\'importVeg\\']').closest('li').hide();
            $('ul[data-value=\\'importObs\\']').closest('li').hide();
            $('ul[data-value=\\'dipsImport\\']').closest('li').hide();
            $('ul[data-value=\\'weatherImport\\']').closest('li').hide();
            $('ul[data-value=\\'loggersImport\\']').closest('li').hide();
            $('ul[data-value=\\'stratImport\\']').closest('li').hide();
          "
          )
      }
      
      if(!login$admin){
        runjs(
          "$('ul[data-value=\\'dataSharing\\']').closest('li').hide();"
        )
      }
      
      # Initialise enterRecords  ----
      observe({
          if(n$enterRecords == 1){
            enterRecordsServer("enterRecords", login, tables, input$menu)
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
          vegManageServer("vegManage", login, tables, input$menu)
        }
      })
      
      # Initialise importVeg  ----
      observe({
        if(n$importVeg == 1){
          importVegServer("importVeg", login, tables)
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
      
      # Initialise importObs  ----
      observe({
        if(login$role == "cru"){
          if(n$importObs == 1){
            importObsServer("importObs", login, tables)
          }
        }else{
          shinyjs::hide(selector = "a[data-value='importObs']" )
        }
      })
      
      # Initialise spIntro  ----
      # observe({
      #   if(n$spIntro == 1){
      #     spIntroServer("spIntro", login, tables)
      #   }
      # })
      
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
      
      # Initialise dipsImport ----
      observe({
        if(n$stratImport == 1 && (login$role == "cru" || login$role == "cr")){
          stratImportServer("stratImport",login,tables)
        }
      })
      
      # Other modules ----
  
      #exploreServer("exploreHydro")
  
        #logger_importServer("importLoggers",login$con)
        #weather_importServer("weatherImport",login$con)      
      
      
      
    }
  })
  
}
