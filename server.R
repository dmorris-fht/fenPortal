
function(input, output, session) {
  #login <- reactiveValues(username = NA, password = NA, test = NA, role = NA, con = NA)
  login <- reactiveValues(username = "fen_test", password = "Alkal1n3F3ns!_test", test = TRUE, role = "cru", con = fenDb("fen_test", "Alkal1n3F3ns!_test"))
  
  #loginServer("loginForm", login)
  
  output$username <- renderText({login$username})
  
  #Counter to catch first click on menu items
  n <- reactiveValues(
    surveys = 0, 
    queryRecords = 0, 
    enterRecords = 0, 
    importRecords = 0
    )
  
  #Login success observer
  observe({
  if(isTruthy(login$test)){
    
    # Home tab
    homeServer("home",login$con)
    
    # Handle tab loading
    observeEvent(input$menu,{
      if(input$menu == "surveys"){
        n$surveys <- n$surveys + 1
      }
      if(input$menu == "queryRecords"){
        n$queryRecords <- n$queryRecords + 1
      }
      if(input$menu == "enterRecords"){
        n$enterRecords <- n$enterRecords + 1
      }
    })
    
  # Initialise surveys module ----  
  observe({
        if(n$surveys == 1){
          surveyServer("surveys",login$con,login$role)
        }
      })

  #Initialise queryRecords  ----
  observe({
      if(n$queryRecords == 1){
        queryRecordsServer("queryRecords", login$con, login$role, login$username, login$password)
      }
    })

  #Initialise enterRecords  ----
  observe({
    if(login$role == "cru" || login$role == "cr"){
      if(n$enterRecords == 1){
        enterRecordsServer("enterRecords", login$con, login$username)
      }
    }else{
    shinyjs::hide(selector = "a[data-value='enterRecords']" )
    }
  })
  

    
    
    # Other modules ----
  #exploreServer("exploreHydro",login$con)
  #loggers_manageServer("loggersManage",login$con,login$role)
  
  
  
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
