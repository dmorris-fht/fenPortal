loginUI <- function(id){
  ns <- NS(id)
  tagList(
    div(
      id = ns("login"), 
      style = "position:absolute; top:0; left:0; 
      z-index:1000000;
      width: 100%; height: 100%; 
      background-color: white;",
      
      div(
        a(href = 'https://freshwaterhabitats.org.uk/',
          img(src="FHT logo large_clear background.png", height = "100px"),
          title = "Freshwater Habitats Trust", target = "_blank")
        , style = "position: absolute;top:0; right:0; padding-top: 10px; padding-right: 10px"),
      
      div(
        style=" 
              position: absolute;
              top: 30%;
              left: 50%;
              margin-top: -150px;
              margin-left: -200px;
              width: 400px;
              height: calc(90vh);
              overflow:hidden;
        text-align:center",
        
        h2("Welcome to Freshwater Habitats Trust's Fen Database"),
        p("Please enter your database login details"),
        
        div(
            textInput(
              inputId     = ns("username"), 
              label       = tagList(icon("user"), 
                                    "User Name"),
              placeholder = "Enter user name",
            ),
            
            passwordInput(
              inputId     = ns("password"), 
              label       = tagList(icon("unlock-alt"), 
                                    "Password"), 
              placeholder = "Enter password",
            )
            , style = "width: 70%; margin-left: 15%;
            text-align:center"), 
        
        div(
          class = "text-center",
          actionButton(
            inputId = ns("loginButton"), 
            label = "Log in",
            class = "btn-primary"
          )
        ), 
        
        div(id = ns("loader"),
            withSpinner(textOutput(ns("loginError")), color = "#00839B", size = 1, type = 4)
            , style="color: red; max-height:50px")
        )
      )
    )
}

loginServer <- function(id, login) {
  moduleServer(
    id,
    function(input, output, session) {
      
      shinyjs::hide("loader")
      
      observeEvent(input$loginButton, {
        
        shinyjs::show("loader")
        
        if (fenDbTest(input$username, input$password) == TRUE) 
        {
          login$username <- input$username
          login$password <- input$password
          login$admin <- FALSE
          login$test <- TRUE
          login$con <- fenDb(input$username, input$password)
          
          roles <- dbGetQuery(login$con, paste0("SELECT rolname FROM pg_roles WHERE
                         pg_has_role( '", input$username ,"', oid, 'member')"))

          if("azure_pg_admin" %in% roles$rolname){
            login$role <- "cru"
            login$admin <- TRUE
          }
          if("fen_create_read_update" %in% roles$rolname){
            login$role <- "cru"
          }
          if("fen_create_read" %in% roles$rolname){
            login$role <- "cr"
            runjs("$('.cru').remove()")
          }
          if("fen_read" %in% roles$rolname){
            login$role <- "r"
            runjs("$('.cru').remove()")
            runjs("$('.cr').remove()")
            
            #runjs("$(\"[data-expanded='manageMenu']\").closest(\".treeview\").remove()")
            runjs("$(\"[data-expanded='importMenu']\").closest(\".treeview\").remove()")
          }
          
          
          shinyjs::hide(id = "login")
          shinyjs::hide("loader")
          
        }
        if (fenDbTest(input$username, input$password) == FALSE){
          output$loginError <- renderText({ "Invalid username or password" })
        }
      })

    }
  
  )}


  