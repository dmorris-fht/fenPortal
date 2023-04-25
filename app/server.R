# Define server logic required to draw a histogram
function(input, output, session) {
  login <- reactiveValues(username = NA, password = NA, test = NA, role = NA, con = NA)
  loginServer("loginForm", login)
  
  observe(
    if(isTruthy(login$test)){
      exploreServer("exploreHydro",login$con)
      
      if(login$role == "cru" || login$role == "cr"){
        dips_importServer("importDips",login$con)
        weather_importServer("weatherImport",login$con)      }
    }
    )
}
