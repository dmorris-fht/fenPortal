installsManageUI <- function(id){
  ns <- NS(id)
  tagList(
    withSpinner(
      tagList(
        column(12,
               # UI HERE
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
      
      # Module initialisation ---- 
      isolate({
        app_tables(tables, c("sites","hydro_installs"))
      })
      
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
    }
  )
}