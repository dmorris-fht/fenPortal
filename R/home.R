homeUI <- function(id){
  ns <- NS(id)
  tagList(
    column(12,
           h1("Welcome!"),
           column(12,
                  column(4,
                         h3("About"),
                         br(),br(),br(),br(),
                         h3("Features"),
                         br(),br(),br(),
                         h3("Documentation"),
                         br(),br(),br()
                         ),
                  column(4,
                         h3("News")
                         ),
                  column(4,
                         h3("Database statistics")
                         
                         )
                  
                  )
           
    )
  )
}

homeServer <- function(id, con) {
  moduleServer(
    id,
    function(input, output, session) {
      
    }
  )
}