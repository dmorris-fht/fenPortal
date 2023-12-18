importRecordsUI <- function(id){
  ns <- NS(id)
  tagList(
    column(12,
           h3("Import records"),
           div(
             style = "font-size:10px;border: 1px solid black; border-radius: 5px; padding: 10px ",
             withSpinner(DT::DTOutput(outputId = ns("recordsTable")),type = 7)
           ),
           div(
             style = "font-size:10px;border: 1px solid black; border-radius: 5px; padding: 10px ",
             withSpinner(DT::DTOutput(outputId = ns("matchSitesTable")),type = 7)
           ),
           div(
             style = "font-size:10px;border: 1px solid black; border-radius: 5px; padding: 10px ",
             withSpinner(DT::DTOutput(outputId = ns("matchTaxaTable")),type = 7)
           )
           )
    ,
    tags$script(src ="script.js")
  )
}

importRecordsServer <- function(id, con, username) {
  moduleServer(
    id,
    function(input, output, session) {

    }
  )
}