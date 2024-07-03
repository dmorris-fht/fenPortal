subsite_modal_dialog <- function(session, d, mode) {
  ns <- session$ns
  if(mode == "add"){x <- "Add new subsite"}else{x <- "Subsite"}
  shiny::modalDialog(
    title = x,
    fluidRow(
      tagList(
        column(12,
               column(12,
                 column(6,
                        selectizeInput(
                          inputId = ns("site_modal"),
                          label = "Site",
                          choices = c(""),
                          multiple = FALSE,
                          options = list(maxItems = 1, placeholder = 'Select a site')
                          )
                        ),
                 column(6,
                        textInput(
                          inputId = ns("subsite_modal"),
                          label = "Subsite name:",
                          value = d[2],
                          placeholder = "Enter a new subsite name"
                        )
                 )
               ),
               column(12,
                 column(6,
                        textInput(
                          inputId = ns("gridref_modal"),
                          label = "Gridref:",
                          value = d[3]
                        )
                 ),
                 column(6,
                        textInput(
                          inputId = ns("vc_modal"),
                          label = "Vice county:",
                          value = d[4]
                        )
                 )
               ),
               column(12,
                      column(12,
                             textAreaInput(
                               inputId = ns("note_modal"),
                               label = "Description:",
                               value = ifelse(!is.na(d[5]),d[5],"")
                               )
                             )
                      )
               )
        )
    )
    ,
    size = "l",
    easyClose = TRUE,
    footer = div(
      class = "pull-right container",
      shiny::actionButton(
        inputId = ns("final_edit_subsite"),
        label = "Submit",
        icon = shiny::icon("edit"),
      )
    )
  ) %>% shiny::showModal()
  
}