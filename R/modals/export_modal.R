survey_modal_dialog <- function(session, s, e) {
  ns <- session$ns
  modalDialog(
    title = "Data export",
    fluidRow(
      tagList(
        column(12,
               column(12,
                      textInput(
                        inputId = ns("modal_sharing"),
                        label = "Sharing definition",
                        value = s
                        )
                      ),
               column(6,
                      textInput(
                        inputId = ns("modal_export_user"),
                        label = "Exported by",
                        value = e[1]
                        )
               ),
               column(6,
                      textInput(
                        inputId = ns("modal_export_date"),
                        label = "Export date",
                        value = format(e[2],format="%Y-%m-%d %H:%M:%S")
                        )
               )
               ),
        column(12,
               column(12,
                      textAreaInput(
                        inputId = ns("modal_export_note"),
                        label = "Export note",
                        value = ifelse(!is.na(e[3]),e[3],""),
                        resize = "vertical"
                      )
                      )
               ),
        column(12,
               column(6,
                      textInput(
                        inputId = ns("modal_created_user"),
                        label = "Created user",
                        value = e[4]
                      )
                      ),
               column(6,
                      textInput(
                        inputId = ns("modal_created_date"),
                        label = "Created date",
                        value = format(e[5],format="%Y-%m-%d %H:%M:%S")
                      )
               ),
               column(6,
                      textInput(
                        inputId = ns("modal_last_edited_user"),
                        label = "Lasted edited user",
                        value = e[6]
                      )
               ),
               column(6,
                      textInput(
                        inputId = ns("modal_last_edited_date"),
                        label = "Last edited date",
                        value = format(e[7],format="%Y-%m-%d %H:%M:%S")
                      )
               )
               )
      )
    )
    ,
    size = "m",
    easyClose = TRUE,
    footer = NULL
  ) %>% shiny::showModal()
}