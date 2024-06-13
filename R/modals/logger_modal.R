logger_modal_dialog <- function(session, serial, model, purchase_date, firmware, hardware, comm_config, notes, edit) {
  ns <- session$ns
  if (edit) {
    x <- "Submit edits"
  } else {
    x <- "Add new logger"
  }
  shiny::modalDialog(
    title = "Logger information",
    div(
      class = "text-center",
      div(
        style = "display: inline-block;width:90%",
        shiny::textInput(
          inputId = ns("serial"),
          label = "Logger serial number:",
          value = serial,
          placeholder = "Input logger serial number"
        )
      ),br(),
      div(
        style = "display: inline-block;width:90%",
        shiny::textInput(
          inputId = ns("model"),
          label = "Logger model:",
          value = model,
          placeholder = "Input logger model"
        )
      ),br(),
      div(
        style = "display: inline-block; width:90%",
        shiny::dateInput(
          inputId = ns("purchase_date"),
          label = "Logger purchase date:",
          value = purchase_date,
          format = "dd/mm/yyyy"
        )
      ),br(),
      div(
        style = "display: inline-block; width:90%",
        shiny::textInput(
          inputId = ns("firmware"),
          label = "Logger firmware version:",
          value = firmware,
          placeholder = "Input logger firmware version"
        )
      ),br(),
      div(
        style = "display: inline-block; width:90%",
        shiny::textInput(
          inputId = ns("hardware"),
          label = "Logger hardware version:",
          value = hardware,
          placeholder = "Input hardware version"
        )
      ),br(),
      div(
        style = "display: inline-block; width:90%",
        shiny::textInput(
          inputId = ns("comm_config"),
          label = "Logger comms config:",
          value = comm_config,
          placeholder = "Input comms config"
        )
      ),br(),
      div(
        style = "display: inline-block; width:90%",
        shiny::textAreaInput(
          inputId = ns("notes"),
          label = "Notes:",
          value = ifelse(!is.na(),notes,""),
          placeholder = "Add any relevant notes",
          resize = "vertical"
        )
      )
    ),
    style="padding: 10px 10px 10px 10px; width:100%",
    size = "s",
    easyClose = TRUE,
    footer = div(
      shiny::actionButton(
        inputId = ns("final_edit_l"),
        label = x,
        icon = shiny::icon("edit")
      ),
      shiny::actionButton(
        inputId = ns("dismiss_modal"),
        label = "Close"
      )
    )
  ) %>% shiny::showModal()
}