logger_modal_dialog <- function(serial, model, purchase_date, firmware, hardware, comm_config, notes, edit) {
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
        style = "display: inline-block;",
        shiny::textInput(
          inputId = "serial",
          label = "Logger serial number:",
          value = serial,
          placeholder = "Input logger serial number"
        )
      ),br(),
      div(
        style = "display: inline-block;",
        shiny::textInput(
          inputId = "model",
          label = "Logger model:",
          value = model,
          placeholder = "Input logger model"
        )
      ),br(),
      div(
        style = "display: inline-block;",
        shiny::dateInput(
          inputId = "purchase_date",
          label = "Logger purchase date:",
          value = purchase_date,
          format = "dd/mm/yyyy"
        )
      ),br(),
      div(
        style = "display: inline-block;",
        shiny::textInput(
          inputId = "firmware",
          label = "Logger firmware version:",
          value = firmware,
          placeholder = "Input logger firmware version"
        )
      ),br(),
      div(
        style = "display: inline-block;",
        shiny::textInput(
          inputId = "hardware",
          label = "Logger hardware version:",
          value = hardware,
          placeholder = "Input hardware version"
        )
      ),br(),
      div(
        style = "display: inline-block;",
        shiny::textInput(
          inputId = "comm_config",
          label = "Logger logger comms config:",
          value = comm_config,
          placeholder = "Input comms config"
        )
      ),br(),
      div(
        style = "display: inline-block;",
        shiny::textAreaInput(
          inputId = "notes",
          label = "Notes:",
          value = notes,
          placeholder = "Add any relevant notes",
          resize = "vertical"
        )
      )
    ),
    size = "m",
    easyClose = TRUE,
    footer = div(
      class = "pull-right container",
      shiny::actionButton(
        inputId = "final_edit_l",
        label = x,
        icon = shiny::icon("edit"),
        class = "btn-info"
      ),
      shiny::actionButton(
        inputId = "dismiss_modal",
        label = "Close",
        class = "btn-danger"
      )
    )
  ) %>% shiny::showModal()
}