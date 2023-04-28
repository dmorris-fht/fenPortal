logger_configs_modal_dialog <- function(name, create_date, create_by, computer_name, application, application_version, overwrite, schedule_start, schedule_end, type, interval_day, interval_hour, interval_min, interval_sec, notes, edit) {
  if(edit == "edit"){
    x <- "Submit edits"
  }
  if(edit == "add"){
    x <- "Add new logger configuration"
  }
  
  modalDialog(
    title = "Logger configuration information",
    div(
      class = "text-center",
      
      div(
        style = "display: inline-block;",
        textInput(
          inputId = "name",
          label = "Logger configuration name:",
          value = name,
          placeholder = "Name for this logger configuration, e.g. 'Troll 1'"
        )
      ),br()
      ,
      div(
        style = "display: inline-block;",
        dateInput(
          inputId = "create_date",
          label = "Configuration creation date:",
          value = create_date,
          format = "dd/mm/yyyy"
        )
      ),br(),
      div(
        style = "display: inline-block;",
        textInput(
          inputId = "create_by",
          label = "Configuration creator:",
          value = create_by,
          placeholder = "Name of person who configured logger"
        )
      ),br(),
      div(
        style = "display: inline-block;",
        textInput(
          inputId = "computer_name",
          label = "Computer name:",
          value = computer_name,
          placeholder = "Name of device on which configuration was created"
        )
      ),br(),
      div(
        style = "display: inline-block;",
        textInput(
          inputId = "application",
          label = "Application name:",
          value = application,
          placeholder = "Name of software application used to configure logger"
        )
      ),br(),
      div(
        style = "display: inline-block;",
        textInput(
          inputId = "application_version",
          label = "Application version:",
          value = application_version,
          placeholder = "Version of software application used to configure logger"
        )
      ),br(),
      div(
        style = "display: inline-block;",
        selectInput(
          inputId = "overwrite",
          label = "Overwrite:",
          selected = application_version,
          choices = c("Enabled", "Disabled")
        )
      ),br(),
      div(
        style = "display: inline-block;",
        airDatepickerInput(
          inputId = "schedule_start",
          label = "Scheduled start date/time:",
          value = schedule_start,
          timepicker = TRUE,
          dateFormat = "yyyy-MM-dd",
          
        )
      ),br(),
      div(
        style = "display: inline-block;",
        airDatepickerInput(
          inputId = "schedule_end",
          label = "Scheduled end date/time:",
          value = schedule_end,
          timepicker = TRUE
        )
      ),br(),
      div(
        style = "display: inline-block;",
        selectInput(
          inputId = "type",
          label = "Configuration type:",
          choices = c("Linear")
        )
      ),br(),
      div(
        style = "display: inline-block;",
        numericInput(
          inputId = "interval_day",
          label = "Recording interval - no. of days:",
          value = interval_day,
          min = 0,
          max = 100,
          step = 1,
        )
      ),br(),
      div(
        style = "display: inline-block;",
        numericInput(
          inputId = "interval_hour",
          label = "Recording interval - no. of hours:",
          value = interval_hour,
          min = 0,
          max = 23,
          step = 1,
        )
      ),br(),
      div(
        style = "display: inline-block;",
        numericInput(
          inputId = "interval_min",
          label = "Recording interval - no. of minutes:",
          value = interval_min,
          min = 0,
          max = 59,
          step = 1,
        )
      ),br(),
      div(
        style = "display: inline-block;",
        numericInput(
          inputId = "interval_sec",
          label = "Recording interval - no. of seconds:",
          value = interval_sec,
          min = 0,
          max = 59,
          step = 1,
        )
      ),br(),
      div(
        style = "display: inline-block;",
        textAreaInput(
          inputId = "notes",
          label = "Configuration notes:",
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
      actionButton(
        inputId = "final_edit_lc",
        label = x,
        icon = icon("edit"),
        class = "btn-info"
      ),
      actionButton(
        inputId = "dismiss_modal",
        label = "Close",
        class = "btn-danger"
      )
    )
  ) %>% showModal()
  
}