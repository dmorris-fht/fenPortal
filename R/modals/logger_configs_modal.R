logger_configs_modal_dialog <- function(session, name, create_date, create_by, computer_name, application, application_version, overwrite, schedule_start, schedule_end, type, interval_day, interval_hour, interval_min, interval_sec, notes, edit) {
  ns <- session$ns
  if(edit == "edit"){
    x <- "Submit edits"
  }
  if(edit == "add"){
    x <- "Add new logger configuration"
  }
  modalDialog(
    title = "Logger configuration",
        column(12,
          div(style = "display: inline-block;width:100%",
          textInput(
            inputId = ns("name"),
            label = "Logger configuration name:",
            value = name,
            placeholder = "Name for this logger configuration, e.g. 'Troll 1'"
          )
          )
        ),
        column(6,
            div(style = "display: inline-block;width:90%",
          dateInput(
            inputId = ns("create_date"),
            label = "Configuration creation date:",
            value = create_date,
            format = "dd/mm/yyyy"
          )
            )
        ),
      column(6,
          div(style = "display: inline-block;width:90%",
          textInput(
            inputId = ns("create_by"),
            label = "Configuration creator:",
            value = create_by,
            placeholder = "Name of person who configured logger"
          )
          )
        ),
      column(6,
          div(style = "display: inline-block;width:90%",
          textInput(
            inputId = ns("computer_name"),
            label = "Computer name:",
            value = computer_name,
            placeholder = "Name of device on which configuration was created"
          )
          )
        ),
      column(6,
          div(style = "display: inline-block;width:90%",
          textInput(
            inputId = ns("application"),
            label = "Application name:",
            value = application,
            placeholder = "Name of software application used to configure logger"
          )
          )
        ),
      column(6,
          div(style = "display: inline-block;width:90%",
          textInput(
            inputId = ns("application_version"),
            label = "Application version:",
            value = application_version,
            placeholder = "Version of software application used to configure logger"
          )
          )
        ),
      column(6,
          div(style = "display: inline-block;width:90%",
          selectInput(
            inputId = ns("overwrite"),
            label = "Overwrite:",
            selected = application_version,
            choices = c("Enabled", "Disabled")
          )
          )
        ),
      column(6,
          div(style = "display: inline-block;width:90%",
          airDatepickerInput(
            inputId = ns("schedule_start"),
            label = "Scheduled start date/time:",
            value = schedule_start,
            timepicker = TRUE,
            dateFormat = "yyyy-MM-dd"
          )
          )
        ),
      column(6,
          div(style = "display: inline-block;width:90%",
          airDatepickerInput(
            inputId = ns("schedule_end"),
            label = "Scheduled end date/time:",
            value = schedule_end,
            timepicker = TRUE,
            dateFormat = "yyyy-MM-dd"
          )
          )
        ),
      column(6,
             ),
      column(6,
          div(style = "display: inline-block;width:90%",
          selectInput(
            inputId = ns("type"),
            label = "Configuration type:",
            choices = c("Linear")
          )
          )
        ),
      column(6,
          div(style = "display: inline-block;width:90%",
          numericInput(
            inputId = ns("interval_day"),
            label = "Recording interval - no. of days:",
            value = interval_day,
            min = 0,
            max = 100,
            step = 1,
          )
          )
        ),
      column(6,
          div(style = "display: inline-block;width:90%",
          numericInput(
            inputId = ns("interval_hour"),
            label = "Recording interval - no. of hours:",
            value = interval_hour,
            min = 0,
            max = 23,
            step = 1,
          )
          )
        ),
      column(6,
          div(style = "display: inline-block;width:90%",
          numericInput(
            inputId = ns("interval_min"),
            label = "Recording interval - no. of minutes:",
            value = interval_min,
            min = 0,
            max = 59,
            step = 1,
          )
          )
        ),
      column(6,
          div(style = "display: inline-block;width:90%",
          numericInput(
            inputId = ns("interval_sec"),
            label = "Recording interval - no. of seconds:",
            value = interval_sec,
            min = 0,
            max = 59,
            step = 1,
          )
          )
        ),
      column(12,
          div(style = "display: inline-block;width:100%",
          textAreaInput(
            inputId = ns("notes"),
            label = "Configuration notes:",
            value = ifelse(!is.na(notes),notes,""),
            placeholder = "Add any relevant notes",
            resize = "vertical"
          )
          )
        )
      
    ,
    style="padding: 10px 10px 10px 10px; width:100%",
    size = "m",    
    easyClose = TRUE,
    footer = div(
      class = "pull-right container",
      actionButton(
        inputId = ns("final_edit_lc"),
        label = x,
        icon = icon("edit")
      ),
      actionButton(
        inputId = ns("dismiss_modal"),
        label = "Close"
      )
    )
  ) %>% showModal()
  
}