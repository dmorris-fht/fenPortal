survey_modal_dialog <- function(session, d, edit) {
  ns <- session$ns
  if (edit) {
    x <- "Submit edits"
  } else {
    x <- "Add new data source"
  }
  shiny::modalDialog(
    title = "Data source / survey",
    fluidRow(
      tagList(
      column(12,
             column(6,
            textInput(
              inputId = ns("survey"),
              label = "Name:",
              value = d[1],
              placeholder = "Enter a unique name"
            )
        ),
        column(6,
            selectizeInput(
              inputId = ns("survey_type"),
              label = "Data source type",
              choices = c(""),
              selected = "",
              multiple = FALSE
            )
            )
        )
      ,
      column(12,
          column(6,
              dateInput(
                inputId = ns("start_date"),
                label = "Start date (e.g. survey start date):",
                value = format(d[3],format = "%Y-%m-%d %H:%M:%S"),
                format = "dd/mm/yyyy"
              )
              ),
          column(6,
              numericInput(
                inputId = ns("start_year"),
                label = "Start year:",
                value = d[5],
                min = 1600 , max = 3000, step = 1
              )
              )
          ),
      column(12,
          column(6,
              dateInput(
                inputId = ns("end_date"),
                label = "End date (e.g. survey end date, publication date):",
                value = format(d[4],format = "%Y-%m-%d %H:%M:%S"),
                format = "dd/mm/yyyy"
              )
              ),
          column(6,
              numericInput(
                inputId = ns("end_year"),
                label = "End year:",
                value = d[6],
                min = 1600 , max = 3000, step = 1
              )
              )
          ),
      column(12,
          column(6,
              selectizeInput(
                inputId = ns("project"),
                label = "Associated project:",
                selected = "",
                choices = c(""),
                multiple = FALSE
              )
              ),
          column(6,
                 div(style="margin-top:21px",
                     actionButton(
                       inputId = ns("new_project"),
                       icon = icon("plus"),
                       label = "Add new project"
                     )
                     )
              )
          ),
      column(12,
          column(6,
              selectizeInput(
                inputId = ns("sharing"),
                label = "Sharing permission:",
                selected = "",
                choices = c(""),
                multiple = FALSE
              ),
              textInput(
                inputId = ns("copyright"),
                label = "Copyright statement:",
                value = d[10],
                placeholder = "E.g. CC-BY"
              )
              ),
          column(6,
              textInput(
                inputId = ns("source"),
                label = "Origin (author, surveyor, organisation etc.):",
                value = d[7],
                placeholder = "Enter an origin if relevant"
              )
          )
          ),

      column(12,
             column(12,
            textAreaInput(
              inputId = ns("description"),
              label = "Description of data source:",
              value = d[11],
              placeholder = "Add a description",
              resize = "vertical"
            )
            )
          ,
      column(12,
          textInput(
            inputId = ns("url"),
            label = "URL of associated documentation (e.g. report):",
            value = d[12]
          )
          )
      ),

      column(12,
          column(6,
              textInput(
                inputId = ns("created_user"),
                label = "Created by:",
                value = d[13]
              ),
              textInput(
                inputId = ns("last_edited_user"),
                label = "Last edited by:",
                value = d[14]
              )
              ),
          column(6,
              textInput(
                inputId = ns("created_date"),
                label = "Date created:",
                value = format(d[15],format = "%Y-%m-%d %H:%M:%S"),
              )
              ,
              textInput(
                inputId = ns("last_edited_date"),
                label = "Date last edited:",
                value = format(d[16],format = "%Y-%m-%d %H:%M:%S"),
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
        inputId = ns("final_edit"),
        label = x,
        icon = shiny::icon("edit"),
      ),
      shiny::actionButton(
        inputId = ns("dismiss_modal"),
        label = "Close",
      )
    )
  ) %>% shiny::showModal()
}



project_modal_dialog <- function(session) {
  ns <- session$ns
  shiny::modalDialog(
    title = "Data source / survey",
    fluidRow(
      tagList(
        column(12,
               textInput(
                 inputId = ns("project_new"),
                 label = "Project name:",
                 placeholder = "Enter a unique project name"
               )
        )
      )
    )
    ,
    size = "s",
    easyClose = TRUE,
    footer = div(
      class = "pull-right container",
      shiny::actionButton(
        inputId = ns("add_new_project"),
        label = "Add project",
      ),
      shiny::actionButton(
        inputId = ns("dismiss_modal"),
        label = "Close",
      )
    )
  ) %>% shiny::showModal()
}