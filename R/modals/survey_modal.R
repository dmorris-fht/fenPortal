# qual parameter enables other modules to use these modals without input id conflict

survey_modal_dialog <- function(session, d, mode, qual) { 
  ns <- session$ns
  x <- "Data source"
  if(isTruthy(mode)){
    if(mode == "add"){
      x <- "Add new data source"
    }
    if(mode == "edit"){
      x <- "Edit data source"
    }
  }
  
  status <- c("open","closed")
  names(status) <- c("Open","Closed")

  shiny::modalDialog(
    title = x,
    fluidRow(
      tagList(
      column(12,
             selectizeInput(
               inputId = ns(paste0("status",qual)),
               label= "Status:",
               choices = status,
               selected = d[17]
               )
             ),

      column(12,
        column(6,
            textInput(
              inputId = ns(paste0("survey",qual)),
              label = "Name:",
              value = d[1],
              placeholder = "Enter a unique name"
            )
        ),
        column(6,
            selectizeInput(
              inputId = ns(paste0("survey_type",qual)),
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
                inputId = ns(paste0("start_date",qual)),
                label = "Start date (e.g. survey start date):",
                value = format(d[3],format = "%Y-%m-%d %H:%M:%S"),
                format = "dd/mm/yyyy"
              )
              ),
          column(6,
              numericInput(
                inputId = ns(paste0("start_year",qual)),
                label = "Start year:",
                value = d[5],
                min = 1600 , max = 3000, step = 1
              )
              )
          ),
      column(12,
          column(6,
              dateInput(
                inputId = ns(paste0("end_date",qual)),
                label = "End date (e.g. survey end date, publication date):",
                value = format(d[4],format = "%Y-%m-%d %H:%M:%S"),
                format = "dd/mm/yyyy"
              )
              ),
          column(6,
              numericInput(
                inputId = ns(paste0("end_year",qual)),
                label = "End year:",
                value = d[6],
                min = 1600 , max = 3000, step = 1
              )
              )
          ),
      column(12,
          column(6,
                 div(style="float:left;width:80%",
                    selectizeInput(
                      inputId = ns(paste0("project",qual)),
                      label = "Associated project:",
                      selected = "",
                      choices = c(""),
                      multiple = FALSE
                      )
                 ),
                 div(style="float:left;width:10%;margin-top:21px",  
                     actionButton(
                       inputId = ns("new_project"),
                       icon = icon("plus"),
                       label = ""
                     )
                 )
              ),
          column(6,
                 selectizeInput(
                   inputId = ns(paste0("sharing",qual)),
                   label = "Sharing permission:",
                   selected = "",
                   choices = c(""),
                   multiple = FALSE
                 )
          )
          ),
      column(12,
             column(6,
                    textInput(
                      inputId = ns(paste0("source",qual)),
                      label = "Origin (author, surveyor, organisation etc.):",
                      value = d[7],
                      placeholder = "Enter an origin if relevant"
                      )
                    ),
            column(6,
                textInput(
                  inputId = ns(paste0("copyright",qual)),
                  label = "Copyright statement:",
                  value = d[10],
                  placeholder = "E.g. CC-BY"
                  )
                )
          ),

      column(12,
             column(12,
            textAreaInput(
              inputId = ns(paste0("description",qual)),
              label = "Description of data source:",
              value = ifelse(!is.na(d[11]),d[11],""),
              placeholder = "Add a description",
              resize = "vertical"
            )
            )
          ,
      column(12,
          textInput(
            inputId = ns(paste0("url",qual)),
            label = "URL of associated documentation (e.g. report):",
            value = d[12]
          )
          )
      ),

      column(12,
          column(6,
              textInput(
                inputId = ns(paste0("created_user",qual)),
                label = "Created by:",
                value = d[13]
              ),
              textInput(
                inputId = ns(paste0("last_edited_user",qual)),
                label = "Last edited by:",
                value = d[14]
              )
              ),
          column(6,
              textInput(
                inputId = ns(paste0("created_date",qual)),
                label = "Date created:",
                value = format(d[15],format = "%Y-%m-%d %H:%M:%S"),
              )
              ,
              textInput(
                inputId = ns(paste0("last_edited_date",qual)),
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
        inputId = ns(paste0("final_edit",qual)),
        label = "Submit",
        icon = shiny::icon("edit")
      )
    )
  ) %>% shiny::showModal()
  
}

project_modal_dialog <- function(session) {
  ns <- session$ns
  shiny::modalDialog(
    title = "Add new project",
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
        icon = shiny::icon("edit")
      )
    )
  ) %>% shiny::showModal()
}