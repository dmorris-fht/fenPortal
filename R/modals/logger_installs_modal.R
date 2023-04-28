logger_installs_modal_dialog <- function(session, install, install_date, install_by, install_notes, remove_date, remove_by, remove_notes, edit) {
  ns <- session$ns
  if(edit == "edit"){
    x <- "Submit edits"
  }
  if(edit == "add"){
    x <- "Add new logger installation"
  }
  if(edit == "remove"){
    x <- "Remove logger installation"
  }
  
  modalDialog(
    title = "Logger installation information",
    div(
      class = "text-center",
      div(
        style = "display: inline-block; width:90%",
        selectizeInput(
          inputId = ns("install"),
          label = "Choose installation:",
          selected = install,
          choices = c("")
        )
      ),br(),
      
        #For adding install
        
      div(
        style = "display: inline-block; width:90%",
        dateInput(
          inputId = ns("install_date"),
          label = "Logger installation date:",
          value = install_date,
          format = "dd/mm/yyyy"
        )
      ),br(),
      div(
        style = "display: inline-block; width:90%",
        shiny::textInput(
          inputId = ns("install_by"),
          label = "Logger installed by:",
          value = install_by,
          placeholder = "Name of person who installed the logger"
        )
      ),br(),
      div(
        style = "display: inline-block; width:90%",
        shiny::textAreaInput(
          inputId = ns("install_notes"),
          label = "Logger installation notes:",
          value = install_notes,
          placeholder = "Add any relevant notes",
          resize = "vertical"
        )
      )  
        #For removing install
        
        ,br(),
        div(
          style = "display: inline-block; width:90%",
          dateInput(
            inputId = ns("remove_date"),
            label = "Date logger removed from installation:",
            value = remove_date,
            format = "dd/mm/yyyy"
          )
        ),br(),
        div(
          style = "display: inline-block; width:90%",
          shiny::textInput(
            inputId = ns("remove_by"),
            label = "Logger installation removed by:",
            value = remove_by,
            placeholder = "Name of person who removed the logger installation"
          )
        ),br(),
        div(
          style = "display: inline-block; width:90%",
          shiny::textAreaInput(
            inputId = ns("remove_notes"),
            label = "Logger installation removal notes:",
            value = remove_notes,
            placeholder = "Add any relevant notes",
            resize = "vertical"
          )
        )
    ),
    style="padding: 10px 10px 10px 10px; width:100%",
    size = "s",
    easyClose = TRUE,
    footer = div(
      class = "pull-right container",
      shiny::actionButton(
        inputId = ns("final_edit_li"),
        label = x,
        icon = shiny::icon("edit"),
        class = "btn-info"
      ),
      shiny::actionButton(
        inputId = ns("dismiss_modal"),
        label = "Close",
        class = "btn-danger"
      )
    )
  ) %>% shiny::showModal()
}