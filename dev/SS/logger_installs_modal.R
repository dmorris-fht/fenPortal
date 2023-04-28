logger_installs_modal_dialog <- function(install, install_date, install_by, install_notes, remove_date, remove_by, remove_notes, edit) {
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
        style = "display: inline-block;",
        selectizeInput(
          inputId = "install",
          label = "Choose installation:",
          selected = install,
          choices = c("")
        )
      ),br(),
      
        #For adding install
        
      div(
        style = "display: inline-block;",
        dateInput(
          inputId = "install_date",
          label = "Logger installation date:",
          value = install_date,
          format = "dd/mm/yyyy"
        )
      ),br(),
      div(
        style = "display: inline-block;",
        shiny::textInput(
          inputId = "install_by",
          label = "Logger installed by:",
          value = install_by,
          placeholder = "Name of person who installed the logger"
        )
      ),br(),
      div(
        style = "display: inline-block;",
        shiny::textAreaInput(
          inputId = "install_notes",
          label = "Logger installation notes:",
          value = install_notes,
          placeholder = "Add any relevant notes",
          resize = "vertical"
        )
      )  
        #For removing install
        
        ,br(),
        div(
          style = "display: inline-block;",
          dateInput(
            inputId = "remove_date",
            label = "Date logger removed from installation:",
            value = remove_date,
            format = "dd/mm/yyyy"
          )
        ),br(),
        div(
          style = "display: inline-block;",
          shiny::textInput(
            inputId = "remove_by",
            label = "Logger installation removed by:",
            value = remove_by,
            placeholder = "Name of person who removed the logger installation"
          )
        ),br(),
        div(
          style = "display: inline-block;",
          shiny::textAreaInput(
            inputId = "remove_notes",
            label = "Logger installation removal notes:",
            value = remove_notes,
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
        inputId = "final_edit_li",
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