enterRecordsUI <- function(id){
  ns <- NS(id)
  tagList(
    withSpinner(
      tagList(
    # Form controls ----
    column(12,h3("Enter biological records")) , 
    column(12,style="padding-right:0;padding-left:20px",
           column(5,style="padding:0;",
                  div(style="height:80vh;overflow-y:scroll;padding:2px;font-size:12px",
                  column(12,style="padding:0;margin-bottom:15px",
                         column(6,style="padding:0",
                                div(class = "record_control",
                                  div(style="float:left",
                                      checkboxInput(
                                        inputId = ns("site_check"), value = FALSE, label = NULL
                                      ) %>% tagAppendAttributes(class = 'compact')
                                  ),
                                  div(style="float:left;width:90%",
                                      selectizeInput(
                                        inputId = ns("site"),
                                        label = "Site",
                                        choices = c(""),
                                        multiple = TRUE,
                                        options = list(maxItems = 1, placeholder = 'Select a site')
                                      ) %>% tagAppendAttributes(class = 'compact')
                                  )
                                )
                         )
                         ,
                         column(6,style="padding:0",
                                div(class = "record_control",
                                  div(style="float:left",
                                      checkboxInput(
                                        inputId = ns("subsite_check"), value = FALSE, label = NULL
                                      ) %>% tagAppendAttributes(class = 'compact')
                                  ),
                                  div(style="float:left;width:80%",
                                      selectizeInput(
                                        inputId = ns("subsite"),
                                        label = "Subsite",
                                        choices = c(""),
                                        multiple = TRUE,
                                        options = list(maxItems = 1, placeholder = 'Select a subsite (optional)')
                                      ) %>% tagAppendAttributes(class = 'compact')),
                                  div(style="float:left;width:10%;margin-top:21px",    
                                          actionButton(
                                            inputId = ns("new_subsite"),
                                            icon = icon("plus"),
                                            label = ""
                                          )
                                  )
                                )
                         ),
                         column(6,style="padding:0",
                                div(
                                  div(style="float:left",
                                      checkboxInput(
                                        inputId = ns("gridref_check"), value = FALSE, label = NULL
                                      ) %>% tagAppendAttributes(class = 'compact')
                                  ),
                                  div(style="float:left;width:90%",
                                      textInput(
                                        inputId = ns("gridref"),
                                        label = "Grid reference",
                                        value = NULL
                                      ) %>% tagAppendAttributes(class = 'compact')
                                  )
                                )
                         ),
                         column(6,style="padding:0",
                                div(
                                  div(style="float:left",
                                      checkboxInput(
                                        inputId = ns("site_record_check"), value = FALSE, label = NULL
                                      ) %>% tagAppendAttributes(class = 'compact')
                                  ),
                                  div(style="float:left;width:90%",
                                      textInput(
                                        inputId = ns("site_record"),
                                        label = "Site name as given in original record",
                                        value = NULL
                                      ) %>% tagAppendAttributes(class = 'compact')
                                  )
                                )
                         )
                         )
                  ,
                  column(12,style="padding:0",
                         div(class = "record_control",
                           div(style="float:left",
                               checkboxInput(
                                 inputId = ns("taxon_nbn_check"), value = FALSE, label = NULL
                               ) %>% tagAppendAttributes(class = 'compact')
                           ),
                           div(style="float:left;width:90%",
                               selectizeInput(
                                 inputId = ns("taxon_nbn"),
                                 label = "Taxon",
                                 choices = c(""),
                                 multiple = TRUE,
                                 options = list(maxItems = 1, placeholder = 'Select a taxon')
                               ) %>% tagAppendAttributes(class = 'compact')
                           )
                         )
                  )
                  ,
                  
                  column(12,style="padding:0",
                         column(3,style="padding:0",
                                div(
                                  div(style="float:left",
                                      checkboxInput(
                                        inputId = ns("quantity_check"), value = FALSE, label = NULL
                                      ) %>% tagAppendAttributes(class = 'compact')
                                  ),
                                  div(style="float:left;width:70%",
                                      textInput(
                                        inputId = ns("quantity"),
                                        label = "Quantity",
                                        value = NULL
                                      ) %>% tagAppendAttributes(class = 'compact')
                                  )
                                ) 
                         )
                         ,
                         column(3,style="padding:0",
                                div(
                                  div(style="float:left",
                                      checkboxInput(
                                        inputId = ns("status_check"), value = FALSE, label = NULL
                                      ) %>% tagAppendAttributes(class = 'compact')
                                  ),
                                  div(style="float:left;width:70%",
                                      selectizeInput(
                                        inputId = ns("status"),
                                        label = "Status",
                                        choices = choices_status,
                                        selected = "",
                                        multiple = TRUE,
                                        options = list(
                                          maxItems = 1,
                                          placeholder = 'None',
                                          onInitialize = I('function() { this.setValue(""); }')
                                        )
                                      ) %>% tagAppendAttributes(class = 'compact')
                                  )
                                ) 
                         )
                         ,
                         column(3,style="padding:0",
                                div(
                                  div(style="float:left",
                                      checkboxInput(
                                        inputId = ns("sex_check"), value = FALSE, label = NULL
                                      ) %>% tagAppendAttributes(class = 'compact')
                                  ),
                                  div(style="float:left;width:70%",
                                      textInput(
                                        inputId = ns("sex"),
                                        label = "Sex",
                                        value = NULL
                                      ) %>% tagAppendAttributes(class = 'compact')
                                  )
                                ) 
                         )
                         ,
                         column(3,style="padding:0",
                                div(
                                  div(style="float:left",
                                      checkboxInput(
                                        inputId = ns("stage_check"), value = FALSE, label = NULL
                                      ) %>% tagAppendAttributes(class = 'compact')
                                  ),
                                  div(style="float:left;width:70%",
                                      textInput(
                                        inputId = ns("stage"),
                                        label = "Stage",
                                        value = NULL
                                      ) %>% tagAppendAttributes(class = 'compact')
                                  )
                                ) 
                         )
                  ),
                  column(12,style="padding:0",
                         
                         column(6,style="padding:0",
                                div(
                                  div(style="float:left",
                                      checkboxInput(
                                        inputId = ns("recorder_check"), value = FALSE, label = NULL
                                      ) %>% tagAppendAttributes(class = 'compact')
                                  ),
                                  div(style="float:left;width:90%",
                                      textInput(
                                        inputId = ns("recorder"),
                                        label = "Recorder",
                                        value = NULL
                                      ) %>% tagAppendAttributes(class = 'compact')
                                  )
                                )
                         )
                         ,
                         column(6,style="padding:0",
                                div(
                                  div(style="float:left",
                                      checkboxInput(
                                        inputId = ns("determiner_check"), value = FALSE, label = NULL
                                      ) %>% tagAppendAttributes(class = 'compact')
                                  ),
                                  div(style="float:left;width:90%",
                                      textInput(
                                        inputId = ns("determiner"),
                                        label = "Determiner",
                                        value = NULL
                                      ) %>% tagAppendAttributes(class = 'compact')
                                  )
                                )
                         )
                         ,
                         column(6,style="padding:0",
                                div(
                                  div(style="float:left",
                                      checkboxInput(
                                        inputId = ns("method_check"), value = FALSE, label = NULL
                                      ) %>% tagAppendAttributes(class = 'compact')
                                  ),
                                  div(style="float:left;width:90%",
                                      textInput(
                                        inputId = ns("method"),
                                        label = "Method",
                                        value = NULL
                                      ) %>% tagAppendAttributes(class = 'compact')
                                  )
                                )
                         )
                         ,
                         column(6,style="padding:0",
                                div(
                                  div(style="float:left",
                                      checkboxInput(
                                        inputId = ns("survey_check"), value = FALSE, label = NULL
                                      ) %>% tagAppendAttributes(class = 'compact')
                                  ),
                                  div(style="float:left;width:80%",
                                      selectizeInput(
                                        inputId = ns("survey"),
                                        label = "Data source",
                                        choices = c(""),
                                        multiple = TRUE,
                                        options = list(maxItems = 1, placeholder = 'Select a data source')
                                      ) %>% tagAppendAttributes(class = 'compact')
                                  ),
                                  div(style="float:left;width:10%;margin-top:21px",    
                                      actionButton(
                                        inputId = ns("new_survey"),
                                        icon = icon("plus"),
                                        label = ""
                                      )
                                  )
                                )
                         )      
                  ),
                  
                  
                  column(12,style="padding:0",
                         # RECORD DATE
                         column(4,style="padding:0",
                                div(
                                  div(style="float:left",
                                      checkboxInput(
                                        inputId = ns("record_date_check"), value = FALSE, label = NULL
                                      ) %>% tagAppendAttributes(class = 'compact')
                                  ),
                                  div(style="float:left;width:70%",
                                      dateInput(
                                        inputId = ns("record_date"),
                                        label = "Record date",
                                        value = NA,
                                        format = "yyyy-mm-dd"
                                      ) %>% tagAppendAttributes(class = 'compact')
                                  )
                                ) 
                         ),
                         
                         column(4,style="padding:0",
                                div(
                                  div(style="float:left",
                                      checkboxInput(
                                        inputId = ns("record_year_check"), value = FALSE, label = NULL
                                      ) %>% tagAppendAttributes(class = 'compact')
                                  ),
                                  div(style="float:left;width:70%",
                                      numericInput(
                                        inputId = ns("record_year"),
                                        label = "Record year",
                                        value = NULL,
                                        min = 1600, max = 3000, step = 1
                                      ) %>% tagAppendAttributes(class = 'compact')
                                  )
                                ) 
                         ), 
                         column(4,style="padding:0",
                                div(
                                  div(style="float:left",
                                      checkboxInput(
                                        inputId = ns("record_month_check"), value = FALSE, label = NULL
                                      ) %>% tagAppendAttributes(class = 'compact')
                                  ),
                                  div(style="float:left;width:70%",
                                      numericInput(
                                        inputId = ns("record_month"),
                                        label = "Record month",
                                        value = NULL,
                                        min = 1, max = 12, step = 1
                                      ) %>% tagAppendAttributes(class = 'compact')
                                  )
                                ) 
                         ), 
                         
                         
                         # RECORD START
                         column(4,style="padding:0",
                                div(
                                  div(style="float:left",
                                      checkboxInput(
                                        inputId = ns("record_date_start_check"), value = FALSE, label = NULL
                                      ) %>% tagAppendAttributes(class = 'compact')
                                  ),
                                  div(style="float:left;width:70%",
                                      dateInput(
                                        inputId = ns("record_date_start"),
                                        label = "Start date",
                                        value = NA,
                                        format = "yyyy-mm-dd"
                                      ) %>% tagAppendAttributes(class = 'compact')
                                  )
                                ) 
                         ), 
                         
                         column(4,style="padding:0",
                                div(
                                  div(style="float:left",
                                      checkboxInput(
                                        inputId = ns("start_year_check"), value = FALSE, label = NULL
                                      ) %>% tagAppendAttributes(class = 'compact')
                                  ),
                                  div(style="float:left;width:70%",
                                      numericInput(
                                        inputId = ns("start_year"),
                                        label = "Start year",
                                        value = NULL,
                                        min = 1600, max = 3000, step = 1
                                      ) %>% tagAppendAttributes(class = 'compact')
                                  )
                                ) 
                         ),
                         column(4,style="padding:0",
                                div(
                                  div(style="float:left",
                                      checkboxInput(
                                        inputId = ns("start_month_check"), value = FALSE, label = NULL
                                      ) %>% tagAppendAttributes(class = 'compact')
                                  ),
                                  div(style="float:left;width:70%",
                                      numericInput(
                                        inputId = ns("start_month"),
                                        label = "Start month",
                                        value = NULL,
                                        min = 1, max = 12, step = 1
                                      ) %>% tagAppendAttributes(class = 'compact')
                                  )
                                ) 
                         )
                         ,
                         
                         # RECORD END
                         column(4,style="padding:0",
                                div(
                                  div(style="float:left",
                                      checkboxInput(
                                        inputId = ns("record_date_end_check"), value = FALSE, label = NULL
                                      ) %>% tagAppendAttributes(class = 'compact')
                                  ),
                                  div(style="float:left;width:70%",
                                      dateInput(
                                        inputId = ns("record_date_end"),
                                        label = "End date",
                                        value = NA,
                                        format = "yyyy-mm-dd"
                                      ) %>% tagAppendAttributes(class = 'compact')
                                  )
                                ) 
                         ),
                         column(4,style="padding:0",
                                div(
                                  div(style="float:left",
                                      checkboxInput(
                                        inputId = ns("end_year_check"), value = FALSE, label = NULL
                                      ) %>% tagAppendAttributes(class = 'compact')
                                  ),
                                  div(style="float:left;width:70%",
                                      numericInput(
                                        inputId = ns("end_year"),
                                        label = "End year",
                                        value = NULL,
                                        min = 1600, max = 3000, step = 1
                                      ) %>% tagAppendAttributes(class = 'compact')
                                  )
                                ) 
                         ),
                         column(4,style="padding:0",
                                div(
                                  div(style="float:left",
                                      checkboxInput(
                                        inputId = ns("end_month_check"), value = FALSE, label = NULL
                                      ) %>% tagAppendAttributes(class = 'compact')
                                  ),
                                  div(style="float:left;width:70%",
                                      numericInput(
                                        inputId = ns("end_month"),
                                        label = "End month",
                                        value = NULL,
                                        min = 1, max = 12, step = 1
                                      ) %>% tagAppendAttributes(class = 'compact')
                                  )
                                ) 
                         )
                  )
                  ,
                
                  column(12,style="padding:0",
                         column(12,style="padding:0",
                         div(
                           div(style="float:left",
                               checkboxInput(
                                 inputId = ns("note_check"), value = FALSE, label = NULL
                               ) %>% tagAppendAttributes(class = 'compact')
                           ),
                           div(style="float:left;width:95%",
                               textAreaInput(
                                 inputId = ns("note"),
                                 label = "Record notes",
                                 value = NULL,
                                 resize = "vertical"
                               ) %>% tagAppendAttributes(class = 'compact')
                           )
                         )
                         )
                         )
           )
                  
                    
                  ),
           
    # DT and buttons ----
           
           column(7,style="padding:5px;",
                  div(
                    style = "font-size:12px;border: 1px solid black; border-radius: 5px; padding: 10px ",
                    withSpinner(DT::DTOutput(outputId = ns("recordsTable")),type = 7)
                  ),
                  div(
                    div(id = ns("add_record_container"),
                        style = "margin-top: 10px;float:left",
                        actionButton(
                          inputId = ns("add_record"),
                          label = "Add record",
                          icon = icon("plus")
                        )
                    ),
                    div(id = ns("clear_records_container"),
                        style = "margin-top: 10px;float:left",
                        actionButton(
                          inputId = ns("clear_records"),
                          label = "Clear all records",
                          icon = icon("trash-alt")
                        )
                    )
                  ),
                  div(
                    div(id = ns("submit_record_container"),
                        style = "margin-top: 10px;float:left",
                        actionButton(
                          inputId = ns("submit_records"),
                          label = "Submit records",
                          icon = icon("upload")
                        )
                    )
                  ),
                  br(),br(),br(),
                  div(
                    div(id = ns("info"),
                        style = "margin-top: 10px;float:left",
                        actionButton(
                          inputId = ns("info"),
                          label = "Instructions",
                          icon = icon("info")
                        )
                    )
                  )
                  )
    ),
      )
    ,
    id = ns("module"),
    type = 4,
    size = 2,
    proxy.height = "100%",
    hide.ui = TRUE,
    caption = "Loading module"),
    tags$script(src ="script.js"),
    tags$script(
      HTML("$('#enterRecords-module').parent().removeClass('shiny-spinner-hidden')")
    )
  )
}

enterRecordsServer <- function(id, login, tables, tab) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ## Initialisation ----
      
      user <- login$username
      password <- login$password

      # Module initialisation ----
      isolate({
        app_tables(tables, c("sites","subsites","surveys","projects"))
        
        uksi_load(c(0,1))
      })
      
      observe({
        req(tables$sites)
        req(tables$surveys)
        req(tables$subsites)
        req(tables$projects)
        req(choices_uksi)
        req(choices_uksi_1)
        
        runjs(
          paste0(
            "$('#",id,"-module').parent().addClass('shiny-spinner-hidden');
                 $('div[data-spinner-id=\\'",id,"-module\\']').css('display','inline')"
          )
        )
      })
      
      # Keyboard shortcut for add record
      
      observe({
        if(tab == "enterRecords"){
          runjs(
            '
              var down = {};
              $(document).keydown(function(e) {
                  down[e.keyCode] = true;
              }).keyup(function(e) {
                  if (down[18] && down[65]) {
                    $("#enterRecords-add_record").click()
                      }
                      down[e.keyCode] = false;
                });
            '
          )
        }
      })
      
      #Load modals
      source("./R/modals/site_modal.R")
      source("./R/modals/survey_modal.R")

      # Update input boxes once tables loaded
      updateSelectizeInput(session,
                           "taxon_nbn",
                           choices=choices_uksi, 
                           selected = "",
                           server = TRUE,
                           options = list(
                             maxItems = 1,
                             onInitialize = I('function() { this.setValue(""); }')
                           )
      )
      
      choices_site <- reactive({
        if(isTruthy(tables$sites)){
          c <- tables$sites$id
          names(c) <- paste0(tables$sites$site, " [",tables$sites$county, "]")
        }
        else{
          c <- c("")
        }
        return(c)
      })
      
      observe({
        updateSelectizeInput(session,
                             "site",
                             choices=choices_site(), 
                             selected = "",
                             server = FALSE,
                             options = list(
                               maxItems = 1,
                               onInitialize = I('function() { this.setValue(""); }')
                             )
        )
      })
      
      choices_site_1 <- reactive({
        if(isTruthy(tables$sites)){
          c <- tables$sites$id
          names(c) <- tables$sites$site
        }
        else{
          c <- c("")
        }
        return(c)
      })
      
      choices_survey <- reactive({
        if(isTruthy(tables$surveys)){
          open <- tables$surveys[tables$surveys$status == "open",]
          c <- open$id
          names(c) <- open$survey
        }
        else{
          c <- c("")
        }
        return(c)
      })
      
      observe({
        updateSelectizeInput(session,
                             "survey",
                             choices=choices_survey(),
                             selected = "",
                             server = FALSE,
                             options = list(
                               maxItems = 1,
                               onInitialize = I('function() { this.setValue(""); }')
                               )
                             )
      })
      
      choices_subsite <- reactive({
        if(isTruthy(input$site)){
          req(tables$subsites)
          c <- tables$subsites[tables$subsites$site == input$site,c("id")]
          names(c) <- tables$subsites[tables$subsites$site == input$site,c("subsite")]
        }
        else{
          c <- c("")
        }
        return(c)
      })
      
      observeEvent(input$site,{
        if(length(choices_subsite()) > 0){
          if(isTruthy(input$recordsTable_rows_selected)){
            updateSelectizeInput(
              session,
              "subsite",
              choices=choices_subsite(),
              selected = d$data[as.numeric(input$recordsTable_rows_selected),c('subsite')],
              server = FALSE,
              options = list(
                maxItems = 1,
                onInitialize = I('function() { this.setValue(""); }')
              )
            )
          }else{
            updateSelectizeInput(
              session,
              "subsite",
              choices=choices_subsite(),
              selected = "",
              server = FALSE,
              options = list(
                maxItems = 1,
                onInitialize = I('function() { this.setValue(""); }')
              )
            )
          }
          }
        else{
          updateSelectizeInput(
            session,
            "subsite",
            choices=choices_subsite(),
            selected = "",
            server = FALSE,
            options = list(
              placeholder = 'No subsites',
              maxItems = 1,
              onInitialize = I('function() { this.setValue(""); }')
            )
          )        }

      })
      
      ## Checkboxes ----
      observeEvent(input$site_record_check,{
        if(input$site_record_check == 0){
          shinyjs::enable('site_record')
        }
        if(input$site_record_check == 1){
          shinyjs::disable('site_record')
          updateTextInput(session,'site_record',value = input$site_record)}
      })
      observeEvent(input$site_check,{
        if(input$site_check == 0){
          shinyjs::enable('site')
        }
        if(input$site_check == 1){
          shinyjs::disable('site')
          updateSelectizeInput(session,'site',selected = input$site)}
      })
      
      observeEvent(input$subsite_check,{
        if(input$subsite_check == 0){
          shinyjs::enable('subsite')
          shinyjs::enable("new_subsite")
        }
        if(input$subsite_check == 1){
          shinyjs::disable('subsite')
          shinyjs::disable("new_subsite")
          updateSelectizeInput(session,'subsite',selected = input$subsite)}
      })
      
      observeEvent(input$taxon_nbn_check,{
        if(input$taxon_nbn_check == 0){
          shinyjs::enable('taxon_nbn')
        }
        if(input$taxon_nbn_check == 1){
          shinyjs::disable('taxon_nbn')
          updateSelectizeInput(session,'taxon_nbn',selected = input$taxon_nbn)}
      })
      observeEvent(input$gridref_check,{
        if(input$gridref_check == 0){
          shinyjs::enable('gridref')
        }
        if(input$gridref_check == 1){
          shinyjs::disable('gridref')
          updateTextInput(session,'gridref',value = input$gridref)}
      })
      observeEvent(input$quantity_check,{
        if(input$quantity_check == 0){
          shinyjs::enable('quantity')
        }
        if(input$quantity_check == 1){
          shinyjs::disable('quantity')
          updateTextInput(session,'quantity',value = input$quantity)}
      })
      observeEvent(input$status_check,{
        if(input$status_check == 0){
          shinyjs::enable('status')
        }
        if(input$status_check == 1){
          shinyjs::disable('status')
          updateSelectizeInput(session,'status',selected = input$status)}
      })
      observeEvent(input$sex_check,{
        if(input$sex_check == 0){
          shinyjs::enable('sex')
        }
        if(input$sex_check == 1){
          shinyjs::disable('sex')
          updateTextInput(session,'sex',value = input$sex)}
      })
      observeEvent(input$stage_check,{
        if(input$stage_check == 0){
          shinyjs::enable('stage')
        }
        if(input$stage_check == 1){
          shinyjs::disable('stage')
          updateTextInput(session,'stage',value = input$stage)}
      })
      observeEvent(input$note_check,{
        if(input$note_check == 0){
          shinyjs::enable('note')
        }
        if(input$note_check == 1){
          shinyjs::disable('note')
          updateTextAreaInput(session,'note',value = input$note)}
      })
      observeEvent(input$record_date_check,{
        if(input$record_date_check == 0){
          shinyjs::enable('record_date')
        }
        if(input$record_date_check == 1){
          shinyjs::disable('record_date')
          updateDateInput(session,'record_date',value = input$record_date)}
      })
      observeEvent(input$record_year_check,{
        if(input$record_year_check == 0){
          shinyjs::enable('record_year')
        }
        if(input$record_year_check == 1){
          shinyjs::disable('record_year')
          updateDateInput(session,'record_year',value = input$record_year)}
      })
      observeEvent(input$record_month_check,{
        if(input$record_month_check == 0){
          shinyjs::enable('record_month')
        }
        if(input$record_month_check == 1){
          shinyjs::disable('record_month')
          updateDateInput(session,'record_month',value = input$record_month)}
      })
      observeEvent(input$record_date_start_check,{
        if(input$record_date_start_check == 0){
          shinyjs::enable('record_date_start')
        }
        if(input$record_date_start_check == 1){
          shinyjs::disable('record_date_start')
          updateDateInput(session,'record_date_start',value = input$record_date_start)}
      })
      observeEvent(input$record_date_end_check,{
        if(input$record_date_end_check == 0){
          shinyjs::enable('record_date_end')
        }
        if(input$record_date_end_check == 1){
          shinyjs::disable('record_date_end')
          updateDateInput(session,'record_date_end',value = input$record_date_end)}
      })
      observeEvent(input$recorder_check,{
        if(input$recorder_check == 0){
          shinyjs::enable('recorder')
        }
        if(input$recorder_check == 1){
          shinyjs::disable('recorder')
          updateTextInput(session,'recorder',value = input$recorder)}
      })
      observeEvent(input$determiner_check,{
        if(input$determiner_check == 0){
          shinyjs::enable('determiner')
        }
        if(input$determiner_check == 1){
          shinyjs::disable('determiner')
          updateTextInput(session,'determiner',value = input$determiner)}
      })
      observeEvent(input$method_check,{
        if(input$method_check == 0){
          shinyjs::enable('method')
        }
        if(input$method_check == 1){
          shinyjs::disable('method')
          updateTextInput(session,'method',value = input$method)}
      })
      observeEvent(input$survey_check,{
        if(input$survey_check == 0){
          shinyjs::enable('survey')
          shinyjs::enable("new_survey")
        }
        if(input$survey_check == 1){
          shinyjs::disable('survey')
          shinyjs::disable("new_survey")
          updateSelectizeInput(session,'survey',selected = input$survey)}
      })
      observeEvent(input$start_year_check,{
        if(input$start_year_check == 0){
          shinyjs::enable('start_year')
        }
        if(input$start_year_check == 1){
          shinyjs::disable('start_year')
          updateDateInput(session,'start_year',value = input$start_year)}
      })
      observeEvent(input$end_year_check,{
        if(input$end_year_check == 0){
          shinyjs::enable('end_year')
        }
        if(input$end_year_check == 1){
          shinyjs::disable('end_year')
          updateNumericInput(session,'end_year',value = input$end_year)}
      })
      observeEvent(input$start_month_check,{
        if(input$start_month_check == 0){
          shinyjs::enable('start_month')
        }
        if(input$start_month_check == 1){
          shinyjs::disable('start_month')
          updateNumericInput(session,'start_month',value = input$start_month)}
      })
      observeEvent(input$end_month_check,{
        if(input$end_month_check == 0){
          shinyjs::enable('end_month')
        }
        if(input$end_month_check == 1){
          shinyjs::disable('end_month')
          updateNumericInput(session,'end_month',value = input$end_month)}
      })
      
      ## Form validation ----
      
      iv <- InputValidator$new()
      iv$add_rule("site",sv_required())
      iv$add_rule("taxon_nbn",sv_required())
      iv$add_rule("survey",sv_required())
      iv$add_rule("gridref",function(value){
        
        v <- validate_gf(input$gridref, 
                         s = as.numeric(input$site), 
                         ss = as.numeric(input$subsite),
                         sites0 = tables$sites0,
                         subsites0 = tables$subsites0)
        
        if(v$error == 1 && isTruthy(input$gridref)){
          v$message
        }
      })
      iv$enable()
      
      gfv <- reactive({
        req(input$site)
        return(validate_gf(input$gridref, 
                           s = as.numeric(input$site), 
                           ss = as.numeric(input$subsite),
                           sites0 = tables$sites0,
                           subsites0 = tables$subsites0))
      })
      
      # Reactive to hold entered records etc. ----
      d <- reactiveValues(
        r = NA,
        mode = "new",
        del_row = NA,
        data = global_records[global_records$user == user,]
      )
      
      # Data table definition ----

      output$recordsTable <- DT::renderDT(
        {
          x <- isolate(d$data)
          
          x[,c("taxon_name","site_name","subsite_name","gridref","record_date","Buttons")]
            
          
        }
        ,
        server = TRUE,
        escape = FALSE,
        rownames = FALSE,
        filter = list(position='top'),
        selection = 'single',
        colnames =  c("Taxon","Site","Subsite","Gridref","Date",""),
        options = list(processing = TRUE,
                       dom = 'tlpi',pageLength = 100,
                       columnDefs = list(
                          list(orderable = FALSE, targets = c(5)),
                          list(targets = c(5),searchable = FALSE),
                          list(width = '10px',targets=c(5))
                       ),
                       extensions = c("FixedHeader"),
                       fixedHeader = TRUE,
                       scrollY = "55vh"
        )
      ) 
      
      #%>% formatStyle(0, target='row', backgroundColor = styleEqual(marked(),rep("red",length(marked()))))
      
      proxy <- DT::dataTableProxy("recordsTable")
      
      observe({
          x <- d$data[,c("taxon_name","site_name","subsite_name","gridref","record_date","Buttons")]
          proxy %>%
            DT::replaceData(data = x, resetPaging = FALSE, rownames = FALSE) %>%
              updateFilters(data = x)
          })
      
      #Record controls ----
      
      ## Click row in DT ----
      
      observeEvent(input$recordsTable_rows_selected,ignoreNULL = FALSE,{
        if(isTruthy(input$recordsTable_rows_selected)){
          d$mode <- "edit"
          d$r <- as.numeric(input$recordsTable_rows_selected)
          a <- d$r
  
          # Update form controls with values from selected DT row
          updateTextInput(session,'site_record', value = d$data[a,c('site_record')])
          updateSelectizeInput(session,'site',selected = d$data[a,c('site')])
          
          updateSelectizeInput(session,'subsite',selected = d$data[a,c('subsite')],choices = choices_subsite())
          
          updateSelectizeInput(session,'taxon_nbn',selected = d$data[a,c('taxon_nbn')])
          
          updateTextInput(session,'gridref', value = d$data[a,c('gridref')])
          updateTextInput(session,'quantity', value = d$data[a,c('quantity')])
          updateSelectizeInput(session,'status', selected = d$data[a,c('status')])
          updateTextInput(session,'sex', value = d$data[a,c('sex')])
          updateTextInput(session,'stage', value = d$data[a,c('stage')])
          updateTextInput(session,'note', value = d$data[a,c('note')])
          updateDateInput(session,'record_date', value = d$data[a,c('record_date')])
          
          updateDateInput(session,'record_year', value = d$data[a,c('record_year')])
          updateDateInput(session,'record_month', value = d$data[a,c('record_month')])
          
          updateDateInput(session,'record_date_start', value = d$data[a,c('record_date_start')])
          updateDateInput(session,'record_date_end', value = d$data[a,c('record_date_end')])
          updateTextInput(session,'recorder', value = d$data[a,c('recorder')])
          updateTextInput(session,'determiner', value = d$data[a,c('determiner')])
          updateTextInput(session,'method', value = d$data[a,c('method')])
          updateSelectizeInput(session,'survey',selected = d$data[a,c('survey')])
          updateNumericInput(session,'start_year', value = d$data[a,c('start_year')])
          updateNumericInput(session,'end_year', value = d$data[a,c('end_year')])
          updateNumericInput(session,'start_month', value = d$data[a,c('start_month')])
          updateNumericInput(session,'end_month', value = d$data[a,c('end_month')])
          
          # Enable all form controls
          updateCheckboxInput(session,'site_record_check',value = 0)
          updateCheckboxInput(session,'site_check',value = 0)
          updateCheckboxInput(session,'subsite_check',value = 0)
          updateCheckboxInput(session,'taxon_nbn_check',value = 0)
          updateCheckboxInput(session,'gridref_check',value = 0)
          updateCheckboxInput(session,'quantity_check',value = 0)
          updateCheckboxInput(session,'status_check',value = 0)
          updateCheckboxInput(session,'sex_check',value = 0)
          updateCheckboxInput(session,'stage_check',value = 0)
          updateCheckboxInput(session,'note_check',value = 0)
          updateCheckboxInput(session,'record_date_check',value = 0)
          updateCheckboxInput(session,'record_year_check',value = 0)
          updateCheckboxInput(session,'record_month_check',value = 0)
          updateCheckboxInput(session,'record_date_start_check',value = 0)
          updateCheckboxInput(session,'record_date_end_check',value = 0)
          updateCheckboxInput(session,'recorder_check',value = 0)
          updateCheckboxInput(session,'determiner_check',value = 0)
          updateCheckboxInput(session,'method_check',value = 0)
          updateCheckboxInput(session,'survey_check',value = 0)
          updateCheckboxInput(session,'start_year_check',value = 0)
          updateCheckboxInput(session,'end_year_check',value = 0)
          updateCheckboxInput(session,'start_month_check',value = 0)
          updateCheckboxInput(session,'end_month_check',value = 0)
        }else{
          d$mode <- "new"
          d$r <- NA
          clear_form()
          }
      })
      
      ## Add record button ----
      
      observeEvent(input$add_record,{
        
        #Validate input
        req(isTruthy(input$site))
        req(isTruthy(input$taxon_nbn))
        req(isTruthy(input$survey))
        req(gfv()$error == 0)
        
        add_record()
      })
      
      add_record <- function(){
        ss <- choices_subsite()
        
        if(d$mode == "new"){
          # Add form values to reactive. 
          # UUID generated in app so that records can be edited post-upload.
          
          if(nrow(d$data) > 0){
            a <- max(d$data$id) + 1
          }else{
            a <- 1
          }
          
          d$data[a,c("verification")] <- 0
          d$data[a,c("site_record")] <- blank(input$site_record)
          d$data[a,c("site")] <- blank(input$site)
          d$data[a,c("subsite")] <- blank(input$subsite)
          d$data[a,c("taxon_nbn")] <- blank(input$taxon_nbn)
          d$data[a,c("gridref")] <- blank(input$gridref)
          d$data[a,c("quantity")] <- blank(input$quantity)
          d$data[a,c("status")] <- blank(input$status) 
          d$data[a,c("sex")] <- blank(input$sex)
          d$data[a,c("stage")] <- blank(input$stage)
          d$data[a,c("note")] <- blank(input$note) 
          d$data[a,c("record_date")] <- input$record_date
          d$data[a,c("record_year")] <- input$record_year
          d$data[a,c("record_month")] <- input$record_month
          d$data[a,c("record_date_start")] <- input$record_date_start
          d$data[a,c("record_date_end")] <- input$record_date_end
          d$data[a,c("recorder")] <- blank(input$recorder) 
          d$data[a,c("determiner")] <- blank(input$determiner) 
          d$data[a,c("method")] <- blank(input$method) 
          d$data[a,c("survey")] <- blank(input$survey)
          d$data[a,c("start_year")] <- blank(input$start_year) 
          d$data[a,c("end_year")] <- blank(input$end_year) 
          d$data[a,c("start_month")] <- blank(input$start_month) 
          d$data[a,c("end_month")] <- blank(input$end_month)
          d$data[a,c("site_name")] <- names(choices_site_1())[which(choices_site_1() == as.numeric(input$site))]
          d$data[a,c("subsite_name")] <- blank(names(ss)[which(ss == as.numeric(input$subsite))])
          d$data[a,c("taxon_name")] <- names(choices_uksi_1)[which(choices_uksi_1 == input$taxon_nbn)]
          d$data[a,c("survey_name")] <- names(choices_survey())[which(choices_survey() == as.numeric(input$survey))]
          d$data[a,c("guid")] <- UUIDgenerate()
          d$data[a,c("id")] <- a
          d$data[a,c("Buttons")] <- del_btns(c(a),"records")
          d$data[a,c("in_db")] <- 0
          d$data[a,c("user")] <- user
        }
        if(d$mode == "edit"){
          
          #Row number being edited
          a <- d$r
          # Update datatable from form
          d$data[a,c("site_record")] <- blank(input$site_record)
          d$data[a,c("site")] <- blank(input$site)
          d$data[a,c("subsite")] <- blank(input$subsite)
          d$data[a,c("taxon_nbn")] <- blank(input$taxon_nbn)
          d$data[a,c("gridref")] <- blank(input$gridref)
          d$data[a,c("quantity")] <- blank(input$quantity)
          d$data[a,c("status")] <- blank(input$status) 
          d$data[a,c("sex")] <- blank(input$sex)
          d$data[a,c("stage")] <- blank(input$stage)
          d$data[a,c("note")] <- blank(input$note) 
          d$data[a,c("record_date")] <- input$record_date
          d$data[a,c("record_year")] <- input$record_year
          d$data[a,c("record_month")] <- input$record_month
          d$data[a,c("record_date_start")] <- input$record_date_start
          d$data[a,c("record_date_end")] <- input$record_date_end
          d$data[a,c("recorder")] <- blank(input$recorder) 
          d$data[a,c("determiner")] <- blank(input$determiner) 
          d$data[a,c("method")] <- blank(input$method) 
          d$data[a,c("survey")] <- blank(input$survey)
          d$data[a,c("start_year")] <- blank(input$start_year) 
          d$data[a,c("end_year")] <- blank(input$end_year) 
          d$data[a,c("start_month")] <- blank(input$start_month) 
          d$data[a,c("end_month")] <- blank(input$end_month)
          d$data[a,c("site_name")] <- names(choices_site_1())[which(choices_site_1() == as.numeric(input$site))]
          d$data[a,c("subsite_name")] <- blank(names(ss)[which(ss == as.numeric(input$subsite))])
          d$data[a,c("taxon_name")] <- names(choices_uksi_1)[which(choices_uksi_1 == input$taxon_nbn)]
          d$data[a,c("survey_name")] <- names(choices_survey())[which(choices_survey() == as.numeric(input$survey))]
          
          #Reset mode
          d$mode <- "new"
        }
        
        clear_form()
        
        runjs("
              $('#enterRecords-taxon_nbn-selectized').click()
              
              "
        )
      }
      
      ## Function to clear form for controls not locked ----
      clear_form <- function(){
        if(input$site_record_check == 0){
          updateTextInput(session,'site_record',value =NA)
        }
        if(input$site_check == 0){
          updateSelectizeInput(session,'site',selected ='')
        }
        if(input$subsite_check == 0){
          updateSelectizeInput(session,'subsite',selected ='')
        }
        if(input$taxon_nbn_check == 0){
          updateSelectizeInput(session,'taxon_nbn',selected ='')
        }
        if(input$gridref_check == 0){
          updateTextInput(session,'gridref',value =NA)
        }
        if(input$quantity_check == 0){
          updateTextInput(session,'quantity',value =NA)
        }
        if(input$status_check == 0){
          updateSelectizeInput(session,'status',selected =NA)
        }
        if(input$sex_check == 0){
          updateTextInput(session,'sex',value =NA)
        }
        if(input$stage_check == 0){
          updateTextInput(session,'stage',value =NA)
        }
        if(input$note_check == 0){
          updateTextInput(session,'note',value =NA)
        }
        if(input$record_date_check == 0){
          updateDateInput(session,'record_date',value =NA)
        }
        if(input$record_year_check == 0){
          updateDateInput(session,'record_year',value =NA)
        }
        if(input$record_month_check == 0){
          updateDateInput(session,'record_month',value =NA)
        }
        if(input$record_date_start_check == 0){
          updateDateInput(session,'record_date_start',value =NA)
        }
        if(input$record_date_end_check == 0){
          updateDateInput(session,'record_date_end',value =NA)
        }
        if(input$recorder_check == 0){
          updateTextInput(session,'recorder',value =NA)
        }
        if(input$determiner_check == 0){
          updateTextInput(session,'determiner',value =NA)
        }
        if(input$method_check == 0){
          updateTextInput(session,'method',value =NA)
        }
        if(input$survey_check == 0){
          updateSelectizeInput(session,'survey',selected ='')
        }
        if(input$start_year_check == 0){
          updateNumericInput(session,'start_year',value =NA)
        }
        if(input$end_year_check == 0){
          updateNumericInput(session,'end_year',value =NA)
        }
        if(input$start_month_check == 0){
          updateNumericInput(session,'start_month',value =NA)
        }
        if(input$end_month_check == 0){
          updateNumericInput(session,'end_month',value =NA)
        }
      }
      
      ## DT row buttons ----
      observeEvent(input$current_id,{
        if(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "records_del")){  
          d$del_row <- as.numeric(substr(input$current_id,1+nchar("records_del_"),nchar(input$current_id)))
          
          if(d$data[which(d$data$id == d$del_row),c("in_db")] == 1){
            showModal(mark_record_modal())
            }
          else{
            showModal(delete_record_modal())
            }
        }
        if(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "records_val")){
          showModal(unmark_record_modal())
        }
          })
      
      ## Delete row modals
      
      delete_record_modal <- function() {
          ns <- session$ns
          modalDialog(
            div(h4("Do you want to delete this record?"),
                actionButton(inputId= ns("delete_record_yes"),label="Yes"),
                actionButton(inputId= ns("delete_record_yes_no"),label="No")
                ,style="width:100%; text-align:center"),
            , footer=NULL,size="s",easyClose=FALSE,fade=TRUE)
      }
      
      observeEvent(input$delete_record_yes,{
        d$data <- d$data[-c(which(d$data$id == d$del_row)),]
        
        clear_form()
        d$mode <- "new"
        removeModal()
      })
      
      ## Mark row modals
      
      mark_record_modal <- function() {
        ns <- session$ns
        modalDialog(
          div(h4("Do you want to mark this record for deletion?"),
              p("This record has already been submitted and cannot be deleted. 
                Click 'yes' to mark it for deletion from the database by an admin."),
              actionButton(inputId= ns("mark_record_yes"),label="Yes"),
              actionButton(inputId= ns("close_modal"),label="No")
              ,style="width:100%; text-align:center"),
          , footer=NULL,size="s",easyClose=FALSE,fade=TRUE)
      }
      
      observeEvent(input$mark_record_yes,{
        d$data[which(d$data$id == d$del_row),c("verification")] <- -1
        d$data[which(d$data$id == d$del_row),c("Buttons")] <- val_btns(d$del_row, "records")
        
        clear_form()
        d$mode <- "new"
        removeModal()
      })
      
      ## Unmark row modals
      
      unmark_record_modal <- function() {
        ns <- session$ns
        modalDialog(
          div(h4("Do you want to unmark this record?"),
              p("Change record to valid?"),
              actionButton(inputId= ns("unmark_record_yes"),label="Yes"),
              actionButton(inputId= ns("close_modal"),label="No")
              ,style="width:100%; text-align:center"),
          , footer=NULL,size="s",easyClose=FALSE,fade=TRUE)
      }
      
      observeEvent(input$unmark_record_yes,{
        d$data[which(d$data$id == d$del_row),c("verification")] <- 0
        d$data[which(d$data$id == d$del_row),c("Buttons")] <- del_btns(d$del_row, "records")
        
        clear_form()
        d$mode <- "new"
        removeModal()
      })
      
      ## Close modal action button
      
      observeEvent(input$close_modal,{
        removeModal()
      })
      
      ## Clear records ----
      observeEvent(input$clear_records,{
        if(nrow(d$data) > 0 && isTruthy(d$data)){
          # Show warning modal
          showModal(delete_all_records_modal())
          }
        })
      
      delete_all_records_modal <- function() {
        ns <- session$ns
        modalDialog(
          div(
              p("Do you want to clear all records?"),
              actionButton(inputId= ns("delete_all_records_yes"),label="Yes"),
              actionButton(inputId= ns("close_modal"),label="No")
              ,
              style="width:100%; text-align:center"),
          , footer=NULL,size="s",easyClose=FALSE,fade=TRUE)
      }
      
      observeEvent(input$delete_all_records_yes,{
        d$data <- d$data[0,]
        d$mode <- "new"
        removeModal()
        clear_form()
      })

      ## Submit records ----
      observeEvent(input$submit_records,{
        if(nrow(d$data) > 0 && isTruthy(d$data)){
          # Show warning modal
          showModal(submit_records_modal())
        }
      })
      
      submit_records_modal <- function() {
        ns <- session$ns
        modalDialog(
          div(
              h4("Submit records to database?"),
              p("Records can be edited after submission and re-submitted."),
              actionButton(inputId= ns("submit_records_yes"),label="Yes"),
              actionButton(inputId= ns("close_modal"),label="No")
              ,style="width:100%; text-align:center")
          , footer=NULL,size="s",easyClose=FALSE,fade=TRUE)
      }
      
      observeEvent(input$submit_records_yes,{
        future_promise({
          showModal(
            modalDialog(
              div(style="text-align:left;width:60%",
                  tags$h4("Uploading",class="loading"),
              )
              ,footer=NULL,size="s",easyClose=FALSE,fade=TRUE
            )
          )
          con <- fenDb0(user,password)
          insert <- pgWriteGeom(con,
                                name = c("records","records"),
                                data.obj = d$data[,1:31],
                                partial.match = TRUE,
                                overwrite = FALSE,
                                upsert.using = "guid"
                                )

          dbDisconnect(con)
          return(insert)
          })%...>% (function(insert) {
            
            if(insert){
              d$data$in_db <- 1
              showModal(
                modalDialog(
                  div(style="text-align:center",
                      tags$h4("Success!")
                  )
                  ,footer=NULL,size="s",easyClose=TRUE,fade=TRUE
                )
              )
            }else{
              showModal(
                modalDialog(
                  div(style="text-align:center",
                      tags$h4("Upload failed")
                  )
                  ,footer=NULL,size="s",easyClose=TRUE,fade=TRUE
                )
              )
            }
          })
        })
      
      # Save record table in global object ----
      
      observe({
        x <- d$data
        if(nrow(x) == 0){
            global_records <<- global_records[-c(which(global_records$user == user)),]
          }
        else{
            global_records <<- global_records[-c(which(global_records$user == user)),]
            global_records <<- rbind(global_records,x)
          }
      })
      # Info modal ----
      
      observeEvent(input$info,{
        showModal(info())
      })
      
      info <- function(){
        ns <- session$id
        modalDialog(
          div(h4("Instructions"),
              p("Use this page to enter biological records. Records entered are held in a temporary table, which is then uploaded to the database."),
              p("To enter a record:"),
              tags$ol(
                tags$li(HTML("Fill out the fields in the form. <b>The site, taxon and data source must be chosen</b>.")),
                tags$li(HTML("The form will <b>validate the grid reference</b> if given. It must  be in the right format (lowercase and spaces are okay), and if a 
                        site or subsite are chosen, then the form will also check that the grid reference is within 10 metres of these areas.")),
                tags$li(HTML("Once the form is filled out correctly, <b>click the 'add record' button</b> and it will appear in the table on the right and the form will be reset. 
                             The keyboard shortcut 'ALT+A' can also be used.")),
                tags$li(HTML("Continue entering records. <b>To go back and edit a record</b>, simply click the row in the table to repopulate the form, update the values and press 'add record' again to update the record.")),
                tags$li(HTML("Once happy with the records entered, <b>click the 'Submit records' button</b>, and all records will be uploaded to the database. Once records are submitted you can continue to edit and add to them."))
              ),
              p(HTML("If adding several records with the same values (such as site), click the checkbox next to the form field to <b>lock the field</b> and set its default value.")),
              HTML("<p>To <b>delete a record</b> from the table, click the 'delete' icon at the end of the row.
                   If a record has already been submitted then it cannot be deleted - it can only be marked as needing deleting from the database by an admin. 
                   This can be undone using the 'mark as valid' button that appears in the table row.</p>"),
              p("To start over, click the 'clear all records' button (you cannot get them back)."),
              p("Finally, if you close the app then it will reload the records you were last working on (unless they were cleared).")
              

              ,style="width:100%;padding:20px"),
          , footer=NULL,size="m",easyClose=TRUE,fade=TRUE)
      }
      
      # Add new subsite ----
      
      observeEvent(input$new_subsite,{
        subsite_modal_dialog(session, d = NULL, mode = "add")
        updateSelectizeInput(session,
                             "site_modal",
                             choices=choices_site(), 
                             selected = "",
                             server = FALSE,
                             options = list(
                               maxItems = 1,
                               onInitialize = I('function() { this.setValue(""); }')
                               )
                             )
        
      # Modal validation
        iv2 <- InputValidator$new()
        iv2$add_rule("site_modal",sv_required())
        iv2$add_rule("subsite_modal",sv_required())
        iv2$add_rule("gridref_modal",function(value){
          v <- validate_gf(input$gridref_modal, 
                           s = as.numeric(input$site_modal), 
                           ss = NULL,
                           sites0 = tables$sites0,
                           subsites0 = NULL)
          if(v$error == 1 && isTruthy(input$gridref_modal)){
            v$message
          }
        })
        iv2$add_rule("subsite_modal",function(value){
          if(input$subsite_modal %in% tables$subsites[tables$subsites$site == as.numeric(input$site_modal),"subsite"]){
            return("Subsite already exists")
          }
        })
        iv2$enable()
      
        })
      
      observe({
        if(
          isTruthy(input$site_modal) && isTruthy(input$subsite_modal) && 
          !(input$subsite_modal %in% tables$subsites[tables$subsites$site == as.numeric(input$site_modal),"subsite"]) &&
          validate_gf(input$gridref_modal, 
                      s = as.numeric(input$site_modal), 
                      ss = NULL,
                      sites0 = tables$sites0,
                      subsites0 = NULL)$error == 0
        ){
          shinyjs::enable("final_edit_subsite")
        }else{
          shinyjs::disable("final_edit_subsite")
        }
      })
      
      observeEvent(input$final_edit_subsite,{
        # submit new subsite
        req(isTruthy(input$site_modal) && isTruthy(input$subsite_modal) && 
              !(input$subsite_modal %in% tables$subsites[tables$subsites$site == as.numeric(input$site_modal),"subsite"]) &&
              validate_gf(input$gridref_modal, 
                          s = as.numeric(input$site_modal), 
                          ss = NULL,
                          sites0 = tables$sites0,
                          subsites0 = NULL)$error == 0)
        
        future_promise({
          con0 <- fenDb0(user,password)
          q <- paste0("INSERT INTO spatial.fen_subsites (site,subsite,gridref,vc,note) VALUES (",
                      null_num_val(input$site_modal),",",
                      null_text_val(con0,input$subsite_modal),",",
                      null_text_val(con0,input$gridref_modal),",",
                      null_text_val(con0,input$vc_modal),",",
                      null_text_val(con0,input$note_modal)
                      ,") 
                      RETURNING id")
          insert <- dbGetQuery(con0,q)
          dbDisconnect(con0)
          return(insert)
        }
        )%...>% (function(i){
          if(isTruthy(i)){
            # add row to tables$subsites
            
            row <- data.frame("id" = c(i),
                              "site" = c(input$site_modal),
                              "site_name" = names(choices_site())[which(choices_site() == input$site_modal)],
                              "subsite" = input$subsite_modal
                              )
            geom <- st_sfc(NA)
            tables$subsites0 <- rbind(tables$subsites0,st_sf(row, geom,crs=st_crs(27700)))
            tables$subsites <- rbind(tables$subsites,row)

            updateSelectizeInput(session,"subsite",choices = choices_subsite(),selected = i)
            
            showModal(
              modalDialog(
                div(style="width:100%;text-align:center",
                tags$h4("New subsite saved")
                ),
                ,footer=NULL,size="s",easyClose=TRUE,fade=TRUE
              )
            )
          }else{
            showModal(
              modalDialog(
                    tags$h4("Error - cannot add new subsite"),
                ,footer=NULL,size="s",easyClose=TRUE,fade=TRUE
              )
              )
          }
          
        })
      })
      
      # Add new survey ----
      
      observeEvent(input$new_survey,{
        survey_modal_dialog(session, d = NULL, mode = "add", qual = "_0")
        
        p <- tables$projects[order(tables$projects$project),]
        choices_p <- p[,"id"]
        names(choices_p) <- p[,"project"]
        
        updateSelectizeInput(session, "status_0", selected="open")
        
        updateSelectizeInput(session, "survey_type_0", choices = choices_st)
        updateSelectizeInput(session, "project_0", choices = choices_p)
        updateSelectizeInput(session, "sharing_0", choices = choices_sh)
        
        shinyjs::hide("created_user_0")
        shinyjs::hide("created_date_0")
        shinyjs::hide("last_edited_user_0")
        shinyjs::hide("last_edited_date_0")
        
        shinyjs::disable("status_0")
        
        shinyjs::enable("survey_0")
        shinyjs::enable("survey_type_0")
        shinyjs::enable("start_date_0")
        shinyjs::enable("end_date_0")
        shinyjs::enable("start_year_0")
        shinyjs::enable("end_year_0")
        shinyjs::enable("source_0")
        shinyjs::enable("project_0")
        shinyjs::enable("sharing_0")
        shinyjs::enable("copyright_0")
        shinyjs::enable("description_0")
        shinyjs::enable("url_0")
        
        # Modal validation
        iv2 <- InputValidator$new()
        iv2$add_rule("survey_0",function(value){
          if(isTruthy(input$survey_0) && input$survey_0 %in% tables$surveys[,c("survey")]){
            return("Data source already exists")
          }
        })
        iv2$add_rule("survey_0",sv_required())
        iv2$add_rule("survey_type_0",sv_required())
        iv2$add_rule("project_0",sv_required())
        iv2$add_rule("sharing_0",sv_required())
        iv2$enable()
        
        shinyjs::disable("final_edit_0")
        
        observe({
          if(isTruthy(input$survey_type_0) &&
             isTruthy(input$project_0) &&
             isTruthy(input$sharing_0) &&
             isTruthy(input$survey_0) && !(input$survey_0 %in% tables$surveys[,c("survey")])
          ){
            shinyjs::enable("final_edit_0")
          }
          else{
            shinyjs::disable("final_edit_0")
          }
        })
      })
      
      # Submit new survey
      
      observeEvent(input$final_edit_0,{
        req(isTruthy(input$survey_type_0))
        req(isTruthy(input$project_0))
        req(isTruthy(input$sharing_0))
        req(isTruthy(input$survey_0) && !(input$survey_0 %in% tables$surveys[,c("survey")]))

        future_promise({
          con0 <- fenDb0(user,password)
          q <- paste0(
            "INSERT INTO records.surveys
            (survey, survey_type,start_date,end_date,start_year,end_year,source,project,sharing,copyright,description,url)
            VALUES
            ('",
            input$survey_0 ,"',",
            input$survey_type_0,",",
            null_date_val(input$start_date_0),",",
            null_date_val(input$end_date_0),",",
            null_num_val(input$start_year_0),",",
            null_num_val(input$end_year_0),",",
            null_text_val(con0, input$source_0),",",
            input$project_0,",",
            input$sharing_0,",",
            null_text_val(con0, input$copyright_0),",",
            null_text_val(con0, input$description_0),",",
            null_text_val(con0, input$url_0)
            ,") RETURNING id, created_user, created_date"
          )
          insert <- dbGetQuery(con0,q)
          dbDisconnect(con0)
          return(insert)
        })%...>%(function(i){
          if(isTruthy(i)){
            row <- c(
              as.numeric(i$id),
              input$survey_0,
              as.numeric(input$survey_type_0),
              ifelse(isTruthy(input$start_date_0),as.Date(input$start_date_0),NA),
              ifelse(isTruthy(input$end_date_0),as.Date(input$end_date_0),NA),
              ifelse(isTruthy(input$start_year_0),as.numeric(input$start_year_0),NA),
              ifelse(isTruthy(input$end_year_0),as.numeric(input$end_year_0),NA),
              input$source_0,
              as.numeric(input$project_0),
              as.numeric(input$sharing_0),
              input$copyright_0,
              input$description_0,
              input$url_0,
              i$created_user,
              NA,
              NA,
              NA,
              "open",
              st[st$code == as.numeric(input$survey_type_0),c("description")],
              sh[sh$code == as.numeric(input$sharing_0),c("description")],
              tables$projects[tables$projects$id == as.numeric(input$project_0),c("project")]
            )
            
            tables$surveys[nrow(tables$surveys)+1,] <- row
            tables$surveys[nrow(tables$surveys),"created_date"] <- i$created_date # Including this in previous line causes error with as.Posixct.character
            
            showModal(
              modalDialog(
                div(style="width:100%;text-align:center",
                    tags$h4("New data source saved"),
                )
                ,footer=NULL,size="s",easyClose=TRUE,fade=TRUE
              )
            )
          }else{
            showModal(
              modalDialog(
                tags$h4("Error - cannot add new data source"),
                ,footer=NULL,size="s",easyClose=TRUE,fade=TRUE
              )
            )
          }
          
        })
      })
      
      
      
      # Add new project ----
      
      # reactive to hold values in survey modal, in case new project modal is launched
      input_survey <- reactiveValues(
        survey = NA,
        survey_type = NA,
        start_date = NA,
        end_date = NA,
        start_year = NA,
        end_year = NA,
        source = NA,
        project = NA,
        sharing = NA,
        copyright = NA,
        description = NA,
        url = NA
      )
      
      ##New project controls ----

      observeEvent(input$new_project,{
        input_survey$survey <- ifelse(isTruthy(input$survey_0),input$survey_0,NA)
        input_survey$survey_type<-ifelse(isTruthy(input$survey_type_0),input$survey_type_0,NA)
        input_survey$start_date<-ifelse(isTruthy(input$start_date_0),input$start_date_0,NA)
        input_survey$end_date<-ifelse(isTruthy(input$end_date_0),input$end_date_0,NA)
        input_survey$start_year<-ifelse(isTruthy(input$start_year_0),input$start_year_0,NA)
        input_survey$end_year<-ifelse(isTruthy(input$end_year_0),input$end_year_0,NA)
        input_survey$source<-ifelse(isTruthy(input$source_0),input$source_0,NA)
        input_survey$sharing<-ifelse(isTruthy(input$sharing_0),input$sharing_0,NA)
        input_survey$copyright<-ifelse(isTruthy(input$copyright_0),input$copyright_0,NA)
        input_survey$description<-ifelse(isTruthy(input$description_0),input$description_0,NA)
        input_survey$url<-ifelse(isTruthy(input$url_0),input$url_0,NA)
        
        project_modal_dialog(session)
        
        iv3 <- InputValidator$new()
        iv3$add_rule("project_new",function(value){
          if(input$project_new %in% tables$projects$project){
            return("Project already exists")
          }
        })
        iv3$add_rule("project_new",sv_required())
        iv3$enable()
      })
      
      observe({
        if(isTruthy(input$project_new) && !(input$project_new %in% tables$projects$project)){
          shinyjs::enable("add_new_project")
        }else{
          shinyjs::disable("add_new_project")
        }
      })
      
      ###Submit new project action button
      observeEvent(input$add_new_project,{
        #Validate form
        req(isTruthy(input$project_new) && !(input$project_new %in% tables$projects$project))
        
        #Insert new project and return new id for passing back to survey form
        future_promise({
          con0 <- fenDb0(user,password)
          insert <- dbGetQuery(con0, paste0("INSERT INTO records.projects (project) VALUES (",null_text_val(con0,input$project_new),
                                                ") RETURNING id, project, created_user, last_edited_user,created_date,last_edited_date"))
          dbDisconnect(con0)
          return(insert)
        }) %...>%(function(i){
          input_survey$project <- i$id
          #Update module project reactive and choices
          tables$projects[nrow(tables$projects)+1,] <- i[1,]
          removeModal()
          
          survey_modal_dialog(session, 
                              c(
                                input_survey$survey,
                                input_survey$survey_type,
                                input_survey$start_date,
                                input_survey$end_date,
                                input_survey$start_year,
                                input_survey$end_year,
                                input_survey$source,
                                input_survey$project,
                                input_survey$sharing,
                                input_survey$copyright,
                                input_survey$description,
                                input_survey$url,
                                "",
                                "",
                                NA,
                                NA
                              )
                              , mode = "add"
                              , qual = "_0"
          )
          
          choices_p <- tables$projects$id
          names(choices_p) <- tables$projects$project
          
          updateSelectizeInput(session, "survey_type_0", choices = choices_st, selected = input_survey$survey_type)
          updateSelectizeInput(session, "project_0", choices = choices_p, selected = input_survey$project)
          updateSelectizeInput(session, "sharing_0", choices = choices_sh, selected = input_survey$sharing)
          
          shinyjs::hide("created_user_0")
          shinyjs::hide("created_date_0")
          shinyjs::hide("last_edited_user_0")
          shinyjs::hide("last_edited_date_0")
          
          shinyjs::enable("survey_0")
          shinyjs::enable("survey_type_0")
          shinyjs::enable("start_date_0")
          shinyjs::enable("end_date_0")
          shinyjs::enable("start_year_0")
          shinyjs::enable("end_year_0")
          shinyjs::enable("source_0")
          shinyjs::enable("project_0")
          shinyjs::enable("sharing_0")
          shinyjs::enable("copyright_0")
          shinyjs::enable("description_0")
          shinyjs::enable("url_0")
        })
      })
    }
  )
}