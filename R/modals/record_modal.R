record_modal_dialog <- function(session, d, edit,c1,c2,c3,c4) {
  ns <- session$ns
  if (edit) {
    x <- "Submit edits"
  } else {
    x <- ""
  }
  shiny::modalDialog(
    div(style="padding:10px",
    tabsetPanel(
      tabPanel("Record",
               #Record fields ----
               div(
                   column(12,style="padding:0;margin-bottom:15px",
                          column(6,style="padding:0",
                                 div(style="float:left;width:90%",
                                     selectizeInput(
                                       inputId = ns("modal_site"),
                                       label = "Site",
                                       selected = d[3],
                                       choices = c1,
                                       multiple = TRUE,
                                       options = list(maxItems = 1, placeholder = 'Select a site')
                                     ) %>% tagAppendAttributes(class = 'compact')
                                 )
                          )
                          ,
                          column(6,style="padding:0",
                                 div(style="float:left;width:90%",
                                     selectizeInput(
                                       inputId = ns("modal_subsite"),
                                       label = "Subsite",
                                       selected = d[4],
                                       choices = c2,
                                       multiple = TRUE,
                                       options = list(maxItems = 1, placeholder = 'Select a subsite (optional)')
                                     ) %>% tagAppendAttributes(class = 'compact')
                                 )
                          ),
                          column(6,style="padding:0",
                                 div(style="float:left;width:90%",
                                     textInput(
                                       inputId = ns("modal_gridref"),
                                       label = "Grid reference",
                                       value = d[5]
                                     ) %>% tagAppendAttributes(class = 'compact')
                                 )
                          ),
                          column(6,style="padding:0",
                                 div(
                                   div(style="float:left;width:90%",
                                       textInput(
                                         inputId = ns("modal_site_record"),
                                         label = "Site name as given in original record",
                                         value = null(d[2])
                                       ) %>% tagAppendAttributes(class = 'compact')
                                   )
                                 )
                          )
                   )
                   ,
                   column(12,style="padding:0",
                          div(class = "record_control",
                              div(style="float:left;width:90%",
                                  selectizeInput(
                                    inputId = ns("modal_taxon_nbn"),
                                    label = "Taxon",
                                    choices = c(""),
                                    multiple = TRUE,
                                    options = list(maxItems = 1, placeholder = 'Select a taxon'),
                                    
                                  ) %>% tagAppendAttributes(class = 'compact')
                              )
                          )
                   )
                   ,
                   
                   column(12,style="padding:0",
                          column(3,style="padding:0",
                                 div(
                                   div(style="float:left;width:90%",
                                       textInput(
                                         inputId = ns("modal_quantity"),
                                         label = "Quantity",
                                         value = null(d[7])
                                       ) %>% tagAppendAttributes(class = 'compact')
                                   )
                                 ) 
                          )
                          ,
                          column(3,style="padding:0",
                                 div(
                                   div(style="float:left;width:90%",
                                       textInput(
                                         inputId = ns("modal_status"),
                                         label = "Status",
                                         value = null(d[8])
                                       ) %>% tagAppendAttributes(class = 'compact')
                                   )
                                 ) 
                          )
                          ,
                          column(3,style="padding:0",
                                 div(
                                   div(style="float:left;width:90%",
                                       textInput(
                                         inputId = ns("modal_sex"),
                                         label = "Sex",
                                         value = null(d[9])
                                       ) %>% tagAppendAttributes(class = 'compact')
                                   )
                                 ) 
                          )
                          ,
                          column(3,style="padding:0",
                                 div(
                                   div(style="float:left;width:90%",
                                       textInput(
                                         inputId = ns("modal_stage"),
                                         label = "Stage",
                                         value = null(d[10])
                                       ) %>% tagAppendAttributes(class = 'compact')
                                   )
                                 ) 
                          )
                   ),
                   column(12,style="padding:0",
                          
                          column(6,style="padding:0",
                                 div(
                                   div(style="float:left;width:90%",
                                       textInput(
                                         inputId = ns("modal_recorder"),
                                         label = "Recorder",
                                         value = null(d[18])
                                       ) %>% tagAppendAttributes(class = 'compact')
                                   )
                                 )
                          )
                          ,
                          column(6,style="padding:0",
                                 div(
                                   div(style="float:left;width:90%",
                                       textInput(
                                         inputId = ns("modal_determiner"),
                                         label = "Determiner",
                                         value = null(d[19])
                                       ) %>% tagAppendAttributes(class = 'compact')
                                   )
                                 )
                          )
                          ,
                          column(6,style="padding:0",
                                 div(
                                   div(style="float:left;width:90%",
                                       textInput(
                                         inputId = ns("modal_method"),
                                         label = "Method",
                                         value = null(d[20])
                                       ) %>% tagAppendAttributes(class = 'compact')
                                   )
                                 )
                          )
                          ,
                          column(6,style="padding:0",
                                 div(
                                   div(style="float:left;width:90%",
                                       selectizeInput(
                                         inputId = ns("modal_survey"),
                                         label = "Data source",
                                         choices = c4,
                                         selected = d[21],
                                         multiple = TRUE,
                                         options = list(maxItems = 1, placeholder = 'Select a data source')
                                       ) %>% tagAppendAttributes(class = 'compact')
                                   )
                                 )
                          )      
                   ),
                   
                   
                   column(12,style="padding:0",
                          column(4,style="padding:0",
                                 div(
                                   div(style="float:left;width:90%",
                                       dateInput(
                                         inputId = ns("modal_record_date"),
                                         label = "Start date",
                                         value = format(d[12],format = "%Y-%m-%d"),
                                         format = "yyyy-mm-dd"
                                       ) %>% tagAppendAttributes(class = 'compact')
                                   )
                                 ) 
                          ),
                          column(4,style="padding:0",
                                 div(
                                   div(style="float:left;width:90%",
                                       numericInput(
                                         inputId = ns("modal_start_year"),
                                         label = "Start year",
                                         value = d[14],
                                         min = 1600, max = 3000, step = 1
                                       ) %>% tagAppendAttributes(class = 'compact')
                                   )
                                 ) 
                          ),
                          column(4,style="padding:0",
                                 div(
                                   div(style="float:left;width:90%",
                                       numericInput(
                                         inputId = ns("modal_start_month"),
                                         label = "Start month",
                                         value = null(d[16]),
                                         min = 1, max = 12, step = 1
                                       ) %>% tagAppendAttributes(class = 'compact')
                                   )
                                 ) 
                          )
                          ,
                          
                          
                          column(4,style="padding:0",
                                 div(
                                   div(style="float:left;width:90%",
                                       dateInput(
                                         inputId = ns("modal_record_date_end"),
                                         label = "End date",
                                         value = format(d[13],format = "%Y-%m-%d"),
                                         format = "yyyy-mm-dd"
                                       ) %>% tagAppendAttributes(class = 'compact')
                                   )
                                 ) 
                          ),
                          column(4,style="padding:0",
                                 div(
                                   div(style="float:left;width:90%",
                                       numericInput(
                                         inputId = ns("modal_end_year"),
                                         label = "End year",
                                         value = null(d[15]),
                                         min = 1600, max = 3000, step = 1
                                       ) %>% tagAppendAttributes(class = 'compact')
                                   )
                                 ) 
                          ),
                          column(4,style="padding:0",
                                 div(style="float:left;width:90%",
                                     numericInput(
                                       inputId = ns("modal_end_month"),
                                       label = "End month",
                                       value = null(d[17]),
                                       min = 1, max = 12, step = 1
                                     ) %>% tagAppendAttributes(class = 'compact')
                                 )
                          )
                   )
                   ,
                   column(12,style="padding:0",
                          div(style="float:left;width:100%",
                              textAreaInput(
                                inputId = ns("modal_note"),
                                label = "Record notes",
                                value = ifelse(!is.na(d[11]),d[11],""),
                                resize = "vertical"
                              ) %>% tagAppendAttributes(class = 'compact')
                          )
                   )
               )
               ), # End of tab panel
      
      # Record verification tab ----
      
      tabPanel("Verification",
               column(12,style="padding:0",
                      column(4,
                             div(style="float:left;width:90%",
                          selectizeInput(
                            inputId = ns("modal_verification"),
                            label = "Verification state",
                            choices = choices_verification,
                            selected = d[27],
                            multiple = TRUE,
                            options = list(maxItems = 1, placeholder = 'No verification state')
                          ) %>% tagAppendAttributes(class = 'compact')
                          )
                          ),
                      column(4,
                             div(style="float:left;width:90%",
                                 textInput(
                                   inputId = ns("modal_verification_user"),
                                   label = "Verification last updated by",
                                   value = d[28]
                                 ) %>% tagAppendAttributes(class = 'compact')
                             )
                      ),
                      column(4,
                             div(style="float:left;width:90%",
                                 textInput(
                                   inputId = ns("modal_verification_date"),
                                   label = "Verification last updated",
                                   value = format(d[29],format = "%Y-%m-%d %H:%M:%S")
                                 ) %>% tagAppendAttributes(class = 'compact')
                             )
                      ),
                      column(12,
                             div(style="float:left;width:95%",
                                 textAreaInput(
                                   inputId = ns("modal_verification_note"),
                                   label = "Verification notes",
                                   value = ifelse(!is.na(d[30]),d[30],""),
                                   resize = "vertical"
                                 ) %>% tagAppendAttributes(class = 'compact')
                             )
                      )
               )
               
               ), # End of tabPanel
      
      # Record history tab ----
      
      tabPanel("Admin",
               column(12,
                      column(12,
                             div(style="float:left;width:90%",
                                 textInput(
                                   inputId = ns("modal_guid"),
                                   label = "Record guid",
                                   value = d[26]
                                 ) %>% tagAppendAttributes(class = 'compact')
                             )
                             ),
                      column(6,
                             div(style="float:left;width:90%",
                                 textInput(
                                   inputId = ns("modal_created_user"),
                                   label = "Record created by",
                                   value = d[22]
                                 ) %>% tagAppendAttributes(class = 'compact')
                             )
                             ),
                      column(6,
                             div(style="float:left;width:90%",
                                 textInput(
                                   inputId = ns("modal_created_date"),
                                   label = "Date record created",
                                   value = format(d[23],format = "%Y-%m-%d %H:%M:%S")
                                 ) %>% tagAppendAttributes(class = 'compact')
                             )
                      ),
                      column(6,
                             div(style="float:left;width:90%",
                                 textInput(
                                   inputId = ns("modal_last_edited_user"),
                                   label = "Record last edited by",
                                   value = d[24]
                                 ) %>% tagAppendAttributes(class = 'compact')
                             )
                      ),
                      column(6,
                             div(style="float:left;width:90%",
                                 textInput(
                                   inputId = ns("modal_last_edited_date"),
                                   label = "Date record last edited",
                                   value = format(d[25],format = "%Y-%m-%d %H:%M:%S")
                                 ) %>% tagAppendAttributes(class = 'compact')
                             ) 
                      )
                      )
               
               ) #End of tabPanel
    )
    )

    # Modal footer ----
    ,
    size = "l",
    easyClose = TRUE,
    footer = div(style="margin-top:10px",
      class = "pull-right container",
      shiny::actionButton(
        inputId = ns("final_edit"),
        label = x,
        icon = shiny::icon("edit"),
      )
    )
  ) %>% shiny::showModal()
  
  ## Modal field initialise ----
  
  updateSelectizeInput(session,
                       "modal_taxon_nbn",
                       choices = c3, 
                       selected = d[6],
                       server = TRUE,
                       options = list(
                         onInitialize = I('function() { this.setValue(""); }')
                       )
  )
  }