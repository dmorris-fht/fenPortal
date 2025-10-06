plot_modal <- function(session,d,edit,c) {
  ns <- session$ns
  shiny::modalDialog(
    title = "Plot information",
    column(12,style="padding:0;margin-bottom:15px",
           column(6,style="padding:0",
                  div(style="float:left;width:90%",
                      selectizeInput(
                        inputId = ns("site"),
                        label = "Site",
                        selected = d[1],
                        choices = c,
                        multiple = TRUE,
                        options = list(maxItems = 1, placeholder = 'Select a site')
                      )
                  )
           ),
           column(6,style="padding:0",
                  div(style="float:left;width:90%",
                      selectizeInput(
                        inputId = ns("subsite"),
                        label = "Subsite",
                        selected = d[2],
                        choices = c(""),
                        multiple = TRUE,
                        options = list(maxItems = 1, placeholder = 'Select a subsite (optional)')
                      )
                  )
           ),
           column(6,style="padding:0",
                  div(style="float:left;width:90%",
                      textInput(
                        inputId = ns("plot"),
                        label = "Plot name",
                        value = d[3],
                        placeholder = "Name of plot, unique for site"
                      )
                  )
           ),
           column(6,style="padding:0",
                  div(style="float:left;width:90%",
                      textInput(
                        inputId = ns("gridref"),
                        label = "Grid reference",
                        value = d[4],
                        placeholder = "Ten-figure grid reference"
                      )
                  )
           ),
           column(6,style="padding:0",
                  div(style="float:left;width:90%",
                      selectInput(
                        inputId = ns("type"),
                        label = "Plot type",
                        selected = d[5], # FIX THIS
                        choices = c("sample","monitoring")
                      )
                  )
           ),
           column(6,style="padding:0",
                  div(style="float:left;width:90%",
                      textInput(
                        inputId = ns("group"),
                        label = "Group / transect name",
                        value = d[6],
                        placeholder = "If plot on a transect"
                      )
                  )
           ),
           column(6,style="padding:0",
                  div(style="float:left;width:90%",
                      textInput(
                        inputId = ns("transect_side"),
                        label = "Transect side",
                        value = d[7],
                        placeholder = "E.g. north"
                      )
                  )
           ),
           column(6,style="padding:0",
                  div(style="float:left;width:90%",
                      textInput(
                        inputId = ns("dim"),
                        label = "Plot dimensions",
                        value = d[8],
                        placeholder = "E.g. 1 m x 1 m"
                      )
                  )
           ),
           column(12,style="padding:0",
                  div(style="float:left;width:95%",
                      textAreaInput(
                        inputId = ns("note"),
                        label = "Description",
                        value = ifelse(!is.na(d[9]),d[9],""),
                        resize = "vertical"
                      )
                  )
           )
           ),
    column(12,style="padding:0;margin-bottom:15px",
           tags$hr(),
           # Admin fields
          column(6,style="padding:0",
                  div(style="float:left;width:90%",
                      textInput(
                        inputId = ns("created_user"),
                        label = "Plot created by",
                        value = d[10]
                        )
                      )
                  ),
           column(6,style="padding:0",
                  div(style="float:left;width:90%",
                      textInput(
                        inputId = ns("created_date"),
                        label = "Date plot created",
                        value = format(d[11],format = "%Y-%m-%d %H:%M:%S")
                        )
                      )
                  ),
          column(6,style="padding:0",
                  div(style="float:left;width:90%",
                      textInput(
                        inputId = ns("last_edited_user"),
                        label = "Plot last edited by",
                        value = d[12]
                        )
                      )
                 ),
          column(6,style="padding:0",
                  div(style="float:left;width:90%",
                      textInput(
                        inputId = ns("last_edited_date"),
                        label = "Date plot last edited",
                        value = format(d[13],format = "%Y-%m-%d %H:%M:%S")
                        )
                      )
                  )
    )
    ,
                     size = "l",
                     easyClose = TRUE,
                     footer = div(style="margin-top:10px",
                                  class = "pull-right container",
                                  shiny::actionButton(
                                    inputId = ns("submit_p"),
                                    label = "Submit",
                                    icon = shiny::icon("edit"),
                                  )
                     )
  ) %>% shiny::showModal()
  
  shinyjs::disable("created_user")
  shinyjs::disable("created_date")
  shinyjs::disable("last_edited_user")
  shinyjs::disable("last_edited_date")
  
}

visit_modal <- function(session,p,v,vd0,vd1,d,edit,s) {
  ns <- session$ns
  shiny::modalDialog(
    title = paste0("Plot '",p,"' - Visit ",v),
    tabsetPanel(
      tabPanel("Visit record",
         column(12,  
           column(4,style="padding:0",
                  div(style="float:left;width:90%",
                      numericInput(
                        inputId = ns("visit"),
                        label = "Visit number",
                        value = v,
                        min = 1,
                        max = 10000,
                        step = 1
                        )
                  )
           ),
           column(4,style="padding:0",
                  div(style="float:left;width:90%",
                      textInput(
                        inputId = ns("recorder"),
                        label = "Recorder",
                        value = d[1],
                        placeholder = "Recorder name(s)"
                      )
                  )
           ),
           column(4,style="padding:0",
                  div(style="float:left;width:90%",
                      selectizeInput(
                        inputId = ns("survey"),
                        label = "Data source",
                        selected = d[4],
                        choices = s,
                        multiple = TRUE,
                        options = list(maxItems = 1, placeholder = 'Select a data source')
                      )
                  )
           ),
           column(4,style="padding:0",
                  div(style="float:left;width:90%",
                      dateInput(
                        inputId = ns("record_date"),
                        label = "Record (start) date",
                        value = d[2][[1]],
                        format = "dd/mm/yyyy",
                        min = vd0,
                        max = vd1
                      )
                  )
           ),
           column(4,style="padding:0",
                  div(style="float:left;width:90%",
                      dateInput(
                        inputId = ns("record_date_end"),
                        label = "Record date end",
                        value = d[3][[1]],
                        format = "dd/mm/yyyy",
                        min = vd0,
                        max = vd1
                      )
                  )
           ),
           column(12,style="padding:0",
                  div(style="float:left;width:95%",
                      textAreaInput(
                        inputId = ns("note"),
                        label = "Visit notes",
                        resize = "vertical",
                        value = ifelse(!is.na(d[5]),d[5],""),
                        placeholder = "Description of vegetation, notes on limitations etc."
                        )
                      )
                  )
           ),
          column(12,
                tags$hr(),
           column(4,style="padding:0",
                  div(style="float:left;width:90%",
                      numericInput(
                        inputId = ns("height"),
                        label = "Vegetation height (cm)",
                        value = d[6],
                        min = 0,
                        max = 30000,
                        step = 1
                      )
                  )
           ),
           column(4,style="padding:0",
                  div(style="float:left;width:90%",
                      numericInput(
                        inputId = ns("bare_ground"),
                        label = "Bare ground (%)",
                        value = d[7],
                        min = 0,
                        max = 100,
                        step = 1
                      )
                  )
           ),
           column(4,style="padding:0",
                  div(style="float:left;width:90%",
                      numericInput(
                        inputId = ns("bryophyte_cover"),
                        label = "Bryophyte cover (%)",
                        value = d[8],
                        min = 0,
                        max = 100,
                        step = 1
                      )
                  )
           ),
           column(4,style="padding:0",
                  div(style="float:left;width:90%",
                      numericInput(
                        inputId = ns("litter_cover"),
                        label = "Litter cover (%)",
                        value = d[9],
                        min = 0,
                        max = 100,
                        step = 1
                        )
                      )
                  ),
           # column(4,style="padding:0",
           #        div(style="float:left;width:90%",
           #            numericInput(
           #              inputId = ns("canopy"),
           #              label = "Canopy height (cm)",
           #              value = d[10],
           #              min = 0,
           #              max = 5000,
           #              step = 100
           #              )
           #            )
           #        ),
    #        column(3,style="padding:0",
    #               div(style="float:left;width:90%",
    #                   checkboxInput(
    #                     inputId = ns("partial_shade"),
    #                     label = "Plot partially shaded?",
    #                     value = d[11]
    #                   )
    #               )
    #        ),
    #        column(3,style="padding:0",
    #               div(style="float:left;width:90%",
    #                   checkboxInput(
    #                     inputId = ns("pool"),
    #                     label = "Surface Water present?",
    #                     value = d[12]
    #                   )   
    #               )
    #        ),
    #        column(3,style="padding:0",
    #               div(style="float:left;width:90%",
    #                   checkboxInput(
    #                     inputId = ns("tufa"),
    #                     label = "Tufa present?",
    #                     value = d[13]
    #                   )
    #               )
    #        ),
            column(4,style="padding:0",
                  div(style="float:left;width:90%",
                      textAreaInput(
                        inputId = ns("nvc"),
                        label = "Vegetation type",
                        resize = "vertical",
                        value = ifelse(!is.na(d[14]),d[14],""),
                        placeholder = "NVC code etc."
                      )
                  )
            ),
            
            ),
    
    column(12,style="padding:0;margin-bottom:15px",
          column(12,
           tags$hr(),
           # Admin fields
           column(6,style="padding:0",
                  div(style="float:left;width:90%",
                      textInput(
                        inputId = ns("created_user"),
                        label = "Plot created by",
                        value = d[15]
                      )
                  )
           ),
           column(6,style="padding:0",
                  div(style="float:left;width:90%",
                      textInput(
                        inputId = ns("created_date"),
                        label = "Date plot created",
                        value = format(d[16],format = "%Y-%m-%d %H:%M:%S")
                      )
                  )
           ),
           column(6,style="padding:0",
                  div(style="float:left;width:90%",
                      textInput(
                        inputId = ns("last_edited_user"),
                        label = "Plot last edited by",
                        value = d[17]
                      )
                  )
           ),
           column(6,style="padding:0",
                  div(style="float:left;width:90%",
                      textInput(
                        inputId = ns("last_edited_date"),
                        label = "Date plot last edited",
                        value = format(d[18],format = "%Y-%m-%d %H:%M:%S")
                      )
                  )
           )
    )
    )
      ),
    
    tabPanel("Photos",
             column(12,
                    column(6,
                           h4("Photos from visit"),
                           div(id=ns("addPhotoMessage"),"Import photos from the visit. 
                               Photos larger than 500 KB will be resized so that their maximum dimension is 2,000 pixels, and compressed by 20%."),
                           br(),
                           fileInput(
                             inputId = ns("add_photo"),
                             label = "Upload photos",
                             multiple = TRUE,
                             accept = "image/jpeg"
                             ),
                           div(
                             style = "font-size:12px;padding: 10px",
                             withSpinner(DT::DTOutput(outputId = ns("photosTable")),type = 7,caption = "Loading photos")
                           )
                           ),
                    column(6,
                                  withSpinner(
                                    uiOutput(ns("photo"))
                                    , type = 7, caption = "Loading photo"
                                    )
                           ),
                    
                    )
             )
    )
    ,
    size = "l",
    easyClose = TRUE,
    footer = div(style="margin-top:10px",
                 class = "pull-right container",
                 shiny::actionButton(
                   inputId = ns("submit_v"),
                   label = "Submit",
                   icon = shiny::icon("edit"),
                 )
    )
  ) %>% shiny::showModal()
  
  shinyjs::disable("visit")
  shinyjs::disable("created_user")
  shinyjs::disable("created_date")
  shinyjs::disable("last_edited_user")
  shinyjs::disable("last_edited_date")
  
  shinyjs::reset("add_photo")
  
  
  
}