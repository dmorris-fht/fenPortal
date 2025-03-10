delete_row <- function(session){
  ns <- session$ns
  modalDialog(
    div(style = "text-align:center",
        h4("Do you want to delete this row?"),
        actionButton(inputId= ns("del_1"),label="Yes"),
        actionButton(inputId= ns("del_0"),label="No")
        ,style="width:100%; text-align:center"
    ),
    , footer=NULL,size="s",easyClose=FALSE,fade=TRUE
  ) %>% showModal()
}

edit_df_row <- function(session,r){
  ns <- session$ns
  modalDialog(
    column(6,
           div(style="float:left;width:90%",
        textInput(inputId =  ns("site_record"),
                  label = "site_record",
                  value = ifelse(!is.na(r[1]),r[1],"")
                  )))
        ,
    column(6,
           div(style="float:left;width:90%",
        textInput(inputId =  ns("gridref"),
                  label = "gridref",
                  value = ifelse(!is.na(r[2]),r[2],"")
        )))
        ,
    column(6,
           div(style="float:left;width:90%",
        textInput(inputId =  ns("taxon_record"),
                  label = "taxon_record",
                  value = ifelse(!is.na(r[3]),r[3],"")
        )))
        ,
    column(6,
           div(style="float:left;width:90%",
        textInput(inputId =  ns("taxon_nbn"),
                  label = "taxon_nbn",
                  value = ifelse(!is.na(r[4]),r[4],"")
        )))
        ,
    column(6,
           div(style="float:left;width:90%",
        textInput(inputId =  ns("quantity"),
                  label = "quantity",
                  value = ifelse(!is.na(r[5]),r[5],"")
        )))
        ,
    column(6,
           div(style="float:left;width:90%",
        textInput(inputId =  ns("status"),
                  label = "status",
                  value = ifelse(!is.na(r[6]),r[6],"")
        )))
        ,
    column(6,
           div(style="float:left;width:90%",textInput(inputId =  ns("sex"),
                   label = "sex",
                   value = ifelse(!is.na(r[7]),r[7],"")
        )))
        ,
    column(6,
           div(style="float:left;width:90%",
               textInput(inputId =  ns("stage"),
                   label = "stage",
                   value = ifelse(!is.na(r[8]),r[8],"")
        )))
        ,
    column(6,
           div(style="float:left;width:90%",
        textInput(inputId =  ns("habitat"),
                   label = "habitat",
                   value = ifelse(!is.na(r[9]),r[9],"")
        )))
        ,
    column(12,
           div(style="float:left;width:100%",
        textAreaInput(inputId =  ns("note"),
                  label = "note",
                  value = ifelse(!is.na(r[10]),r[10],""),
                  resize = "vertical"
        )))
        ,
    column(6,
           div(style="float:left;width:90%",
        textInput(inputId =  ns("recorder"),
                  label = "recorder",
                  value = ifelse(!is.na(r[11]),r[11],"")
        )))
        ,
    column(6,
           div(style="float:left;width:90%",
        textInput(inputId =  ns("determiner"),
                  label = "determiner",
                  value = ifelse(!is.na(r[12]),r[12],"")
        )))
        ,
    column(6,
           div(style="float:left;width:90%",
        textInput(inputId =  ns("method"),
                  label = "method",
                  value = ifelse(!is.na(r[13]),r[13],"")
        )))
        ,
    column(6,
           div(style="float:left;width:90%",
        textInput(inputId =  ns("sample"),
                 label = "sample",
                 value = ifelse(!is.na(r[14]),r[14],"")
        )))
        ,
    column(6,
           div(style="float:left;width:90%",
        textInput(inputId = ns("record_date"),
                  label = "record_date",
                  value = ifelse(!is.na(r[15]),r[15],"")
                  )))
        ,
    column(6,
           div(style="float:left;width:90%",
               textInput(inputId = ns("record_year"),
                         labe = "record_year",
                         value = ifelse(!is.na(r[19]),r[19],"")
               )))
    ,
    column(6,
           div(style="float:left;width:90%",
        textInput(inputId = ns("record_date_start"),
                  label = "record_date_start",
                  value = ifelse(!is.na(r[17]),r[17],"")
                  )))
        ,
    column(6,
           div(style="float:left;width:90%",
               textInput(inputId = ns("record_date_end"),
                  label = "record_date_end",
                  value = ifelse(!is.na(r[18]),r[18],"")
                  )))
        ,
    column(6,
           div(style="float:left;width:90%",
               textInput(inputId = ns("start_year"),
                     labe = "start_year",
                     value = ifelse(!is.na(r[19]),r[19],"")
                     )))
        ,
    column(6,
           div(style="float:left;width:90%",
               textInput(inputId = ns("end_year"),
                     labe = "end_year",
                     value = ifelse(!is.na(r[20]),r[20],"")
                     )))
        ,
    column(6,
           div(style="float:left;width:90%",
               textInput(inputId = ns("start_month"),
                     labe = "start_month",
                     value = ifelse(!is.na(r[21]),r[21],"")
                     )))
        ,
    column(6,
           div(style="float:left;width:90%",
               textInput(inputId = ns("end_month"),
                     labe = "end_month",
                     value = ifelse(!is.na(r[22]),r[22],"")
                     )))
        ,
    column(6,
           div(style="float:left;width:90%",
        textInput(inputId =  ns("origin_key"),
                  label = "origin_key",
                  value = ifelse(!is.na(r[23]),r[23],"")
        )))
    
    , size="m",easyClose= TRUE,fade=TRUE,
    footer = div(style="margin-top:10px",
                 class = "pull-right container",
                 actionButton(
                   inputId = ns("final_edit_r"),
                   label = "Submit",
                   icon = icon("edit"),
                 )
    )
  ) %>% showModal()
}

edit_df_s_row <- function(session,r, s_choices, s, ss_choices){
  ns <- session$ns
  modalDialog(
    column(6,
           div(style="float:left;width:90%",
               textInput(ns("match_site_record"),
                         label="site_record",
                         value =  ifelse(!is.na(r[1]),r[1],"")
                         )
               )),
    column(6,
           div(style="float:left;width:90%",
               textInput(ns("match_gridref"),
                         label="gridref",
                         value = ifelse(!is.na(r[2]),r[2],"")
                         )
               )),
    column(6,
           div(style="float:left;width:90%",
               selectizeInput(ns("site_match"),
                              label = "site_match",
                              choices = s_choices,
                              selected = s,
                              multiple = TRUE,
                              options = list(maxItems = 1)
                              )
               )),
    column(6,
           div(style="float:left;width:90%",
               selectizeInput(ns("subsite"),
                              label = "subsite",
                              choices = c(""),
                              selected = "",
                              multiple = TRUE,
                              options = list(maxItems = 1))
           ))
    
    , size="m",easyClose= TRUE,fade=TRUE,
    footer = div(style="margin-top:10px",
                 class = "pull-right container",
                 actionButton(
                   inputId = ns("final_edit_s"),
                   label = "Submit",
                   icon = icon("edit"),
                 )
    )
  ) %>% showModal()
}

edit_df_t_row <- function(session,r){
  ns <- session$ns
  modalDialog(
    column(6,
           div(style="float:left;width:90%",
               textInput(ns("match_taxon_record"),
                         label="taxon_record",
                         value =  ifelse(!is.na(r),r,"")
               )
           )),
    column(6,
           div(style="float:left;width:90%",
               selectizeInput(ns("tvk_match"),
                              label = "taxon_match",
                              choices = c(""),
                              selected = "",
                              multiple = TRUE,
                              options = list(maxItems = 1)
               )
           )),
    
    , size="m",easyClose= TRUE,fade=TRUE,
    footer = div(style="margin-top:10px",
                 class = "pull-right container",
                 actionButton(
                   inputId = ns("final_edit_t"),
                   label = "Submit",
                   icon = icon("edit"),
                 )
    )
  ) %>% showModal()
}

upload_modal <- function(session){
  ns <- session$ns
  modalDialog(
    div(style = "text-align:center",
        h4("Are you sure you want to upload?"),
        p("Any unmatched sites and taxa will not be uploaded."),
        actionButton(inputId= ns("up_1"),label="Yes"),
        actionButton(inputId= ns("up_0"),label="No")
        ,style="width:100%; text-align:center"
    ),
    , footer=NULL,size="s",easyClose=FALSE,fade=TRUE
  ) %>% showModal()
}