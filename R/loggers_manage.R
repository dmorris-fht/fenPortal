loggers_manageUI <- function(id){
  ns <- NS(id)
  tagList(
    column(12,
           column(5,
                  h3("Loggers"),
                  div(
                    div(id = ns("add_logger_container"),
                        style = "margin-top: 10px;",
                        actionButton(
                          inputId = ns("add_logger"),
                          label = "Add new logger",
                          icon = icon("plus"),
                          class = "btn-success"
                        )
                    )
                  ),
                  div(
                    style = "margin-top: 10px;",
                    withSpinner(DT::DTOutput(outputId = ns("loggersTable")), type = 7)
                  )
           ),
           column(7,
                  h3("Logger installation record"),
                  div(
                    div(
                      style = "margin-top: 10px;",
                      disabled(actionButton(
                        inputId = ns("remove_logger_install"),
                        label = "Remove active logger installation",
                        icon = icon("minus"),
                        class = "btn-success"
                      )),
                      disabled(actionButton(
                        inputId = ns("add_logger_install"),
                        label = "Add new logger installation",
                        icon = icon("plus"),
                        class = "btn-success"
                      ))
                    )
                  ),
                  div(
                    style = "margin-top: 10px;",
                    withSpinner(DT::DTOutput(outputId = ns("logger_installsTable")), type = 7)
                  )                
           )
           ),
    column(12,
           column(5,
                  h3("Logger configuration record"),
                  div(
                    div(id = ns("add_logger_config_container"),
                        style = "margin-top: 10px;",
                        disabled(actionButton(
                          inputId = ns("add_logger_config"),
                          label = "Add new logger configuration",
                          icon = icon("plus"),
                          class = "btn-success"
                        ))
                    )
                  ),
                  div(
                    style = "margin-top: 10px;",
                    withSpinner(DT::DTOutput(outputId = ns("logger_configsTable")), type = 7)
                  ) 
           )
    ),
    tags$script(src ="script.js")
  )
}

loggers_manageServer <- function(id, con, role) {
  moduleServer(
    id,
    function(input, output, session) {

      #Load modals
      source("./R/modals/logger_modal.R")
      source("./R/modals/logger_installs_modal.R")
      source("./R/modals/logger_configs_modal.R")
      
      #Configure interface according to permissions
      if(!grepl("c",role,fixed = TRUE)){
        runjs("$('#add_logger_container').remove()")
        runjs("$('#add_logger_install').remove()")
        runjs("$('#add_logger_container').remove()")
      }
      if(!grepl("u",role,fixed = TRUE)){
        runjs("$('#remove_logger_install').remove()")
      }
      
      #Get installs info
      inst <- dbGetQuery(con, "SELECT A.id AS installid, 
                     A.install_name AS installname, 
                     B.id AS siteid, B.site AS sitename, 
                     C.description AS install_type, 
                     A.install_location, 
                     A.install_reason, 
                     A.installed_by, 
                     A.install_depth, 
                     A.install_protrusion,
                     A.install_geology, 
                     A.install_hydrogeo, 
                     A.install_hydroeco  
                     FROM spatial.monitoring_hydro_installs A, spatial.fen_sites B, lookups.lookup_hydro_install C 
                     WHERE A.site = B.id AND A.install_type = C.code ORDER BY B.site, A.install_name")
      
      #Get logger info
      log <- dbGetQuery(con,"SELECT * from hydro_monitoring.loggers")
      
      #Get logger installs
      log_inst <- dbGetQuery(con,"SELECT A.id, A.logger, A.install, C.site || ' - ' || B.install_name, A.install_date, A.install_by, A.install_notes, A.remove_date, A.remove_by, A.remove_notes from hydro_monitoring.logger_installs A, spatial.monitoring_hydro_installs B, spatial.fen_sites C
                           WHERE A.install = B.id AND B.site = C.id")
      
      #Get logger configurations
      log_conf <- dbGetQuery(con,"SELECT * from hydro_monitoring.logger_configs")
      x <- dbGetQuery(con,"SELECT * from hydro_monitoring.logger_configs")
      #Add buttons ----
      log <- add_btns(log, role, "logs")
      rownames(log) <- log[,c("id")]
      
      log_inst <- add_btns(log_inst, role, "log_insts")
      log_inst_0 <- log_inst[0,]
      
      log_conf <- add_btns(log_conf, role, "log_conf")
      log_conf_0 <- log_conf[0,]
      
      rv <- reactiveValues(installs = inst , logger_installs = log_inst, logger_configs = log_conf, 
                           df = log, df_li = log_inst_0, df_lc = log_conf_0,
                           dt_row = NULL, add_or_edit = 0,
                           edit_button = NULL
      )
      #Create data tables ----
      
      ##Loggers table ----
      output$loggersTable <- DT::renderDT(
        {
          shiny::isolate(rv$df)
        },
        escape = F,
        rownames = FALSE,
        colnames = c("Serial", "Model","", "", "", "", "", "", "", "", "", ""),
        selection = 'single',
        options = list(bLengthChange=0, processing = FALSE,
                       columnDefs = list(
                         list(orderable = FALSE, targets = c(12)),
                         list(visible=FALSE,targets=c(0,3:11)),
                         list(width = '90px',targets=c(12))
                       ))
      )
      
      proxy <- DT::dataTableProxy("loggersTable")
      shiny::observe({
        DT::replaceData(proxy, rv$df, resetPaging = FALSE, rownames = FALSE)
      })
      
      ##Logger installs table ----
      output$logger_installsTable <- DT::renderDT(
        {
          isolate(rv$df_li)
        },
        escape = F,
        rownames = FALSE,
        selection = 'single',
        colnames = c("","Logger serial", "", "Installation", "Date installed", "Installed by", "","Date removed", "Removed by","",""),
        options = list(bLengthChange=0, processing = FALSE,
                       columnDefs = list(
                         list(visible=FALSE,targets=c(0,2,5,6,8,9)),
                         list(orderable = FALSE, targets = c(10)),
                         list(width = '90px',targets=c(10))
                       ),
                       language = list(zeroRecords = "No logger installations"))
      )
      
      proxy1 <- DT::dataTableProxy("logger_installsTable")
      
      observe({
        DT::replaceData(proxy1, rv$df_li, resetPaging = FALSE, rownames = FALSE)
      })
      
      ##Logger configs table ----
      output$logger_configsTable <- DT::renderDT(
        {
          isolate(rv$df_lc)
        },
        escape = F,
        rownames = FALSE,
        selection = 'single',
        colnames = c("","Name", "","Created date","","","","","","","","","","","","","","","","","",""),
        options = list(bLengthChange=0, processing = FALSE,
                       columnDefs = list(
                         list(visible=FALSE,targets=c(0,2,4:20)),
                         list(orderable = FALSE, targets = c(21)),
                         list(width = '90px',targets=c(21))
                       ),
                       language = list(zeroRecords = "No logger configurations"))
      )
      
      proxy2 <- DT::dataTableProxy("logger_configsTable")
      
      observe({
        DT::replaceData(proxy2, rv$df_lc, resetPaging = FALSE, rownames = FALSE)
      })
      
      #Logger controls ----
      ##Logger submit ----
      shiny::observeEvent(input$final_edit_l, {
        if(!is.null(input$current_id) && stringr::str_detect(input$current_id, pattern = "edit") && rv$add_or_edit == 0){
          #validate input
          req(input$serial)
          req(!(input$serial %in% rv$df[,c("serial")])) #Test serial input is unique
          req(input$model)
          req(input$purchase_date)
          
          #Update row
          rv$df[rv$dt_row, c("serial")] <- input$serial
          rv$df[rv$dt_row, c("model")] <- input$model
          rv$df[rv$dt_row, c("purchase_date")] <- input$purchase_date
          rv$df[rv$dt_row, c("firmware")] <- input$firmware
          rv$df[rv$dt_row, c("hardware")] <- input$hardware
          rv$df[rv$dt_row, c("comm_config")] <- input$comm_config
          rv$df[rv$dt_row, c("notes")] <- input$notes
          
          log_id <- rv$df[rv$dt_row, c("id")]
          
          dbExecute(con, paste0("UPDATE hydro_monitoring.loggers SET 
                              serial = '", input$serial ,"',
                              model = '", input$model ,"',
                              purchase_date = TO_DATE('", input$purchase_date ,"','yyyy-mm-dd'),
                              firmware = '", input$firmware ,"',
                              hardware = '", input$hardware ,"',
                              comm_config = '", input$comm_config ,"',
                              notes = '", input$notes ,"'
                              WHERE id = " , log_id ))
          
          removeModal()
        }
        else{
          #validate input
          req(input$serial)
          req(!(input$serial %in% rv$df[,c("serial")])) #Test serial input is unique
          req(input$model)
          req(input$purchase_date)
          
          #Insert new logger
          insert <- dbGetQuery(con, paste0("INSERT INTO hydro_monitoring.loggers (serial, model, purchase_date, firmware, hardware, comm_config, notes) VALUES ('"
                                           , input$serial, "','",
                                           input$model, "',TO_DATE('",
                                           input$purchase_date, "','yyyy-mm-dd'),'",
                                           input$firmware , "','",
                                           input$hardware , "','",
                                           input$comm_config, "','",
                                           input$notes, "') RETURNING id"
          ))
          
          #Get new row back, add button and insert into DT
          add_row <- dbGetQuery(con, paste0("SELECT * FROM hydro_monitoring.loggers WHERE id = ", insert$id))
          x <- create_btns(insert$id)
          add_row <- add_row %>% dplyr::bind_cols(tibble("Buttons" = x))
          
          rv$df <- add_row %>%
            dplyr::bind_rows(rv$df)
          
          removeModal()
        }
        
        
      })
      
      ##Logger add button ----
      shiny::observeEvent(input$add_logger, {
        logger_modal_dialog(session, serial = "", model = "", purchase_date = "", firmware = "", hardware = "", comm_config = "", notes = "", edit = FALSE
        )
        rv$add_or_edit <- 1
      })
      
      #Logger install controls----
      ##Logger install add button ----
      observeEvent(input$add_logger_install,{
        logger_installs_modal_dialog(session, install = "", install_date = "", install_by = "", install_notes = "", remove_date = "", remove_by = "", remove_notes = "", edit = "add")
        
        installs_choices <- inst[!(inst$id %in% rv$df_li[is.na(rv$df_li$remove_date),c("id")]),c("id")] #List of installs that don't have active logger installs
        names(installs_choices) <- paste(inst$sitename, inst$installname,sep = " - ")
        updateSelectizeInput(session, "install", choices = installs_choices)
        
        min <- max(as.Date(rv$df_li[rv$df_li$logger == rv$df[input$loggersTable_rows_selected,c("serial")],c("install_date")], 'yyyy-mm-dd'), na.rm = TRUE)
        updateDateInput(session,"install_date", min = min)
        
        shinyjs::hide("remove_date")
        shinyjs::hide("remove_by")
        shinyjs::hide("remove_notes")
        
        rv$add_or_edit <- 1
      })
      
      #Remove active logger install action button
      observeEvent(input$remove_logger_install,{
        a <- rv$df_li[is.na(rv$df_li$remove_date),]
        logger_installs_modal_dialog(session = session, install = a$install, install_date = a$install_date, install_by = a$install_by, install_notes = a$install_notes, remove_date = "", remove_by = "", remove_notes = "", edit = "remove")
        installs_choices <- inst$installid
        names(installs_choices) <- paste(inst$sitename, inst$installname,sep = " - ")
        updateSelectizeInput(session, "install", choices = installs_choices)
        
        updateDateInput(session, "remove_date", min = a$install_date)
        
        shinyjs::disable("install")
        shinyjs::disable("install_date")
        shinyjs::disable("install_by")
        shinyjs::disable("install_notes")
        
        shinyjs::show("remove_date")
        shinyjs::show("remove_by")
        shinyjs::show("remove_notes")
        
        rv$add_or_edit <- -1
      })
      
      ##Logger install submit modal ----
      observeEvent(input$final_edit_li, {
        if(rv$add_or_edit == 1){        
          #validate input
          req(input$install)
          req(input$install_date)
          req(input$install_by)
          
          #Insert new logger install
          dbGetQuery(con, paste0("INSERT INTO hydro_monitoring.logger_installs (logger, install, install_date, install_by, install_notes) VALUES ('"
                                 , rv$df[input$loggersTable_rows_selected,c("serial")], "','",
                                 input$install, "',TO_DATE('",
                                 input$install_date, "','yyyy-mm-dd'),'",
                                 input$install_by , "','",
                                 input$install_notes, "')"
          ))
          
          #Get updated data back from DB
          rv$logger_installs <- add_btns(
            dbGetQuery(con,"SELECT A.id, A.logger, A.install, C.site || ' - ' || B.install_name, A.install_date, A.install_by, A.install_notes, A.remove_date, A.remove_by, A.remove_notes from hydro_monitoring.logger_installs A, spatial.monitoring_hydro_installs B, spatial.fen_sites C
                                                  WHERE A.install = B.id AND B.site = C.id"),
            role, "log_insts")
          rv$df_li <- rv$logger_installs[rv$logger_installs$logger == rv$df[input$loggersTable_rows_selected,c("serial")],]
          
          removeModal()
        }
        if(rv$add_or_edit == 0){
          #validate input
          req(input$install_notes)
          
          #Update logger install
          dbGetQuery(con, paste0("UPDATE hydro_monitoring.logger_installs SET
                               install_notes = '", input$install_notes ,"' 
                               WHERE id = ", rv$df_li[is.na(rv$df_li$remove_date),c("id")])
          )
          
          #Get updated data back from DB
          rv$logger_installs <- add_btns(
            dbGetQuery(con,"SELECT A.id, A.logger, A.install, C.site || ' - ' || B.install_name, A.install_date, A.install_by, A.install_notes, A.remove_date, A.remove_by, A.remove_notes from hydro_monitoring.logger_installs A, spatial.monitoring_hydro_installs B, spatial.fen_sites C
                                                  WHERE A.install = B.id AND B.site = C.id"),
            role, "log_insts")
          rv$df_li <- rv$logger_installs[rv$logger_installs$logger == rv$df[input$loggersTable_rows_selected,c("serial")],]
          
          removeModal()      }
        if(rv$add_or_edit == -1){
          #validate input
          req(input$remove_date)
          req(input$remove_by)
          req(as.Date(input$remove_date,'%Y-%m-%d') > as.Date(input$install_date,'%Y-%m-%d'))
          
          #Update logger install
          dbGetQuery(con, paste0("UPDATE hydro_monitoring.logger_installs SET
                               remove_date = TO_DATE('", input$remove_date, "','yyyy-mm-dd'),
                               remove_by = '", input$remove_by ,"',
                               remove_notes = '", input$remove_notes ,"' 
                               WHERE id = ", rv$df_li[is.na(rv$df_li$remove_date),c("id")])
          )
          
          #Get updated data back from DB
          rv$logger_installs <- add_btns(
            dbGetQuery(con,"SELECT A.id, A.logger, A.install, C.site || ' - ' || B.install_name, A.install_date, A.install_by, A.install_notes, A.remove_date, A.remove_by, A.remove_notes from hydro_monitoring.logger_installs A, spatial.monitoring_hydro_installs B, spatial.fen_sites C
                                                  WHERE A.install = B.id AND B.site = C.id"),
            role, "log_insts")
          rv$df_li <- rv$logger_installs[rv$logger_installs$logger == rv$df[input$loggersTable_rows_selected,c("serial")],]
          
          removeModal()
        }
      })
      
      #Logger config controls----
      ##Logger config add button----
      observeEvent(input$add_logger_config,{
        logger_configs_modal_dialog(session = session, name = "", create_date = "", create_by = "", computer_name = "", application = "", application_version = "", 
                                    overwrite = "", schedule_start = "", schedule_end = "", type = "", interval_day = "", interval_hour = "", interval_min = "", 
                                    interval_sec = "", notes = "", edit = "add")
        
        
        min <- max(as.Date(rv$df_lc[rv$df_lc$logger == rv$df[input$loggersTable_rows_selected,c("serial")],c("create_date")], 'yyyy-mm-dd'), na.rm = TRUE)
        updateDateInput(session,"create_date", min = min)
        
        rv$add_or_edit <- 1
      })
      ##Logger config submit modal ----
      observeEvent(input$final_edit_lc, {
        if(rv$add_or_edit == 1){        
          #validate input
          req(input$name)
          req(input$create_date)
          req(input$create_by)
          req(input$application)
          req(input$overwrite)
          req(input$schedule_start)
          req(input$interval_day)
          req(input$interval_hour)
          req(input$interval_min)
          req(input$interval_sec)
          
          #Insert new logger install
          dbGetQuery(con, paste0("INSERT INTO hydro_monitoring.logger_configs (logger, create_date, create_by, computer_name, application, application_version, overwrite, schedule_start, schedule_end, type, interval_day, interval_hour, interval_min, interval_sec, notes) VALUES ('"
                                 , rv$df[input$loggersTable_rows_selected,c("serial")], "',TO_DATE('",
                                 input$create_date , "','yyyy-mm-dd'),'",
                                 input$create_by, "','",
                                 input$computer_name, "','",
                                 input$application, "','",
                                 input$application_version, "','",
                                 input$overwrite, "',TO_TIMESTAMP('",
                                 input$schedule_start,"','YYYY-MM-DD HH24:MI:SS') , TO_TIMESTAMP('",
                                 input$schedule_end, "','YYYY-MM-DD HH24:MI:SS'), '",
                                 input$type, "',",
                                 input$interval_day, ",",
                                 input$interval_hour, ",",
                                 input$interval_min, ",",
                                 input$interval_sec, ",'",
                                 input$notes, "')"
          ))
          
          #Get updated data back from DB
          rv$logger_configs <- add_btns(
            dbGetQuery(con,"SELECT * from hydro_monitoring.logger_configs"),
            role, "log_conf")
          rv$df_lc <- rv$logger_configs[rv$logger_configs$logger == rv$df[input$loggersTable_rows_selected,c("serial")],]
          
          removeModal()
        }
        if(rv$add_or_edit == 0){
          
          #Update logger config
          dbGetQuery(con, paste0("UPDATE hydro_monitoring.logger_configs SET
                               notes = '", input$notes ,"' 
                               WHERE id = ", rv$dt_row)
          )
          
          #Get updated data back from DB
          rv$logger_configs <- add_btns(
            dbGetQuery(con,"SELECT * from hydro_monitoring.logger_configs"),
            role, "log_conf")
          rv$df_lc <- rv$logger_configs[rv$logger_configs$logger == rv$df[input$loggersTable_rows_selected,c("serial")],]
          
          removeModal()    
        }
      })
      #Common button controls ----
      
      ##Remove modal ----
      observeEvent(input$dismiss_modal, {
        removeModal()
      })
      
      
      ##Logger table select rows ----
      observeEvent(input$loggersTable_rows_selected,ignoreNULL = FALSE,{
        
        if(isTruthy(input$loggersTable_rows_selected)){
          rv$df_li <- rv$logger_installs[rv$logger_installs$logger == rv$df[input$loggersTable_rows_selected,c("serial")],]
          rv$df_lc <- rv$logger_configs[rv$logger_configs$logger == rv$df[input$loggersTable_rows_selected,c("serial")],]
          
          shinyjs::enable("add_logger_config")
          
          if(nrow(rv$df_li) == 0 || nrow(rv$df_li[is.na(rv$df_li$remove_date),]) == 0){
            shinyjs::enable("add_logger_install")
            shinyjs::disable("remove_logger_install")
          }
          if(nrow(rv$df_li) > 0 && nrow(rv$df_li[is.na(rv$df_li$remove_date),]) == 1){
            shinyjs::disable("add_logger_install")
            shinyjs::enable("remove_logger_install")
          }
        }
        else{      
          rv$df_li <- log_inst_0
          rv$df_lc <- log_conf_0
          
          shinyjs::disable("add_logger_install")
          shinyjs::disable("remove_logger_install")
          shinyjs::disable("add_logger_config")
        }

      })
      
      ##Row button click event  ----
      observeEvent(input$current_id, {
        #Logger row buttons
        if(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "logs_edit")){      
          rv$dt_row <- which(stringr::str_detect(rv$df$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
          
          df <- rv$df[rv$dt_row, ]
          logger_modal_dialog(session = session, serial = df$serial, model = df$model, purchase_date = df$purchase_date, 
                              firmware = df$firmware, hardware = df$hardware, comm_config = df$comm_config,
                              notes = df$notes, edit = TRUE
          )
          
          shinyjs::disable("serial")
          shinyjs::disable("model")
          shinyjs::disable("purchase_date")
          shinyjs::enable("firmware")
          shinyjs::enable("hardware")
          shinyjs::enable("comm_config")
          shinyjs::enable("notes")
        }
        
        if(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "logs_info")){
          rv$dt_row <- which(stringr::str_detect(rv$df$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
          df <- rv$df[rv$dt_row, ]
          logger_modal_dialog(session = session, serial = df$serial, model = df$model, purchase_date = df$purchase_date, 
                              firmware = df$firmware, hardware = df$hardware, comm_config = df$comm_config,
                              notes = df$notes, edit = FALSE
          )
          
          shinyjs::disable("serial")
          shinyjs::disable("model")
          shinyjs::disable("purchase_date")
          shinyjs::disable("firmware")
          shinyjs::disable("hardware")
          shinyjs::disable("comm_config")
          shinyjs::disable("notes")
          shinyjs::hide("final_edit_l")
        }
        
        #Logger installs row buttons
        if(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "log_insts_edit")){      
          rv$dt_row <- which(stringr::str_detect(rv$df_li$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
          
          df_li <- rv$df_li[rv$dt_row, ]
          logger_installs_modal_dialog(session = session, install = df_li$install, install_date = df_li$install_date, install_by = df_li$install_by, 
                                       install_notes = df_li$install_notes, remove_date = df_li$remove_date, 
                                       remove_by = df_li$remove_by, remove_notes = df_li$remove_notes, edit = "edit")
          
          shinyjs::disable("install")
          shinyjs::disable("install_date")
          shinyjs::disable("install_by")
          shinyjs::enable("install_notes")
          shinyjs::hide("remove_date")
          shinyjs::hide("remove_by")
          shinyjs::hide("remove_notes")
          
          installs_choices <- inst$installid
          names(installs_choices) <- paste(inst$sitename, inst$installname,sep = " - ")
          updateSelectizeInput(session, "install", choices = installs_choices)
          
          shinyjs::show("final_edit_li")
        }
        
        if(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "log_insts_info")){      
          rv$dt_row <- which(stringr::str_detect(rv$df_li$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
          
          df_li <- rv$df_li[rv$dt_row, ]
          logger_installs_modal_dialog(session = session, install = df_li$install, install_date = df_li$install_date, install_by = df_li$install_by, 
                                       install_notes = df_li$install_notes, remove_date = df_li$remove_date, 
                                       remove_by = df_li$remove_by, remove_notes = df_li$remove_notes, edit = "edit")
          
          shinyjs::disable("install")
          shinyjs::disable("install_date")
          shinyjs::disable("install_by")
          shinyjs::disable("install_notes")
          shinyjs::disable("remove_date")
          shinyjs::disable("remove_by")
          shinyjs::disable("remove_notes")
          
          if(is.na(rv$df_li[rv$dt_row, c("remove_date")])){
            shinyjs::hide("remove_date")
            shinyjs::hide("remove_by")
            shinyjs::hide("remove_notes")
          }
          else{
            shinyjs::show("remove_date")
            shinyjs::show("remove_by")
            shinyjs::show("remove_notes")
          }
          
          
          installs_choices <- inst$installid
          names(installs_choices) <- paste(inst$sitename, inst$installname,sep = " - ")
          updateSelectizeInput(session, "install", choices = installs_choices)
          
          shinyjs::hide("final_edit_li")
        }
        
        #Logger configs row buttons
        if(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "log_conf_edit")){      
          rv$dt_row <- which(stringr::str_detect(rv$df_li$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
          
          df_lc <- rv$df_lc[rv$dt_row, ]
          logger_configs_modal_dialog(session = session, name = df_lc$name, 
                                      create_date = df_lc$create_date, 
                                      create_by = df_lc$create_by, 
                                      computer_name = df_lc$computer_name,
                                      application = df_lc$application, 
                                      application_version = df_lc$application_version, 
                                      overwrite = df_lc$overwrite, 
                                      schedule_start = df_lc$schedule_start, 
                                      schedule_end = df_lc$schedule_end, 
                                      type = df_lc$type, 
                                      interval_day = df_lc$interval_day, 
                                      interval_hour = df_lc$interval_hour, 
                                      interval_min = df_lc$interval_min, 
                                      interval_sec = df_lc$interval_sec, 
                                      notes = df_lc$notes, 
                                      edit = "edit")
          
          shinyjs::disable("name")
          shinyjs::disable("create_date")
          shinyjs::disable("create_by")
          shinyjs::disable("computer_name")
          shinyjs::disable("application")
          shinyjs::disable("application_version")
          shinyjs::disable("schedule_start")
          shinyjs::disable("schedule_end")
          shinyjs::disable("overwrite")
          shinyjs::disable("type")
          shinyjs::disable("interval_day")
          shinyjs::disable("interval_hour")
          shinyjs::disable("interval_min")
          shinyjs::disable("interval_sec")
          
          shinyjs::enable("notes")
          
          shinyjs::show("final_edit_lc")
        }
        
        if(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "log_conf_info")){      
          rv$dt_row <- which(stringr::str_detect(rv$df_li$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
          
          df_lc <- rv$df_lc[rv$dt_row, ]
          logger_configs_modal_dialog(session = session, name = df_lc$name, 
                                      create_date = df_lc$create_date, 
                                      create_by = df_lc$create_by, 
                                      computer_name = df_lc$computer_name,
                                      application = df_lc$application, 
                                      application_version = df_lc$application_version, 
                                      overwrite = df_lc$overwrite, 
                                      schedule_start = df_lc$schedule_start, 
                                      schedule_end = df_lc$schedule_end, 
                                      type = df_lc$type, 
                                      interval_day = df_lc$interval_day, 
                                      interval_hour = df_lc$interval_hour, 
                                      interval_min = df_lc$interval_min, 
                                      interval_sec = df_lc$interval_sec, 
                                      notes = df_lc$notes, 
                                      edit = "edit")
          
          shinyjs::disable("name")
          shinyjs::disable("create_date")
          shinyjs::disable("create_by")
          shinyjs::disable("computer_name")
          shinyjs::disable("application")
          shinyjs::disable("application_version")
          shinyjs::disable("schedule_start")
          shinyjs::disable("schedule_end")
          shinyjs::disable("type")
          shinyjs::disable("overwrite")
          
          shinyjs::disable("interval_day")
          shinyjs::disable("interval_hour")
          shinyjs::disable("interval_min")
          shinyjs::disable("interval_sec")
          
          shinyjs::disable("notes")
          
          shinyjs::hide("final_edit_lc")
        }
        
        rv$add_or_edit <- 0
      })
      
    }
  )
}