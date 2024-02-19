loggersManageUI <- function(id){
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
                          icon = icon("plus")
                        )
                    )
                  ),
                  div(
                    style = "margin-top: 10px;",
                    withSpinner(DT::dataTableOutput(outputId = ns("loggersTable")), type = 7)
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
                        icon = icon("minus")
                      )),
                      disabled(actionButton(
                        inputId = ns("add_logger_install"),
                        label = "Add new logger installation",
                        icon = icon("plus")
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
                          icon = icon("plus")
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

loggersManageServer <- function(id, login, tables) {
  moduleServer(
    id,
    function(input, output, session) {
      
      role <- login$role
      user <- login$username
      password <- login$password
      
      req(tables$hydro_installs)
      inst <- tables$hydro_installs
      
      #Load modals ----
      source("./R/modals/logger_modal.R")
      source("./R/modals/logger_installs_modal.R")
      source("./R/modals/logger_configs_modal.R")
      
      #Configure interface according to permissions ----
      if(!grepl("c",role,fixed = TRUE)){
        runjs(paste0("$('#", id ,"-add_logger_container').remove()"))
        runjs(paste0("$('#", id ,"-add_logger_install').remove()"))
        runjs(paste0("$('#", id ,"-add_logger_container').remove()"))
      }
      if(!grepl("u",role,fixed = TRUE)){
        runjs(paste0("$('#", id ,"-remove_logger_install').remove()"))
      }
      
      # Reactives and other variables ----
      
      rv <- reactiveValues(installs = NA , logger_installs = NA, logger_configs = NA, 
                           df = NA, df_li = NA, df_lc = NA,
                           dt_row = NULL, add_or_edit = 0,
                           edit_button = NULL
                           )
      
      mode <- reactiveVal({})
      
      rv_0 <- reactiveValues(installs_choices = NA, log_inst_0 = NA, log_conf_0 = NA)

    #Create data tables ----
      
      ##Loggers table  ----
      output$loggersTable <- DT::renderDataTable(
        {
          req(rv$df)
          isolate(rv$df)
          x <- rv$df[,c("serial","model","Buttons")]
          x
        }
        ,escape = FALSE,
        rownames = FALSE,
        selection = 'single',
        colnames = c("Serial","Model",""),
        options = list(bLengthChange=0, processing = FALSE,
                       columnDefs = list(
                         list(orderable = FALSE, targets = c(2)),
                         list(width = '60px',targets=c(2))
                       ),
                       extensions = c("FixedHeader"),#, "Scroller")
                       fixedHeader = TRUE,
                       scrollY = "30vh"
        )
        
      )
      
      proxy <- DT::dataTableProxy("loggersTable")

      observe({
        req(isTruthy(rv$df))
        x <- rv$df[,c("serial","model","Buttons")]
        DT::replaceData(proxy, x, resetPaging = FALSE, rownames = FALSE)
      })
      
      # ##Logger installs ----
      output$logger_installsTable <- DT::renderDataTable(
        {
          isolate(rv$df_li)
          req(isTruthy(rv$df_li))
          x <- rv$df_li[,c("logger","installation","install_date","install_by","remove_date","remove_by","Buttons")]
          x
        },
        escape = F,
        rownames = FALSE,
        selection = 'single',
        colnames = c("Logger serial","Installation","Date installed", "Installed by","Date removed", "Removed by",""),
        options = list(bLengthChange=0, processing = FALSE,
                       columnDefs = list(
                         list(orderable = FALSE, targets = c(6)),
                         list(width = '60px',targets=c(6))
                       ),
                       language = list(zeroRecords = "No logger installations"),
                       extensions = c("FixedHeader"),#, "Scroller")
                       fixedHeader = TRUE,
                       scrollY = "30vh"
                       )
      )

      proxy1 <- DT::dataTableProxy("logger_installsTable")

      observe({
        req(isTruthy(rv$df_li))
        x <- rv$df_li[,c("logger","installation","install_date","install_by","remove_date","remove_by","Buttons")]
        DT::replaceData(proxy1, x, resetPaging = FALSE, rownames = FALSE)
      })

      ##Logger configs table ----
      output$logger_configsTable <- DT::renderDataTable(
        {
          req(isTruthy(rv$df_lc))
          isolate(rv$df_lc)
          x <- rv$df_lc[,c("name","create_date","Buttons")]
          x
        },
        escape = F,
        rownames = FALSE,
        selection = 'single',
        colnames = c("Name","Created date",""),
        options = list(bLengthChange=0, processing = FALSE,
                       columnDefs = list(
                         list(orderable = FALSE, targets = c(2)),
                         list(width = '60px',targets=c(2))
                       ),
                       language = list(zeroRecords = "No logger configurations"),
                       extensions = c("FixedHeader"),#, "Scroller")
                       fixedHeader = TRUE,
                       scrollY = "30vh"
                       )
      )

      proxy2 <- DT::dataTableProxy("logger_configsTable")

      observe({
        req(isTruthy(rv$df_lc))
        x <- rv$df_lc[,c("name","create_date","Buttons")]
        DT::replaceData(proxy2, x, resetPaging = FALSE, rownames = FALSE)
      })
      
    #Get data ----
      future_promise({

        con <- poolCheckout(con_global)

        #Get logger info
        log <- dbGetQuery(con,"SELECT * from hydro_monitoring.loggers")
        #Get logger installs
        log_inst <- dbGetQuery(con,"SELECT A.id, A.logger, A.install, C.site || ' - ' || B.install_name AS installation, A.install_date, A.install_by, A.install_notes, A.remove_date, A.remove_by, A.remove_notes from hydro_monitoring.logger_installs A, spatial.monitoring_hydro_installs B, spatial.fen_sites C
                           WHERE A.install = B.id AND B.site = C.id")
        #Get logger configurations
        log_conf <- dbGetQuery(con,"SELECT * from hydro_monitoring.logger_configs")

        # Close connection
        poolReturn(con)

        #Add buttons
        log <- add_btns(log, role, "logs")
        rownames(log) <- log[,c("id")]

        log_inst <- add_btns(log_inst, role, "log_insts")
        log_inst_0 <- log_inst[0,]

        log_conf <- add_btns(log_conf, role, "log_conf")
        log_conf_0 <- log_conf[0,]

        installs_choices <- inst$installid
        names(installs_choices) <- paste0(inst$sitename, " - ", inst$installname)
        
        return(list(
          "log_inst" = log_inst,
          "log_conf" = log_conf,
          "log" = log,
          "log_inst_0" = log_inst_0,
          "log_conf_0" = log_conf_0,
          "install_choices" = installs_choices
        ))
      })%...>% (function(result) {
        
        rv$installs <- inst
        rv$logger_installs <- result$log_inst
        rv$logger_configs <- result$log_conf
        rv$df <- result$log
        rv$df_li <- result$log_inst_0
        rv$df_lc <- result$log_conf_0

        rv_0$log_inst_0 <- result$log_inst_0
        rv_0$log_conf_0 <- result$log_conf_0
        rv_0$installs_choices <- result$install_choices
        
      })

    #Logger controls ----
      ##Logger submit ----
      observeEvent(input$final_edit_l, {
        #Update
        if(!is.null(input$current_id) && stringr::str_detect(input$current_id, pattern = "edit") && rv$add_or_edit == 0){
          #Update row
          log_id <- rv$df[rv$dt_row, c("id")]

          future_promise({
            con0 <- fendb0(user,password)
            q <- paste0("UPDATE hydro_monitoring.loggers SET
                              firmware = ", null_text_val(con0,input$firmware) ,",
                              hardware = ", null_text_val(con0,input$hardware) ,",
                              comm_config = ", null_text_val(con0,input$comm_config) ,",
                              notes = ", null_text_val(con0,input$notes) ,"
                              WHERE id = " , log_id ,"RETURNING last_edited_user, last_edited_date")
            update <- dbGetQuery(con0,q)
            dbDisconnect(con0)
            return(update)
          })%...>%(function(result){
            rv$df[rv$dt_row, c("firmware")] <- input$firmware
            rv$df[rv$dt_row, c("hardware")] <- input$hardware
            rv$df[rv$dt_row, c("comm_config")] <- input$comm_config
            rv$df[rv$dt_row, c("notes")] <- input$notes
            rv$df[rv$dt_row,c("last_edited_user","last_edited_date")] <- result
          })

          removeModal()
        }
        else{
          # Add new logger
          #validate input
          req(input$serial)
          req(!(input$serial %in% rv$df[,c("serial")])) #Test serial input is unique
          req(input$model)
          req(input$purchase_date)

          future_promise({
            con0 <- fenDb0(user,password)
            q <- paste0("INSERT INTO hydro_monitoring.loggers (serial, model, purchase_date, firmware, hardware, comm_config, notes) VALUES (",
                        null_text_val(con0,input$serial), ",",
                        null_text_val(con0,input$model), ",",
                        null_date_val(input$purchase_date),",",
                        null_text_val(con0,input$firmware) , ",",
                        null_text_val(con0,input$hardware) , ",",
                        null_text_val(con0,input$comm_config), ",",
                        null_text_val(con0,input$notes), ") RETURNING id, created_user, created_date"
                        )
            insert <- dbGetQuery(con0, q)
            dbDisconnect(con0)
            return(insert)
          })%...>%(function(result){
            row <- list(
              result$id,
              input$serial,
              input$model,
              input$purchase_date,
              result$created_user,
              result$created_date,
              NA,
              NA,
              blank(input$firmware),
              blank(input$hardware),
              blank(input$com_config),
              blank(input$notes)
            )
            row <- as.data.frame(row)
            colnames(row) <- colnames(rv$df)[1:(ncol(rv$df)-1)]
            row <- add_btns(row,role,"logs")
            rv$df <- rbind(rv$df, row)
          })
          removeModal()
        }

      })

      ##Logger add button ----
      shiny::observeEvent(input$add_logger, {
        logger_modal_dialog(session, serial = "", model = "", purchase_date = "", firmware = "", hardware = "", comm_config = "", notes = "", edit = FALSE
        )
        mode("add")

        # Modal validation

        iv <- InputValidator$new()
        iv$add_rule("serial",function(value){
          if(isTruthy(input$serial) && input$serial %in% rv$df[,c("serial")] && mode() == "add"){
            return("Serial already in database")
          }
        })
        iv$add_rule("serial",sv_required())
        iv$add_rule("model",sv_required())
        iv$add_rule("purchase_date",sv_required())
        iv$enable()

        shinyjs::disable("final_edit_l")

        observe({
          if(
            isTruthy(input$serial) &&
            !(input$serial %in% rv$df[,c("serial")]) && # Serial input is unique
            isTruthy(input$model) &&
            isTruthy(input$purchase_date)
          ){
            shinyjs::enable("final_edit_l")
          }
          else{
            shinyjs::disable("final_edit_l")
          }
        })

        rv$add_or_edit <- 1
      })

    #Logger install controls----
      ##Logger install add button ----
      observeEvent(input$add_logger_install,{
        logger_installs_modal_dialog(session, install = "", install_date = "", install_by = "", install_notes = "", remove_date = "", remove_by = "", remove_notes = "", edit = "add")
        mode("add")

        # Limit choice of installs to those that don't have active logger installs
        inst_0 <- rv$installs[!(rv$installs$installid %in% rv$logger_installs$install) | !(rv$installs$installid %in% rv$logger_installs[is.na(rv$logger_installs$remove_date),c("install")]),]
        inst_0_choices <- inst_0$installid
        names(inst_0_choices) <- paste0(inst_0$sitename," - ",inst_0$installname)
        updateSelectizeInput(session, "install", choices = inst_0_choices)

        inst_0 <- inst[!(inst$installid %in% log_inst$install) | !(inst$installid %in% log_inst[is.na(log_inst$remove_date),c("install")]),]
        inst_0_choices <- inst_0$installid
        names(inst_0_choices) <- paste0(inst_0$sitename," - ",inst_0$installname)

        shinyjs::hide("remove_date")
        shinyjs::hide("remove_by")
        shinyjs::hide("remove_notes")

        # Modal validation
        iv <- InputValidator$new()
        iv$add_rule("install",sv_required())
        iv$add_rule("install_date",sv_required())
        iv$add_rule("install_by",sv_required())
        iv$enable()

        # Set min logger install date as latest removal date
        serial <- rv$df[input$loggersTable_rows_selected,c("serial")]
        if(serial %in% rv$df_li$logger){
          min <- max(as.Date(rv$df_li[rv$df_li$logger == serial,c("removal_date")], 'yyyy-mm-dd'), na.rm = TRUE)
          updateDateInput(session,"install_date", min = min)
          }

        shinyjs::disable("final_edit_li")

        observe({
          if(isTruthy(input$install) &&
             isTruthy(input$install_date) &&
             isTruthy(input$install_by) &&
             mode() == "add"
             ){
            shinyjs::enable("final_edit_li")
          }
          else{
            shinyjs::disable("final_edit_li")
          }
        })

        rv$add_or_edit <- 1
      })

      #Remove active logger install action button ----
      observeEvent(input$remove_logger_install,{
        a <- rv$df_li[is.na(rv$df_li$remove_date),]
        logger_installs_modal_dialog(session = session, install = a$install, install_date = a$install_date, install_by = a$install_by, install_notes = a$install_notes, remove_date = "", remove_by = "", remove_notes = "", edit = "remove")
        mode("remove")
        updateSelectizeInput(session, "install", choices = rv_0$installs_choices)

        updateDateInput(session, "remove_date", min = a$install_date)

        shinyjs::disable("install")
        shinyjs::disable("install_date")
        shinyjs::disable("install_by")
        shinyjs::disable("install_notes")

        shinyjs::show("remove_date")
        shinyjs::show("remove_by")
        shinyjs::show("remove_notes")

        # Modal validation
        iv <- InputValidator$new()
        iv$add_rule("remove_date",function(value){
          if(isTruthy(input$remove_date) &&
            !(as.Date(input$remove_date,'%Y-%m-%d') > as.Date(input$install_date,'%Y-%m-%d')) &&
            mode() == "remove"
          ){
            return("Removal date must be after installation date")
          }
        })
        iv$add_rule("remove_date",sv_required())
        iv$add_rule("remove_by",sv_required())
        iv$enable()

        shinyjs::disable("final_edit_li")

        observe({
          if(
            isTruthy(input$remove_date) && isTruthy(input$remove_by) &&
            as.Date(input$remove_date,'%Y-%m-%d') > as.Date(input$install_date,'%Y-%m-%d')
            && mode() == "remove"
            ){
            shinyjs::enable("final_edit_li")
          }
          else{
            shinyjs::disable("final_edit_li")
          }
        })

        rv$add_or_edit <- -1
      })

      ##Logger install submit modal ----
      observeEvent(input$final_edit_li, {
        # Add new logger install
        if(rv$add_or_edit == 1){
          #validate input
          req(input$install)
          req(input$install_date)
          req(input$install_by)

          serial <- rv$df[input$loggersTable_rows_selected,c("serial")]
          install <- paste0(rv$installs[rv$installs$installid == input$install,c("sitename")],
                 " - ",
                 rv$installs[rv$installs$installid == input$install,c("installname")])

          future_promise({
            con0 <- fenDb0(user,password)
            q <- paste0("INSERT INTO hydro_monitoring.logger_installs (logger, install, install_date, install_by, install_notes) VALUES ('"
                        , serial, "',",
                        null_text_val(con0,input$install), ",",
                        null_date_val(input$install_date), ",",
                        null_text_val(con0,input$install_by) , ",",
                        null_text_val(con0,input$install_notes), ") RETURNING id")
            insert <- dbGetQuery(con0, q)
            dbDisconnect(con0)
            return(insert)

          })%...>%(function(result){
            row <- list(
              result$id,
              serial,
              input$install,
              install,
              input$install_date,
              input$install_by,
              blank(input$install_notes),
              NA,NA,NA
            )
            row <- as.data.frame(row)
            colnames(row) <- colnames(rv$logger_installs)[1:(ncol(rv$logger_installs)-1)]
            row <- add_btns(row,role,"log_insts")
            rv$logger_installs <- rbind(rv$logger_installs,row)
          })
          removeModal()
        }
        # Edit logger install
        if(rv$add_or_edit == 0){
          #validate input
          req(input$install_notes)
          id <- rv$df_li[rv$dt_row, c("id")]

          future_promise({
            con0 <- fenDb0(user,password)
            q <- paste0("UPDATE hydro_monitoring.logger_installs SET
                               install_notes = ", null_text_val(con0,input$install_notes) ,"
                               WHERE id = ", id)
            update <- dbGetQuery(con0, q)
            dbDisconnect(con0)
            return(update)
          })%...>%(function(result){
            rv$logger_installs[rv$logger_installs$id == id,c("install_notes")] <- input$install_notes
          })

          removeModal()
        }
        # Remove logger install
        if(rv$add_or_edit == -1){
          #validate input
          req(input$remove_date)
          req(input$remove_by)
          req(as.Date(input$remove_date,'%Y-%m-%d') > as.Date(input$install_date,'%Y-%m-%d'))

          id <- rv$df_li[is.na(rv$df_li$remove_date), c("id")]

          future_promise({
            con0 <- fenDb0(user,password)
            q <- paste0("UPDATE hydro_monitoring.logger_installs SET
                               remove_date = ", null_date_val(input$remove_date), ",
                               remove_by = ", null_text_val(con0,input$remove_by) ,",
                               remove_notes = ", null_text_val(con0,input$remove_notes) ,"
                               WHERE id = ", id)
            update <- dbGetQuery(con0, q)
            dbDisconnect(con0)
            return(update)
          })%...>%(function(result){
            rv$logger_installs[rv$logger_installs$id == id,c("remove_date")] <- input$remove_date
            rv$logger_installs[rv$logger_installs$id == id,c("remove_by")] <- input$remove_by
            rv$logger_installs[rv$logger_installs$id == id,c("remove_notes")] <- input$remove_notes

            })

          removeModal()
        }
      })

    #Logger config controls----
      ##Logger config add button----
      observeEvent(input$add_logger_config,{
        logger_configs_modal_dialog(session = session, name = NULL, create_date = NULL, create_by = NULL, computer_name = NULL, application = NULL, application_version = NULL,
                                    overwrite = NULL, schedule_start = NULL, schedule_end = NULL, type = NULL, interval_day = NULL, interval_hour = NULL, interval_min = NULL,
                                    interval_sec = NULL, notes = NULL, edit = "add")
        mode("add")

        # Modal validation
        iv <- InputValidator$new()
        iv$add_rule("name",sv_required())
        iv$add_rule("create_date",sv_required())
        iv$add_rule("create_by",sv_required())
        iv$add_rule("application",sv_required())
        iv$add_rule("overwrite",sv_required())
        iv$add_rule("schedule_start",sv_required())
        iv$add_rule("interval_day",sv_required())
        iv$add_rule("interval_hour",sv_required())
        iv$add_rule("interval_min",sv_required())
        iv$add_rule("interval_sec",sv_required())
        iv$enable()

        min <- max(as.Date(rv$df_lc[rv$df_lc$logger == rv$df[input$loggersTable_rows_selected,c("serial")],c("create_date")], 'yyyy-mm-dd'), na.rm = TRUE)
        updateDateInput(session,"create_date", min = min)

        shinyjs::show("final_edit_lc")

        shinyjs::disable("final_edit_lc")

        observe({
          if(isTruthy(input$name) &&
             isTruthy(input$create_date) &&
             isTruthy(input$create_by) &&
             isTruthy(input$application) &&
             isTruthy(input$overwrite) &&
             isTruthy(input$schedule_start) &&
             isTruthy(input$interval_day) &&
             isTruthy(input$interval_hour) &&
             isTruthy(input$interval_min) &&
             isTruthy(input$interval_sec) &&
             mode() == "add"
          ){
            shinyjs::enable("final_edit_lc")
          }
          else{
            shinyjs::disable("final_edit_lc")
          }
        })

        rv$add_or_edit <- 1
      })
      ##Logger config submit modal ----
      observeEvent(input$final_edit_lc, {
        serial <- rv$df[input$loggersTable_rows_selected,c("serial")]

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

          future_promise({
            con0 <- fenDb0(user,password)
            q <- paste0("INSERT INTO hydro_monitoring.logger_configs (logger, name, create_date, create_by, computer_name, application, application_version, overwrite, schedule_start, schedule_end, type, interval_day, interval_hour, interval_min, interval_sec, notes) VALUES ('"
                        , serial, "',",
                        null_text_val(con0,input$name), ",",
                        null_date_val(input$create_date) , ",",
                        null_text_val(con0,input$create_by), ",",
                        null_text_val(con0,input$computer_name), ",",
                        null_text_val(con0,input$application), ",",
                        null_text_val(con0,input$application_version), ",",
                        null_text_val(con0,input$overwrite), ",",
                        null_timestamp_val(input$schedule_start),",",
                        null_timestamp_val(input$schedule_end), ",",
                        null_text_val(con0,input$type), ",",
                        null_num_val(input$interval_day), ",",
                        null_num_val(input$interval_hour), ",",
                        null_num_val(input$interval_min), ",",
                        null_num_val(input$interval_sec), ",",
                        null_text_val(con0,input$notes), ") RETURNING id, created_user, created_date"
                        )
            insert <- dbGetQuery(con0, q)
            dbDisconnect(con0)
            return(insert)
          })%...>% (function(result) {
            row <- as.data.frame(list(
              result$id,
              input$name,
              input$create_by,
              input$create_date,
              blank(input$computer_name),
              input$application,
              blank(input$application_version),
              input$overwrite,
              input$schedule_start,
              blank(input$schedule_end),
              input$type,
              input$interval_day,
              input$interval_hour,
              input$interval_min,
              input$interval_sec,
              result$created_user,
              result$created_date,
              NA,
              NA,
              serial,
              blank(input$notes),
              NA,
              NA,
              NA,
              NA,
              NA
            ))
            colnames(row) <- colnames(rv$logger_configs)[1:(ncol(rv$logger_configs)-1)]
            row <- add_btns(row, role, "log_conf")
            rv$logger_configs <- rbind(rv$loger_configs, row)
          })
          removeModal()
        }
        if(rv$add_or_edit == 0){
          req(input$notes)
          id <- rv$df_lc[rv$dt_row,c("id")]

          future_promise({
            con0 <- fenDb0(user,password)
            q <- paste0("UPDATE hydro_monitoring.logger_configs SET
                               notes = ", null_text_val(con0,input$notes) ,"
                               WHERE id = ", id, " RETURNING last_edited_user, last_edited_date")
            update <- dbGetQuery(con0, q)
            dbDisconnect(con0)
            return(update)
          })%...>% (function(result) {
            rv$logger_configs[,c("notes")] <- input$notes
            rv$logger_configs[,c("last_edited_user")] <- result$last_edited_user
              rv$logger_configs[,c("last_edited_date")] <- result$last_edited_date
          })

          removeModal()
        }
      })
    #Common button controls ----

      ##Remove modal ----
      observeEvent(input$dismiss_modal, {
        removeModal()
      })


      ##Logger table select rows FIX THIS for no logger selected ----
      #observeEvent(input$loggersTable_rows_selected,ignoreNULL = FALSE,{

      observe({
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
          rv$df_li <- rv_0$log_inst_0
          rv$df_lc <- rv_0$log_conf_0

          shinyjs::disable("add_logger_install")
          shinyjs::disable("remove_logger_install")
          shinyjs::disable("add_logger_config")
        }

      })

      ##Row button click event  ----
      observeEvent(input$current_id, {
        #Logger row buttons
        if(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "logs_edit")){
          mode("edit")
          rv$dt_row <- which(stringr::str_detect(rv$df$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
          id <- rv$df[rv$dt_row, c("id")]
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
          mode("info")
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
          mode("edit")
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

          updateSelectizeInput(session, "install", choices = rv_0$installs_choices, selected = df_li$install)

          shinyjs::show("final_edit_li")

          # Modal validation
          iv <- InputValidator$new()
          iv$add_rule("install_notes",sv_required())
          iv$enable()

          shinyjs::disable("final_edit_li")

          observe({
            if(isTruthy(input$install_notes)){
              shinyjs::enable("final_edit_li")
            }
            else{
              shinyjs::disable("final_edit_li")
            }
          })
        }

        if(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "log_insts_info")){
          rv$dt_row <- which(stringr::str_detect(rv$df_li$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
          mode("info")
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

          updateSelectizeInput(session, "install", choices = rv_0$installs_choices, selected = df_li$install)

          shinyjs::hide("final_edit_li")
        }

        #Logger configs row buttons
        if(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "log_conf_edit")){
          rv$dt_row <- which(stringr::str_detect(rv$df_lc$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
          mode("edit")
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

          iv <- InputValidator$new()
          iv$add_rule("notes",sv_required())
          iv$enable()

          shinyjs::show("final_edit_lc")

          shinyjs::disable("final_edit_lc")

          observe({
            if(isTruthy(input$notes) &&
               mode() == "edit"
            ){
              shinyjs::enable("final_edit_lc")
            }
            else{
              shinyjs::disable("final_edit_lc")
            }
          })
        }

        if(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "log_conf_info")){
          rv$dt_row <- which(stringr::str_detect(rv$df_lc$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
          mode("info")
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