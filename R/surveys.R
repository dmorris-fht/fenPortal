surveyUI <- function(id){
  ns <- NS(id)
  tagList(
    withSpinner(
      tagList(
    column(12,
           column(4,
                  h3("Data sources"),
                  p("Data sources describe the origin of data stored in the database, 
                  and all biological data have an associated data source."),
                  p("Data sources can include:"),
                  tags$ul(
                    tags$li("Site surveys and monitoring, with their associated biological records, habitat mapping etc."),
                    tags$li("Data obtained from third parties, such as biological records centres."),
                    tags$li("Data digitised from books, journals etc.")
                  ),
                  p("The table shows all data source records in the database. If your login has permission,
                    then you can edit existing data sources and add new ones."),
                  p("Open data sources, for which new associated records can be added, are highlighted in green. Closed data sources are greyed out.")
                  
                  
                  
                  ),
           column(8,
                  div(
                    div(id = ns("add_survey_container"),
                        style = "margin-top: 10px;",
                        actionButton(
                          inputId = ns("add_survey"),
                          label = "Add new data source",
                          icon = icon("plus")
                        )
                    )
                  ),
                  div(
                    style = "padding: 10px;font-size:12px ",
                             withSpinner(DT::DTOutput(outputId = ns("surveysTable")),type = 7)
                  )
           )
  ),
      ),
  id = ns("module"),
  type = 4,
  size = 2,
  proxy.height = "100%",
  hide.ui = TRUE,
  caption = "Loading module"),
  tags$script(src ="script.js"),
  tags$script(
    HTML(
      paste0("$('#",id,"-module').parent().removeClass('shiny-spinner-hidden')")
    )
  )
  )
}

surveyServer <- function(id, login, tables) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Initialisation ----
      role <- login$role
      user <- login$username
      password <- login$password

      isolate({
        app_tables(tables, c("surveys","projects"))
      })
      
      observe({
        req(tables$surveys)
        req(tables$projects)
        
        runjs(
          paste0(
            "$('#",id,"-module').parent().addClass('shiny-spinner-hidden');
                 $('div[data-spinner-id=\\'",id,"-module\\']').css('display','inline')"
          )
        )
      })
      
      #Load modals
      source("./R/modals/survey_modal.R")
      
      #Configure interface according to permissions
      if(!grepl("c",role,fixed = TRUE)){
        runjs(paste0("$('#", id ,"-add_survey_container').remove()"))
      }
      
      rv <- reactiveValues(df = NA, 
                           #surveys = surveys0,
                           p = NA, 
                           dt_row = NULL, add_or_edit = 0,
                           edit_button = NULL)
      
      observe({
        req(tables$projects)
        rv$p <- tables$projects[order(tables$projects$project),]
      })
      
      #Create data table ----
      
      output$surveysTable <- DT::renderDataTable(
        {
          x <- data.frame(survey = character(), range = character(), project = character(), survey_type_description = character(),status = character(),Buttons = character())
          DT::datatable(
            x
            ,
            escape = F,
            rownames = FALSE,
            selection = 'single',
            filter = list(position='top'),
            colnames = c("Data source name", "Year(s)","Project", "Data source type","Status",""),
            options = list(
              processing = FALSE,
              dom = 'tpliB',
              lengthMenu = list(c(50, 100, -1), c('50', '100', 'All')),
              pageLength = 50,
              buttons = list(
                list(
                  extend = "collection",
                  text = 'Show All',
                  action = DT::JS("function ( e, dt, node, config ) {
                                    dt.page.len(-1);
                                    dt.ajax.reload();
                                }")
                )
              ),
              language = list(zeroRecords = "No data sources"),
              columnDefs = list(
                list(className = "dt-center", targets = c(1)),
                list(orderable = FALSE, targets = c(ncol(x)-1)),
                list(targets = c(ncol(x)-1),searchable = FALSE),
                list(width = '60px',targets=c(ncol(x)-1))
              ),
              extensions = c("FixedHeader", "Scroller","Buttons"),
              fixedHeader = TRUE,
              scrollY = "400"
              )  
            ) %>% 
            formatStyle("status",target='row',backgroundColor = styleEqual("open","rgb(128, 255, 0,0.3)")) %>% 
            formatStyle("status",target='row',backgroundColor = styleEqual("closed","rgb(160, 160, 160,0.3)"))
          }
      )
      
      proxy <- DT::dataTableProxy("surveysTable")

      observe({
        req(tables$surveys)
        rv$df <- add_btns(tables$surveys,role,"surveys")
        x <- rv$df
        x$year_0 <- pmin(year(x$start_date),x$start_year,na.rm=T)
        x$year_1 <- pmax(year(x$end_date),x$end_year,na.rm=T)
        x$range <- apply(x[,c("year_0","year_1")],1,year_range)
        d <- x[,c("survey","range","project","survey_type_description","status","Buttons")]
        
        proxy %>% 
          DT::replaceData(data = d, resetPaging = FALSE, rownames = FALSE) %>%
          updateFilters(data = d)
      })
     
      ##Remove modal ----
      observeEvent(input$dismiss_modal, {
        removeModal()
      })
      
      ##Row button click event  ----
      mode <- reactiveVal("")
      
      observeEvent(input$current_id, {

        #Edit button
        if(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "surveys_edit")){      
          rv$dt_row <- which(stringr::str_detect(rv$df$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
          id <- rv$df[rv$dt_row, c("id")]
          d <- rv$df[rv$dt_row,c(2:18)]
          survey_modal_dialog(session, d, mode = "edit", qual = "")
          
          choices_p <- rv$p$id
          names(choices_p) <- rv$p$project
          
          updateSelectizeInput(session, "survey_type", choices = choices_st, selected = d[2])
          updateSelectizeInput(session, "project", choices = choices_p, selected = d[8])
          updateSelectizeInput(session, "sharing", choices = choices_sh, selected = d[9])
          
          shinyjs::disable("created_user")
          shinyjs::disable("created_date")
          shinyjs::disable("last_edited_user")
          shinyjs::disable("last_edited_date")
          
          shinyjs::enable("status")
          
          if(d[17] == "open"){
            shinyjs::enable("survey")
            shinyjs::enable("survey_type")
            shinyjs::enable("start_date")
            shinyjs::enable("end_date")
            shinyjs::enable("start_year")
            shinyjs::enable("end_year")
            shinyjs::enable("source")
            shinyjs::enable("project")
            shinyjs::enable("sharing")
            shinyjs::enable("copyright")
            shinyjs::enable("description")
            shinyjs::enable("url")
          }else{
            shinyjs::disable("survey")
            shinyjs::disable("survey_type")
            shinyjs::disable("start_date")
            shinyjs::disable("end_date")
            shinyjs::disable("start_year")
            shinyjs::disable("end_year")
            shinyjs::disable("source")
            shinyjs::disable("project")
            shinyjs::disable("sharing")
            shinyjs::disable("copyright")
            shinyjs::disable("description")
            shinyjs::disable("url")
          }

          observeEvent(input$status,{
            if(input$status == "open"){
              shinyjs::enable("survey")
              shinyjs::enable("survey_type")
              shinyjs::enable("start_date")
              shinyjs::enable("end_date")
              shinyjs::enable("start_year")
              shinyjs::enable("end_year")
              shinyjs::enable("source")
              shinyjs::enable("project")
              shinyjs::enable("sharing")
              shinyjs::enable("copyright")
              shinyjs::enable("description")
              shinyjs::enable("url")
            }else{
              shinyjs::disable("survey")
              shinyjs::disable("survey_type")
              shinyjs::disable("start_date")
              shinyjs::disable("end_date")
              shinyjs::disable("start_year")
              shinyjs::disable("end_year")
              shinyjs::disable("source")
              shinyjs::disable("project")
              shinyjs::disable("sharing")
              shinyjs::disable("copyright")
              shinyjs::disable("description")
              shinyjs::disable("url")
            }
          })
          
          
          # Modal validation
          iv <- InputValidator$new()
          iv$add_rule("survey",function(value){
            shiny::isolate(rv$df)
            if(isTruthy(input$survey) && input$survey %in% rv$df[which(rv$df$id != id),c("survey")]){
              return("Data source already exists")
            }
          })
          iv$add_rule("survey",sv_required())
          iv$add_rule("survey_type",sv_required())
          iv$add_rule("project",sv_required())
          iv$add_rule("sharing",sv_required())
          iv$enable()

          shinyjs::disable("final_edit")

          observe({
            if(isTruthy(input$survey_type) &&
               isTruthy(input$project) &&
               isTruthy(input$sharing) &&
               isTruthy(input$survey) && !(input$survey %in% rv$df[which(rv$df$id != id),c("survey")])
            ){
              shinyjs::enable("final_edit")
            }
            else{
              shinyjs::disable("final_edit")
            }
          })
        }
        
      #Info button  
      if(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "surveys_info")){
        rv$dt_row <- which(stringr::str_detect(rv$df$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
        mode("info")
        d <- rv$df[rv$dt_row,c(2:18)]
        survey_modal_dialog(session, d, mode = NULL, qual = "")
        
        choices_p <- rv$p$id
        names(choices_p) <- rv$p$project
        
        updateSelectizeInput(session, "survey_type", choices = choices_st, selected = d[2])
        updateSelectizeInput(session, "project", choices = choices_p, selected = d[8])
        updateSelectizeInput(session, "sharing", choices = choices_sh, selected = d[9])
        
        shinyjs::disable("created_user")
        shinyjs::disable("created_date")
        shinyjs::disable("last_edited_user")
        shinyjs::disable("last_edited_date")
        
        shinyjs::disable("status")
        
        shinyjs::disable("survey")
        shinyjs::disable("survey_type")
        shinyjs::disable("start_date")
        shinyjs::disable("end_date")
        shinyjs::disable("start_year")
        shinyjs::disable("end_year")
        shinyjs::disable("source")
        shinyjs::disable("project")
        shinyjs::disable("sharing")
        shinyjs::disable("copyright")
        shinyjs::disable("description")
        shinyjs::disable("url")
        
        shinyjs::hide("new_project")
        shinyjs::hide("final_edit")
        
      }
        rv$add_or_edit <- 0
      })
      
      ##Add survey button ----
      observeEvent(input$add_survey,{
        mode("add")
        survey_modal_dialog(session, NA, mode = "add", qual = "")
        
        choices_p <- rv$p$id
        names(choices_p) <- rv$p$project
        
        updateSelectizeInput(session, "survey_type", choices = choices_st)
        updateSelectizeInput(session, "project", choices = choices_p)
        updateSelectizeInput(session, "sharing", choices = choices_sh)
        
        shinyjs::hide("created_user")
        shinyjs::hide("created_date")
        shinyjs::hide("last_edited_user")
        shinyjs::hide("last_edited_date")
        
        updateSelectizeInput(session, "status", selected = "open")
        shinyjs::disable("status")
        
        shinyjs::enable("survey")
        shinyjs::enable("survey_type")
        shinyjs::enable("start_date")
        shinyjs::enable("end_date")
        shinyjs::enable("start_year")
        shinyjs::enable("end_year")
        shinyjs::enable("source")
        shinyjs::enable("project")
        shinyjs::enable("sharing")
        shinyjs::enable("copyright")
        shinyjs::enable("description")
        shinyjs::enable("url")
        
        # Modal validation
        iv <- InputValidator$new()
        iv$add_rule("survey",function(value){
          if(isTruthy(input$survey) && input$survey %in% rv$df[,c("survey")] && mode() == "add"){
            return("Data source already exists")
          }
        })
        iv$add_rule("survey",sv_required())
        iv$add_rule("survey_type",sv_required())
        iv$add_rule("project",sv_required())
        iv$add_rule("sharing",sv_required())
        iv$enable()

        shinyjs::disable("final_edit")

        observe({
          if(isTruthy(input$survey_type) &&
             isTruthy(input$project) &&
             isTruthy(input$sharing) &&
             isTruthy(input$survey) && !(input$survey %in% rv$df[,c("survey")])
          ){
            shinyjs::enable("final_edit")
          }
          else{
            shinyjs::disable("final_edit")
          }
        })
        
        rv$add_or_edit <- 1
      })
      
      ##Submit edits button ----
      observeEvent(input$final_edit, {
        
        #validate input
        req(isTruthy(input$survey_type))
        req(isTruthy(input$project))
        req(isTruthy(input$sharing))

        if(!is.null(input$current_id) && stringr::str_detect(input$current_id, pattern = "edit") && rv$add_or_edit == 0){
          id <- rv$df[rv$dt_row, c("id")]
          req(isTruthy(input$survey) && !(input$survey %in% rv$df[which(rv$df$id != id),c("survey")]))
          
          #update row
          
          future_promise({
            con0 <- fenDb0(user,password)
            update <- dbGetQuery(con0, paste0(
              "UPDATE records.surveys SET
            survey = ", null_text_val(con0,input$survey) ,",
            survey_type = ", input$survey_type,",",
              "start_date = ", null_date_val(input$start_date),",",
              "end_date = ", null_date_val(input$end_date),",",
              "start_year = ", null_num_val(input$start_year),",",
              "end_year = ", null_num_val(input$end_year),",",
              "source = " , null_text_val(con0, input$source),",",
              "project = ", input$project,",",
              "sharing = ", input$sharing,",",
              "copyright = ", null_text_val(con0, input$copyright),",",
              "description = ", null_text_val(con0, input$description),",", 
              "url = ", null_text_val(con0, input$url),",",
              "status = ", null_text_val(con0,input$status),
              " WHERE id = ", id, " RETURNING last_edited_user, last_edited_date"
            ))
            dbDisconnect(con0)
            return(update)
          })%...>%(function(result){
            row <- list(
              id,
              input$survey,
              as.numeric(input$survey_type),
              as.Date(input$start_date),
              as.Date(input$end_date),
              as.numeric(ifelse(isTruthy(input$start_year),input$start_year,NA)),
              as.numeric(ifelse(isTruthy(input$end_year),input$end_year,NA)),
              input$source,
              as.numeric(input$project),
              as.numeric(input$sharing),
              input$copyright,
              input$description,
              input$url,
              input$created_user,
              result$last_edited_user,
              as.Date(input$created_date),
              as.Date(result$last_edited_date),
              input$status,
              st[st$code == as.numeric(input$survey_type),c("description")],
              sh[sh$code == as.numeric(input$sharing),c("description")],
              rv$p[rv$p$id == as.numeric(input$project),c("project")]
            )
            tables$surveys[tables$surveys$id == id,] <- row
          })
        }
        else{
          req(isTruthy(input$survey) && !(input$survey %in% rv$df[,c("survey")]))
          
          future_promise({
            con0 <- fenDb0(user,password)
            q <- paste0(
              "INSERT INTO records.surveys
            (survey, survey_type,start_date,end_date,start_year,end_year,source,project,sharing,copyright,description,url,status)
            VALUES
            ('",
              input$survey ,"',",
              input$survey_type,",",
              null_date_val(input$start_date),",",
              null_date_val(input$end_date),",",
              null_num_val(input$start_year),",",
              null_num_val(input$end_year),",",
              null_text_val(con0, input$source),",",
              input$project,",",
              input$sharing,",",
              null_text_val(con0, input$copyright),",",
              null_text_val(con0, input$description),",",
              null_text_val(con0, input$url),",",
              null_text_val(con0, input$status)
              ,") RETURNING id, created_user, created_date"
            )
            r <- dbGetQuery(con0,q)
            dbDisconnect(con0)
            return(r)
          })%...>%(function(result){
            row <- list(
              as.numeric(result$id),
              input$survey,
              as.numeric(input$survey_type),
              as.Date(input$start_date),
              as.Date(input$end_date),
              as.numeric(ifelse(isTruthy(input$start_year),input$start_year,NA)),
              as.numeric(ifelse(isTruthy(input$end_year),input$end_year,NA)),
              input$source,
              as.numeric(input$project),
              as.numeric(input$sharing),
              input$copyright,
              input$description,
              input$url,
              result$created_user,
              NA,
              as.Date(result$created_date),
              NA,
              input$status,
              st[st$code == as.numeric(input$survey_type),c("description")],
              sh[sh$code == as.numeric(input$sharing),c("description")],
              rv$p[rv$p$id == as.numeric(input$project),c("project")]
            )
            
            if(grepl("u",role)){
              b <- create_btns_ru(c(as.numeric(result$id)),"surveys")
            }else{
              b <- create_btns_r(c(as.numeric(result$id)),"surveys")
            }
            tables$surveys[nrow(tables$surveys)+1,] <- row
          })
        }
        
        removeModal()
          })
      
      # reactive to hold values in survey modal, in case new project modal is launched ----
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
      ###New project action button
      observeEvent(input$new_project,{
        input_survey$survey <- ifelse(isTruthy(input$survey),input$survey,NA)
        input_survey$survey_type<-ifelse(isTruthy(input$survey_type),input$survey_type,NA)
        input_survey$start_date<-ifelse(isTruthy(input$start_date),input$start_date,NA)
        input_survey$end_date<-ifelse(isTruthy(input$end_date),input$end_date,NA)
        input_survey$start_year<-ifelse(isTruthy(input$start_year),input$start_year,NA)
        input_survey$end_year<-ifelse(isTruthy(input$end_year),input$end_year,NA)
        input_survey$source<-ifelse(isTruthy(input$source),input$source,NA)
        input_survey$sharing<-ifelse(isTruthy(input$sharing),input$sharing,NA)
        input_survey$copyright<-ifelse(isTruthy(input$copyright),input$copyright,NA)
        input_survey$description<-ifelse(isTruthy(input$description),input$description,NA)
        input_survey$url<-ifelse(isTruthy(input$url),input$url,NA)
        
        project_modal_dialog(session)
        
        iv3 <- InputValidator$new()
        iv3$add_rule("project_new",function(value){
          if(input$project_new %in% tables$projects$project){
            return("Project already exists")
          }
        })
        iv3$add_rule("project_new",sv_required())
        iv$enable()
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
        req(isTruthy(input$project_new) && !(input$project_new %in% rv$p$project))
        
        #Insert new project and return new id for passing back to survey form
        future_promise({
          con0 <- fenDb0(user,password)
          new_project <- dbGetQuery(con0, paste0("INSERT INTO records.projects (project) VALUES (",null_text_val(con0,input$project_new),
                                                ") RETURNING id, project, created_user, last_edited_user,created_date,last_edited_date"))
          dbDisconnect(con0)
          return(new_project)
        }) %...>%(function(new_project){
          input_survey$project <- new_project$id
          #Update module project reactive and choices
          tables$projects[nrow(tables$projects)+1,] <- new_project[1,]
          p <- tables$projects
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
                              , mode = ifelse(rv$add_or_edit == 0, "edit", "add")
                              , qual = ""
                              )
          
          choices_p <- p$id
          names(choices_p) <- p$project
          
          updateSelectizeInput(session, "survey_type", choices = choices_st, selected = input_survey$survey_type)
          updateSelectizeInput(session, "project", choices = choices_p, selected = input_survey$project)
          updateSelectizeInput(session, "sharing", choices = choices_sh, selected = input_survey$sharing)
          
          shinyjs::hide("created_user")
          shinyjs::hide("created_date")
          shinyjs::hide("last_edited_user")
          shinyjs::hide("last_edited_date")
          
          shinyjs::enable("survey")
          shinyjs::enable("survey_type")
          shinyjs::enable("start_date")
          shinyjs::enable("end_date")
          shinyjs::enable("start_year")
          shinyjs::enable("end_year")
          shinyjs::enable("source")
          shinyjs::enable("project")
          shinyjs::enable("sharing")
          shinyjs::enable("copyright")
          shinyjs::enable("description")
          shinyjs::enable("url")
        })
      })
    }
  )
}