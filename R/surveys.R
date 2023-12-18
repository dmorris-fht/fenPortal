surveyUI <- function(id){
  ns <- NS(id)
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
                  p("The table shows all data source records in the Fen Database. If your login has permission,
                    then you can edit existing data sources and add new ones."),
                  
                  
                  
                  tags$figure(
                    img(src='EMO.jpg', style = "width:100%; border: solid 1px black; border-radius: 5px"), 
                    tags$figcaption("Early Marsh-orchid, Lye Valley, Oxford. Tonie Gillie.")
                    )
                  
                  
                  ),
           column(8,
                  
                  div(
                    style = "border: 1px solid black; border-radius: 5px; padding: 10px ",
                             withSpinner(DT::DTOutput(outputId = ns("surveysTable")),type = 7)
                  ),
                  div(
                    div(id = ns("add_survey_container"),
                        style = "margin-top: 10px;",
                        actionButton(
                          inputId = ns("add_survey"),
                          label = "Add new data source",
                          icon = icon("plus")
                        )
                    )
                  )
           )
  ),
  tags$script(src ="script.js")
  )
}

surveyServer <- function(id, con, role) {
  moduleServer(
    id,
    function(input, output, session) {
      #Load modals
      source("./R/modals/survey_modal.R")
      
      #Configure interface according to permissions
      if(!grepl("c",role,fixed = TRUE)){
        runjs(paste0("$('#", id ,"-add_survey_container').remove()"))
      }

      #Get survey info ----
      surveys <- dbGetQuery(con, "SELECT
                            s.id AS id,
                            s.survey,
                            s.survey_type AS survey_type_code,
                            s.start_date,
                            s.end_date,
                            s.start_year,
                            s.end_year,
                            s.source,
                            s.project AS projectid,
                            s.sharing AS sharing_code,
                            s.copyright,
                            s.description AS description,
                            s.url,
                            s.created_user,
                            s.last_edited_user,
                            s.created_date,
                            s.last_edited_date,
                            st.description AS survey_type,
                            st.description AS sharing,
                            p.project AS project
                            FROM
                              records.surveys s,
                              records.projects p,
                              lookups.lookup_sharing sh,
                              lookups.lookup_survey_types st
                            WHERE
                              s.project = p.id AND
                              s.sharing = sh.code AND
                              s.survey_type = st.code
                            ")
      #Add buttons ----
      s <- add_btns(surveys,role,"surveys")

      #Get lookups
      p <- dbGetQuery(con, "SELECT * FROM records.projects ORDER BY project")
      sh <- dbGetQuery(con, "SELECT * FROM lookups.lookup_sharing")
      st <- dbGetQuery(con, "SELECT * FROM lookups.lookup_survey_types")

      choices_p <- p$id
      names(choices_p) <- p$project

      choices_sh <- sh$code
      names(choices_sh) <- sh$description

      choices_st <- st$code
      names(choices_st) <- st$description

      rv <- reactiveValues(df = s, 
                           p = p, choices_p = choices_p, 
                           st = st, choices_st = choices_st, 
                           sh =sh, choices_sh = choices_sh,
                           dt_row = NULL, add_or_edit = 0,
                           edit_button = NULL)
      
      #Create data table ----
      
      output$surveysTable <- DT::renderDT(
        {
          shiny::isolate(rv$df)

          x <- rv$df
          x$year_0 <- pmin(year(x$start_date),x$start_year,na.rm=T)
          x$year_1 <- pmax(year(x$end_date),x$end_year,na.rm=T)
          x$range <- apply(x[,c("year_0","year_1")],1,year_range)
          d <- x[,c("id","survey","range","project","survey_type","Buttons")]
          rownames(d) <- s[,c("id")]
          colnames(d) <- c("id","Data source name", "Year(s)","Project", "Data source type","")

          return(d)
        },
        escape = F,
        rownames = FALSE,
        selection = 'single',
        options = list(processing = FALSE,
                       columnDefs = list(
                         list(className = "dt-center", targets = c(2)),
                         list(orderable = FALSE, targets = c(5)),
                         list(visible=FALSE,targets=c(0)),
                         list(width = '60px',targets=c(5))
                       ),
                       extensions = c("FixedHeader"),#, "Scroller")
                       fixedHeader = TRUE,
                       scrollY = "400px"
        )
      )
      
      proxy <- DT::dataTableProxy("surveysTable")
      
      observe({
        x <- rv$df
        x$year_0 <- pmin(year(x$start_date),x$start_year,na.rm=T)
        x$year_1 <- pmax(year(x$end_date),x$end_year,na.rm=T)
        x$range <- apply(x[,c("year_0","year_1")],1,year_range)
        d <- x[,c("id","survey","range","project","survey_type","Buttons")]
        rownames(d) <- s[,c("id")]
        colnames(d) <- c("id","Data source name", "Year(s)","Project", "Data source type","")
        
        DT::replaceData(proxy, d, resetPaging = FALSE, rownames = FALSE)
      })
     
      ##Remove modal ----
      observeEvent(input$dismiss_modal, {
        removeModal()
      })
      
      ##Row button click event  ----
      observeEvent(input$current_id, {

        #Edit button
        if(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "surveys_edit")){      
          rv$dt_row <- which(stringr::str_detect(rv$df$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
          
          d <- rv$df[rv$dt_row,c(2:17)]
          survey_modal_dialog(session = session, d, edit = TRUE)
          
          updateSelectizeInput(session, "survey_type", choices = rv$choices_st, selected = d[2])
          updateSelectizeInput(session, "project", choices = rv$choices_p, selected = d[8])
          updateSelectizeInput(session, "sharing", choices = rv$choices_sh, selected = d[9])
          
          shinyjs::disable("created_user")
          shinyjs::disable("created_date")
          shinyjs::disable("last_edited_user")
          shinyjs::disable("last_edited_date")
          
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
        }
        
      #Info button  
      if(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "surveys_info")){
        rv$dt_row <- which(stringr::str_detect(rv$df$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
        
        d <- rv$df[rv$dt_row,c(2:17)]
        survey_modal_dialog(session = session, d, edit = FALSE)
        
        updateSelectizeInput(session, "survey_type", choices = rv$choices_st, selected = d[2])
        updateSelectizeInput(session, "project", choices = rv$choices_p, selected = d[8])
        updateSelectizeInput(session, "sharing", choices = rv$choices_sh, selected = d[9])
        
        shinyjs::disable("created_user")
        shinyjs::disable("created_date")
        shinyjs::disable("last_edited_user")
        shinyjs::disable("last_edited_date")
        
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
        survey_modal_dialog(session, NA, edit = FALSE)
        
        updateSelectizeInput(session, "survey_type", choices = rv$choices_st)
        updateSelectizeInput(session, "project", choices = rv$choices_p)
        updateSelectizeInput(session, "sharing", choices = rv$choices_sh)
        
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
        
        rv$add_or_edit <- 1
      })
      
      ##Submit edits button ----
      observeEvent(input$final_edit, {
        id <- rv$df[rv$dt_row, c("id")]
        #validate input
        req(isTruthy(input$survey) && !(input$survey %in% rv$df[which(rv$df$id != id),c("survey")]))
        req(isTruthy(input$survey_type))
        req(isTruthy(input$project))
        req(isTruthy(input$sharing))

        if(!is.null(input$current_id) && stringr::str_detect(input$current_id, pattern = "edit") && rv$add_or_edit == 0){

          #update row
          
          update <- dbGetQuery(con, paste0(
            "UPDATE records.surveys SET
            survey = '", input$survey ,"',
            survey_type = ", input$survey_type,",",
            "start_date = ", null_date_val(input$start_date),",",
            "end_date = ", null_date_val(input$end_date),",",
            "start_year = ", null_num_val(input$start_year),",",
            "end_year = ", null_num_val(input$end_year),",",
            "source = " , null_text_val(con, input$source),",",
            "project = ", input$project,",",
            "sharing = ", input$sharing,",",
            "copyright = ", null_text_val(con, input$copyright),",",
            "description = ", null_text_val(con, input$description),",", #NEED TO ESCAPE CHARACTERS
            "url = ", null_text_val(con, input$url),
            " WHERE id = ", id
            ))
          surveys_0 <- dbGetQuery(con, paste0("SELECT 
                            s.id AS id,
                            s.survey,
                            s.survey_type AS survey_type_code,
                            s.start_date,
                            s.end_date,
                            s.start_year,
                            s.end_year,
                            s.source,
                            s.project AS projectid,
                            s.sharing AS sharing_code,
                            s.copyright,
                            s.description AS description,
                            s.url,
                            s.created_user,
                            s.last_edited_user,
                            s.created_date,
                            s.last_edited_date,
                            st.description AS survey_type,
                            st.description AS sharing,
                            p.project AS project
                            FROM 
                              records.surveys s,
                              records.projects p,
                              lookups.lookup_sharing sh,
                              lookups.lookup_survey_types st
                            WHERE 
                              s.project = p.id AND
                              s.sharing = sh.code AND
                              s.survey_type = st.code AND
                              s.id = ", id)
                            )
          
          rv$df[rv$dt_row,2:20] <- surveys_0[,2:20]

          removeModal()
        }
        else{
          dbGetQuery(con,paste0(
            "INSERT INTO records.surveys VALUES ('",
            input$survey ,"',", 
            input$survey_type,",",
            null_date_val(input$start_date,"start_date"),",",
            null_date_val(input$end_date,"end_date"),",",
            null_num_val(input$start_year,"start_year"),",",
            null_num_val(input$end_year,"end_year"),",",
            null_text_val(con, input$source,"source"),",",
            "project = ", input$project,",",
            "sharing = ", input$sharing,",",
            null_text_val(con, input$copyright,"copyright"),",",
            null_text_val(con, input$description,"description"),",", #NEED TO ESCAPE CHARACTERS
            null_text_val(con, input$url,"url")
            ,")"
          ))
          
          #Refresh DT
          surveys <- dbGetQuery(con, "SELECT 
                            s.id AS id,
                            s.survey,
                            s.survey_type AS survey_type_code,
                            s.start_date,
                            s.end_date,
                            s.start_year,
                            s.end_year,
                            s.source,
                            s.project AS projectid,
                            s.sharing AS sharing_code,
                            s.copyright,
                            s.description AS description,
                            s.url,
                            s.created_user,
                            s.last_edited_user,
                            s.created_date,
                            s.last_edited_date,
                            st.description AS survey_type,
                            st.description AS sharing,
                            p.project AS project
                            FROM 
                              records.surveys s,
                              records.projects p,
                              lookups.lookup_sharing sh,
                              lookups.lookup_survey_types st
                            WHERE 
                              s.project = p.id AND
                              s.sharing = sh.code AND
                              s.survey_type = st.code
                            ")
          rv$df <- add_btns(surveys, role, "surveys")
          
          removeModal()
        }
        
          })
      
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
      })
      
      observeEvent(input$add_new_project,{
        #Validate form
        req(isTruthy(input$project_new) && !(input$project_new %in% p$project))
        
        #Insert new project
        new_project <- dbGetQuery(con, paste0("INSERT INTO records.projects (project) VALUES ('",postgresqlEscapeStrings(con,input$project_new),
                                         "') RETURNING id"))
        
        input_survey$project <- new_project$id
        
        rv$p <- dbGetQuery(con, "SELECT * FROM records.projects ORDER BY project")
        rv$choices_p <- rv$p$id
        names(rv$choices_p) <- rv$p$project
        
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
                            , edit = ifelse(rv$add_or_edit == 0, TRUE, FALSE))
        
        updateSelectizeInput(session, "survey_type", choices = rv$choices_st, selected = input_survey$survey_type)
        updateSelectizeInput(session, "project", choices = rv$choices_p, selected = input_survey$project)
        updateSelectizeInput(session, "sharing", choices = rv$choices_sh, selected = input_survey$sharing)
        
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
    }
  )
}