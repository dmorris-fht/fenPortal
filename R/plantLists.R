plantListsUI <- function(id){
  ns <- NS(id)
  tagList(
    withSpinner(
  
  tagList(
    
    column(5,
           column(12,
                  h3("Site plant lists"),
                  p("Use this tool to generate lists of plants for use in site monitoring."), 
                  p("Choose a site and, optionally, one or more subsites, and click 'Download lists'. 
                    A comma separated value (.csv) download will be generated listing all plants recorded from the site
                    and latest year of record for the selected site and subsite(s).")
                  ),
           column(12,
                  withSpinner(
                    div(id = ns("select"),style="height:200px",
                        selectizeInput(ns("site"),
                                       label = "Site",
                                       multiple = TRUE,
                                       choices = c(""),
                                       selected = ""
                        ),
                        selectizeInput(ns("subsite"),
                                       label = "Subsite",
                                       multiple = TRUE,
                                       choices = c(""),
                                       selected = ""
                        )
                        )
                    ),
                  actionButton(ns("run"), label = "Download lists"),
                  div(downloadButton(ns("dlExport")),class="buttonHidden")
              )
           )
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

plantListsServer <- function(id, tables) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Module initialisation ----
        isolate({
          app_tables(tables, c("sites","subsites"))
          })
        
        observe({
          req(tables$sites)
          req(tables$subsites)
  
          runjs(
            paste0(
              "$('#",id,"-module').parent().addClass('shiny-spinner-hidden');
                   $('div[data-spinner-id=\\'",id,"-module\\']').css('display','inline')"
              )
            )
          })
      
        choices_site <- reactive({
          if(isTruthy(tables$sites)){
            c <- tables$sites$id
            names(c) <- tables$sites$site
            return(c)
          }
          else{
            return(c(""))
          }
        })
        
        choices_subsite <- reactive({
          if(isTruthy(tables$sites) && isTruthy(tables$subsites)){
            if(isTruthy(input$site)){
              ss <- tables$subsites[tables$subsites$site %in% input$site,]
              c <- ss[,c("id")]
              names(c) <- ss[,c("subsite")]
            }else{
              c <- c("")
            }
            return(c)
          }
          else{
            return(c(""))
          }
        })
         
        observe({
          updateSelectizeInput(session,
                               "site",
                               choices = choices_site(),
                               selected = "",
                               server = FALSE,
                               options = list(
                                 maxItems = 1,
                                 placeholder = "Select a site",
                                 onInitialize = I('function() { this.setValue(""); }')
                               ))
        }) 
        
          
        observeEvent(input$site,{
            if(length(choices_subsite()) > 0){
              updateSelectizeInput(session,
                                   "subsite",
                                   choices = choices_subsite(),
                                   selected = "",
                                   server = FALSE,
                                   options = list(
                                     placeholder = "Select one or more subsites",
                                     onInitialize = I('function() { this.setValue(""); }')
                                   ))
            }
            else{
              updateSelectizeInput(session,
                                   "subsite",
                                   choices = c(""),
                                   selected = "",
                                   server = FALSE,
                                   options = list(
                                     placeholder = "No subsites"
                                   ))
            }
          })
          
        
          
      # Download handling ----
      
      output$dlExport <- downloadHandler(
        filename = function() {
          paste0("blank", ".csv")
        },
        content = function(file) {
          write.csv(NA, file)
        }
      )
      
      rv <- reactiveValues(dl_ready = FALSE)
      
      observeEvent(rv$dl_ready,{
        req(rv$dl_ready == TRUE)
        runjs("document.getElementById('plantLists-dlExport').click();")
        rv$dl_ready <- FALSE
      })
      
      # Validation ----
      iv <- InputValidator$new()
      iv$add_rule("site",sv_required())
      iv$enable()
      
      shinyjs::disable("run")
      
      observe({
        if(isTruthy(input$site)){
          shinyjs::enable("run")
        }else{
          shinyjs::disable("run")
        }
      })
      
      # Run query ----
      observeEvent(input$run,{
        future_promise({
          con0 <- poolCheckout(con_global)
          q1 <- paste0("SELECT r.taxon_nbn, u.taxon_name || ' ' || IIF(u.taxon_qualifier IS NULL, '',u.taxon_qualifier) AS taxon,
                      ss.subsite AS subsite, 
                      IIF(array_max(ARRAY[
                        MAX(EXTRACT(YEAR FROM r.record_date)),
                        MAX(EXTRACT(YEAR FROM r.record_date_end)),
                        MAX(r.start_year),
                        MAX(r.end_year)
                      ]) IS NULL, 
                      CAST(array_max(ARRAY[
                        MAX(EXTRACT(YEAR FROM s.start_date)),
                        MAX(EXTRACT(YEAR FROM s.end_date)),
                        MAX(s.start_year),
                        MAX(s.end_year)
                      ]) AS text), 
                      CAST(array_max(ARRAY[
                        MAX(EXTRACT(YEAR FROM r.record_date)),
                        MAX(EXTRACT(YEAR FROM r.record_date_end)),
                        MAX(r.start_year),
                        MAX(r.end_year)
                      ]) AS text)
                      ) AS year
                      FROM
                      records.records r,
                      spatial.fen_subsites ss,
                      records.surveys s,
                      lookups.uksi u
                      WHERE
                      u.nbn_taxon_version_key = r.taxon_nbn AND
                      r.subsite = ss.id AND
                      r.survey = s.id AND
                      (r.verification < 2 OR r. verification IS NULL) AND 
                      u.informal_group IN ('stonewort','flowering plant','conifer','clubmoss','hornwort','horsetail','quillwort','fern','moss','liverwort') AND
                      r.site = ",input$site," AND ",
                      sql_in("r.subsite",input$subsite),
                      " GROUP BY ss.subsite, taxon_nbn, taxon"
          )
          q2 <- paste0("SELECT r.taxon_nbn, u.taxon_name || ' ' || IIF(u.taxon_qualifier IS NULL, '',u.taxon_qualifier) AS taxon,
                      NULL AS subsite, 
                      IIF(array_max(ARRAY[
                        MAX(EXTRACT(YEAR FROM r.record_date)),
                        MAX(EXTRACT(YEAR FROM r.record_date_end)),
                        MAX(r.start_year),
                        MAX(r.end_year)
                      ]) IS NULL, 
                      CAST(array_max(ARRAY[
                        MAX(EXTRACT(YEAR FROM s.start_date)),
                        MAX(EXTRACT(YEAR FROM s.end_date)),
                        MAX(s.start_year),
                        MAX(s.end_year)
                      ]) AS text), 
                      CAST(array_max(ARRAY[
                        MAX(EXTRACT(YEAR FROM r.record_date)),
                        MAX(EXTRACT(YEAR FROM r.record_date_end)),
                        MAX(r.start_year),
                        MAX(r.end_year)
                      ]) AS text)
                      ) AS year
                      FROM
                      records.records r,
                      records.surveys s,
                      lookups.uksi u
                      WHERE
                      u.nbn_taxon_version_key = r.taxon_nbn AND
                      r.survey = s.id AND
                      (r.verification < 2 OR r. verification IS NULL) AND 
                      u.informal_group IN ('stonewort','flowering plant','conifer','clubmoss','hornwort','horsetail','quillwort','fern','moss','liverwort') AND
                      r.site = ",input$site,
                       " GROUP BY taxon_nbn, taxon"
          )
          if(!isTruthy(input$subsite)){
            q3 <- q2
          }
          else{
            q3 <- paste0("(",q1,") UNION (",q2,")")
          }
          d <- dbGetQuery(con0,q3)
          poolReturn(con0)
          return(d)
        })%...>% (function(d) {
          if(isTruthy(d) && nrow(d) > 0){
            p <- spread(d, key = "subsite", value = "year")
            name <- paste0(tables$sites[tables$sites$id == as.numeric(input$site), c("site")], " list ",format(Sys.time(),format="%Y%m%d%H%M%S"))
            
            output$dlExport <- downloadHandler(
              filename = function() {
                paste0(name, ".csv")
              },
              content = function(file) {
                tmp.path <- dirname(file)
                name.base <- file.path(tmp.path, name)
                name.glob <- paste0(name.base, ".*")
                name.csv  <- paste0(name.base, ".csv")
                
                if (length(Sys.glob(name.glob)) > 0) file.remove(Sys.glob(name.glob))
                
                write.csv(p, name.csv)
                req(file.copy(name.csv, file))
                
                if (length(Sys.glob(name.glob)) > 0) file.remove(Sys.glob(name.glob))
              }
            )
            Sys.sleep(0.5)
            rv$dl_ready <- TRUE
          }
          else{
            showModal(
              modalDialog(
                tagList(
                  div(style = "text-align:center;width:100%",
                      h4("No plant records for selected site")
                      )
                  )
                , footer=NULL,size="s",easyClose=TRUE,fade=TRUE
              )
            )
          }
            
          })
      })
      
      outputOptions(output, "dlExport")
    })}