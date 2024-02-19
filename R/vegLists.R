vegListsUI <- function(id){
  ns <- NS(id)
  tagList(
    
    column(5,
           column(12,
                  h3("Site vegetation plot data"),
                  p("Use this tool to generate data from vegetation monitoring plots for use in future monitoring."), 
                  p("Choose a site and, optionally, one or more plots, and click 'Download data'. 
                    A comma separated value (.csv) download will be generated with the data from the specified site and plots.
                    If you do not select any plots then all plots for the site will be included in the download.")
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
                        selectizeInput(ns("plots"),
                                       label = "Plots",
                                       multiple = TRUE,
                                       choices = c(""),
                                       selected = ""
                        )
                        )
                    ),
                  actionButton(ns("run"), label = "Download data"),
                  div(downloadButton(ns("dlExport")),class="buttonHidden")
              )
           )
  )
}

vegListsServer <- function(id, tables) {
  moduleServer(
    id,
    function(input, output, session) {
      req(tables$sites)
      req(tables$plots)
      
      # Get tables & choices ----
        
        site <- tables$sites
        choices_site <- site$id
        names(choices_site) <- site$site
        
        updateSelectizeInput(session,
                             "site",
                             choices = choices_site,
                             selected = "",
                             server = FALSE,
                             options = list(
                               maxItems = 1,
                               placeholder = "Select a site",
                               onInitialize = I('function() { this.setValue(""); }')
                             ))

      plots <- tables$plots
        
      observe({
        req(input$site)
        p <- plots[plots$site %in% input$site,]
        if(nrow(p) > 0){
          choices_plots <- sort(p$plot_reference)

          updateSelectizeInput(session,
                             "plots",
                             choices = choices_plots,
                             selected = "",
                             server = FALSE,
                             options = list(
                               placeholder = "Select one or more plots",
                               onInitialize = I('function() { this.setValue(""); }')
                             ))
        }
        else{
          updateSelectizeInput(session,
                               "plots",
                               choices = c(""),
                               selected = "",
                               server = FALSE,
                               options = list(
                                 placeholder = "No plots"
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
        runjs("document.getElementById('vegLists-dlExport').click();")
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
          q1 <- paste0("SELECT d.plot_reference_visit, u.taxon_name || ' ' || IIF(u.taxon_qualifier IS NULL, '',u.taxon_qualifier) AS att,
                      d.abundance_domin AS value
                      FROM
                        spatial.monitoring_vegetation p,
                        records.plot_data d,
                        records.plot_visits v,
                        lookups.uksi u
                      WHERE
                        p.plot_reference = v.plot_reference AND
                        v.plot_reference_visit = d.plot_reference_visit AND
                        u.nbn_taxon_version_key = d.taxon_nbn AND
                        p.site = ",input$site, " ORDER BY d.plot_reference_visit, att"
                       )
          q2 <- paste0("SELECT d.plot_reference_visit, u.taxon_name || ' ' || IIF(u.taxon_qualifier IS NULL, '',u.taxon_qualifier) AS att,
                      d.abundance_domin AS value
                      FROM
                        spatial.monitoring_vegetation p,
                        records.plot_data d,
                        records.plot_visits v,
                        lookups.uksi u
                      WHERE
                        p.plot_reference = v.plot_reference AND
                        v.plot_reference_visit = d.plot_reference_visit AND
                        u.nbn_taxon_version_key = d.taxon_nbn AND ",
                        sql_in("p.plot_reference",input$plots), " 
                       ORDER BY d.plot_reference_visit, att"
                       )
          q3 <- paste0("SELECT v.plot_reference_visit, v.record_date, v.height, v.bare_ground, v.bryophyte_cover, v.pool, v.tufa, v.canopy, v.partial_shade, v.litter_cover, v.nvc, v.note, v.recorder
                          FROM 
                            spatial.monitoring_vegetation p,
                            records.plot_visits v
                          WHERE
                            p.plot_reference = v.plot_reference AND
                            p.site = ", input$site, "ORDER BY v.plot_reference_visit")
          q4 <- paste0("SELECT v.plot_reference_visit, v.record_date, v.height, v.bare_ground, v.bryophyte_cover, v.pool, v.tufa, v.canopy, v.partial_shade, v.litter_cover, v.nvc, v.note, v.recorder
                          FROM 
                            spatial.monitoring_vegetation p,
                            records.plot_visits v
                          WHERE
                            p.plot_reference = v.plot_reference AND ",
                            sql_in("p.plot_reference",input$plots)," ORDER BY v.plot_reference_visit")
          
          if(!isTruthy(input$plots)){
            Q1 <- q1
            Q2 <- q3
          }
          else{
            Q1 <- q2
            Q2 <- q4
          }
          d <- dbGetQuery(con0,Q1)
          v <- dbGetQuery(con0, Q2)
          poolReturn(con0)
          return(list("d" = d, "v" = v))
        })%...>% (function(result) {
          d <- result$d
          v <- result$v
          if(isTruthy(d) && nrow(d) > 0){
            d <- unique(d)

            v$record_date <- as.character(v$record_date)
            v2 <- gather(v, key="att",value = "value", 2:13, na.rm = TRUE)
            
            d2 <- rbind(v2,d)
            levels <- c(unique(v2$att),sort(unique(d$att)))
            d2$att <- factor(d2$att, levels = levels)
            
            p <- spread(d2, key = "plot_reference_visit", value = "value")
            
            name <- paste0(site[site$id == as.numeric(input$site), c("site")], " veg ",format(Sys.time(),format="%Y%m%d%H%M%S"))
            
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
                      h4("No vegetation data for selected site")
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