vegManageUI <- function(id){
  ns <- NS(id)
  tagList(
    withSpinner(
      tagList(
        div(style="width:100%",
        column(12,
               column(8,
                      
                      column(7,
                             h3("Enter & manage vegetation data"),
                             h4("Plots"),
                             tabsetPanel( 
                               tabPanel("Table",id = ns("table"),
                                        div(
                                          style = "font-size:12px;padding: 10px",
                                          withSpinner(DT::DTOutput(outputId = ns("plotsTable")),type = 7,caption = "Loading plots")
                                        )
                                        ),
                               tabPanel("Map",id = ns("map"),
                                        withSpinner(leafletOutput(ns("plotsMap")),type = 7, caption = "Loading map")
                                        )
                               ),
                             actionButton(
                               inputId = ns("add_plot"),
                               label = "Add new plot",
                               icon = icon("plus")
                               )
                             ),
                      column(5,
                             h4("Plot visits"),
                             div(
                               style = "font-size:12px;padding: 10px",
                               withSpinner(DT::DTOutput(outputId = ns("visitsTable")),type = 7,caption = "Loading visits")
                             ),
                             actionButton(
                               inputId = ns("add_visit"),
                               label = "Add new plot visit",
                               icon = icon("plus")
                             )
                             
                             )
                      ),
               column(4,
                      h4("Plant species data"),
                      div(
                        style = "font-size:12px;padding: 10px",
                        withSpinner(DT::DTOutput(outputId = ns("plotDataTable")),type = 7,caption = "Loading data")
                        ),
                      column(12,
                             uiOutput(ns("taxonForm")),
                             actionButton(ns("add_taxon"),"Add species",icon=icon("plus")),
                             actionButton(ns("submitTaxa"),"Upload")
                             
                             )
                      )
               ))
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

vegManageServer <- function(id, login, tables) {
  moduleServer(
    id,
    function(input, output, session) {
      role <- login$role
      user <- login$username
      password <- login$password
      
      # Module initialisation ----
      isolate({
        app_tables(tables, c("sites","subsites","surveys","plots"))
      })
      
      observe({
        req(tables$sites0)
        req(tables$sites)
        req(tables$subsites)
        req(tables$surveys)
        req(tables$plots)
        
        runjs(
          paste0(
          "$('#",id,"-module').parent().addClass('shiny-spinner-hidden');
                 $('div[data-spinner-id=\\'",id,"-module\\']').css('display','inline')"
                 )
        )
        
      })
      
      choices_site <- reactive({
        req(tables$sites)
        c <- tables$sites$id
        names(c) <- paste0(tables$sites$site, " [",tables$sites$county, "]")
        return(c)
      })
      choices_subsite <- reactive({
        req(tables$sites)
        req(tables$subsites)
        if(!isTruthy(input$site)){
          c <- c("")
        }
        if(isTruthy(input$site)){
          ss <- tables$subsites[tables$subsites$site == as.numeric(input$site),]
          if(nrow(ss) > 0){
            c <- ss[,c("id")]
            names(c) <- paste0(ss$subsite," [",ss$site_name,"]")
          }else{
            c <- c("")
          }
        }
        return(c)
      })
      
      choices_surveys <- reactive({
        req(tables$surveys)
        c <- tables$surveys$id
        names(c) <- tables$surveys$survey
        return(c)
      })
      
      if(!grepl('c',role)){
        shinyjs::hide("add_plot")
        shinyjs::hide("add_visit")
        shinyjs::hide("add_taxon")
      }
      if(!grepl('u',role)){
        shinyjs::disable("taxon")
        shinyjs::disable("domin")
        shinyjs::disable("cover")
        shinyjs::disable("freq")
        shinyjs::disable("pres")
        shinyjs::disable("taxon_note")
        shinyjs::hide("submitTaxa")
      }
      
      source("./R/modals/veg_modal.R")
      
      # Reactive to hold data ----
      
      rv <- reactiveValues(
        df = NULL,
        p = NULL, # selected plot
        df_v = NULL,
        v = NULL, # selected visit
        v_guid = NULL, # visit guid, needed to handle photos
        ph = NULL, # selected photo
        df_ph = NULL, # photos
        df_ph_v = NULL, # temp storage of photos of selected visit
        df_d = data.frame(
          taxon_nbn = character(),
          abundance_domin = integer(),
          abundance_cover = integer(),
          abundance_presence = integer(),
          abundance_frequency = numeric(),
          abundance = numeric(),
          note = character(),
          created_date = character(),
          created_user = character(),
          last_edited_date = character(),
          last_edited_user = character(),
          guid = character(),
          taxon_name = character(),
          edit = integer(),
          Buttons = character()
        ),
        dt_row = NULL,
        i = 0,
        sp_mode = 0,
        add_or_edit = 0,
        edit_button = NULL
        )
      

      observe({
          req(tables$plots)
          x <- add_btns(st_drop_geometry(tables$plots), role, "plots")
          rv$df <- x[order(x$site_name,x$plot),]
        })
      
      # Plots DT ----
      
      output$plotsTable <- DT::renderDT({
        
        data.frame(
          "site_name" = character(),
          "plot" = character(),
          "Buttons" = character()
                   )
        },
          server = TRUE,
          escape = FALSE,
          rownames = FALSE,
          selection = 'single',
          filter = list(position='top'),
          colnames = c("Site","Plot",""),
          options = list(
                        pageLength = 25,
                        dom = 'tpli',
                        processing = TRUE,
                         columnDefs = list(
                           list(className = 'dt-center', targets = 1),
                           list(targets = c(2),searchable = FALSE),
                           list(orderable = FALSE, targets = c(2)),
                           list(width = '60px',targets=c(2))
                         ),
                         extensions = c("FixedHeader", "Scroller"),
                         fixedHeader = TRUE,
                         scrollY = "50vh"
          )
        )
      
      proxy_DT <- DT::dataTableProxy("plotsTable")
      
      observe({
        req(rv$df)
        x <- rv$df[,c("site_name","plot","Buttons")]
        proxy_DT %>% 
          DT::replaceData(data = x, resetPaging = FALSE, rownames = FALSE) %>% 
          updateFilters(data = x)
      })
      
      # Visits DT ----
      
      output$visitsTable <- DT::renderDT({
        data.frame("visit" = integer(),"record_date"=Date(),"Buttons" = character())
        },
        server = TRUE,
        escape = FALSE,
        rownames = FALSE,
        selection = 'single',
        filter = list(position='top'),
        colnames = c("Visit","Date",""),
        options = list(
          pageLength = 25,
          dom = 'tpi',
          language = list(
            infoEmpty = "No plot visits",
            emptyTable = "No plot visits recorded"
          ),
          processing = TRUE,
          columnDefs = list(
            list(className = 'dt-center', targets = 0),
            list(targets = c(2),searchable = FALSE),
            list(orderable = FALSE, targets = c(2)),
            list(width = '60px',targets=c(2))
          ),
          extensions = c("FixedHeader", "Scroller"),
          fixedHeader = TRUE,
          scrollY = "50vh"
          )
        )
      
      proxy_DT_v <- DT::dataTableProxy("visitsTable")
      
      observe({
        req(rv$df_v)
        req(rv$p)
        x <- rv$df_v[rv$df_v$plot_reference == rv$p,c("visit","record_date","Buttons")]
        proxy_DT_v %>%
          DT::replaceData(data = x, resetPaging = FALSE, rownames = FALSE) %>%
          updateFilters(data = x)
      })
      
      # Species DT ----
      
      output$plotDataTable <- DT::renderDT({
        data.frame("taxon_name" = character(),"abundance" = character(),"Buttons" = character())
      }
      ,
      server = TRUE,
      escape = FALSE,
      rownames = FALSE,
      selection = 'single',
      colnames = c("Taxon","Abundance",""),
      options = list(
        pageLength = 25,
        dom = 'tlpi',
        language = list(
          infoEmpty = "No data",
          emptyTable = "No species recorded"
        ),
        processing = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = 1),
          list(orderable = FALSE, targets = c(2)),
          list(width = '10px',targets=c(2))
        ),
        extensions = c("FixedHeader", "Scroller"),
        fixedHeader = TRUE,
        scrollY = "30vh"
      )
      )
      
      proxy_DT_d <- DT::dataTableProxy("plotDataTable")
      
      observe({
        req(rv$df_d)
        req(rv$v)
        x <- rv$df_d[rv$df_d$plot_reference_visit == rv$v,c("taxon_name","abundance","Buttons")]
        proxy_DT_d %>%
          DT::replaceData(data = x, resetPaging = FALSE, rownames = FALSE) %>%
          updateFilters(data = x)
      })
      
      # Plot select ----
      selectPlot <- function(p){
        q <- paste0("SELECT id, 
                            plot_reference,
                            visit,
                            survey,
                            plot_reference_visit,
                            record_date,
                            record_date_end,
                            height,
                            bare_ground,
                            bryophyte_cover,
                            litter_cover,
                            canopy,
                            partial_shade,
                            tufa,
                            pool,
                            nvc,
                            note,
                            recorder,
                            created_user,
                            created_date,
                            last_edited_user,
                            last_edited_date,
                            guid 
                            FROM records.plot_visits 
                            WHERE plot_reference = '",p,"' 
                            ORDER BY visit"
        )
        showSpinner("visitsTable")
        if(isTruthy(rv$df_v)){
          if(p %in% rv$df_v$plot_reference){
            rv$p <- p
            hideSpinner("visitsTable")
          }
          if(!(p %in% rv$df_v$plot_reference)){
            future_promise({
              con0 <- poolCheckout(con_global)
              result <- dbGetQuery(con0,q)
              poolReturn(con0)
              return(result)
            })%...>% (function(r){
              rv$df_v <- rbind(rv$df_v,add_btns(r,role,"visits"))
              rv$p <- p
              hideSpinner("visitsTable")
            })
          }
        }
        if(!isTruthy(rv$df_v)){
          future_promise({
            con0 <- poolCheckout(con_global)
            result <- dbGetQuery(con0,q)
            poolReturn(con0)
            return(result)
          })%...>% (function(r){
            rv$df_v <- add_btns(r,role,"visits")
            rv$p <- p
            hideSpinner("visitsTable")
          })
        }
        }
      
      observeEvent(input$plotsTable_rows_selected,{
        rv$v <- "000"
        rv$i <- 0
        p <- rv$df[input$plotsTable_rows_selected,c("plot_reference")]
        selectPlot(p)
      })
      
      observeEvent(input$plotsMap_marker_click,{
        rv$v <- "000"
        p <- input$plotsMap_marker_click$id
        if(p %in% rv$df$plot_reference){
          selectPlot(p)
        }
      })
      
      
      # Visit select ----
      observeEvent(input$visitsTable_rows_selected,{
        v <- rv$df_v[rv$df_v$plot_reference == rv$p,][input$visitsTable_rows_selected,c("plot_reference_visit")]
        rv$i <- 0
        q <- paste0("SELECT d.id, plot_reference_visit,
                      taxon_nbn,
                      abundance_domin, abundance_cover, abundance_presence::integer, abundance_frequency,
                      COALESCE(abundance_domin, abundance_cover,abundance_presence::integer, abundance_frequency) AS abundance,
                      note,
                      d.created_date,d.created_user,d.last_edited_date,d.last_edited_user,
                      d.guid,
                      u.recommended_scientific_name || COALESCE(' ' || u.recommended_name_qualifier, '') AS taxon_name
                      FROM records.plot_data d,
                            lookups.uksi u
                      WHERE plot_reference_visit = '",v,"' AND u.nbn_taxon_version_key = d.taxon_nbn
                    ORDER BY taxon_name, abundance"
                    )
        showSpinner("plotDataTable")
        if(isTruthy(rv$df_d)){
          if(v %in% rv$df_d$plot_reference_visit){
            rv$v <- v
            hideSpinner("plotDataTable")
          }
          if(!(v %in% rv$df_d$plot_reference_visit)){
            future_promise({
              con0 <- poolCheckout(con_global)
              result <- dbGetQuery(con0,q)
              poolReturn(con0)
              return(result)
            })%...>% (function(r){
              if(nrow(r) > 0){
                r$edit <- NA
                r$Buttons <- ""
              }else{
                r[1,] <- NA
                r$edit <- NA
                r$Buttons <- ""
                r <- r[0,]
              }
              
              rv$df_d <- rbind(rv$df_d,r)
              rv$v <- v
              hideSpinner("plotDataTable")
            })
          }
        }
        if(!isTruthy(rv$df_d)){
          future_promise({
            con0 <- poolCheckout(con_global)
            result <- dbGetQuery(con0,q)
            poolReturn(con0)
            return(result)
          })%...>% (function(r){
            if(nrow(r) > 0){
              r$edit <- NA
              r$Buttons <- ""
            }else{
              r[1,] <- NA
              r$edit <- NA
              r$Buttons <- ""
              r <- r[0,]
            }
            
            rv$df_d <- r
            rv$v <- v
            hideSpinner("plotDataTable")
          })
        }
        
        # Remove any rows that have blank species entries
        rv$df_d <- rv$df_d[which(
          !is.na(rv$df_df$taxon_nbn) & (!is.na(rv$df_df$abundance_domin) | !is.na(rv$df_df$abundance_cover) | !is.na(rv$df_df$abundance_freq) | !is.na(rv$df_df$abundance_pres))
          ),]
      })
      
      # Plots map ----
      
      output$plotsMap <- renderLeaflet({
        map <- leaflet() %>%
          addTiles(group="OpenStreetMap.Mapnik") %>% 
          addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
          addProviderTiles("OpenStreetMap.HOT", group = "OpenStreetMap.HOT") %>%
          addMiniMap(tiles = "OpenStreetMap.HOT", toggleDisplay = TRUE) %>%
          addSearchOSM() %>%
          addEasyButton(easyButton(icon="fa-home", title="Home view", onClick=JS("function(btn, map){ map.fitBounds([[-5.515982,50.024189],[1.35876,55.948577]]); }"))) %>%
          addEasyButton(easyButton(icon="fa-crosshairs", title="Locate Me", onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
          addMeasure() %>%
          addFullscreenControl() %>%
          addScaleBar(position = c("bottomleft")) %>%
          addLayersControl(baseGroups = c("OpenStreetMap.Mapnik","Satellite","OpenStreetMap.HOT","Stamen.TerrainBackground"),
                           options = layersControlOptions(collapsed = TRUE)) %>%
          fitBounds(-5.515982,50.024189,1.35876,55.948577)
          # addDrawToolbar(
          #   position = "bottomleft",
          #   polylineOptions = FALSE,
          #   polygonOptions = FALSE, #drawPolygonOptions(),
          #   circleOptions = FALSE,
          #   rectangleOptions = drawRectangleOptions(),
          #   markerOptions = FALSE,
          #   circleMarkerOptions = FALSE, editOptions = FALSE,
          #   singleFeature = TRUE
          # )
        
        map
      })
      
      proxy_map <- leafletProxy("plotsMap")
      
      observe({
        req(tables$sites0)
        req(tables$plots)
        
        sites1 <- tables$sites0 %>% filter(!st_is_empty(.)) %>% st_transform(crs = 4326) # Filter out empty geoms and transform to 4326
        
        bbox <- st_bbox(tables$plots)
        proxy_map %>% 
          fitBounds(bbox[[1]],bbox[[2]],bbox[[3]],bbox[[4]]) %>%
          addPolygons(data = sites1[sites1$id %in% tables$plots$site,], 
                      group = "sites", 
                      color = "red", weight = 1, opacity = 1, fillOpacity = 0, label = ~site) %>%
          addCircleMarkers(data = tables$plots,
                           radius = 3,
                           stroke = TRUE,
                           color = "black",
                           weight = 0.5,
                           opacity = 1,
                           fill = TRUE,
                           fillColor =  'green',
                           fillOpacity = 1,
                           popup = NA,
                           label = tables$plots$plot,
                           layerId = tables$plots$plot_reference
                           )
        
        future_promise({
          con0 <- poolCheckout(con_global)
          p <- st_read(dsn = con0, query = "SELECT ST_TRANSFORM(geom,4326), plot_reference FROM spatial.monitoring_vegetation_plots")
          poolReturn(con0)
          return(p)
        })%...>%(function(p){
          proxy_map %>% addPolygons(data = p, 
                                    fill = FALSE, 
                                    stroke = TRUE, 
                                    color = "black", 
                                    weight = 0.8,
                                    label = p$plot_reference
                                    )
        })
      })
      
      # Modal for submit message ----
      submitModal <- function(){
        ns <- session$ns
        modalDialog(
          div(style="text-align:left",
              uiOutput(ns("submitMessage")),
          )
          ,footer=NULL,size="s",easyClose=TRUE,fade=TRUE
        ) %>% showModal()
      }
      
      # Add plot ----
      
      # Modal
      observeEvent(input$add_plot,{
        plot_modal(session,d = NULL,edit = TRUE, choices_site())
        
        observeEvent(input$site,{
                     if(length(choices_subsite()) > 0){
                       updateSelectizeInput(session,"subsite",
                                            choices = choices_subsite(),
                                            selected = "")
                     }else{
                       updateSelectizeInput(session,"subsite",
                                            choices = choices_subsite(),
                                            selected = "",
                                            options=list(placeholder = "No subsites"))
                       }
          }
        )
                     
        shinyjs::hide("created_user")
        shinyjs::hide("created_date")
        shinyjs::hide("last_edited_user")
        shinyjs::hide("last_edited_date")

        shinyjs::disable("submit_p")
        
        ## Modal validation
        iv_modal <- InputValidator$new()
        iv_modal$add_rule("site",sv_required())
        iv_modal$add_rule("plot",sv_required())
        iv_modal$add_rule("gridref",sv_required())
        iv_modal$add_rule("gridref",function(value){
          if(nchar(gsub(" ", "",input$gridref)) != 12){
            "Enter ten-figure grid reference"
          }
        })
        iv_modal$add_rule("gridref",function(value){
          v <- validate_gf(input$gridref, 
                           s = as.numeric(input$site), 
                           ss = as.numeric(input$subsites),
                           sites0 = tables$sites0,
                           subsites0 = tables$subsites0)
          if(v$error == 1 && isTruthy(input$gridref)){
            v$message
          }
        })
        iv_modal$add_rule("plot",function(value){
          if(input$plot %in% rv$df[rv$df$site == as.numeric(input$site),c("plot")]){
            "Plot name is not unique"
            }
        })
        iv_modal$enable()
        
        observe({
          req(input$site)
          req(input$gridref)
          req(input$plot)
          
          if(
            nchar(gsub(" ", "",input$gridref)) == 12 &&
            validate_gf(input$gridref, 
                        s = as.numeric(input$site), 
                        ss = as.numeric(input$subsites),
                        sites0 = tables$sites0,
                        subsites0 = tables$subsites0)$error == 0 &&
            !(input$plot %in% tables$plots[tables$plots$site == as.numeric(input$site),c("plot")])
          ){
            shinyjs::enable("submit_p")
          }else{
            shinyjs::disable("submit_p")
          }
          
          })
        
        rv$add_or_edit <- 1
      })
      
      # Submit new plot / edits
      observeEvent(input$submit_p,{
        # Validation
        req(input$site)
        req(input$gridref)
        req(input$plot)
        req(nchar(gsub(" ", "",input$gridref)) == 12 &&
              validate_gf(input$gridref, 
                          s = as.numeric(input$site), 
                          ss = as.numeric(input$subsites),
                          sites0 = tables$sites0,
                          subsites0 = tables$subsites0)$error == 0)
        showModal(
          modalDialog(
            div(style="text-align:left;width:60%",
                tags$h4("Submitting",class="loading"),
            )
            ,footer=NULL,size="s",easyClose=FALSE,fade=TRUE
          )
        )
        
        # Update
        if(!is.null(input$current_id) && stringr::str_detect(input$current_id, pattern = "edit") && rv$add_or_edit == 0){
          id <- rv$df[rv$dt_row, c("id")]
          req(!(input$plot %in% rv$df[rv$df$site == as.numeric(input$site) & rv$df$id != id,c("plot")]))
          
          future_promise({
            con0 <- fenDb0(user,password)
            q <- paste0("UPDATE spatial.monitoring_vegetation
                        SET
                          site = ",as.numeric(input$site),",
                          subsite = ",null_num_val(input$subsite),",
                          transect = ",null_text_val(con0,input$transect),",
                          plot = ",null_text_val(con0,input$plot),",
                          gridref = ",null_text_val(con0,gsub(" ","",toupper(input$gridref))),",
                          transect_side = ",null_text_val(con0,input$transect_side),",
                          dim = ",null_text_val(con0,input$dim),",
                          note = ",null_text_val(con0,input$note)," 
                        WHERE id = ",id,
                      " RETURNING 
                          id,
                          ST_AsText(ST_TRANSFORM(geom, 4326)) AS geom,
                          site AS site,
                          subsite AS subsite,
                          transect,
                          plot,
                          plot_reference,
                          gridref,
                          transect_side,
                          dim,
                          note,
                          created_user,
                          created_date,
                          last_edited_user,
                          last_edited_date")
            r <- list(error = NA, data = NA)
            tryCatch({
              r0 <- postgresqlExecStatement(con0, q)
              r1 <- postgresqlFetch(r0)
              dbDisconnect(con0)
              r$data <- r1
            },
            error=function(err){
              dbDisconnect(con0)
              r$error <- err
            }
            )
            return(r)
          })%...>%(function(r){
            if(isTruthy(r$error)){
              submitModal()
              output$submitMessage <- renderUI({
                tagList(
                  h4("An error occured"),
                  p(r$error)
                )
              })
            }else{
              u <- r$data
              u$site_name <- tables$sites[tables$sites$id == u$site, c("site")]
              if(isTruthy(u$subsite)){
                u$subsite_name <- tables$subsites[tables$subsites$id == u$subsite,c("subsite")]
              }else{
                u$subsite_name <- NA
              }
              row <- st_as_sf(u,wkt = "geom",crs=st_crs(4326))
              tables$plots[tables$plots$id == id,] <- row
              
              submitModal()
              output$submitMessage <- renderUI({
                tagList(
                  h4("Plot updated!")
                )
              })
            }
          })
        }else{
          # Add new plot
          req(!(input$plot %in% tables$plots[tables$plots$site == as.numeric(input$site),c("plot")]))
          
          future_promise({
            con0 <- fenDb0(user,password)
            q <- paste0("INSERT INTO spatial.monitoring_vegetation 
                        (site,subsite,transect,plot,gridref,transect_side,dim,note)
                        VALUES (",
                          as.numeric(input$site),",",
                          null_num_val(input$subsite),",",
                          null_text_val(con0,input$transect),",",
                          null_text_val(con0,input$plot),",",
                          null_text_val(con0,gsub(" ","",toupper(input$gridref))),",",
                          null_text_val(con0,input$transect_side),",",
                          null_text_val(con0,input$dim),",",
                          null_text_val(con0,input$note),") 
                         RETURNING 
                          id,
                          ST_AsText(ST_TRANSFORM(geom, 4326)) AS geom,
                          site AS site,
                          subsite AS subsite,
                          transect,
                          plot,
                          plot_reference,
                          gridref,
                          transect_side,
                          dim,
                          note,
                          created_user,
                          created_date")
            r <- list(error = NA, data = NA, query = q)
            tryCatch({
              r0 <- postgresqlExecStatement(con0, q)
              r1 <- postgresqlFetch(r0)
              dbDisconnect(con0)
              r$data <- r1
            },
            error=function(err){
              dbDisconnect(con0)
              r$error <- err
            }
            )
            return(r)
          })%...>%(function(r){
            if(isTruthy(r$error)){
              submitModal()
              output$submitMessage <- renderUI({
                tagList(
                  h4("An error occured"),
                  p(r$error)
                )
              })
            }else{
              i <- r$data
              i$last_edited_user <- NA
              i$last_edited_date <- NA
              i$site_name <- tables$sites[tables$sites$id == i$site, c("site")]
              if(isTruthy(i$subsite)){
                i$subsite_name <- tables$subsites[tables$subsites$id == i$subsite,c("subsite")]
              }else{
                i$subsite_name <- NA
              }
              
              row <- st_as_sf(i,wkt = "geom",crs=st_crs(4326))
              tables$plots <- rbind(tables$plots,row, deparse.level = 1)
              
              submitModal()
              output$submitMessage <- renderUI({
                tagList(
                  h4("Plot created!")
                )
              })
            }
          })
        }
      })
      
      # Add visit ----
      
      # Activate add button
      observe({
        if(isTruthy(rv$p)){
          shinyjs::enable("add_visit")
        }else{
          shinyjs::disable("add_visit")
        }
      })
      
      # Modal
      observeEvent(input$add_visit,{
        # Next visit number
        v <- 1 
        if(isTruthy(rv$df_v[rv$df_v$plot_reference == rv$p,c("visit")])){
          v <- 1 + max(rv$df_v[rv$df_v$plot_reference == rv$p,c("visit")])
        }
        # Date of last visit
        vd0 <- NULL
        if(isTruthy(rv$df_v[rv$df_v$plot_reference == rv$p,c("record_date")])){
          vd0 <- max(unlist(rv$df_v[rv$df_v$plot_reference == rv$p,c("record_date")]))
        }
        # New guid
        rv$v_guid <- UUIDgenerate()
        
        visit_modal(session,rv$p,v,vd0,vd1 = NULL,d = NULL,edit = TRUE,choices_surveys())
        photoHandler()
        output$photo <- renderUI({p("No photo selected")})
        
        # Validation
        
        iv_modal <- InputValidator$new()
        iv_modal$add_rule("recorder",sv_required())
        iv_modal$add_rule("survey",sv_required())
        iv_modal$add_rule("record_date",sv_required())
        # iv_modal$add_rule("height",sv_required()) # Disabled these for when plot cannot be recorded
        # iv_modal$add_rule("bare_ground",sv_required())
        # iv_modal$add_rule("bryophyte_cover",sv_required())
        # iv_modal$add_rule("litter_cover",sv_required())
        iv_modal$enable()
        
        observe({
          if(
            isTruthy(input$recorder) && 
            isTruthy(input$survey) && 
            isTruthy(input$record_date)  
            # && isTruthy(input$height) && 
            # isTruthy(input$bare_ground) && 
            # isTruthy(input$bryophyte_cover) && 
            # isTruthy(input$litter_cover)
            ){
              shinyjs::enable("submit_v")
            }else{
              shinyjs::disable("submit_v")
            }
        })
        
        rv$add_or_edit <- 1
      })
      
      # Detect changes in visit modal
      change <- reactive({
        req(rv$df_v)
        req(rv$df_ph_v)
        req(isTruthy(rv$v) && rv$v != "000" )
        
        c_v <- 0
        c_ph <- 0
        
        v <- rv$df_v[rv$df_v$plot_reference_visit == rv$v,]

        if(
          !compareNA(v$recorder, input$recorder) ||
          !compareNA(v$survey, as.numeric(input$survey)) ||
          !compareNA(v$record_date, input$record_date) ||
          !compareNA(v$record_date_end,blank(input$record_date_end)) ||
          !compareNA(v$note,blank(input$note)) ||
          !compareNA(v$height,as.numeric(blank(input$height))) ||
          !compareNA(v$bare_ground,as.numeric(blank(input$bare_ground))) ||
          !compareNA(v$bryophyte_cover,as.numeric(blank(input$bryophyte_cover))) ||
          !compareNA(v$litter_cover,as.numeric(blank(input$litter_cover))) ||
          !compareNA(v$nvc,blank(input$nvc))
          ){
          c_v <- 1
        }

        if(sum(!is.na(rv$df_ph_v$edit)) > 0){
          c_ph <- 1
        }
        return(c_v + c_ph)
      })
      
      # Submit new/edit visit
      observeEvent(input$submit_v,{
        req(input$recorder)
        req(input$survey)
        req(input$record_date)
        # req(input$height)
        # req(input$bare_ground)
        # req(input$bryophyte_cover)
        # req(input$litter_cover)

        showModal(
          modalDialog(
            div(style="text-align:left;width:60%",
                tags$h4("Submitting",class="loading"),
            )
            ,footer=NULL,size="s",easyClose=FALSE,fade=TRUE
          )
        )
        
        # Update
        if(!is.null(input$current_id) && stringr::str_detect(input$current_id, pattern = "edit") && rv$add_or_edit == 0){
          req(change() > 0)
          v <- rv$df_ph_v[!is.na(rv$df_ph_v$edit),] # Edited photos & remove id col
          v <- v[,-c(1)]
          
          future_promise({
            con0 <- fenDb0(user,password)
            q <- paste0("UPDATE records.plot_visits
                        SET
                            survey = ",as.numeric(input$survey),",
                            record_date = ", null_date_val(input$record_date),",
                            record_date_end = ", null_date_val(input$record_date_end),",
                            height = ",null_num_val(input$height),",
                            bare_ground = ",null_num_val(input$bare_ground),",
                            bryophyte_cover = ",null_num_val(input$bryophyte_cover),",
                            litter_cover = ",null_num_val(input$litter_cover),",
                            nvc = ",null_text_val(con0,input$nvc),",
                            note = ",null_text_val(con0,input$note),",
                            recorder = ",null_text_val(con0,input$recorder),"
                        WHERE plot_reference_visit = '",rv$v,"' 
                        RETURNING 
                            id, 
                            plot_reference,
                            visit,
                            survey,
                            plot_reference_visit,
                            record_date,
                            record_date_end,
                            height,
                            bare_ground,
                            bryophyte_cover,
                            litter_cover,
                            canopy,
                            partial_shade,
                            tufa,
                            pool,
                            nvc,
                            note,
                            recorder,
                            created_user,
                            created_date,
                            last_edited_user,
                            last_edited_date,
                            guid")
            
            r <- list(error = NA, visits = NA, photos = NA, photos_u = NA)
              
            r$visits <- dbGetQuery(con0,q)

            if(isTruthy(v) && nrow(v) > 0){
              ph <- which(!is.na(v$file)) # New photos
              ed <- which(is.na(v$file))
              u1 <- TRUE
              u2 <- TRUE
              if(isTruthy(ph)){        # Compress new photos
                d <- 2000
                sz <- 500

                for(i in 1:length(ph)){
                  f <- v[i,c("file")]
                  img <- image_read(f)
                  if(image_info(img)$filesize > sz * 1024){
                    g <- magick::geometry_size_pixels(width = d, height = d, preserve_aspect = TRUE)
                    image_write(image_resize(img,geometry =g),f,quality=20, compression = "JPEG")
                    v[i,c("att_size")] <- image_info(image_read(f))$filesize
                  }
                  
                  z <- readRaw(f)
                  v[i,c("att")] <- paste0("\\x", paste(z$fileRaw, collapse = ""))
                  
                  v[i,c("att64")] <- base64encode(f)
                  v[i,c("file")] <- NA
                }
                u1 <- pgWriteGeom(con0,
                                  name = c("records","plot_visits_attach"),
                                  data.obj = v[ph,],
                                  partial.match = TRUE,
                                  overwrite = FALSE,
                                  upsert.using = "guid"
                                  )
              }
              if(isTruthy(ed)){
                u2 <- pgWriteGeom(con0,
                                  name = c("records","plot_visits_attach"),
                                  data.obj = v[ed,-which(colnames(v) %in% c("att"))],
                                  partial.match = TRUE,
                                  overwrite = FALSE,
                                  upsert.using = "guid"
                )
              }
              
              v$att <- NA # Remove binary data so just storing base64 data
              r$photos <- v
              r$photos_u <- u1*u2
              }
            
            dbDisconnect(con0)
            return(r)
          })%...>%(function(r){
            if(isTruthy(r$error)){
              submitModal()
              output$submitMessage <- renderUI({
                tagList(
                  h4("An error occured"),
                  p(r$error)
                )
              })
            }else{
              u <- r$visits
              rv$df_v[rv$df_v$plot_reference_visit == rv$v,c(1:ncol(u))] <- u
              
              submitModal()
              output$submitMessage <- renderUI({
                tagList(
                  div(style="width:100%;text-align:center",
                      h4("Plot visit updated!")
                  )
                )
              })
              
              # Update photo reactive
              if(isTruthy(r$photos_u)){
                v <- r$photos
                if(isTruthy(rv$df_ph)){
                  rv$df_ph <- rv$df_ph[-c(which(rv$df_ph$guid %in% v$guid)),] # Remove edited rows from whole photo dataset
                  rv$df_ph <- rbind(rv$df_ph,v[,-which(colnames(rv$df_ph) %in% c("Buttons","edit"))]) # Add edited rows back in
                  }else{
                  rv$df_ph <- v[,-which(colnames(rv$df_ph) %in% c("Buttons","edit"))]
                  }
                }
              }
          })
          }else{
        # New visit
            v <- rv$df_ph_v[,-c(1)] # Photos to insert, remove id column
            future_promise({
              con0 <- fenDb0(user,password)
              q0 <- paste0("INSERT INTO records.plot_visits 
                        (plot_reference,visit,survey,record_date,record_date_end,
                        height,bare_ground,bryophyte_cover,litter_cover,nvc,note,recorder,guid)
                        VALUES ('",rv$p,"',",
                          as.numeric(input$visit),",",
                          as.numeric(input$survey),",",
                          null_date_val(input$record_date),",",
                          null_date_val(input$record_date_end),",",
                          null_num_val(input$height),",",
                          null_num_val(input$bare_ground),",",
                          null_num_val(input$bryophyte_cover),",",
                          null_num_val(input$litter_cover),",",
                          null_text_val(con0,input$nvc),",",
                          null_text_val(con0,input$note),",",
                          null_text_val(con0,input$recorder),",'",UUIDgenerate(),"')
                         RETURNING 
                            id, 
                            plot_reference,
                            visit,
                            survey,
                            plot_reference_visit,
                            record_date,
                            record_date_end,
                            height,
                            bare_ground,
                            bryophyte_cover,
                            litter_cover,
                            canopy,
                            partial_shade,
                            tufa,
                            pool,
                            nvc,
                            note,
                            recorder,
                            created_user,
                            created_date,
                            last_edited_user,
                            last_edited_date,
                            guid")
              r <- list(error = NA, visits = NA, photos = NA, photos_u = NA)
              
              # Insert the new visit and photos
              if(isTruthy(v) && nrow(v) > 0){
                # query string
                q1 <- paste0("WITH v AS (",q0,") ","\n ","INSERT INTO records.plot_visits_attach (guid,rel_guid,att_type,att_name,att_size,att_note,att)
                        VALUES ")
                
                # Compress photos
                d <- 2000
                sz <- 500
                
                Q <- NULL
                for(i in 1:nrow(v)){
                  f <- v[i,c("file")]
                  img <- image_read(f)
                  if(image_info(img)$filesize > sz * 1024){
                    g <- magick::geometry_size_pixels(width = d, height = d, preserve_aspect = TRUE)
                    image_write(image_resize(img,geometry =g),f,quality=20, compression = "JPEG")
                    v[i,c("att_size")] <- image_info(image_read(f))$filesize
                  }
                  
                  z <- readRaw(f)
                  v[i,c("att")] <- paste0("\\x", paste(z$fileRaw, collapse = ""))
                  
                  v[i,c("att64")] <- base64encode(f)
                  v[i,c("file")] <- NA
                  
                  # Construct query string
                  Q <- c(Q,
                         paste0("('",v[i,c("guid")],"',(SELECT guid FROM v),'",
                              v[i,c("att_type")],"','",v[i,c("att_name")],"',",v[i,c("att_size")],",",null_text_val(con0,v[i,c("att_note")])
                              ,",'", v[i,c("att")] ,"')")
                         )
                  }
                
                q2 <- paste0(q1,paste(Q,collapse=", \n ")," RETURNING (SELECT id FROM v)")

                r$visits <- dbGetQuery(con0,q2)

                v$att <- NA # Remove binary data so just storing base64 data
                r$photos <- v
                
              }else{
                # Insert the new visit only
                r$visits <- dbGetQuery(con0,q0)
              }

              dbDisconnect(con0)
              return(r)
            })%...>%(function(r){
              if(!isTruthy(r$visits)){
                submitModal()
                output$submitMessage <- renderUI({
                  tagList(
                    h4("An error occured")
                  )
                })
              }else{
                con0 <- poolCheckout(con_global)
                row <- dbGetQuery(con0,paste0("
                          SELECT
                            id, 
                            plot_reference,
                            visit,
                            survey,
                            plot_reference_visit,
                            record_date,
                            record_date_end,
                            height,
                            bare_ground,
                            bryophyte_cover,
                            litter_cover,
                            canopy,
                            partial_shade,
                            tufa,
                            pool,
                            nvc,
                            note,
                            recorder,
                            created_user,
                            created_date,
                            last_edited_user,
                            last_edited_date,
                            guid
                          FROM records.plot_visits WHERE id = ",r$visits))
                i <- add_btns(row,role,"visits")
                rv$df_v <- rbind(rv$df_v, i)
                
                submitModal()
                output$submitMessage <- renderUI({
                  tagList(
                    h4("Plot visit submitted!")
                  )
                })

                # Update photo reactive
                  v <- r$photos
                  if(isTruthy(v) && nrow(v) > 0){
                    if(isTruthy(rv$df_ph)){
                      rv$df_ph <- rbind(rv$df_ph,v[,-which(colnames(rv$df_ph) %in% c("Buttons","edit"))])
                    }else{
                      rv$df_ph <- v[,-which(colnames(rv$df_ph) %in% c("Buttons","edit"))]
                    }
                  }
                  poolReturn(con0)
              }
            })
            }
      })
      
      # Species select and add / edit----

      ## Species form render function
      taxonForm <- function(session,mode,t){
        ns <- session$ns
        
        if(mode > 0){
          output$taxonForm <- renderUI({
            tagList(
              tags$hr(),
              column(12,
                     div(style="float:left;width:95%",
                         selectizeInput(ns("taxon"),
                                        "Taxon",
                                        choices = c(""),
                                        selected = "",
                                        multiple = TRUE,
                                        options = list(maxItems = 1)
                         )
                     )
              ),
              column(3,
                     div(style="float:left;width:90%",
                         numericInput(ns("domin"),
                                      "Domin"
                                      ,
                                      min = 0,
                                      max = 10,
                                      step = 1,
                                      value = t$abundance_domin
                         )
                     )
              ),
              column(3,
                     div(style="float:left;width:90%",
                         numericInput(ns("cover"),
                                      "% cover",
                                      min = 0,
                                      max = 100,
                                      step = 1,
                                      value = t$abundance_cover
                         )
                     )
              ),
              column(3,
                     div(style="float:left;width:90%",
                         numericInput(ns("freq"),
                                      "% frequency",
                                      min = 0,
                                      max = 100,
                                      step = 1,
                                      value = t$abundance_freq
                         )
                     )
              ),
              column(3,
                     div(style="float:left;width:90%",
                         numericInput(ns("pres"),
                                      "Presence / absence",
                                      min = 0,
                                      max = 1,
                                      step = 1,
                                      value = t$abundance_presence
                         )
                     )
              ),
              column(12,
                     div(style="float:left;width:95%",
                         textAreaInput(ns("taxon_note"),
                                       "Note",
                                       value = ifelse(!is.na(t$note),t$note,""),
                                       resize = "none",
                                       height = "50px")
                     )
              )
            )
          })

          updateSelectizeInput(session,"taxon",choices = choices_uksi_plants,selected = t$taxon_nbn,server=TRUE)
          iv_t <- InputValidator$new()
          iv_t$add_rule("taxon",sv_required())
          iv_t$add_rule("abundance_domin",function(value){
            if(!((isTruthy(input$domin) && input$domin > 0) || 
                 (isTruthy(input$cover) && input$cover > 0) || 
                 (isTruthy(input$freq) && input$freq > 0) || 
                 (isTruthy(input$pres) && input$pres > 0))){
              return('Add abundance')
              }
            }
          )
          iv_t$add_rule("abundance_cover",function(value){
            if(!((isTruthy(input$domin) && input$domin > 0) || 
                 (isTruthy(input$cover) && input$cover > 0) || 
                 (isTruthy(input$freq) && input$freq > 0) || 
                 (isTruthy(input$pres) && input$pres > 0))){
              return('Add abundance')
            }
          }
          )
          iv_t$add_rule("abundance_freq",function(value){
            if(!((isTruthy(input$domin) && input$domin > 0) || 
                 (isTruthy(input$cover) && input$cover > 0) || 
                 (isTruthy(input$freq) && input$freq > 0) || 
                 (isTruthy(input$pres) && input$pres > 0))){
              return('Add abundance')
            }
          }
          )
          iv_t$add_rule("abundance_pres",function(value){
            if(!((isTruthy(input$domin) && input$domin > 0) || 
                 (isTruthy(input$cover) && input$cover > 0) || 
                 (isTruthy(input$freq) && input$freq > 0) || 
                 (isTruthy(input$pres) && input$pres > 0))){
              return('Add abundance')
            }
          }
          )
          iv_t$add_rule("abundance_domin",function(value){
            if(value<1 || value > 10){
              return("1-10")
            }
          })
          iv_t$add_rule("abundance_cover",function(value){
            if(value<1 || value > 100){
              return("1-100")
            }
          })
          iv_t$add_rule("abundance_pres",function(value){
            if(value != 1){
              return("1 = present")
            }
          })
          iv_t$add_rule("abundance_freq",function(value){
            if(value<0.00001 || value > 1){
              return("0-1")
            }
          })
          iv_t$enable() 
        }else{
          output$taxonForm <- renderUI({
            tagList(
              tags$hr(),
              p("No species selected"),br())
            })
        }
      }
      
      ## DT row select
      observeEvent(input$plotDataTable_rows_selected,{
        req(rv$v)
        req(rv$df_d)
        req(input$plotDataTable_rows_selected)
        d <- rv$df_d[rv$df_d$plot_reference_visit == rv$v,]
        rv$i <- d[input$plotDataTable_rows_selected,c("id")]
      })
      
      ## Populate species form
      observeEvent(rv$i,{
        if(rv$i > 0 ){
          t <- rv$df_d[rv$df_d$id == rv$i,]
          taxonForm(session,1,t)
        }else{
            taxonForm(session,0,NULL)
        }
      })
      
      ## Enable add species button
      observe({
        if(isTruthy(rv$v) & rv$sp_mode == 0){
          shinyjs::enable("add_taxon")
        }else{
          shinyjs::disable("add_taxon")
        }
      })
      
      ## Add new species
      observeEvent(input$add_taxon,{
        req(rv$sp_mode == 0)
        rv$sp_mode <- 1
        
        if(nrow(rv$df_d) == 0){
          i <- 1
          rv$df_d[1,] <- NA
          rv$df_d[1,c("id")] <- 1
          rv$df_d[1,c("plot_reference_visit")] <- rv$v
          rv$df_d[1,c("guid")] <- UUIDgenerate()
          rv$df_d[1,c("taxon_name")] <- "..."
          rv$df_d[i,c("edit")] <- 2
          rv$df_d[i,c("Buttons")] <- del_btns(c(i),"sp")
        }else{
          i <- max(rv$df_d$id) + 1
          j <- nrow(rv$df_d)+1
          rv$df_d[j,] <- NA
          rv$df_d[j,c("id")] <- i
          rv$df_d[j,c("plot_reference_visit")] <- rv$v
          rv$df_d[j,c("guid")] <- UUIDgenerate()
          rv$df_d[j,c("taxon_name")] <- "..."
          rv$df_d[j,c("edit")] <- 2
          rv$df_d[j,c("Buttons")] <- del_btns(c(i),"sp")
          }
        
        rv$i <- i
        })
      
      # Set taxon edit mode
      observe({
        if(
          isTruthy(rv$i) && rv$i > 0 &&
          !(
            isTruthy(input$taxon) &&
            (
              (isTruthy(input$domin) && input$domin > 0) || 
             (isTruthy(input$cover) && input$cover > 0) || 
             (isTruthy(input$freq) && input$freq > 0) || 
             (isTruthy(input$pres) && input$pres > 0)
              )
          )
        ){
          rv$sp_mode <- 1
        }else{
          rv$sp_mode <- 0
        }
      })

      ## Update species data from form
      observeEvent(input$taxon,{
        req(rv$df_d)
        req(rv$i > 0)
        #req(!compareNA(input$taxon , rv$df_d[rv$df_d$id == rv$i,c("taxon_nbn")]))
        rv$df_d[rv$df_d$id == rv$i,c("taxon_nbn")] <- input$taxon
        rv$df_d[rv$df_d$id == rv$i,c("taxon_name")] <- uksi_full[uksi_full$nbn_taxon_version_key == input$taxon,c("taxon_name")]
        if(is.na(rv$df_d[rv$df_d$id == rv$i,c("edit")])){
          rv$df_d[rv$df_d$id == rv$i,c("edit")] <- 1
          }
        })
      observeEvent(input$domin,{
        req(rv$df_d)
        req(rv$i > 0)
        req(!compareNA(input$domin , rv$df_d[rv$df_d$id == rv$i,c("abundance_domin")]))
        rv$df_d[rv$df_d$id == rv$i,c("abundance_domin")] <- input$domin
        rv$df_d[rv$df_d$id == rv$i,c("abundance")] <- coalesce.list(rv$df_d[rv$df_d$id == rv$i,c("abundance_domin","abundance_cover","abundance_frequency","abundance_presence")])
        if(is.na(rv$df_d[rv$df_d$id == rv$i,c("edit")])){
          rv$df_d[rv$df_d$id == rv$i,c("edit")] <- 1
          }
        })
      observeEvent(input$cover,{
        req(rv$df_d)
        req(rv$i > 0)
        req(!compareNA(input$cover, rv$df_d[rv$df_d$id == rv$i,c("abundance_cover")]))
        rv$df_d[rv$df_d$id == rv$i,c("abundance_cover")] <- input$cover
        rv$df_d[rv$df_d$id == rv$i,c("abundance")] <- coalesce.list(rv$df_d[rv$df_d$id == rv$i,c("abundance_domin","abundance_cover","abundance_frequency","abundance_presence")])
        if(is.na(rv$df_d[rv$df_d$id == rv$i,c("edit")])){
          rv$df_d[rv$df_d$id == rv$i,c("edit")] <- 1
          }
        })
      observeEvent(input$freq,{
        req(rv$df_d)
        req(rv$i > 0)
        req(!compareNA(input$freq, rv$df_d[rv$df_d$id == rv$i,c("abundance_frequency")]))
        rv$df_d[rv$df_d$id == rv$i,c("abundance_frequency")] <- input$freq
        rv$df_d[rv$df_d$id == rv$i,c("abundance")] <- coalesce.list(rv$df_d[rv$df_d$id == rv$i,c("abundance_domin","abundance_cover","abundance_frequency","abundance_presence")])
        if(is.na(rv$df_d[rv$df_d$id == rv$i,c("edit")])){
          rv$df_d[rv$df_d$id == rv$i,c("edit")] <- 1
          }
        })
      observeEvent(input$pres,{
        req(rv$df_d)
        req(rv$i > 0)
        req(!compareNA(input$pres, rv$df_d[rv$df_d$id == rv$i,c("abundance_presence")]))
        rv$df_d[rv$df_d$id == rv$i,c("abundance_presence")] <- input$pres
        rv$df_d[rv$df_d$id == rv$i,c("abundance")] <- coalesce.list(rv$df_d[rv$df_d$id == rv$i,c("abundance_domin","abundance_cover","abundance_frequency","abundance_presence")])
        if(is.na(rv$df_d[rv$df_d$id == rv$i,c("edit")])){
          rv$df_d[rv$df_d$id == rv$i,c("edit")] <- 1
          }
        })
      observeEvent(input$taxon_note,{
        req(rv$df_d)
        req(rv$i > 0)
        req(!compareNA(ifelse(isTruthy(input$taxon_note),input$taxon_note,NA), rv$df_d[rv$df_d$id == rv$i,c("note")]))
        rv$df_d[rv$df_d$id == rv$i,c("note")] <- ifelse(isTruthy(input$taxon_note),input$taxon_note,NA)
        if(is.na(rv$df_d[rv$df_d$id == rv$i,c("edit")])){
          rv$df_d[rv$df_d$id == rv$i,c("edit")] <- 1
          }
        })

      ## Enable submit taxa button
      observe({
        d <- rv$df_d[
          !is.na(rv$df_d$taxon_nbn) & !is.na(rv$df_d$abundance)
          ,]
        if(nrow(d)>0){
          shinyjs::enable("submitTaxa")
        }else{
          shinyjs::disable("submitTaxa")
        }
      })
      
      ## Submit additions / edits
      observeEvent(input$submitTaxa,{
        rv$df_d <- rv$df_d[
          !is.na(rv$df_d$taxon_nbn) & !is.na(rv$df_d$abundance) 
        ,]
        d <- rv$df_d[!is.na(rv$df_d$edit),which(!(colnames(rv$df_d) %in% c("id")))]
        req(nrow(d) > 0)
        
        # saving modal
        showModal(
          modalDialog(
            div(style="text-align:left;width:60%",
                tags$h4("Uploading",class="loading"),
            )
            ,footer=NULL,size="s",easyClose=FALSE,fade=TRUE
          )
        )
        
        # Upload promise
        future_promise({
          # Remove any rows that have blank species entries
          con0 <- fenDb0(user,password)
          
          i <- pgWriteGeom(con0, 
                           name = c("records","plot_data"),
                           data.obj = d,
                           partial.match = TRUE,
                           overwrite = FALSE,
                           upsert.using = "guid"
                            )
          dbDisconnect(con0)
          return(i)
        })%...>%(function(i){
          if(i){
            # success modal
            showModal(
              modalDialog(
                div(style="text-align:center",
                    tags$h4("Success!")
                )
                ,footer=NULL,size="s",easyClose=TRUE,fade=TRUE
              )
            )
            
            # Clear edits
            rv$df_d$edit <- NA
            rv$df_d$Buttons <- NA
          }else{
            # failure modal
            showModal(
              modalDialog(
                div(style="text-align:center",
                    tags$h4("Upload failed!")
                )
                ,footer=NULL,size="s",easyClose=TRUE,fade=TRUE
              )
            )
          }
        })
      })
      
      # Photo controls ----
      
      photoHandler <- function(){
        # fetch the photo info if not a new visit
        if(rv$add_or_edit == 0){
          if(!isTruthy(rv$df_ph) || !(rv$v_guid %in% rv$df_ph$rel_guid)){
            q <- paste0("SELECT
                        id,
                        guid, 
                        rel_guid,
                        att_type,
                        att_name,
                        att_size,
                        att_note,
                        NULL AS att64,
                        NULL AS file,
                        NULL AS att,
                        created_user,
                        created_date,
                        last_edited_user,
                        last_edited_date 
                      FROM records.plot_visits_attach
                      WHERE rel_guid = '",rv$v_guid,"'")
            future_promise({
              con0 <- poolCheckout(con_global)
              att <- dbGetQuery(con0,q)
              poolReturn(con0)
              return(att)
            })%...>%(function(att){
              if(isTruthy(att) && nrow(att) > 0){
                if(!isTruthy(rv$df_ph)){
                  rv$df_ph <- att
                }else{
                  rv$df_ph <- rbind(rv$df_ph,att)
                }
                att$Buttons <- ""
                att$edit <- NA
                rv$df_ph_v <- att
              }else{
                rv$df_ph_v <- data.frame(
                  "att_name" = character(),
                  "att_size" = numeric(),
                  "Buttons" = character()
                )
              }
              })
          }else{
            # Photo info already downloaded
            x <- rv$df_ph[rv$df_ph$rel_guid == rv$v_guid,]
              x$Buttons <- ""
              x$edit <- NA
              rv$df_ph_v <- x
          }
        }
      }

        # Photo DT
        output$photosTable <- DT::renderDT({
            data.frame(
              "att_name" = character(),
              "att_size" = numeric(),
              "Buttons" = character()
              )
        },
        server = TRUE,
        escape = FALSE,
        rownames = FALSE,
        selection = 'single',
        colnames = c("File name","File size (KB)",""),
        options = list(
          pageLength = 25,
          dom = 'ti',
          language = list(
            infoEmpty = "No photos",
            emptyTable = "No photos"
          ),
          processing = TRUE,
          columnDefs = list(
            list(className = 'dt-center', targets = 1),
            list(orderable = FALSE, targets = c(2)),
            list(targets = c(2),searchable = FALSE),
            list(width = '30px',targets=c(2))
          ),
          extensions = c("FixedHeader", "Scroller"),
          fixedHeader = TRUE
          ,scrollY = "20vh"
        )
        )
        
        proxy_DT_ph <- DT::dataTableProxy("photosTable")
        
        observe({
          if(isTruthy(rv$df_ph_v) && nrow(rv$df_ph_v) > 0){
            x <- rv$df_ph_v[,c("att_name","att_size","Buttons")]
            x$att_name <- apply(x[c("att_name")],1,function(x){
              if(nchar(x)>20){return(paste0(substring(x,1,20),"..."))}else{return(x)}
            })
            x$att_size <- round(x$att_size / 1024,0)
          }else{
            x <- data.frame(
              "att_name" = character(),
              "att_size" = numeric(),
              "Buttons" = character()
            )
          }

            proxy_DT_ph %>% 
              DT::replaceData(data = x, resetPaging = FALSE, rownames = FALSE)
            hideSpinner("photosTable")
        })
        
        # Click a row to display a photo
        
        ## Common UI for photo display
        
        photoBox <- function(session,src,ph_row){
          ns <- session$ns
          tagList(
            div(style="margin:0 auto;height:400px;width:100%;display: flex;align-items: center;justify-content: center;background-color:rgba(0,0,0,0.05);",
                a(
                  href="#",
                  onclick = "openImg(this.firstElementChild.src);",
                  tags$img(src = src, 
                           style="margin-left:2.5%;max-height:380px;max-width:95%;"
                           )
                  )  
                 ),
            textAreaInput(ns("photoCaption"),label = "Caption",resize="vertical",value=ifelse(!is.na(ph_row[,c("att_note")]),ph_row[,c("att_note")],""))
          )
        }
        
        ## Row click and display photo
        
        observeEvent(input$photosTable_rows_selected,{
          ph_guid <- rv$df_ph_v[input$photosTable_rows_selected,c("guid")]
          if(isTruthy(ph_guid)){
            rv$ph <- ph_guid
            ph_row <- rv$df_ph_v[rv$df_ph_v$guid == ph_guid,]
            if(is.na(ph_row[,c("att64")]) && is.na(ph_row[,c("file")])){
              q <- paste0("SELECT encode(att, 'base64') AS att64 
                          FROM records.plot_visits_attach
                          WHERE guid = '", ph_guid,"'")
              future_promise({
                con0 <- poolCheckout(con_global)
                ph <- dbGetQuery(con0,q)
                poolReturn(con0)
                return(ph)
              })%...>%(function(ph){
                rv$df_ph[rv$df_ph$guid == ph_guid,c("att64")] <- ph
                if(is.na(ph)){
                  output$photo <- renderUI({
                    p("Error: can't display photo")
                  })
                }else{
                  output$photo <- renderUI({
                    src <- paste0("data:",gsub("jpeg","jpg",ph_row[,c("att_type")]),";base64,",ph)
                    photoBox(session,src,ph_row)
                    })
                  }
                })
              }
            if(is.na(ph_row[,c("att64")]) && !is.na(ph_row[,c("file")])){
              output$photo <- renderUI({
                src <- paste0("tempdir",substring(ph_row[,c("file")],1+nchar(tempdir()),nchar(ph_row[,c("file")])))
                photoBox(session,src,ph_row)
              })
            }
            if(!is.na(ph_row[,c("att64")])){
                output$photo <- renderUI({
                  src <- paste0("data:",ph_row[,c("att_type")],";base64,",ph_row[,c("att64")])
                  photoBox(session,src,ph_row)
                })
              }
          }
          })
        
        # Load photos from file input
        
        observeEvent(input$add_photo,{
          req(input$add_photo)
          photos <- input$add_photo
          ext <- photos$type
          req(photos)
          shiny::validate(need(ext == "image/jpeg", "Please upload a jpeg file"))
          
          m <- ifelse(isTruthy(rv$df_ph_v) && nrow(rv$df_ph_v)>0, 1 + max(rv$df_ph_v$id),1)
          
          att <- data.frame(
            id = seq(m, m+nrow(photos)-1,1),
            guid = unlist(lapply(c(1:nrow(photos)),UUIDgenerate)),
            rel_guid = rep(rv$v_guid,nrow(photos)),
            att_type = photos$type, 
            att_name = photos$name, 
            att_size = photos$size,
            att_note = rep(NA,nrow(photos)),
            att64 = rep(NA,nrow(photos)),
            file = photos$datapath,
            att = rep(NA,nrow(photos)),
            created_user = rep(NA,nrow(photos)),
            created_date = rep(NA,nrow(photos)),
            last_edited_user = rep(NA,nrow(photos)),
            last_edited_date = rep(NA,nrow(photos))
          )
          att$Buttons <- del_btns(att[,c("id")],"photos")
          att$edit <- 2

          if(isTruthy(rv$df_ph_v)){
            rv$df_ph_v <- rbind(rv$df_ph_v,att)
            }
          if(!isTruthy(rv$df_ph_v)){
            rv$df_ph_v <- att
            }
        })
        
        # Add caption

        observeEvent(input$photoCaption,{
          req(input$photoCaption)
          rv$df_ph_v[rv$df_ph_v$guid == rv$ph,c("att_note")] <- input$photoCaption
          rv$df_ph_v[rv$df_ph_v$guid == rv$ph,c("edit")] <- 1
        })
        
      # Row button click events ----
      
      observeEvent(input$current_id, {
        # Plot table row buttons
        if(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "plots_edit")){
          rv$dt_row <- which(stringr::str_detect(rv$df$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
          d <- rv$df[rv$dt_row, c("site","subsite","plot","gridref",
                                  "transect","transect_side","dim","note",
                                  "created_user","created_date","last_edited_user","last_edited_date")]
          plot_modal(session, d, edit = TRUE, choices_site())
          
          observeEvent(input$site,{
            if(length(choices_subsite()) > 0){
              updateSelectizeInput(session,"subsite",
                                   choices = choices_subsite(),
                                   selected = "")
            }else{
              updateSelectizeInput(session,"subsite",
                                   choices = choices_subsite(),
                                   selected = "",
                                   options=list(placeholder = "No subsites"))
              }
            })
          
          shinyjs::disable("submit_p")
          
          ## Modal validation
          iv_modal <- InputValidator$new()
          iv_modal$add_rule("site",sv_required())
          iv_modal$add_rule("plot",sv_required())
          iv_modal$add_rule("gridref",sv_required())
          iv_modal$add_rule("gridref",function(value){
            if(nchar(gsub(" ", "",input$gridref)) != 12){
              "Enter ten-figure grid reference"
            }
          })
          iv_modal$add_rule("gridref",function(value){
            v <- validate_gf(input$gridref, 
                             s = as.numeric(input$site), 
                             ss = as.numeric(input$subsites),
                             sites0 = tables$sites0,
                             subsites0 = tables$subsites0)
            if(v$error == 1 && isTruthy(input$gridref)){
              v$message
            }
          })
          iv_modal$add_rule("plot",function(value){
            if(input$plot %in% rv$df[rv$df$site == as.numeric(input$site) & rv$df$id != rv$df[rv$dt_row,c("id")],c("plot")]){
              "Plot name is not unique"
            }
          })
          iv_modal$enable()
          
          observe({
            req(input$site)
            req(input$gridref)
            req(input$plot)
            
            if(
              nchar(gsub(" ", "",input$gridref)) == 12 &&
              validate_gf(input$gridref, 
                          s = as.numeric(input$site), 
                          ss = as.numeric(input$subsites),
                          sites0 = tables$sites0,
                          subsites0 = tables$subsites0)$error == 0 &&
              !(input$plot %in% tables$plots[tables$plots$site == as.numeric(input$site) & rv$df$id != rv$df[rv$dt_row,c("id")],c("plot")])
            ){
              shinyjs::enable("submit_p")
            }else{
              shinyjs::disable("submit_p")
            }
            
          })
        }
        if(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "plots_info")){
          rv$dt_row <- which(stringr::str_detect(rv$df$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
          d <- rv$df[rv$dt_row, c("site","subsite","plot","gridref",
                                  "transect","transect_side","dim","note",
                                  "created_user","created_date","last_edited_user","last_edited_date")]
          plot_modal(session, d, edit = TRUE, choices_site())
          
          shinyjs::disable("site")
          shinyjs::disable("subsite")
          shinyjs::disable("plot")
          shinyjs::disable("gridref")
          shinyjs::disable("transect")
          shinyjs::disable("transect_side")
          shinyjs::disable("dim")
          shinyjs::disable("note")
          
          shinyjs::hide("submit_p")
          
          observeEvent(input$site,{
            if(length(choices_subsite()) > 0){
              updateSelectizeInput(session,"subsite",
                                   choices = choices_subsite(),
                                   selected = "")
              }else{
                updateSelectizeInput(session,"subsite",
                                   choices = choices_subsite(),
                                   selected = "",
                                   options=list(placeholder = "No subsites"))
              }
            })
          }
        
        # Visit table row buttons
        if(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "visits_edit")){
          rv$dt_row <- which(stringr::str_detect(rv$df_v$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
          rv$v_guid <- rv$df_v[rv$dt_row,c("guid")]
            
          v <- rv$df_v[rv$dt_row,c("visit")]
          d <- rv$df_v[rv$dt_row, c(
            "recorder","record_date","record_date_end","survey","note",
            "height","bare_ground","bryophyte_cover","litter_cover",
            "canopy","partial_shade","pool","tufa","nvc",
            "created_user","created_date","last_edited_user","last_edited_date"
          )] # partial_shade,pool,tufa not implemented in modal as checkboxes don't accept nulls
          
          vd0 <- NULL
          vd1 <- NULL
          if(isTruthy(rv$df_v[rv$df_v$plot_reference == rv$p & rv$df_v$visit == v - 1,c("record_date")])){
            vd0 <- rv$df_v[rv$df_v$plot_reference == rv$p & rv$df_v$visit == v - 1,c("record_date")]
          }
          if(isTruthy(rv$df_v[rv$df_v$plot_reference == rv$p & rv$df_v$visit == v + 1,c("record_date")])){
            vd1 <- rv$df_v[rv$df_v$plot_reference == rv$p & rv$df_v$visit == v + 1,c("record_date")]
          }

          visit_modal(session,p = rv$p,v = v,vd0 = vd0, vd1 = vd1,d = d,edit = FALSE,s = choices_surveys())
          photoHandler()
          output$photo <- renderUI({p("No photo selected")})
          
          # Validation
          
          iv_modal <- InputValidator$new()
          iv_modal$add_rule("recorder",sv_required())
          iv_modal$add_rule("survey",sv_required())
          iv_modal$add_rule("record_date",sv_required())
          # iv_modal$add_rule("height",sv_required())
          # iv_modal$add_rule("bare_ground",sv_required())
          # iv_modal$add_rule("bryophyte_cover",sv_required())
          # iv_modal$add_rule("litter_cover",sv_required())
          iv_modal$enable()
          
          observe({
            if(
              isTruthy(input$recorder) && 
              isTruthy(input$survey) && 
              isTruthy(input$record_date) &&
              # isTruthy(input$height) && 
              # isTruthy(input$bare_ground) && 
              # isTruthy(input$bryophyte_cover) && 
              # isTruthy(input$litter_cover) &&
              change() > 0
            ){
              shinyjs::enable("submit_v")
            }else{
              shinyjs::disable  ("submit_v")
            }
          })
          
          }
        if(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "visits_info")){
          rv$dt_row <- which(stringr::str_detect(rv$df_v$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
          rv$v_guid <- rv$df_v[rv$dt_row,c("guid")]
          v <- rv$df_v[rv$dt_row,c("visit")]
          d <- rv$df_v[rv$dt_row, c(
            "recorder","record_date","record_date_end","survey","note",
            "height","bare_ground","bryophyte_cover","litter_cover",
            "canopy","partial_shade","pool","tufa","nvc",
            "created_user","created_date","last_edited_user","last_edited_date"
          )] # partial_shade,pool,tufa not implemented in modal as checkboxes don't accept nulls
          
          visit_modal(session,p = rv$p,v = v,vd0 = NULL, vd1 = NULL,d = d,edit = FALSE,s = choices_surveys())
          photoHandler()
          output$photo <- renderUI({p("No photo selected")})
          
          shinyjs::disable("recorder")
          shinyjs::disable("record_date")
          shinyjs::disable("record_date_end")
          shinyjs::disable("survey")
          shinyjs::disable("note")
          shinyjs::disable("height")
          shinyjs::disable("bare_ground")
          shinyjs::disable("bryophyte_cover")
          shinyjs::disable("litter_cover")
          shinyjs::disable("canopy")
          shinyjs::disable("partial_shade")
          shinyjs::disable("pool")
          shinyjs::disable("tufa")
          shinyjs::disable("nvc")
          
          shinyjs::hide("addPhotoMessage")
          shinyjs::hide("add_photo")
          shinyjs::hide("submit_v")
          
        }
        
         # Photo table row buttons
        if(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "photos_del")){
          i <- which(stringr::str_detect(rv$df_ph_v$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
          rv$df_ph_v <- rv$df_ph_v[-c(i),]
        }
        
        # Species table row buttons
        if(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "sp_del")){
          i <- which(stringr::str_detect(rv$df_d$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
          rv$df_d <- rv$df_d[-c(i),]
        }
        
        rv$add_or_edit <- 0
      })
      
      outputOptions(output, 'plotsMap', suspendWhenHidden = FALSE)
      outputOptions(output, 'photosTable', suspendWhenHidden = FALSE)
      }
  )
}