exploreUI <- function(id, label = "exploreHydro"){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12,
             
                    column(12,
                           selectInput(ns("choose_site"),label="Choose a site (required):",choices=NULL,multiple=FALSE),
                    ),
                    
                    #Leaflet map for map of site and installs
                    column(5,
                           h3("Hydrological monitoring installs"),
                           column(12,
                                  withSpinner(leafletOutput(ns("siteMap")))
                           ),
                           column(12,
                                  h4("Install information (click map):"),
                                  column(12,
                                        strong("Installation details:"),
                                        htmlOutput(ns("installInfo"))
                                  ),
                                  column(12,
                                         column(6,
                                                strong("Topo survey:"),
                                                div(htmlOutput(ns("topoInfo")),style="max-height:120px; overflow-x:hidden; overflow-y: auto")
                                         ),
                                         column(6,
                                                strong("Loggers installed:"),
                                                div(htmlOutput(ns("loggerInfo")),style="max-height:120px; overflow-x:hidden; overflow-y: auto")
                                         )
                                         )
                                  )
                    ),
                    column(7,
                           h3("Plot of monitoring data"),
                           column(12,
                                  withSpinner(plotlyOutput(ns("plot"),width = "100%", height = 500))
                                  
                           ),
                           column(12,
                                  div(selectInput(ns("plotMode"), label = "Plot water table depth relative to:", choices = c("Ground level","Ordnance Datum"), multiple = FALSE, selected = "mbgl"),style="width: 150px"),
                                  selectInput(ns("choose_install"),label="Choose install(s) (optional):",choices=NULL,multiple=TRUE),
                                  sliderInput(ns("choose_date_slider"),
                                              label = "Date range to plot",
                                              min = floor_date(as.Date("2022-04-01","%Y-%m-%d"),"week"),
                                              max = ceiling_date(Sys.Date(),"week"),
                                              value = c(as.Date("2022-04-01","%Y-%m-%d"), Sys.Date()),
                                              timeFormat="%Y-%m-%d",
                                              step = 7
                                  ),
                                  selectInput(ns("weatherStation"), label = "Add data from weather station", multiple = TRUE, choices = c()),
                                  
                                  )
                           )
                    ,
                    
                    #div(withSpinner(DT::dataTableOutput(ns("dipsTable"))), style = "font-size:70%;width:80%")
             
             )
    )
  )
}

exploreServer <- function(id,con){
  moduleServer(
      id,
      function(input, output, session) {
        
        #Reactive of installs plus site name for given site  
        installs <- reactiveValues(data=NA,sites=NA,topo=NA)
        
        #Set initial selectInput choices
        observe({
          
          #Get list of sites
          sites <- pgGetGeom(con, query = "SELECT ST_Transform(B.geom,4326) AS geom, B.id AS siteid, B.site AS sitename  FROM spatial.monitoring_hydro_installs A, spatial.fen_sites B WHERE A.site = B.id ORDER BY B.site")
          inst <- pgGetGeom(con, query = "SELECT ST_Transform(A.geom,4326) AS geom, A.id AS installid, A.install_name AS installname, B.id AS siteid, B.site AS sitename, C.description AS install_type, A.install_location, A.install_reason, A.installed_by, A.install_depth, A.install_protrusion, A.install_geology, A.install_hydrogeo, A.install_hydroeco  FROM spatial.monitoring_hydro_installs A, spatial.fen_sites B, lookups.lookup_hydro_install C WHERE A.site = B.id AND A.install_type = C.code ORDER BY B.site, A.install_name")
          topo <- dbGetQuery(con, "SELECT * FROM hydro_monitoring.installs_topo")
          
          installs$data <- inst
          installs$sites <- sites
          installs$topo <- topo
          
          #update sites input choices with installs  
          sites_choices <- sites$siteid
          names(sites_choices) <- sites$sitename
          updateSelectInput(session, inputId = "choose_site", choices = sites_choices)
          
          inst <- inst[inst$install_type != "Barometric logger",]
          installs_choices <- inst$installid
          names(installs_choices) <- paste(inst$sitename, inst$installname,sep = " - ")
          updateSelectInput(session, inputId = "choose_install", choices = installs_choices)
        })
        
        #When site is chosen, update installs input choices
        observeEvent(input$choose_site,{
          inst <- installs$data
          inst <- inst[inst$siteid %in% input$choose_site & inst$install_type != "Barometric logger",]
          installs_choices <- inst$installid
          names(installs_choices) <- paste(inst$sitename, inst$installname,sep = " - ")
          updateSelectInput(session, inputId = "choose_install", choices = installs_choices)

        })

        #Map of sites and installs

        output$siteMap <- renderLeaflet ({
          data <- installs$data[installs$data$siteid == input$choose_site,]
          map <- leaflet() %>%
            addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
            addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
            addMiniMap(tiles = "OpenStreetMap", toggleDisplay = TRUE, minimized = TRUE) %>%
            addEasyButton(easyButton(icon="fa-home", title="Home view", onClick=JS("function(btn, map){ map.fitBounds([[49.959999905, -7.57216793459 ],[58.6350001085, 1.68153079591 ]]); }"))) %>%
            addEasyButton(easyButton(icon="fa-crosshairs", title="Locate Me", onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
            addPolygons(data = installs$sites[installs$sites$siteid == input$choose_site,],
                        stroke = TRUE,
                        weight = 2,
                        color = "red",
                        fill = FALSE,
                        popup = NA
            ) %>%
            addCircleMarkers(data = data,
                             radius = 3,
                             stroke = TRUE,
                             color = "black",
                             weight = 0.5,
                             opacity = 1,
                             fill = TRUE,
                             fillColor =  ifelse(data$install_type == "Barometric logger","blue","white"),
                             fillOpacity = 1,
                             popup = NA,
                             layerId = unlist(as.data.frame(installs$data[installs$data$siteid == input$choose_site,])$installid),
                             group = "installs",
                             label = unlist(as.data.frame(installs$data[installs$data$siteid == input$choose_site,])$installname)
                             ) %>%
            addMeasure() %>%
            addScaleBar(position = c("bottomleft")) %>%
            addLayersControl(baseGroups = c("Satellite","OpenStreetMap"),
                             options = layersControlOptions(collapsed = TRUE))
          map
        })

        #Initial install details
        output$installInfo <- renderUI({
          tagList(
            p("No install selected")
          )
        })
        output$topoInfo <- renderUI({
          tagList(
            p("No install selected")
          )
        })
        output$loggerInfo <- renderUI({
          tagList(
            p("No install selected")
          )
        })

        #Proxy leaflet map
        siteMapProxy <- leafletProxy(mapId = "siteMap", session)

        #On marker click, show install info and highlight point
        observeEvent(input$siteMap_marker_click,{

          if(input$siteMap_marker_click$id == "highlight"){
            clearGroup(map = siteMapProxy, group = "highlight")

            output$installInfo <- renderUI({
              HTML("<p>No install selected</p>")
            })
            output$topoInfo <- renderUI({
              HTML("<p>No install selected</p>")
            })
            output$loggerInfo <- renderUI({
              HTML("<p>No install selected</p>")
            })

          }
          else{
            clicked <- installs$data[installs$data$installid == input$siteMap_marker_click$id,]
            clearGroup(map = siteMapProxy, group = "highlight")

            addCircleMarkers(map = siteMapProxy,
                             lng = input$siteMap_marker_click$lng,
                             lat = input$siteMap_marker_click$lat,
                             group = "highlight",
                             layerId = "highlight",
                             radius = 3,
                             stroke = TRUE,
                             color = "red",
                             weight = 1,
                             opacity = 1,
                             fill = FALSE,
                             popup = NA,
            )


            x <- c("Name:",
                   "Type:",
                   "Location:",
                   "Reason:",
                   "Installed by:",
                   "Depth below ground (m):",
                   "Protrusion above ground (m):",
                   "Geology:",
                   "Hydrogeology:",
                   "Habitat:"
                   )
            x <- paste("<b>",x,"</b>",sep="")
            y <- c(clicked$installname,
                   clicked$install_type,
                   clicked$install_location,
                   clicked$install_reason,
                   clicked$installed_by,
                   clicked$install_depth,
                   clicked$install_protrusion,
                   clicked$install_geology,
                   clicked$install_hydrogeo,
                   clicked$install_hydroeco )

            m <- matrix(c(x, y),ncol = 2)


            output$installInfo <- renderUI({
              HTML(m %>%
                     addHtmlTableStyle(
                       col.rgroup = c("none", "#F7F7F7"),
                       align="ll",
                       css.cell = "vertical-align: top;") %>%
                     htmlTable
                   )
            })
          }
        })

        #Reactive of dip data to use
        dips <- reactiveValues(data = NA)

        #Get dip data
        observe({
          req(input$choose_site)
          #Retrieve dip data

          d <- dbGetQuery(con, paste("SELECT B.install_name, A.install, A.dip_date_time, A.dip_measurer, A.dip_depth_top, A.dip_notes, A.dips_import FROM hydro_monitoring.dips A, spatial.monitoring_hydro_installs B WHERE A.install = B.id AND B.site = ",input$choose_site,sep=""))

          d <- merge(d, installs$data, by.x = "install", by.y = "installid")
          d <- merge(d, installs$topo, by.x = "install", by.y = "install")

          d$mbgl <- d$install_protrusion - d$dip_depth_top / 100
          d$maod <-  - d$dip_depth_top / 100 + d$z_w

          dips$data <- d
        })

        #Modify date slider by dip date range
        observe({
          if(isTruthy(dips$data)){
            d <- dips$data
            if(isTruthy(input$choose_install)){
              min <- floor_date(min(as.Date(d[d$install %in% input$choose_install,c("dip_date_time")])),"week")
              max <- ceiling_date(max(as.Date(d[d$install %in% input$choose_install,c("dip_date_time")])),"week")
            }
            else{
              min <- floor_date(min(as.Date(d$dip_date_time)),"week")
              max <- ceiling_date(max(as.Date(d$dip_date_time)),"week")
            }

            updateSliderInput(session,"choose_date_slider", min = min, max = max)          }

        })

        #Plot data
        observe({
          #Define data to plot
          if(isTruthy(dips$data)){
            # if(isTruthy(input$choose_install)){
            #   dips_plot <- dips$data[dips$data$install %in% input$choose_install & (dips$data$dip_date_time >= input$choose_date_slider[1] & dips$data$dip_date_time <= input$choose_date_slider[2]),]
            # }
            # else{
            #   dips_plot <- dips$data[dips$data$dip_date_time >= input$choose_date_slider[1] & dips$data$dip_date_time <= input$choose_date_slider[2],]
            # }
            if(isTruthy(input$choose_install)){
              dips_plot <- dips$data[dips$data$install %in% input$choose_install,]
            }
            else{
              dips_plot <- dips$data
            }

            n <- ifelse(isTruthy(input$choose_install),length(input$choose_install),nrow(installs$data[installs$data$install_type != "Barometric logger",]))
            cols <- brewer.pal(min(n,12),"Set3")
            cols <- colorRampPalette(cols)(n)


              if(input$plotMode == "Ground level"){
                p <- ggplot(dips_plot) +
                  geom_line(aes(x = dip_date_time, y = mbgl, color = install_name), linewidth = 1) +
                  scale_color_manual(values = cols) +
                  geom_point(aes(x = dip_date_time, y = mbgl, color = install_name)) +
                  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
                  theme(
                    legend.title = element_text(size = 10, face = "bold"),
                    legend.text = element_text(size = 10),
                    legend.spacing.y = unit(2,"mm"),
                    plot.background = element_blank(),
                    panel.background = element_rect(fill = alpha("white", 0.2)),
                    panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                                    colour = "grey"),
                    panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                                    colour = "lightgrey")
                  ) +
                  xlab("Dip date") + ylab("Water table height above ground level (m)") + labs(color = "Install name")

                ggplotly(p)
              }
              if(input$plotMode == "Ordnance Datum"){
                p <- ggplot(dips_plot) +
                         geom_line(aes(x = dip_date_time, y = maod, color = install_name), linewidth = 1) +
                         scale_color_manual(values = cols) +
                         geom_point(aes(x = dip_date_time, y = maod, color = install_name)) +
                         theme(
                           legend.title = element_text(size = 10, face = "bold"),
                           legend.text = element_text(size = 10),
                           legend.spacing = unit(2,"mm"),
                           plot.background = element_blank(),
                           panel.background = element_rect(fill = alpha("white", 0.2)),
                           panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                                           colour = "grey"),
                           panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                                           colour = "lightgrey")
                         ) +
                         xlab("Dip date") + ylab("Water table height above Ordnance Datum (m)") + labs(color = "Install name")
              }

            output$plot <- renderPlotly({
              ggplotly(p, dynamicTicks = TRUE)
            })


            }
        })

        observeEvent(input$choose_date_slider,{
          lims <- c(input$choose_date_slider[1],input$choose_date_slider[2])
          plotlyProxy("plot", session) %>%
            plotlyProxyInvoke("relayout", list(xaxis = list(range = lims)))
        })
        
    })}
  