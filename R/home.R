homeUI <- function(id){
  ns <- NS(id)
  tagList(
    column(12,
           column(12,
                  column(12,
                         h1("Welcome!"),
                         ),
                  column(4,
                         HTML("
                              <p>fenPortal is the front-end for Freshwater Habitats Trust's database of 
                              headwater fens in England and Wales. </p>
                              
                              <p>fenPortal was created as a repository for biological and hydrological data generated by Freshwater Habitats Trust's work on headwater fens, and as a proof-of-concept for a national central database for 
                              these habitats across England and Wales.</p>
                              
                              <p>Currently, the data holding is best developed for Oxfordshire and Buckinghamshire in England, 
                              to-date a focal region for the organisation's work on fens.</p>
                              "),
                         h3("Features"),
                         
                         HTML("                              
                         <p>fenPortal provides tools to view and manage Freshwater Habitats Trust's inventory of headwater fen sites and information about these special places.</p> 
                         <p>Use the sidebar to:</p>
                          <ul style='margin-left:-1em !important'>
                            <li>View a web map of key data.</li>
                            <li>Manage the inventory of sites.</li>
                            <li>Manage data sources.</li>
                            <li>Search, enter and edit biological records, and other biological data.</li>
                            <li>Use tools for biological and hydrological monitoring workflows.</li>
                            <li>Explore hydrological data.</li>
                          </ul>
                        The fenPortal includes an integration with ArcGIS Online so that data can be collected using web maps etc. and imported here.
                              "),
                         
                         
                         
                         ),
                  column(4,
                         h3("Other ways to access"),
                         HTML("
                              <p>The backend database is a fully GIS-enabled relational database, using the open source PostGIS platform. </p> 
                              
                              <p>A subset of data are served via GeoServer as Web Mapping Services (WMS) and Web Feature Services (WFS), accessible in 
                              desktop and web GIS platforms. Use the following URLs to access these. </p>
                              <ul style='margin-left:-1em !important'>
                                <li><b>Public services:</b></li>
                                  <ul>
                                    <li>https://gis.freshwaterhabitats.org.uk/fens/wms</li>
                                    <li>https://gis.freshwaterhabitats.org.uk/fens/wfs</li>
                                  </ul>
                                <li><b>Private services:</b></li>
                                  <ul>
                                    <li>https://gis.freshwaterhabitats.org.uk/fens-fht/wms</li>
                                    <li>https://gis.freshwaterhabitats.org.uk/fens-fht/wfs</li>
                                  </ul>
                              </ul>
                              <p>For private services, use your fenPortal login.</p>
                              
                              <p>The whole database is also accessible in desktop GIS applications. Connect by QGIS to access 
                              a dedicated project. See the documentation for more details.</p>
                              "),
                         h3("Documentation"),
                         HTML("
                              <p>A guide to using fenPortal can be found <a href=''>here</a>.</p>
                              
                              <p>Full documentation for the database backend and associated architecture is available <a href=''>here</a>.</p>
                              "),
                         h3("Latest updates")
                         ),
                  column(4,
                         h3("Database statistics"),
                         # No records by taxa - pie chart
                         # No sites by county - table
                         # Area of habitat mapped
                         withSpinner(
                                     div(id = ns("stats"),style="height:300px",
                                       column(12,
                                              uiOutput(ns("n_r")),
                                              
                                              billboarderOutput(ns("recPie"),width = "230", height = "230")
                                              
                                       ),
                                       column(12,
                                              uiOutput(ns("n_t")),
                                              
                                              billboarderOutput(ns("taxaPie"),width = "230", height = "230")
                                              
                                       )
                                     )
                         , id = ns("module"), type = 7,caption = "Loading statistics")
                         )
                  )
    ),
    tags$script(
      HTML("$('#home-module').parent().removeClass('shiny-spinner-hidden')")
    )
  )
}

homeServer <- function(id, login) {
  moduleServer(
    id,
    function(input, output, session) {
      q_taxa <- "SELECT COUNT(a.taxa), a.informal_group FROM 
                  (SELECT DISTINCT(u.nbn_taxon_version_key_for_recommended_name) AS taxa, u.informal_group
                  FROM
                  records.records r,
                  lookups.uksi u,
                  spatial.fen_sites s
                  WHERE r.taxon_nbn = u.nbn_taxon_version_key  AND r.site = s.id) a
                  GROUP BY informal_group
                  ORDER BY informal_group"
      
      q_rec_cnty <- "SELECT COUNT(r.id), u.informal_group, s.county 
                      FROM
                      records.records r,
                      lookups.uksi u,
                      spatial.fen_sites s
                      WHERE r.taxon_nbn = u.nbn_taxon_version_key AND r.site = s.id
                      GROUP BY informal_group, county
                      ORDER BY informal_group"
      
      q_rec <- "SELECT COUNT(r.id), u.informal_group
                FROM
                records.records r,
                lookups.uksi u,
                spatial.fen_sites s
                WHERE r.taxon_nbn = u.nbn_taxon_version_key AND r.site = s.id
                GROUP BY informal_group
                ORDER BY informal_group"
      
      rv <- reactiveValues(
                           t = data.frame(gp = character(), count = numeric()),
                           r = data.frame(gp = character(), count = numeric())
                           )
      
      observe({
        n_t <- sum(rv$t$count)
        output$n_t <- renderUI({tagList(
          HTML(paste0("<div style='font-size:16px'>",format(n_t,big.mark=","),"</div><div>taxa</div>"))
        )
        })
        
        n_r <- sum(rv$r$count)
        output$n_r <- renderUI({
          tagList(
          HTML(paste0("<div style='font-size:16px'>",format(n_r,big.mark=","),"</div><div>records</div>")
          )
        )
        })
        
        output$taxaPie <- renderBillboarder({
          billboarder() %>% bb_donutchart(rv$t[,c(2,1)]) %>% bb_legend(hide = TRUE)
        })
        output$recPie <- renderBillboarder({
          billboarder() %>% bb_donutchart(rv$r[,c(2,1)]) %>% bb_legend(hide = TRUE)
        })
        hideSpinner("stats")
      })
      
      future_promise({
        showSpinner("stats")
        con0 <- poolCheckout(con_global)
        t <- dbGetQuery(con0, q_taxa)
        r <- dbGetQuery(con0, q_rec)
        r_cnty <- dbGetQuery(con0, q_rec_cnty)
        poolReturn(con0)
        return(list(t = t, t_cnty = NA, r = r, r_cnty = r_cnty))
      })%...>% (function(result) {
        rv$r <- result$r  
        rv$t <- result$t
        })
      
      }
  )
}