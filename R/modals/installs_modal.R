import_installs_modal <- function(session) {
  ns <- session$ns
  shiny::modalDialog(
    tagList(
    tabsetPanel(
      tabPanel("From file",
                      h4("Import from file")
               ),
      tabPanel("From AGOL",
                      h4("Import from ArcGIS Online"),
                      HTML("
                      <p>Specify the REST URL for a layer in an ArcGIS Online feature service containing the hydrological installations to be imported.</p>
                  
                      <p>You can find the URL for a service on its details page - you just need to add the layer number to the end. 
                      To import from a feature service, it must follow the template available <a href='', target='_blank'>here</a>. 
                      The service must be shared publicly or the URL must include an access token.</p>
                      
                      <p>Photos attached to feature will be imported. Photos must be in jpeg format and photos over 500 kb will be resized and compressed.</p>     
                           "),br(),
                      
                      textAreaInput(ns("agol_url"), 
                                    label="Enter URL of layer to import", 
                                    width = "100%", 
                                    resize = "vertical", 
                                    placeholder ="https://services9.arcgis.com/ABCD123/arcgis/rest/services/FeatureServiceName/FeatureServer/1"),
                      actionButton(ns("importAGOL"), "Import", icon = icon("file-import"))
               )
    )
    )
    ,
    size = "m",
    easyClose = TRUE,
    footer = NULL
  ) %>% shiny::showModal()
}