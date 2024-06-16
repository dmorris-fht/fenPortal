dashboardPage(
  dashboardHeader(title = "fenPortal",
                  titleWidth = 250,
                  tags$li(class = "dropdown",
                          tags$i(class = "fa fa-user",style="display:inline-block"),
                          style = "height:50px;margin-right:3px;padding:15px 4px;font-size:14px;vertical-align:middle;display:inline-block"
                          ),
                  tags$li(class = "dropdown", 
                          textOutput("username"),
                          style = "height:50px;margin-right:10px;padding:15px 4px;font-size:14px;vertical-align:middle;display:inline-block"
                  ),
                  tags$li(class = "dropdown", 
                          actionButton("logout", "logout",
                                       icon = icon("sign-out-alt"),
                                       style = "height:50px;margin-right:10px;background-color:white;border:none")
                          ),
                  tags$li(a(href = 'https://freshwaterhabitats.org.uk/',
                            img(src="FHT logo large_clear background.png", height = "40px"),
                            title = "Freshwater Habitats Trust", style = "padding: 5px 5px 5px 5px !important", target="_blank"),
                          class = "dropdown", style = "padding: 0px !important")
                  ),
  dashboardSidebar(
    width = 250,
    sidebarMenu(id = "menu",
      useShinyjs(),
      useShinyFeedback(),
      menuItem("Home", tabName = "home", icon = icon("home")),
      
      menuItem("FenMap", tabName = "spatial", icon = icon("map")),
      
      div(h4("Data management"),style="margin-left:10px"),
        menuItem("Sites & subsites", tabName  = "sites", icon = icon("compass")),
        menuItem("Data sources", tabName  = "surveys", icon = icon("database")),
      
      div(h4("Biological data & monitoring"),style="margin-left:10px"),
      
      menuItem("Biological records",icon = icon("clipboard-list"),expandedName = "records",
               menuSubItem("Search records", tabName  = "queryRecords"),
               menuSubItem("Enter records", tabName = "enterRecords"),
               menuSubItem("Import records", tabName = "importRecords")
               ),
      
      menuItem("Vegetation monitoring",icon = icon("leaf"),expandedName = "veg",
               menuSubItem("Enter & manage data", tabName = "vegManage"),
               menuSubItem("Analyse vegetation data", tabName = "vegAnalyse")
               
               ),

        menuItem("Survey & monitoring tools", icon = icon("tools"), expandedName = "monTools",
                 menuSubItem("Plant lists", tabName = "plantLists"),
                 menuSubItem("Vegetation data", tabName = "vegLists"),
                 menuSubItem("Import target notes", tabName = "importObs")
        ),
        menuItem("Species introductions", icon = icon("plus"), tabName = "spIntro"),
        menuItem("Data sharing", icon = icon("share"), tabName = "dataSharing"),
      
      
      div(h4("Hydrological monitoring"),style="margin-left:10px"),
      menuItem("Explore data", tabName  = "explore", icon = icon("tint")),
      menuItem("Manage network", icon = icon("water"), expandedName = "manageMenu",
               menuSubItem("Manage installs", tabName = "installsManage"),
               menuSubItem("Manage loggers", tabName = "loggersManage"),
               menuSubItem("Logger derivation points", tabName  = "loggersDer")
      ),
      menuItem("Import data", icon = icon("file-import"), expandedName = "importMenu",
               menuSubItem("Import weather data" ,tabName = "weatherImport"),
               menuSubItem("Import dip data", tabName  = "dipsImport"),
               menuSubItem("Import logger data", tabName  = "loggersImport"),
               menuSubItem("Import stratigraphy data", tabName  = "stratImport")
               
      )
               
    )
  ),
  dashboardBody(
    
    #Favicon
    tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),
    
    #Custom css
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tags$script(HTML("$('body').addClass('fixed');")),
    
    #Login page
    loginUI("loginForm"),
    
    
    tabItems(
      tabItem(tabName = "home",
              fluidRow(
                  homeUI("home")
                )
      )
      ,
      
      tabItem(tabName = "spatial",
              fluidRow(
                fenMapUI("fenmap")
              )
      ),
      
      tabItem(tabName = "sites",
              fluidRow(
                sitesUI("sites")
              )
      ),
       
      tabItem(tabName = "surveys",
              fluidRow(
                surveyUI("surveys")
              )
      ),
      
      tabItem(tabName = "queryRecords",
              fluidRow(
                queryRecordsUI("queryRecords")
              )
      ),
      
      tabItem(tabName = "enterRecords",
              fluidRow(
                enterRecordsUI("enterRecords")
              )
      ),
      
      tabItem(tabName = "vegManage",
              fluidRow(
                vegManageUI("vegManage")
              )
      ),
      
      tabItem(tabName = "importRecords",
              fluidRow(
                importRecordsUI("importRecords")
              )
      ),
      
      tabItem(tabName = "plantLists",
              fluidRow(
                plantListsUI("plantLists")
              )
      ),
      
      tabItem(tabName = "vegLists",
              fluidRow(
                vegListsUI("vegLists")
              )
      ),
      
      tabItem(tabName = "importObs",
              fluidRow(
                importObsUI("importObs")
              )
      ),
      
      tabItem(tabName = "dataSharing",
              fluidRow(
                dataSharingUI("dataSharing")
              )
      )
      ,
      tabItem(tabName = "spIntro",
              fluidRow(
                spIntroUI("spIntros")
              )
      )
      ,
      
      tabItem(tabName = "explore",
                fluidRow(
                  exploreUI("exploreHydro")
                )
              ),
      tabItem(tabName = "installsManage",
              fluidRow(
                installsManageUI("installsManage")
              )
      ),
      
      tabItem(tabName = "loggersManage",
              fluidRow(
                loggersManageUI("loggersManage"),
              )
      )
      ,
      tabItem(tabName = "dipsImport",
                fluidRow(
                  dipsImportUI("dipsImport")
                )
              )
      # ,
      # tabItem(tabName = "loggersImport",
      #           fluidRow(
      #             logger_importUI("importLoggers")
      #           )
      #         ),
      # tabItem(tabName = "weatherImport", class = "active",
      #           fluidRow(
      #             weather_importUI("weatherImport")
      #           )
      #         )
    )
    
    
  )
  )
