dashboardPage(
  dashboardHeader(title = "FHT Fen Database",
                  tags$li(a(href = 'https://freshwaterhabitats.org.uk/',
                            img(src="FHT logo large_clear background.png", height = "40px"),
                            title = "Freshwater Habitats Trust", style = "padding: 5px 5px 5px 5px !important"),
                          class = "dropdown", style = "padding: 0px !important")
                  ),
  dashboardSidebar(
    sidebarMenu(id = "menu",
      useShinyjs(),
      menuItem("Home", tabName = "home", icon = icon("home")),
      div(h4("Hydrology"),style="margin-left:10px"),
      menuItem("Explore data", tabName  = "explore", icon = icon("stats",lib="glyphicon")),
      menuItem("Manage installs", expandedName = "manageMenu",
                 menuSubItem("Manage dipwells", tabName = "dipsManage"),
                 menuSubItem("Manage loggers", tabName = "loggersManage")
               ),
      menuItem("Import data", icon = icon("file-import"), expandedName = "importMenu",
                 menuSubItem("Import dip data", tabName  = "dipsImport"),
                 menuSubItem("Import logger data", tabName  = "loggersImport"),
                 menuSubItem("Import weather data" ,tabName = "weatherImport")
               ),
      div(h4("Biological records"),style="margin-left:10px")
      
    )
  ),
  dashboardBody(
    
    #Custom css
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    #Login page
    loginUI("loginForm"),
    
    
    tabItems(
      tabItem(tabName = "home",
              fluidRow(
                column(12,
                       h2("Welcome to Freshwater Habitats Trust's Fen Database")
                       )
                )
      )
              ,
      tabItem(tabName = "explore",
                fluidRow(
                  exploreUI("exploreHydro")
                )
              ),
      tabItem(tabName = "dipsManage",
              fluidRow(
                #dips_manageUI("dipsManage")
              )
      ),
      tabItem(tabName = "loggersManage",
              fluidRow(
                #loggers_manageUI("loggersManage),
              )
      ),
      tabItem(tabName = "dipsImport",
                fluidRow(
                  dips_importUI("importDips")
                )
              ),
      tabItem(tabName = "loggersImport",
                fluidRow()
              ),
      tabItem(tabName = "weatherImport", class = "active",
                fluidRow(
                  weather_importUI("weatherImport")
                )
              )
    )
    
    
  )
  )
