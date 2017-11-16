library(shiny)
library(shinydashboard)
library(leaflet)
library(ggplot2)
library(plotly)
pdf(NULL)

shinyUI(
  dashboardPage(
    dashboardHeader(disable = TRUE
    ),
    
    dashboardSidebar(
      # sliderInput("bins","Number of Breaks",1,50,25),
      sidebarMenu(
        menuItem("Home", tabName = "Home", icon = icon("home")),
        menuItem("Drilling Productivity Summary", tabName = "Drilling Productivity", icon = icon("line-chart")),
        menuSubItem("Permian", tabName = "Permian"),
        menuSubItem("Appalachian", tabName = "Appalachian"),
        menuSubItem("Bakken", tabName = "Bakken"),
        menuSubItem("Eagle Ford", tabName = "Eagle Ford"),
        menuSubItem("Haynesville", tabName = "Haynesville"),
        menuSubItem("Niobrara", tabName = "Niobrara"),
        menuSubItem("Anadarko", tabName = "Anadarko"),
        menuItem("Help", tabName = "Help", icon = icon("question"))
      )),
    
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "Home",
                fluidRow(
                  box(
                    title = "Domestic Oil and Gas Production Dashboard", width = 12, background = "light-blue",
                    "The Drilling Productivity Report uses recent data on the total number of drilling rigs in operation along with estimates of drilling productivity and estimated changes in production from existing oil and natural gas wells to provide estimated changes in oil and natural gas production for seven key regions"
                  )),
                
                fluidRow(
                  tabBox(title = "US Production and Rig Count", side = "right",
                         id = "tabset1", height = "300px", width = 9,
                         tabPanel("Oil", plotlyOutput("plot"), "Oil Production"),
                         tabPanel("Gas", "Gas Production")
                  ),
                  box(
                    title = "Last Updated", side = "right", width = 3),
                  box(
                    title = "Inputs", side = "right", status = "warning", width = 3,
                    
                    dateRangeInput('dateRange2',
                                   label = paste('Select date range:'),
                                   start = Sys.Date() - 3, end = Sys.Date() + 3,
                                   min = Sys.Date() - 10, max = Sys.Date() + 10,
                                   separator = " - ", format = "mm/dd/yy",
                                   startview = 'year', language = 'en', weekstart = 1)
                  )
                ),
                fluidRow(
                  # A static infoBox
                  #infoBox("EIA Production Data Release", "11/30/2017", width = 6)
                )
        ),
        tabItem(tabName = "Permian",
                fluidRow(
                  # A static infoBox
                  infoBox("Rig Count This Week", DPR_RIGS$Permian[1], icon = icon("calendar-o")),
                  infoBoxOutput("WeeklyChangeBox")
                ),
                fluidRow(
                  tabBox(title = "Permian Production and Rig Count", side = "right",
                         id = "tabset1", height = "300px", width = 9,
                         tabPanel("Gas", "Gas Production"),
                         tabPanel("Oil", plotlyOutput("permianoil"), "Oil Production")
                         
                  ),
                  
                  box(
                    title = "Inputs", side = "right", status = "warning", width = 3,
                    
                    dateRangeInput('dateRange2',
                                   label = paste('Select date range:'),
                                   start = Sys.Date() - 3, end = Sys.Date() + 3,
                                   min = Sys.Date() - 10, max = Sys.Date() + 10,
                                   separator = " - ", format = "mm/dd/yy",
                                   startview = 'year', language = 'en', weekstart = 1)
                    
                  ))),
        
        tabItem(tabName = "Anadarko",
                fluidRow(
                  tabBox(title = "Anadarko Production and Rig Count", side = "right",
                         id = "tabset1", height = "300px", width = 9,
                         tabPanel("Gas", "Gas Production"),
                         tabPanel("Oil", plotlyOutput("Anadarkooil"), "Oil Production")
                         
                  ),
                  
                  box(
                    title = "Inputs", side = "right", status = "warning", width = 3,
                    
                    dateRangeInput('dateRange2',
                                   label = paste('Select date range:'),
                                   start = Sys.Date() - 3, end = Sys.Date() + 3,
                                   min = Sys.Date() - 10, max = Sys.Date() + 10,
                                   separator = " - ", format = "mm/dd/yy",
                                   startview = 'year', language = 'en', weekstart = 1)
                    
                  ))),
        
        
        tabItem(tabName = "Bakken",
                fluidRow(
                  tabBox(title = "Bakken Production and Rig Count", side = "right",
                         id = "tabset1", height = "300px", width = 9,
                         tabPanel("Gas", "Gas Production"),
                         tabPanel("Oil", plotlyOutput("Bakkenoil"), "Oil Production")
                         
                  ),
                  
                  box(
                    title = "Inputs", side = "right", status = "warning", width = 3,
                    
                    dateRangeInput('dateRange2',
                                   label = paste('Select date range:'),
                                   start = Sys.Date() - 3, end = Sys.Date() + 3,
                                   min = Sys.Date() - 10, max = Sys.Date() + 10,
                                   separator = " - ", format = "mm/dd/yy",
                                   startview = 'year', language = 'en', weekstart = 1)
                    
                  ))),
        
        tabItem(tabName = "STEO",
                h1("STEO Data"),
                fluidRow(
                  box(plotlyOutput("plot1"), width=6, height=500)
                )
        ),
        tabItem(tabName = "scoopstack",
                h4("SCOOPSTACK Dashboard")
        )
      )
    )
    
  )
)

