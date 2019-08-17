library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
source("chart.R")

#UI---------------------------------------------------------------
header <-dashboardHeader(title = "DV Capstone Project")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Map", icon = icon("map"), tabName = "Map"),
    menuItem("Compare Data",icon = icon("layer-group"), tabName = "CompareData"),
    menuItem("Scatter Plot",icon = icon("chart-line"), tabName = "CheckRelation")
  )
)

body <- dashboardBody(tags$head(
  tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
  tags$style(HTML('button#add1{position:relative;left:47%;margin-bottom:10px;background-color:#3c8dbc;color: #FFFFFF;border-color: #3c8dbc}
                   button#add2{position:relative;left:47%;margin:left:-50px;background-color:#3c8dbc;color: #FFFFFF;border-color: #3c8dbc;}')),
  tabItems(
    tabItem(tabName = "dashboard",
              fluidRow(
                column(7,
                       box(
                         title="Human Development Index, Gross Domestic Product compared to Total Cases",solidHeader=T, 
                         width=12,
                         plotlyOutput("plot5", height = 750),
                         radioButtons("radio5", label = h5("Choose Option for HDI and GDP"),
                                      choices = list("Using HDI and GDP over the Year" = 1, "Using mean of HDI and GDP per Country" = 2), 
                                      selected = 1,inline = T)
              
                          )
                        ),
                column(5,
                        box(
                          title="Top Country with Suicide Cases", solidHeader=T,
                          width=12,
                          plotlyOutput("plot1", height = 325),
                          selectInput("slct1", label = h5("Select Count of Country "), 
                                      choices = list("5" = 5, "10" = 10, "15" = 15, "20" = 20), 
                                      selected = 20)
                          )
                        ),
                  column(5,
                         tabBox(
                           title="Total Suicide Case",
                           width=12,
                           tabPanel("By Age Category", plotlyOutput("plot2", height = 325)),
                           tabPanel("By Generation", plotlyOutput("plot7", height = 325))
                           )  
                        )
              )
          ),
    tabItem(tabName = "Map",
              fluidRow(
                box(title = "Map of Total Suicide Case per Country", solidHeader=T,
                    width=12,
                    leafletOutput("plot4", height = 600),
                    sliderTextInput(inputId = "sldr4",
                                    label = "Select Range of Year",
                                    choices = as.character(sort(unique(scd$year),decreasing = FALSE)),
                                    selected = c(min(scd$year),max(scd$year))
                                    )
                    )
              )

    ),
    tabItem(tabName ="CompareData",
            fluidRow(
                     box(
                       title="Total Suicide Case over Year",solidHeader=T, 
                       width=12,
                       plotlyOutput("plot3", height = 350),
                       selectInput("slct3", label = h5("Select Category"), 
                                   choices = list("By Age and Sex" = 1, "By Generation and Sex" = 2, "By Sex" = 3), 
                                   selected = 1)
                        )
                  
            ),
            uiOutput("add1"),
            uiOutput("boxes31"),
            uiOutput("add2"),
            uiOutput("boxes32")
    ),
    tabItem(tabName ="CheckRelation",
            fluidRow(
              column(12,
                     box(
                       title="Scatter plot to Check Correlation",solidHeader=T, 
                       width=12,
                       plotOutput("plot8", height = 700),
                       selectInput("slct8", label = h5("Variable to Compare"), 
                                   choices = list("Human Development Index" = 1, "Gross Domestic Product" = 2), 
                                   selected = 1)
                      )
                    )
                  )
            )
  )
)

  
#Dashbard
dashboardPage(
  header,
  sidebar,
  body
)

