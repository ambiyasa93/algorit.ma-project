library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
source("chart.R")
source("ui.R")

shinyServer(function(input, output) {
  output$plot1 <- renderPlotly(ggplotly(plot1(input$slct1[1]),tooltip = c("text")) %>% 
                               config(displayModeBar = F))
  
  output$plot2 <- renderPlotly(ggplotly(plot2,tooltip = c("text")) %>% 
                               config(displayModeBar = F))
  output$plot7 <- renderPlotly(ggplotly(plot7,tooltip = c("text")) %>% 
                                 config(displayModeBar = F))
  
  output$plot4 <- renderLeaflet(plot4(input$sldr4[1],input$sldr4[2]))
  
  output$plot5 <- renderPlotly(ggplotly(plot5(input$radio5[1]),tooltip = c("text")) %>% 
                                 config(displayModeBar = F))
  
  output$plot3 <- renderPlotly(ggplotly(plot3(input$slct3[1]),tooltip = c("text")) %>% 
                                 config(displayModeBar = F))
  output$add1 <- renderUI(div(fluidRow(actionButton("add1", "Add New Chart"))))
  
  observeEvent(input$add1 ,{
    output$add1 <- renderUI(fluidRow())
    output$boxes31 <- renderUI({
      fluidRow(
        box(title=p("Total Suicide Case over Year", actionButton("rmv1", "  ", icon = icon("minus-circle"),class = "btn-xs", title = "Remove this Chart")),
            solidHeader=T, 
            width=12,
            renderPlotly(ggplotly(plot3(input$slct31[1]),tooltip = c("text")) %>% 
                         config(displayModeBar = F)),
            selectInput("slct31", label = h5("Select Category"), 
            choices = list("By Age and Sex" = 1, "By Generation and Sex" = 2, "By Sex" = 3), 
            selected = 1)
            )
      )
    })
    if (a == 1){
      output$add2 <- renderUI(fluidRow())
    }
    else{
    output$add2 <- renderUI(div(fluidRow(actionButton("add2", "Add New Chart"))))
    }
    b <<- 0
  })
  
  observeEvent(input$add2 ,{
    output$add2 <- renderUI(fluidRow())
    output$boxes32 <- renderUI({
      fluidRow(
        box(title=p("Total Suicide Case over Year", actionButton("rmv2", "  ", icon = icon("minus-circle"),class = "btn-xs", title = "Remove this Chart")),
            solidHeader=T, 
            width=12,
            renderPlotly(ggplotly(plot3(input$slct32[1]),tooltip = c("text")) %>% 
                         config(displayModeBar = F)),
            selectInput("slct32", label = h5("Select Category"), 
                      choices = list("By Age and Sex" = 1, "By Generation and Sex" = 2, "By Sex" = 3), 
                      selected = 1)
        )
      )
    })
    a <<- 1
  })
  
  observeEvent(input$rmv1,{
    output$add1 <- renderUI(div(fluidRow(actionButton("add1", "Add New Chart"))))
    output$boxes31 <- renderUI(fluidRow())
    output$add2 <- renderUI(fluidRow())
    b <<- 1
  })
  
  observeEvent(input$rmv2,{
    if (b == 1){
      output$add2 <- renderUI(fluidRow())
    }
    else {
      output$add2 <- renderUI(div(fluidRow(actionButton("add2", "Add New Chart")))) 
    }
    output$boxes32 <- renderUI(fluidRow())
    a <<- 0
    })
  
  output$plot8 <- renderPlot(plot8(input$slct8[1]))

})
  

  
