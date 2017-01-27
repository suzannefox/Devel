#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)
library(ggplot2)


shinyServer(function(input, output) 
{
  set.seed(1234)
  pt1 <- qplot(rnorm(500),fill=I("red"),binwidth=0.2)
  pt2 <- reactive({
    input$do2
    if (input$do2){
      return(qplot(rnorm(500),fill=I("blue"),binwidth=0.2))
    } else {
      return(NULL)
    }
  })
  output$plotgraph1 = renderPlot({pt1})
  output$plotgraph2 = renderPlot({pt2()})
}
)