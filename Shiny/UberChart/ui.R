#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

shinyUI(fluidPage(
  titlePanel("title panel"),
  
  sidebarLayout(position = "left",
                sidebarPanel("sidebar panel",
                             checkboxInput("do2", "Make 2 plots", value = T)
                ),
                mainPanel("main panel",
                          fluidRow(
                            splitLayout(cellWidths = c("50%", "50%"), 
                                        plotOutput("plotgraph1"), plotOutput("plotgraph2"))
                          )
                )
  )
)
)