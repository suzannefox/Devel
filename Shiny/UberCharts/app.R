#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

ui <- (fluidPage(
  titlePanel("title panel"),
  
  sidebarLayout(position = "left",
                sidebarPanel("sidebar panel",
                             checkboxInput("do2", "Make 2 plots", value = T)
                ),
                mainPanel("main panel",
                          fluidRow(
                            splitLayout(cellWidths = c("50%", "50%"), plotOutput("plotgraph1"), plotOutput("plotgraph2"))
                          )
                )
  )
)
)


server <- (function(input, output) 
{
  set.seed(1234)
  pt1 <- qplot(rnorm(500),fill=I("red"),binwidth=0.2,title="plotgraph1")
  pt2 <- reactive({
    input$do2
    if (input$do2){
      return(qplot(rnorm(500),fill=I("blue"),binwidth=0.2,title="plotgraph2"))
    } else {
      return(NULL)
    }
  })
  output$plotgraph1 = renderPlot({pt1})
  output$plotgraph2 = renderPlot({pt2()})
}
)

# Run the application 
shinyApp(ui = ui, server = server)

