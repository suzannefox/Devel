#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      
      mainPanel("main panel",
                fluidRow(
                  splitLayout(cellWidths = c("50%", "50%"), 
                              plotOutput("p1"), plotOutput("p2")),
                  splitLayout(cellWidths = c("50%", "50%"), 
                              plotOutput("p3"), plotOutput("p4"))
                )
      )


         )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
   
   output$distPlot1 <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })

   output$distPlot2 <- renderPlot({
     # generate bins based on input$bins from ui.R
     x    <- faithful[, 2] 
     bins <- seq(min(x), max(x), length.out = input$bins + 1)
     
     # draw the histogram with the specified number of bins
     hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
   
   output$distPlot3 <- renderPlot({
     # generate bins based on input$bins from ui.R
     x    <- faithful[, 2] 
     bins <- seq(min(x), max(x), length.out = input$bins + 1)
     
     # draw the histogram with the specified number of bins
     hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })

   output$distPlot4 <- renderPlot({
     # generate bins based on input$bins from ui.R
     x    <- faithful[, 2] 
     bins <- seq(min(x), max(x), length.out = input$bins + 1)
     
     # draw the histogram with the specified number of bins
     hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
   
   
   # This example uses the ChickWeight dataset, which comes with ggplot2
   # First plot
   output$p1 <- renderPlot({ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet, group=Chick)) +
     geom_line() +
     ggtitle("Growth curve for individual chicks")
   })
   
   # Second plot
   output$p2 <- renderPlot({ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet)) +
     geom_point(alpha=.3) +
     geom_smooth(alpha=.2, size=1) +
     ggtitle("Fitted growth curve per diet")
   })
   
   # Third plot
   output$p3 <- renderPlot({ggplot(subset(ChickWeight, Time==21), aes(x=weight, colour=Diet)) +
     geom_density() +
     ggtitle("Final weight, by diet")
   })
   
   # Fourth plot
   p4 <- ggplot(subset(ChickWeight, Time==21), aes(x=weight, fill=Diet)) +
     geom_histogram(colour="black", binwidth=50) +
     facet_grid(Diet ~ .) +
     ggtitle("Final weight, by diet") +
     theme(legend.position="none")
   
   output$p4 <- renderPlot({ p4       # No legend (redundant in this graph)    
   })
})

# Run the application 
shinyApp(ui = ui, server = server)

