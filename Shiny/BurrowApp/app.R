library(shiny)
library(ggplot2)
library(dplyr)

burrow <- read.csv("D:/Dissertation/R/Burrow.csv", stringsAsFactors = FALSE)
head(burrow)

ui <- fluidPage(
  titlePanel("Burrow Data for iris"),
  sidebarLayout(
    sidebarPanel(
      selectInput("InfoLevel", "Info Level", choices = c("FILE","FIELD")),
      selectInput("MyField", "Info Detail",""),
      selectInput("param", "Select Param","")
    ),
    mainPanel(
      textOutput("text1"),
      tableOutput("results")
      
    )
  )
)

server <- function(input, output, session) {

#   output$results <- renderTable({
#     filtered <-
#       burrow %>%
#       filter(InfoType == input$InfoInput,
#              param == input$InfoDetail)
#     filtered
#   })
  
  output$text1 <- renderText(input$InfoInput)
  
  abc <- reactive({a <- subset(burrow, burrow$InfoLevel %in% input$InfoInput,
                        select=c(burrow$Variable1))
                   return(a)})

  #xyz <- burrow[burrow$Variable1 %in% abc(),]
  
  updateSelectInput(session, "MyField", choices = burrow$Variable1)
  updateSelectInput(session, "param", choices = burrow$InfoDetail)
  
  output$results <- renderTable({
    filtered <-
      burrow %>% 
      filter(InfoLevel == input$InfoLevel)
    filtered
  })
}

shinyApp(ui = ui, server = server)
