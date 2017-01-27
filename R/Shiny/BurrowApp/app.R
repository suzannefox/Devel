library(shiny)
library(ggplot2)
library(dplyr)

burrow <- read.csv("D:/R/Burrow.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  titlePanel("Burrow Data for iris"),
  sidebarLayout(
    sidebarPanel(
      selectInput("InfoInput", "Info Type",
                  choices = c("File","Field")),
      
      selectInput("param", "Select Param","")
    ),
    mainPanel(
      tableOutput("results")
    )
  )
)

server <- function(input, output, session) {

  output$results <- renderTable({
    filtered <-
      burrow %>%
      filter(InfoType == input$InfoInput,
             Param == input$param)
    filtered
  })
  
  updateSelectInput(session, "param", choices = burrow$Param)
}

shinyApp(ui = ui, server = server)
