library(shiny)
library(ggplot2)
library(dplyr)

bcl <- read.csv("D:/R/bcl/bcl-data.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  titlePanel("BC Liquor Store prices"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput", "Price", 0, 100, c(25, 40), pre = "$"),
      radioButtons("typeInput", "Product type",
                   choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                   selected = "WINE"),
      selectInput("countryInput", "Country",
                  choices = c("CANADA", "FRANCE", "ITALY"))
    ),
    mainPanel(
      plotOutput("coolplot"),
      br(), br(),
      tableOutput("results")
    )
  )
)

server <- function(input, output, session) {
  output$coolplot <- renderPlot({
    filtered <-
      bcl %>%
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Type == input$typeInput,
             Country == input$countryInput
      )
    ggplot(filtered, aes(Alcohol_Content)) +
      geom_histogram()
  })
  
  output$results <- renderTable({
     filtered <-
         bcl %>%
       filter(Price >= input$priceInput[1],
                              Price <= input$priceInput[2],
                               Type == input$typeInput,
                               Country == input$countryInput
                        )
       filtered
     })
}

shinyApp(ui = ui, server = server)

# 
# library(shiny)
# library(ggplot2)
# bcl <- read.csv("D:/R/bcl/bcl-data.csv", stringsAsFactors = FALSE)
# 
# # FileName <- ""
# # 
# # myContent("","Header",FileName)
# # myContent(bcl,"File",FileName)
# # 
# # for(i in names(bcl)){
# #   myContent(bcl[[i]],i,FileName)
# # }
# # 
# # return()
# #ui <- fluidPage()
# #ui <- fluidPage("BC Liquor Store", "prices")
# #ui <- fluidPage(h1("My app"),"BC","Liquor",br(),"Store",strong("prices"))
# #ui <- fluidPage(titlePanel("BC Liquor Store prices"))
# 
# ui <- fluidPage(
#   titlePanel("Hello Shiny!"),
#   fluidRow(column(width = 4,"4"),
#            column(width = 3, offset = 2,"3 offset 2")
#   ),
#   
#   sidebarLayout(
#     sidebarPanel("our inputs will go here",   
#                  numericInput("obs", "Observations:", 10, min = 1, max = 100),
#                  radioButtons("dist", "Distribution type:",
#                               c("Normal" = "norm",
#                                 "Uniform" = "unif",
#                                 "Log-normal" = "lnorm",
#                                 "Exponential" = "exp"))
#     ),
#     mainPanel("the results will go here",
#               plotOutput("coolplot"),
#               br(),
#               tableOutput("results"))
#   )
# )
# 
# print(ui)
# 
# #server <- function(input, output, session) {}
# 
# server <- function(input, output, session) {
#   #print(str(bcl))
#   # output$coolplot <- renderPlot({
#   # plot(rnorm(100)),
#     
#   #output$coolplot <- renderPlot({
#   #plot(rnorm(input$obs[1])),
#     
#   #output$coolplot <- renderPlot({
#   #ggplot(bcl, aes(Alcohol_Content)) +
#   #geom_histogram()
#     
#   output$results <- iris
#   })
#   
# }
# shinyApp(ui = ui, server = server)
