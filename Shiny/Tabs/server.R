library(shiny)
library(ggplot2)

shinyServer(function(input, output) {
  
  # a large table, reative to input$show_vars
  output$mytable1 = renderDataTable({diamonds[, input$show_vars, drop = FALSE]})
  
  # sorted columns are colored now because CSS are attached to them
  output$mytable2 = renderDataTable({mtcars}, options = list(orderClasses = TRUE))
  
  # customize the length drop-down menu; display 5 rows per page by default
  output$mytable3 = renderDataTable({iris[, input$show_vars, drop = FALSE]}, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  
})
