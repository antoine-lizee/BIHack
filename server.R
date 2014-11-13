
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(fmsb)

shinyServer(function(input, output) {
  
  data <- reactive({
    data.frame(check.names = F,
               "Data Science" = c(5,0,input$i_DS),
               "Back-End" = c(5,0, input$i_BE),
               "Front-End" = c(5,0,input$i_FE))
  })
  
  color <- reactive({
    topo.colors(3)[which.max(data()[3,])]
  })
  
  output$radarPlot <- renderPlot({
    
    # draw radar chart
    radarchart(df = data(), axistype = 0, seg = 5, 
               pcol = "black", pfcol = color())
    
  })
  
})
