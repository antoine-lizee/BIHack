
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(fmsb)

## UTILITIES

getCartCoord <- function(V) {
  r <- V[1]
  theta <- V[2]
  r * c(cos(theta), sin(theta))
}

getPolarCoord <- function(V) {
  x <- V[1]
  y <- V[2]
  c(r = sqrt(x^2 + y^2), theta = atan2(y,x) %% (2*pi))
}

getHex <- function(RGB) {
  paste0("#", paste(sapply(RGB, function(i) as.character(as.hexmode(i))), collapse = ""))
}

shinyServer(function(input, output, session) {
  
  ### DATA Unpacking
  DS <- reactive({input$i_DS})
  BE <- reactive({input$i_BE})
  FE <- reactive({input$i_FE})
  data <- reactive({
    data.frame(check.names = F,
               "Data Science" = c(5,0,DS()),
               "Back-End" = c(5,0, BE()),
               "Front-End" = c(5,0, FE()))
  })
  
  
  ### PLOT
  color.bckp <- reactive({
    topo.colors(3)[which.max(data()[3,])]
  })
  color <- reactive({
    getHex(floor( -(c(DS(), BE(), FE()) / 5)^1.5 * 150 + 250))
  })
  output$radarPlot <- renderPlot({
    # draw radar chart
    radarchart(df = data(), axistype = 0, seg = 5, 
               pcol = "black", pfcol = color())
  })
  
  ### PROFILE
  profile <- reactive({
    getPolarCoord(apply(
      apply(
        matrix(c(DS(), BE(), FE(), 0:2 * 2*pi/3 + pi/2), nrow = 2, byrow = 3),
        2, getCartCoord), 
      1, sum))
  })
  r <- reactive({profile()[1]})
  theta <- reactive({profile()[2]})
  output$Profile <- renderText({
    ifelse(r()<=1, "Balanced", 
           ifelse(theta() < 0, "ERROR", 
                  ifelse(theta() < pi/3, "Data Viz guy",
                         ifelse(theta() < 2*pi/3, "Data Scientist",
                                ifelse(theta() < pi, "Data dude",
                                       ifelse(theta() < 4*pi/3, "Back-End geek", 
                                              ifelse(theta() < 5*pi/3,  "Full Stack dev", "Front-End dev")))))))
  })
  
  ### Updates to the UI
  output$textInvolvement <- reactive({
    c("A few hours", "Half of the time", "I'll be out a couple of hours", "All the time but when sleeping home")[input$i_Involvement]
  })
  
  ### Debugging
  output$DEBUG <- renderPrint({
    print(profile())
    print(color())
  })
})
