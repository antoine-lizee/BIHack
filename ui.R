
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Define your skills"),
  
  fluidRow( "Log-in stuff" ),
  
  fluidRow( 
    column(4, 
           "Back-end skills",
           "slider",
           "Text"),
    column(4, 
           "Front-end skills",
           "slider",
           "Text"),
    column(4, 
           "Data-Science skills",
           "slider",
           "Text")),
  plotOutput("plot"),
)
)
