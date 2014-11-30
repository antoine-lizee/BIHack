# Hackathoner Profiler
# This script defines the debugging part of the ui side function for the Shiny app.
# 
# Copyright Antoine Lizee 11/2014 antoine.lizee@gmail.com. See the license included in the project.



{if (b_DEBUG) {
  tabPanel("Debug",
           fluidRow(column(2,uiOutput("Console")),
                    column(10, 
                           helpText(HTML("The console should display a Browse prompt: <code>Browse[1]></code>")),
                           helpText(HTML("Enter <code>c</code> at the prompt to stop communication with the console and resume with the shiny app")))
           ),
           br(),
           verbatimTextOutput("DEBUG")
  )
} else {
  NULL
}}