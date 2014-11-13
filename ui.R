
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

listOfNames <- as.list(c("blabla"="321", "antoine.lizee"="684"))
listOfSkills <- list(c("R", "Python", "C++", "C", "go", "java", "Hadoop", "sysops", "Node.js", "PHP", "Frameworks", "Django", "SQL", "noSQL", "ruby", "ROR"),
                     c("js", "jQuery", "CSS", "D3S", "Shiny", "Objective-C", "Android"),
                     c("R", "Python", "Julia", "ML", "DataVis", "Algorithmics", "Stats"))

library(shiny)

shinyUI(navbarPage(
  "BayesImpact Hackathon Profiler",
  tabPanel("Your Profile",
           fluidPage(fluidRow(
             column(3, 
                    
                    # Application title
                    titlePanel("Define your skills"),
                    hr(),
                    wellPanel(
                      h3("Login Info", align = "center"),
                      helpText("Create a login or select one that you have already created."),
                      selectizeInput(inputId = "Login", label = NULL, choices = c(list("Login" = ""), listOfNames), multiple = FALSE, options = list(create = "true")),
                      helpText("Type the associated password (or create one). Use a Weak, non-important password, like the four random digits suggested below."),
                      textInput(inputId = "Password", label = NULL, value = paste0(sample(9,4), collapse = "")) ,
                      
                      fluidRow(actionButton("b_Create", "Create Profile")),
                      fluidRow(actionButton("b_Load", "Load Profile")), 
                      fluidRow(actionButton("b_Update", "Update Profile"))
                    )),
             column(9,
                    fluidRow( 
                      column(4, 
                             wellPanel(
                               h4("Back-end"),
                               sliderInput("i_BE", label = "Overall Skill Level", min = 0, max = 5, step = 0.5, value = 0, ticks = TRUE),
                               selectInput("c_BE", label = "Select / add some skills", choices = c("keyword..."="", listOfSkills[[1]]), multiple = TRUE, selectize = TRUE, options("create" = "yes"))
                             )),
                      column(4, 
                             wellPanel(
                               h4("Front-end"),
                               sliderInput("i_FE", label = "Overall Skill Level", min = 0, max = 5, step = 0.5, value = 0, ticks = TRUE),
                               selectInput("c_FE", label = "Select / add some skills", choices = c("keyword..."="", listOfSkills[[2]]), multiple = TRUE, selectize = TRUE, options("create" = "yes"))
                             )),
                      column(4, 
                             wellPanel(
                               h4("Data-Science"),
                               sliderInput("i_DS", label = "Overall Skill Level", min = 0, max = 5, step = 0.5, value = 0, ticks = TRUE),
                               selectInput("c_DS", label = "Select / add some skills", choices = c("keyword..."="", listOfSkills[[3]]), multiple = TRUE, selectize = TRUE, options("create" = "yes"))
                             ))
                    ),
                    hr(),
                    plotOutput("radarPlot")
             )
           )
           )
  ),
  tabPanel("All Profiles",
           h4("The database with all profiles is available", a(href = "www",  "here"))
    ),
  tabPanel("Potential Teams"
           )
)
)