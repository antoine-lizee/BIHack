
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


source("StaticVariables.R")

library(shiny)

shinyUI(navbarPage(
  "BayesImpact Hackathon Profiler",
  tabPanel("Your Profile",
           fluidPage(
             fluidRow(
               column(3, 
                      # Application title
                      titlePanel("Define your skills"),
                      hr(),
                      wellPanel(
                        h2("Login Info", align = "center"),
                        #                         helpText("Create a meaningful, easily recognized pseudo or select one that you have already created."),
                        uiOutput("LoginField"), 
                        #                         selectizeInput(inputId = "s_Name", label = NULL, choices = c(list("Login" = ""), list("blou", "bli")), multiple = FALSE, options = list(create = "true")),
                        helpText(paste0("Type the associated 'identifying token' that you will have to remember. Like '", paste(sample(10,4), collapse = ""), "' for instance.")),
                        textInput(inputId = "s_Password", label = NULL, ""), #value = paste0(sample(9,4), collapse = "")),
                        hr(),
                        helpText("Select the action to perform from that info")
                        actionButton("b_Create", "Create Profile"),
                        textOutput("LoginErrorCreate"),
                        actionButton("b_Load", "Load Profile"), 
                        textOutput("LoginErrorLoad"),
                        actionButton("b_Update", "Update Profile"),
                        textOutput("LoginErrorUpdate"),
                        actionButton("b_Delete", "Delete Profile"), 
                        textOutput("LoginErrorDelete")
                      )),
               column(9,
                      fluidRow( 
                        column(4, 
                               wellPanel(
                                 h4("Back-end"),
                                 sliderInput("i_BE", label = "Estimate your overall skill Level", min = 0, max = 5, step = 0.5, value = 0, ticks = TRUE),
                                 selectInput("s_BE", label = "Select / add some skills", choices = c("keyword..."="", listOfSkills[[1]]), multiple = TRUE, options("create" = "yes"))
                               )),
                        column(4, 
                               wellPanel(
                                 h4("Front-end"),
                                 sliderInput("i_FE", label = "Estimate your overall skill Level", min = 0, max = 5, step = 0.5, value = 0, ticks = TRUE),
                                 selectInput("s_FE", label = "Select / add some skills", choices = c("keyword..."="", listOfSkills[[2]]), multiple = TRUE, options("create" = "yes"))
                               )),
                        column(4, 
                               wellPanel(
                                 h4("Data-Science"),
                                 sliderInput("i_DS", label = "Estimate your overall skill Level", min = 0, max = 5, step = 0.5, value = 0, ticks = TRUE),
                                 selectInput("s_DS", label = "Select / add some skills", choices = c("keyword..."="", listOfSkills[[3]]), multiple = TRUE, options("create" = "yes"))
                               ))
                      ),
                      hr(),
                      fluidRow(
                        column(8,
                               plotOutput("radarPlot")),
                        column(4,
                               p("Your Profie is:", align = "center"),
                               h2(textOutput("Profile", inline = TRUE), align = "center"),
                               wellPanel(
                                 selectInput(inputId = "s_Datasets", label = "Select the dataset(s) your are interesting to work on:", choices = listOfDatasets, multiple = TRUE),
                                 sliderInput("i_Involvement", label = "How much time do you intend to spend at the hackathon?", min = 1, max = 4, value = 2, step = 1 ),
                                 p(textOutput("textInvolvement"))
                               )
                        )
                      )
               )
             )
           )
  ),
  tabPanel("All Profiles",
           h4("The database with all profiles is available", a(href = "BIH_users.sqlite",  "here")),
           hr(),
           renderUI("AllProfiles1")
  ),
  tabPanel("Potential Teams",
           "Coming soon"
  ),
  tabPanel("Debug",
           verbatimTextOutput("DEBUG")
  )
)
)