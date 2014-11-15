
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
                      h3("Define your skills", align = "center"),
                      hr(),
                      wellPanel(
                        h3("Login Info", align = "center"),
                        uiOutput("LoginAction"),
                        #                         helpText("Create a meaningful, easily recognized pseudo or select one that you have already created."),
                        uiOutput("LoginField"), 
                        #                         selectizeInput(inputId = "s_Name", label = NULL, choices = c(list("Login" = ""), list("blou", "bli")), multiple = FALSE, options = list(create = "true")),
                        helpText(paste0("'Password'")),# that you will have to remember. Like '", paste(sample(10,4), collapse = ""), "' for instance.")),
                        textInput(inputId = "s_Password", label = NULL, ""), #value = paste0(sample(9,4), collapse = "")),
                        hr(),
                        helpText("Optional information to identify you better"),
                        textInput(inputId = "s_FirstName", label = "First Name", ""),
                        textInput(inputId = "s_LastName", label = "Last Name", "")
                      )
                      #                       wellPanel(
                      #                         actionButton("b_Create", "Create Profile"),
                      #                         textOutput("LoginErrorCreate"),
                      #                         actionButton("b_Load", "Load Profile"), 
                      #                         textOutput("LoginErrorLoad"),
                      #                         actionButton("b_Update", "Save Profile"),
                      #                         textOutput("LoginErrorUpdate"),
                      #                         actionButton("b_Delete", "Delete Profile"), 
                      #                         textOutput("LoginErrorDelete")
                      #                       )
               ),
               column(9,
                      fluidRow( 
                        column(4, 
                               wellPanel(
                                 h4("Back-end", align = "center"),
                                 sliderInput("i_BE", label = "Estimate your overall skill Level", min = 0, max = 5, step = 0.5, value = 0, ticks = TRUE),
                                 selectInput("s_BE", label = "Select / add some skills", choices = c("keyword..."="", listOfSkills[[1]]), multiple = TRUE, options("create" = "yes"))
                               )),
                        column(4, 
                               wellPanel(
                                 h4("Front-end", align = "center"),
                                 sliderInput("i_FE", label = "Estimate your overall skill Level", min = 0, max = 5, step = 0.5, value = 0, ticks = TRUE),
                                 selectInput("s_FE", label = "Select / add some skills", choices = c("keyword..."="", listOfSkills[[2]]), multiple = TRUE, options("create" = "yes"))
                               )),
                        column(4, 
                               wellPanel(
                                 h4("Data-Science", align = "center"),
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
                                 sliderInput("i_Involvement", label = "How much time do you intend to spend at the hackathon?", min = 1, max = 5, value = 2, step = 1 ),
                                 p(textOutput("textInvolvement"))
                               )
                        )
                      )
               )
             )
           )
  ),
  tabPanel("All Profiles",
           fluidRow(
             column(4, offset = 1, h3("Browse hackers")),#,style = "vertical-align: bottom; display: table-cell;float: none;"),
             #              column(2, hr(), style = "vertical-align: bottom; display: table-cell;float: none;"),
             column(4, offset = 3, h4("The database with all profiles is available", a(href = "https://www.dropbox.com/s/a9wmnne0ditzuxw/BIH_users.sqlite?dl=1",  "here")),style = "vertical-align: top;")#, style = "vertical-align: top; align:left; display: table-cell;float: none;")
           ),
           hr(),
           uiOutput("AllProfiles1")
  ),
  #   tabPanel("Find your Team",
  #            fluidRow(
  #              column(4, offset = 1, h3("Choose some teammates")),
  #              column(3, offset = 2, checkboxInput(inputId = "b_FilterDatasets", "Filter for common datasets"))),
  #            hr(),
  #            fluidRow(column(4,
  #                            h4("Like-minded people", align = "center"),
  #                            uiOutput("Team1")),
  #                     column(8,
  #                            h4("Complementary people", align = "center")),
  #                     fluidRow(
  #                       column(6,
  #                              div(class="thin_vertical_line", style="position:absolute;left:-15px;height:70%;top:0px;"),
  #                              uiOutput("Team2")),
  #                       column(6,
  #                              uiOutput("Team3")))
  #            )
  #            
  #   ),
  tabPanel("Find your Team",
           fluidRow(
             column(4, offset = 1, h3("Choose some teammates")),
             column(3, offset = 2, checkboxInput(inputId = "b_FilterDatasets", "Filter for common datasets"))),
           hr(),
           fluidRow(column(6,
                           h4("Like-minded people", align = "center"),
                           uiOutput("Team1")),
                    column(6,
                           h4("Complementary people", align = "center"),
                           uiOutput("Team2"))
           )
           
  ),
  tabPanel("Debug",
           verbatimTextOutput("DEBUG")
  ),
  hr(),
  p("Created with Shiny, love and pain by Antoine Lizee ", a("(Github)", href = "https://github.com/antoine-lizee/BIHack"), align = "right")
)
)