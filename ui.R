# Hackathoner Profiler
# This script defines the user interface of the app.
# As a convention, "input" variable names are prefixed by a one-letter indicator on the type of the variable,
# while "output" variable names are capitalized.
# 
# Copyright Antoine Lizee 11/2014 antoine.lizee@gmail.com. See the license included in the project.


library(shiny)

shinyUI(navbarPage(
  "The Hackathoner Profiler",
  tabPanel("Your Profile",
           fluidPage(
             fluidRow(
               column(3, 
                      # Application title
                      h3("Define your skills", align = "center"),
                      hr(),
                      wellPanel(
                        h3("Login Info", align = "center"),
                        uiOutput("LoginField"), 
                        conditionalPanel(
                          condition = "typeof input.s_Name !== 'undefined' & input.s_Name != ''",
                          wellPanel(
                            textInput(inputId = "s_Password", label = "Password", ""),
                            uiOutput("LoginAction")
                          )),
                        #                         helpText(paste0("'Password'")),# that you will have to remember. Like '", paste(sample(10,4), collapse = ""), "' for instance.")),
                        #                         textInput(inputId = "s_Password", label = "Password", ""), #value = paste0(sample(9,4), collapse = "")),
                        hr(),
                        helpText("Optional information to identify you better"),
                        textInput(inputId = "s_FirstName", label = "First Name", ""),
                        textInput(inputId = "s_LastName", label = "Last Name", "")
                      )
               ),
               column(9,
                      fluidRow( 
                        column(4, 
                               wellPanel(
                                 h4("Back-end", align = "center"),
                                 sliderInput("i_BE", label = "Estimate your overall skill Level", min = 0, max = 5, step = 0.5, value = 0, ticks = TRUE),
                                 selectInput("s_BE", label = "Select / add some skills", choices = c("keyword..."="", listOfSkills[["BE"]]), multiple = TRUE, options("create" = "yes"))
                               )),
                        column(4, 
                               wellPanel(
                                 h4("Front-end", align = "center"),
                                 sliderInput("i_FE", label = "Estimate your overall skill Level", min = 0, max = 5, step = 0.5, value = 0, ticks = TRUE),
                                 selectInput("s_FE", label = "Select / add some skills", choices = c("keyword..."="", listOfSkills[["FE"]]), multiple = TRUE, options("create" = "yes"))
                               )),
                        column(4, 
                               wellPanel(
                                 h4("Data-Science", align = "center"),
                                 sliderInput("i_DS", label = "Estimate your overall skill Level", min = 0, max = 5, step = 0.5, value = 0, ticks = TRUE),
                                 selectInput("s_DS", label = "Select / add some skills", choices = c("keyword..."="", listOfSkills[["DS"]]), multiple = TRUE, options("create" = "yes"))
                               ))
                      ),
                      hr(),
                      fluidRow(
                        column(8,
                               style = "position:relative",
                               plotOutput("radarPlot"),
                               div(actionButton("b_Clear", label = "Reset Dashboard", icon = icon("eraser")), style = "position:absolute; left:2px; top:2px")), #fa-times-circle / fa-close (or fa-remove or fa-times) / 
                        column(4,
                               p("Your Profie is:", align = "center"),
                               h2(textOutput("Profile", inline = TRUE), align = "center"),
                               wellPanel(
                                 selectInput(inputId = "s_Datasets", label = "Select the dataset(s) your are interested in:", choices = listOfDatasets, multiple = TRUE),
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
             column(4, offset = 3, h4("The database with all profiles is available", a(href = "https://www.dropbox.com/sh/e8v6r03jv1g7bl5/AAC8Cgs8Z3paC1AJ7tIHIWZ1a?dl=1",  "here")),style = "vertical-align: top;")#, style = "vertical-align: top; align:left; display: table-cell;float: none;")
           ),
           hr(),
           uiOutput("AllProfiles1")
  ),
  tabPanel("Find your Team",
           fluidRow(
             column(4, offset = 1, h3("Choose some teammates")),
             column(3, offset = 2, checkboxInput(inputId = "b_FilterDatasets", "Filter for common datasets"))),
           hr(),
           uiOutput("OrderedProfiles")
  ),
  tabPanel("Remarks",
           includeHTML("www/remarks.html")
  ),
  source("ui/debugUI.R", local = TRUE)$value,
  hr(),
  p("Created with Shiny, love and pain by Antoine Lizee ", a("(Github)", href = "https://github.com/antoine-lizee/BIHack"), align = "right"),
  
  #### CSS & additional scripts ###########
  includeCSS("www/quickPatches.css")
))


