
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

listOfNames <- as.list(c("blabla"="321", "antoine.lizee"="684"))
listOfSkills <- list(c("R", "Python", "C++", "C", "go", "java", "Hadoop", "sysops", "Node.js", "PHP", "Frameworks", "Django", "SQL", "noSQL", "ruby", "ROR"),
                     c("js", "jQuery", "CSS", "D3S", "Shiny", "Objective-C", "Android"),
                     c("R", "Python", "Julia", "ML", "DataVis", "Algorithmics", "Stats"))
listOfDatasets <- list("Education" = c("01 - Help Teachers Get What They Need", 
                                       "04 - Help Donors Choose Better Serve Teachers"),
                       "Safety & Crime" = c("02 - Fight Child Sexual Exploitation", 
                                            "05 - Help Police Prevent Domestic Violence",
                                            "13 - Visualize 911 Service Calls & Police Reports"),
                       "Environment" = c("06 - Parks and Rec",
                                         "09 - Predicting Climate Threats"),
                       "Healthcare" = c("08 - Prepare California for Elderly Health Needs",
                                        "07 - Ebola – Mining Available Data"),
                       "Industry" =c("03 - Reduce Accidents in Mines",
                                     "10 - Assess Flood Risk of Roads and Airports",
                                     "11 - Prevent Supertankers from Grounding",
                                     "12 - Predict Whereabouts of Types of Fish"
                       )
)

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
                                 sliderInput("i_BE", label = "Estimate your overall skill Level", min = 0, max = 5, step = 0.5, value = 0, ticks = TRUE),
                                 selectInput("c_BE", label = "Select / add some skills", choices = c("keyword..."="", listOfSkills[[1]]), multiple = TRUE, selectize = TRUE, options("create" = "yes"))
                               )),
                        column(4, 
                               wellPanel(
                                 h4("Front-end"),
                                 sliderInput("i_FE", label = "Estimate your overall skill Level", min = 0, max = 5, step = 0.5, value = 0, ticks = TRUE),
                                 selectInput("c_FE", label = "Select / add some skills", choices = c("keyword..."="", listOfSkills[[2]]), multiple = TRUE, selectize = TRUE, options("create" = "yes"))
                               )),
                        column(4, 
                               wellPanel(
                                 h4("Data-Science"),
                                 sliderInput("i_DS", label = "Estimate your overall skill Level", min = 0, max = 5, step = 0.5, value = 0, ticks = TRUE),
                                 selectInput("c_DS", label = "Select / add some skills", choices = c("keyword..."="", listOfSkills[[3]]), multiple = TRUE, selectize = TRUE, options("create" = "yes"))
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
                                 sliderInput("i_Involvement", label = "How much time do you intend to spend at the hackathon?", min = 1, max = 4, value = 2, step = 1 )
                               )
                        )
                      )
               )
             )
           )
  ),
  tabPanel("All Profiles",
           h4("The database with all profiles is available", a(href = "www",  "here"))
  ),
  tabPanel("Potential Teams"
  ),
  tabPanel("Debug",
           verbatimTextOutput("DEBUG"))
)
)