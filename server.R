
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
options(shiny.trace=TRUE)
options(shiny.error=traceback)
library(fmsb)
library(RSQLite)
# library(jsonlite)
library(rjson)

# Functions ---------------------------------------------------------------

## UTILITIES #######

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


## BACKEND - Database  #######

# Schema:
user0 <- data.frame(Name = "USER0", Password = "765",
                    FirstName = "Yo", LastName = "YO", 
                    DS = 0, BE = 3, FE = 2, 
                    DSTags = "json", BETags = "json", FETags = "json",
                    Datasets = toJSON(c(listOfDatasets$Education[1], listOfDatasets$Education[2])),
                    Involvement = 2,
                    stringsAsFactors = F)
# Initialization
userDBName <- "BIH_users.sqlite"
userTableName <- "BIH_users"
con <- RSQLite::dbConnect(SQLite(), userDBName, cache_size = 5000, synchronous = "full")
if (!dbExistsTable(con, userTableName)) {
  dbWriteTable(con, name = userTableName, value = user0, row.names = F)
  dbGetQuery(con, paste("DELETE FROM", userTableName, "WHERE Name = 'USER0'"))
}
# # Reset code:
# dbDisconnect(con)
# file.remove(userDBName)

# Functions
getUser <- function(Name) {
  user <- dbGetQuery(con, paste0("SELECT * FROM ", userTableName, " WHERE Name = '", Name, "'"))
  if (nrow(user) == 0) {
    return(NULL)
  } else {
    return(user)
  }
}

unpackDF <- function(User) {
  stopifnot(nrow(User) == 1)
  modDf <- sapply(User, 
                  function(col) { 
                    if(is.character(col)) 
                      paste0("'", col, "'")
                    else
                      col
                  })
  return(paste0("( ", paste(modDf, collapse = ", "), ")"))
}

createUser <- function(User) {
  if (User$Name == "") {
    warning("No name provided for the user")
    return(FALSE)
  }
  dbSendQuery(con, paste0("INSERT INTO ", userTableName, " VALUES ", unpackDF(User)))
}

updateUser <- function(User) {
  if (User$Name == "") {
    warning("No name provided for the user")
    return(FALSE)
  }
  ## UGLY hack
  dbGetQuery(con, paste0("DELETE FROM ", userTableName, " WHERE Name = '", User$Name,"'"))
  dbSendQuery(con, paste0("INSERT INTO ", userTableName, " VALUES ", unpackDF(User)))
}

deleteUser <- function(Name) {
  dbGetQuery(con, paste0("DELETE FROM ", userTableName, " WHERE Name = '", Name,"'"))
}

getListOfUsers <- function() {
  dbGetQuery(con, paste("SELECT Name FROM", userTableName))[[1]]
}

getUserTable <- function() {
  dbReadTable(con, userTableName)
}



# Server definition -------------------------------------------------------

shinyServer(function(input, output, session) {
  
  ### DATA Unpacking
  DS <- reactive({input$i_DS})
  BE <- reactive({input$i_BE})
  FE <- reactive({input$i_FE})
  
  User <- reactive({
    user <- user0
    user$Name <- input$s_Name
    user$Password <- input$s_Password
    user$FirstName <- input$s_FirstName
    user$LastName <- input$s_LastName
    user$DS <- DS()
    user$BE <- BE()
    user$FE <- FE()
    user$DSTags <- toJSON(input$s_DS)
    user$BETags <- toJSON(input$s_BE)
    user$FETags <- toJSON(input$s_FE)
    user$Datasets <- toJSON(input$s_Datasets)
    user$Involvement <- input$i_Involvement
    return(user)
  })
  
  plotData <- function(user){
    data.frame(check.names = F,
               "Data Science" = c(5,0,user[['DS']]),
               "Back-End" = c(5,0, user[['BE']]),
               "Front-End" = c(5,0, user[['FE']]))
  }
  
  ### PLOT
  
  output$radarPlot <- renderPlot({
    # draw radar chart
    user <- User()
    color <- getHex(floor( -(c(user$DS, user$BE, user$FE) / 5)^1.5 * 150 + 250))
    par(mar = c(0,0,0,0))
    radarchart(df = plotData(user), axistype = 0, seg = 5, 
               pcol = "black", pfcol = color)
  })
  
  ### PROFILE
  getProfile <- function(DBF) {
    getPolarCoord(apply(
      apply(
        matrix(c(DBF, 0:2 * 2*pi/3 + pi/2), nrow = 2, byrow = 3),
        2, getCartCoord), 
      1, sum))
  }
  profile <- reactive({
    getProfile(c(DS(), BE(), FE()))
  })
  r <- reactive({profile()[1]})
  theta <- reactive({profile()[2]})
  output$Profile <- renderText({
    ifelse((r()/max(DS(), BE(), FE()))<=1/3, "Balanced", 
           ifelse(theta() < 0, "ERROR", 
                  ifelse(theta() < pi/3, "Data Viz guy",
                         ifelse(theta() < 2*pi/3, "Data Scientist",
                                ifelse(theta() < pi, "Data dude",
                                       ifelse(theta() < 4*pi/3, "Back-End geek", 
                                              ifelse(theta() < 5*pi/3,  "Full Stack dev", "Front-End dev")))))))
  })
  
  ### Updates to the UI
  #   
  output$LoginField <- renderUI(
    selectizeInput(inputId = "s_Name", label = "Select or Create a pseudo below", 
                   choices = c("Login" = "", getListOfUsers()), multiple = FALSE, 
                   options = list(create = "true", createOnBlur = "true", persist = "false", addPrecedence = "true"))
  )
  
  output$textInvolvement <- reactive({
    c("A few hours", "Half of the time", "I'll be out a couple of hours", "I'll go home to rest at night", "I don't intend to sleep")[input$i_Involvement]
  })
  
  ### Backend Listeners
    
  observe({
    
    if (input$s_Name == "") { #Listen and check for the name inbox
      output$LoginAction <- renderUI(
        wellPanel(
          p("Choose or Create a login to start.")))
    }
    
    # Check if user is in database
    if (!is.null(user <- getUser(input$s_Name))) {
      # Load the user data
      updateTextInput(session, inputId = "s_Password", value = "")
      updateTextInput(session, inputId = "s_FirstName", value = user[["FirstName"]])
      updateTextInput(session, inputId = "s_LastName", value = user[["LastName"]])
      updateSliderInput(session, inputId = "i_DS", value = user[["DS"]])
      updateSliderInput(session, inputId = "i_BE", value = user[["BE"]])
      updateSliderInput(session, inputId = "i_FE", value = user[["FE"]])
      updateSliderInput(session, inputId = "i_Involvement", value = user[["Involvement"]])
      updateSelectInput(session, inputId = "s_Datasets", selected = fromJSON(user[["Datasets"]]))
      updateSelectInput(session, inputId = "s_DS", selected = fromJSON(user[["DSTags"]]))
      updateSelectInput(session, inputId = "s_BE", selected = fromJSON(user[["BETags"]]))
      updateSelectInput(session, inputId = "s_FE", selected = fromJSON(user[["FETags"]]))
      output$LoginMessage <- renderText( "User Loaded.")
      output$LoginAction <- renderUI(
        wellPanel(
          p(textOutput("LoginMessage"), align = "center"),
          fluidRow(
            column(6,
                   actionButton("b_Update", "Save Changes"),
                   textOutput("LoginErrorUpdate")),
            column(6,
                   actionButton("b_Delete", "Delete Profile"), 
                   textOutput("LoginErrorDelete"))
          )))  
    } else {
      # Check if the password field is empty
      if (input$s_Password == "") {
        output$LoginAction <- renderUI(
          wellPanel(
            p("Create a weak, non-important password to be sure nobody edits your profile by mistake")
          ))
      } else {
        output$LoginMessage <- "Don't forget to save your profile!"
        output$LoginAction <- renderUI(
          wellPanel(
            p(textOutput("LoginMessage"), align = "center"),
            fluidRow(
              column(6,
                     actionButton("b_Create", "Save Profile"),
                     textOutput("LoginErrorCreate")),
              column(6,
                     actionButton("b_Clear", "Delete Profile"), 
                     textOutput("LoginErrorClear"))
            )))  
        
      }
    }
    
  })
  
  # Clear data Listener
  observe({
    input$b_Clear
    # Default all values
    #     updateTextInput(session, inputId = "s_Password", value = "")
    updateTextInput(session, inputId = "s_FirstName", value = "")
    updateTextInput(session, inputId = "s_LastName", value = "")
    updateSliderInput(session, inputId = "i_DS", value = "")
    updateSliderInput(session, inputId = "i_BE", value = "")
    updateSliderInput(session, inputId = "i_FE", value = "")
    updateSliderInput(session, inputId = "i_Involvement", value = "")
    updateSelectInput(session, inputId = "s_Datasets", selected = "")
    updateSelectInput(session, inputId = "s_DS", selected = "")
    updateSelectInput(session, inputId = "s_BE", selected = "")
    updateSelectInput(session, inputId = "s_FE", selected = "")
#     output$LoginMessage <- renderText("Dashboard Cleared...")
  })
  
  # Create profile listener
  observe({
    if (input$b_Create != 0){
      # Check for missing info
      if (isolate(User()['Name']) == "") {
        output$LoginMessage <- renderText("ERROR: No pseudo provided.")
      }
      if (isolate(input$s_Password == "")) {
        output$LoginMessage <- renderText("ERROR: No password provided.")
      }
      ## Try to retrieve the user data in a non-dependent fashion
      user <- isolate(getUser(User()['Name']))
      # Check for existing user
      if (!is.null(user)) {
        output$LoginMessage <- renderText("ERROR: The user already exists... Try another one or load the existing user by also typing the password.")
      }
      createUser(isolate(User()))
      output$LoginMessage <- renderText("User Created")
    }
  })
  
  # Update profile listener
  observe <- ({
    if (input$b_Update != 0){ 
      # Check for missing info
      if (isolate(User()['Name']) == "") {
        output$LoginMessage <- renderText("ERROR: No pseudo provided.")
      }
      if (isolate(input$s_Password == "")) {
        output$LoginMessage <- renderText("ERROR: No password provided.")
      }
      user <- isolate(getUser(User()['Name']))
      # Check for non-existing user
      if (is.null(user)) {
        output$LoginMessage <- renderText("ERROR: The user with this pseudo doesn't exist yet.")
      } 
      # Check for matching password
      if (user$Password != isolate(User()['Password'])){
        output$LoginMessage <- renderText("ERROR: The password doesn't match the records...")
      }
      # Load the user data
      updateUser(User())
      output$LoginMessage <- renderText("User data updated.")
    }    
  })
  
  # Delete profile listener
  observe({
    if (input$b_Delete != 0){ 
      # Check for missing info
      if (isolate(User()['Name']) == "") {
        output$LoginMessage <- renderText("ERROR: No pseudo provided.")
      }
      if (isolate(input$s_Password == "")) {
        output$LoginMessage <- renderText("ERROR: No password provided.")
      }
      user <- isolate(getUser(User()['Name']))
      # Check for non-existing user
      if (is.null(user)) {
        output$LoginMessage <- renderText("ERROR: The user with this pseudo doesn't exist yet.")
      } 
      # Check for matching password
      if (user$Password != isolate(User()['Password'])){
        output$LoginMessage <- renderText("ERROR: The password doesn't match the records...")
      }
      # Load the user data
      deleteUser(user$Name)
      output$LoginMessage <- renderText("User data Deleted.")
    }    
  })
  
  ### All Profiles pane
  
  Users <- reactive({
    input$b_Create
    input$b_Save
    input$b_Delete
    return(getUserTable())
  })
  
  output$AllProfiles1 <- renderUI({
    users <- Users()
    users <- users[order(users$Name),]
    for (i_user in 1:nrow(users)) {
      local({
        user <- users[i_user, ]
        output[[paste0("plot", user['Name'])]] <- renderPlot({
          color <- getHex(floor( -(c(user$DS, user$BE, user$FE) / 5)^1.5 * 150 + 250))
          par(mar = c(0,0,0,0))
          radarchart(df = plotData(user), axistype = 0, seg = 5, 
                     pcol = "black", pfcol = color, vlabel = c("", "", ""))
        })
      })
    }
    return(apply(users, 1, function(user) {
      fluidRow(fluidRow(
        column(2,
               h4(user['Name'], align = "center"),
               p(user['FirstName'],user['LastName'], align = "center")),
        column(1,
               plotOutput(paste0("plot", user['Name']), height = "80px")),
        column(4,
               withTags(div(class='row-fluid',
                            div(class = 'span2', p("Datasets:")),
                            lapply(unname(substr(unlist(listOfDatasets),1,2)),
                                   function(datasetName){
                                     div(class='span2', checkboxInput(inputId = "simOption", label = datasetName, 
                                                                      value=(datasetName %in% substr(fromJSON(user[["Datasets"]]), 1, 2))))
                                   } )
               ))),
        #         column(4,
        #                selectInput(paste0("bla", 1), label = "Datasets", 
        #                            choices = unname(substr(unlist(listOfDatasets),1,2)), selected = substr(fromJSON(user[["Datasets"]]), 1, 2), multiple = TRUE)),
        column(5,
               #                selectInput(user, label = NULL, choices = fromJSON(user[["BETags"]]), selected = fromJSON(user[["BETags"]]), multiple = TRUE, width = 400),
               #                selectInput(user, label = NULL, choices = fromJSON(user[["FETags"]]), selected = fromJSON(user[["FETags"]]), multiple = TRUE, width = 400),
               #                selectInput(user, label = NULL, choices = fromJSON(user[["DSTags"]]), selected = fromJSON(user[["DSTags"]]), multiple = TRUE, width = 400)
               p(paste("BE:", paste(fromJSON(user[["BETags"]]), collapse = " - "))),
               p(paste("FE:", paste(fromJSON(user[["FETags"]]), collapse = " - "))),
               p(paste("DS:", paste(fromJSON(user[["DSTags"]]), collapse = " - ")))
        )
      ),
      hr()
      )
    }))
  })
  
  
  output$Team1 <- renderUI({
    if (input$s_Name == "") {
      output$Team2 <- renderUI(helpText("Load or Create your profile first"))
      return(helpText("Load or Create your profile first"))
    }
    users <- Users()
    if (input$b_FilterDatasets) {
      myDatasets <- fromJSON(User()[["Datasets"]])
      users <- users[sapply(users$Datasets, function(udi) {
        length(intersect(myDatasets, fromJSON(udi))) != 0
      }),]
    }
    myProfile <- profile()
    profiles <- apply(users[c("DS", "BE", "FE")], 1, getProfile)
    distances <- abs(profiles[1,] - myProfile[1]) + abs(profiles[2,] - myProfile[2])
    users1 <- users[order(distances),]
    users2 <- users[order(-distances),]
    ## Define the graph outputs
    for (i_user in 1:nrow(users)) {
      local({
        user <- users[i_user, ]
        output[[paste0("plot1", user['Name'])]] <- renderPlot({
          color <- getHex(floor( -(c(user$DS, user$BE, user$FE) / 5)^1.5 * 150 + 250))
          par(mar = c(0,0,0,0))
          radarchart(df = plotData(user), axistype = 0, seg = 5, 
                     pcol = "black", pfcol = color, vlabel = c("", "", ""))
        })
        output[[paste0("plot2", user['Name'])]] <- renderPlot({
          color <- getHex(floor( -(c(user$DS, user$BE, user$FE) / 5)^1.5 * 150 + 250))
          par(mar = c(0,0,0,0))
          radarchart(df = plotData(user), axistype = 0, seg = 5, 
                     pcol = "black", pfcol = color, vlabel = c("", "", ""))
        })
      })
    }
    ## Define the other team representation
    output$Team2 <- renderUI({
      return(apply(users2, 1, function(user) {
        fluidRow(fluidRow(
          column(4,
                 h4(user['Name'], align = "center"),
                 p(user['FirstName'],user['LastName'], align = "center")),
          column(4,
                 plotOutput(paste0("plot2", user['Name']), height = "80px")),
          column(2,
                 p("Involvement:", align = "center"),
                 p(user[["Involvement"]], align = "center"))
        )
        )
      }))
    })
    return(apply(users1, 1, function(user) {
      fluidRow(fluidRow(
        column(4,
               h4(user['Name'], align = "center"),
               p(user['FirstName'],user['LastName'], align = "center")),
        column(4,
               plotOutput(paste0("plot1", user['Name']), height = "80px")),
        column(2,
               p("Involvement:", align = "center"),
               p(user[["Involvement"]], align = "center"))
      )
      )
    }))
    
  })
  
  
  
  
  ### Debugging
  output$DEBUG <- renderPrint({
    #     print(profile())
    #     print(getListOfUsers())
    #     dput(User())
    #     print(User())
    #     if (!is.null(getUser(User()['Name'])))
    #       print(fromJSON(getUser(User()['Name'])[["DSTags"]]))
    #     print(users)
    users <- Users()
    myProfile <- profile()
    profiles <- apply(users[c("DS", "BE", "FE")], 1, getProfile)
    distances <- abs(profiles[1,] - myProfile[1]) + abs(profiles[2,] - myProfile[2])
    print(users)
    print(profiles)
    print(myProfile)
    print(distances)
  })
})
