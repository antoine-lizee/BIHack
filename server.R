
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

source("StaticVariables.R")

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
  
  output$LoginErrorCreate <- renderText({
    if (input$b_Create != 0){
      # Check for missing info
      if (isolate(User()['Name']) == "") {
        return("ERROR: No pseudo provided.")
      }
      if (isolate(input$s_Password == "")) {
        return("ERROR: No password provided.")
      }
      ## Try to retrieve the user data in a non-dependent fashion
      user <- isolate(getUser(User()['Name']))
      # Check for existing user
      if (!is.null(user)) {
        return("ERROR: The user already exists... Try another one or load the existing user by also typing the password.")
      }
      createUser(isolate(User()))
      return("User Created")
    }
  })
  
  output$LoginAction <- renderUI({
    if (input$s_Name == "") {
      return(wellPanel(
        p("Choose or Create a pseudo to start.")))
    }
    
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
      return(wellPanel(
        p("User Loaded.", align = "center"),
        fluidRow(
          column(6,
                 actionButton("b_Update", "Save Profile"),
                 textOutput("LoginErrorUpdate")),
          column(6,
                 actionButton("b_Delete", "Delete Profile"), 
                 textOutput("LoginErrorDelete"))
          )))  
    } else {
      if (input$s_Password == "") {
        return(wellPanel(
          p("Create a weak, non-important password to be sure nobody edits your profile by mistake")
          ))
      }
      return(wellPanel(
        p( actionButton("b_Create", "Create Profile"),
           actionButton("b_Clear", "Clear Dashboard")),
        textOutput("LoginErrorCreate", inline = T)))
    }
  })
  
  # Clear data
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
  })
  
  output$LoginErrorUpdate <- renderText({
    if (input$b_Update != 0){ 
      # Check for missing info
      if (isolate(User()['Name']) == "") {
        return("ERROR: No pseudo provided.")
      }
      if (isolate(input$s_Password == "")) {
        return("ERROR: No password provided.")
      }
      user <- isolate(getUser(User()['Name']))
      # Check for non-existing user
      if (is.null(user)) {
        return("ERROR: The user with this pseudo doesn't exist yet.")
      } 
      # Check for matching password
      if (user$Password != isolate(User()['Password'])){
        return("ERROR: The password doesn't match the records...")
      }
      # Load the user data
      updateUser(User())
      return("User data updated.")
    }    
  })
  
  output$LoginErrorDelete <- renderText({
    if (input$b_Delete != 0){ 
      # Check for missing info
      if (isolate(User()['Name']) == "") {
        return("ERROR: No pseudo provided.")
      }
      if (isolate(input$s_Password == "")) {
        return("ERROR: No password provided.")
      }
      user <- isolate(getUser(User()['Name']))
      # Check for non-existing user
      if (is.null(user)) {
        return("ERROR: The user with this pseudo doesn't exist yet.")
      } 
      # Check for matching password
      if (user$Password != isolate(User()['Password'])){
        return("ERROR: The password doesn't match the records...")
      }
      # Load the user data
      deleteUser(user$Name)
      return("User data Delted.")
    }    
  })
  
  ### All Profiles pane
  
  users <- getUserTable()
  
  output$AllProfiles1 <- renderUI({
    users <<- dbReadTable(con, userTableName) # try to refresh
    return(apply(users, 1, function(user) {
      fluidRow(fluidRow(
        column(2,
               h4(user['Name'], align = "center"),
               p(user['FirstName'],user['LastName'], align = "center")),
        column(4,
               selectInput(paste0("bla", 1), label = "Datasets", 
                           choices = unname(substr(unlist(listOfDatasets),1,2)), selected = substr(fromJSON(user[["Datasets"]]), 1, 2), multiple = TRUE)),
        column(3,
               plotOutput(paste0("plot", user['Name']), height = "100px")),
        column(3,
               h2("..."))
      ),
      hr()
      )
    }))
  })
  
  users <- getUserTable()
  for (i_user in 1:nrow(users)) {
    user <- users[i_user, ]
    output[[paste0("plot", user['Name'])]] <- renderPlot({
      color <- getHex(floor( -(c(user$DS, user$BE, user$FE) / 5)^1.5 * 150 + 250))
      par(mar = c(0,0,0,0))
      radarchart(df = plotData(user), axistype = 0, seg = 5, 
                 pcol = "black", pfcol = color)
    })
  }
  
  ### Debugging
  output$DEBUG <- renderPrint({
    print(profile())
    print(color())
    print(getListOfUsers())
    dput(User())
    print(User())
    print(fromJSON(getUser(User()['Name'])[["DSTags"]]))
    print(users)
  })
})
