# Hackathoner Profiler
# This script defines the global variables and other general conventions used throughout the app.
# 
# Copyright Antoine Lizee 11/2014 antoine.lizee@gmail.com. See the license included in the project.



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

ifelseFun <- function(vec, ifVec, fun, ...) {
  resVec <- fun(vec, ...)
  if (length(resVec) == 1){
    if (resVec) {
      return(ifVec)
    } else {
      return(vec)
    }
  } else {
    return(ifelse(resVec, ifVec, vec))
  }
}
# Tested like that:
ifelseFun(c(1,2,NA,3), 0, is.na)
ifelseFun(c(1,2,NA,3), "", is.null)
ifelseFun(NULL, "", is.null)



## BACKEND - Database  #######

# Functions
getCon <- function() {
  RSQLite::dbConnect(RSQLite::SQLite(), userDBName, cache_size = 5000, synchronous = "full")
}

closeCon <- function(con) {
  RSQLite::dbDisconnect(con)
}

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
  ## UGLY update hack
  dbGetQuery(con, paste0("DELETE FROM ", userTableName, " WHERE Name = '", User$Name,"'"))
  dbSendQuery(con, paste0("INSERT INTO ", userTableName, " VALUES ", unpackDF(User)))
}

deleteUser <- function(Name) {
  dbGetQuery(con, paste0("DELETE FROM ", userTableName, " WHERE Name = '", Name,"'"))
}

getListOfUsers <- function() {
  sort(dbGetQuery(con, paste("SELECT Name FROM", userTableName))[[1]])
}

getUserTable <- function() {
  dbReadTable(con, userTableName)
}

# Initialization
con <- getCon()
if (!dbExistsTable(con, userTableName)) {
  dbWriteTable(con, name = userTableName, value = user0, row.names = F)
  dbGetQuery(con, paste("DELETE FROM", userTableName, "WHERE Name = 'USER0'"))
}
# # Reset code:
# dbDisconnect(con)
# file.remove(userDBName)



# Server definition -------------------------------------------------------

shinyServer(function(input, output, session) {
  
  ### Session management ####################
  
  con <- getCon()
  
  # Timing & schedule (hardly necessary)
  values <- reactiveValues(starting = TRUE,
                           loadingProfile = FALSE)
  session$onFlushed(function() {
    values$starting <- FALSE
    values$loadingProfile <- FALSE
  }, once = FALSE)
  
  # Closing connection
  session$onSessionEnded(function(){
    closeCon(con)
  })
  
  
  ### DATA Unpacking ####################
  
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
  
  
  ### PLOT  ####################
  
  plotData <- function(user = list(DS = 0, BE = 0, FE = 0)){
    data.frame(check.names = F,
               "Data Science" = c(5,0,user[['DS']]),
               "Back-End" = c(5,0, user[['BE']]),
               "Front-End" = c(5,0, user[['FE']]))
  }
  
  color <- function(user) {
    getHex(floor( -(c(user$DS, user$BE, user$FE) / 5)^1.5 * 150 + 250))
  }
  
  output$radarPlot <- renderPlot({
    # Doesn't work as expected
    #     if (values$loadingProfile) {
    #       #       cat("##### DEBUG", file = stderr())
    #       invalidateLater(2000, session)
    #     } else {
    # draw radar chart
    user <- User()
    par(mar = c(0,0,0,0))
    radarchart(df = plotData(user), axistype = 0, seg = 5, 
               pcol = "black", pfcol = color(user))
    #     }
  })
  
  
  ### PROFILE  ####################
  
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
  
  
  ### UIs  ####################
  
  # Login selectize
  output$LoginField <- renderUI(
    selectizeInput(inputId = "s_Name", label = "Select or Create a pseudo below", 
                   choices = c("Login" = "", getListOfUsers()), multiple = FALSE, 
                   options = list(create = "true", createOnBlur = "true", persist = "false", addPrecedence = "true"))
  )
  # Update the choices
  #   observe({
  #     input$b_Login
  #     updateSelectizeInput("s_Name", choices = c("Login" = "", getListOfUsers()))
  #   })
  
  output$textInvolvement <- reactive({
    c("A few hours", "Half of the time", "I'll be out a couple of hours", "I'll go home to rest at night", "I don't intend to sleep")[input$i_Involvement]
  })
  
  loggedInUI <- wellPanel(
    p(textOutput("LoginMessage", inline = TRUE), align = "center"),
    fluidRow(
      column(6, actionButton("b_Update", "Save Changes")),
      column(6, actionButton("b_Delete", "Delete Profile"))
    ))
  
  firstLoggingUI <- wellPanel(
    p(textOutput("LoginMessage"), align = "center"),
    fluidRow(
      column(6, actionButton("b_Create", "Save Profile"))
    ))
  
  
  ### Backend Listeners ##################################
  
  ## Main listener for the login and password field.
  output$LoginAction <- renderUI({
    
    #     if (values$starting) {
    #       return(helpText("Loading..."))
    #     }
    
    if (input$s_Name == "") { #Listen and check for the name inbox
      return(
        wellPanel(
          p("Choose or Create a login to start.")))
    }
    
    # Check if user is in database
    if (!is.null(user <- getUser(input$s_Name))) {
      # Load the user data
      values$loadingProfile <- TRUE
      updateTextInput(session, inputId = "s_Password", value = "")
      updateTextInput(session, inputId = "s_FirstName", value = user[["FirstName"]])
      updateTextInput(session, inputId = "s_LastName", value = user[["LastName"]])
      updateSliderInput(session, inputId = "i_DS", value = user[["DS"]])
      updateSliderInput(session, inputId = "i_BE", value = user[["BE"]])
      updateSliderInput(session, inputId = "i_FE", value = user[["FE"]])
      updateSliderInput(session, inputId = "i_Involvement", value = user[["Involvement"]])
      updateSelectInput(session, inputId = "s_Datasets", selected = fromJSON(user[["Datasets"]]))
      updateSelectInput(session, inputId = "s_DS", selected = ifelseFun(fromJSON(user[["DSTags"]]), "", is.null))
      updateSelectInput(session, inputId = "s_BE", selected = ifelseFun(fromJSON(user[["BETags"]]), "", is.null))
      updateSelectInput(session, inputId = "s_FE", selected = ifelseFun(fromJSON(user[["FETags"]]), "", is.null))
      output$LoginMessage <- renderText( "User Loaded.")
      return(loggedInUI)  
    } else {
      # Check if the password field is empty
      if (input$s_Password == "") {
        return(
          wellPanel(
            p("Create a weak, non-important password to be sure nobody edits your profile by mistake")
          ))
      } else {
        output$LoginMessage <- renderText("Don't forget to save your profile!")
        # Reset personal info
        updateTextInput(session, inputId = "s_FirstName", value = "")
        updateTextInput(session, inputId = "s_LastName", value = "")
        return(firstLoggingUI)  
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
  
  # Listener for profile creation
  observe({
    # Need that to prevent eecution of code before creation of the widget (and thus the variable)
    if (!is.null(input$b_Create) && input$b_Create != 0){
      # Check for missing info
      if (isolate(User()['Name']) == "") {
        output$LoginMessage <- renderText("ERROR: No pseudo provided.")
      } else if (isolate(input$s_Password == "")) {
        output$LoginMessage <- renderText("ERROR: No password provided.")
      } else {
        ## Try to retrieve the user data in a non-dependent fashion
        user <- isolate(getUser(User()['Name']))
        # Check for existing user
        if (!is.null(user)) {
          output$LoginMessage <- renderText("ERROR: The user already exists... Try another login or load the existing user by also typing the password.")
          return()
        } else {
          tryCatch({
            createUser(isolate(User()))
            output$LoginMessage <- renderText("User Created")
            output$LoginUI <- loggedInUI
          }, error = function(e) {
            output$LoginMessage <- renderText("ERROR: ", e$message)
          })
        }
      }
    }
  })
  
  # Listener for profile update
  observe({
    if (!is.null(input$b_Update) && input$b_Update != 0){ 
      # Check for missing info
      if (isolate(User()['Name']) == "") {
        output$LoginMessage <- renderText("ERROR: No pseudo provided.")
        return
      }
      if (isolate(input$s_Password == "")) {
        output$LoginMessage <- renderText("ERROR: No password provided.")
        return
      }
      user <- isolate(getUser(User()['Name']))
      # Check for non-existing user
      if (is.null(user)) {
        output$LoginMessage <- renderText("ERROR: The user with this pseudo doesn't exist yet.")
        return
      } 
      # Check for matching password
      if (user$Password != isolate(User()['Password'])){
        output$LoginMessage <- renderText("ERROR: The password doesn't match the records...")
        return
      }
      # Load the user data
      updateUser(User())
      output$LoginMessage <- renderText("User data updated.")
    }    
  })
  
  # Listener for profile deletion
  observe({
    if (!is.null(input$b_Delete) && input$b_Delete != 0){ 
      # Check for missing info
      if (isolate(User()['Name']) == "") {
        output$LoginMessage <- renderText("ERROR: No pseudo provided.")
      }
      else if (isolate(input$s_Password == "")) {
        #         output$LoginMessage <- renderText(HTML('<font color="red"> Please type the password to delete this profile!</font>'))
        output$LoginMessage <- renderText("Please type the password to delete this profile!") #TODO put in red
      } else {
        user <- isolate(getUser(User()['Name']))
        # Check for non-existing user
        if (is.null(user)) {
          output$LoginMessage <- renderText("ERROR: The user with this pseudo doesn't exist yet.")
        } else if (user$Password != isolate(User()['Password'])){ # Check for matching password
          output$LoginMessage <- renderText("The password doesn't match the records... Try again?")
        } else {
          # Delete the user data
          tryCatch({
            deleteUser(user$Name)
            output$LoginMessage <- renderText("User data Deleted.")
          },
          error = function(e) {
            output$LoginMessage <- renderText("ERROR:", e$message)
          })
        }
      }
    }    
  })
  
  
  ### All Profiles pane ################################################
  
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
          par(mar = c(0,0,0,0))
          radarchart(df = plotData(user), axistype = 0, seg = 5, 
                     pcol = "black", pfcol = color(user), vlabel = c("", "", ""))
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
               #                selectInput(user, label = NULL, choices = fromJSON(user[["DSTags"]]), selected = fromJSON(user[["DSTags"]]), multiple = TRUE, width = 400) # width="100%" seems the actual way to fill them up
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
          par(mar = c(0,0,0,0))
          radarchart(df = plotData(user), axistype = 0, seg = 5, 
                     pcol = "black", pfcol = color(user), vlabel = c("", "", ""))
        })
        output[[paste0("plot2", user['Name'])]] <- renderPlot({
          par(mar = c(0,0,0,0))
          radarchart(df = plotData(user), axistype = 0, seg = 5, 
                     pcol = "black", pfcol = color(user), vlabel = c("", "", ""))
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
  
  
  
  
  ### Debugging #############################################
  
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
  
  ###########################################################
  # Debug Area, from https://gist.github.com/ptoche/8405209 #
  
  output$Console <- renderUI({
    btnTags <- function(){tags$style(type = 'text/css',"")}
    if (is.null(input$console) || !nzchar(input$console) || input$console == 0) {
      btnTags <- function(){tags$style(type = 'text/css'
                                       , '#console {color: rgb(221,17,68);}'
      )}
    }
    list(btnTags(),actionButton(inputId = "console", label = "console"))
  })
  
  observe(label = "console", {
    if (is.null(input$console) || !nzchar(input$console)) {return()}
    if (input$console != 0) {
      options(browserNLdisabled = TRUE)
      saved_console <- ".RDuetConsole"
      if (file.exists(saved_console)) {load(saved_console)}
      isolate(browser())
      save(file=saved_console,list=ls(environment()))
    }
  })
  
  
})
