# Hackathoner Profiler
# This script defines the global variables and other general conventions used throughout the app.
# 
# Copyright Antoine Lizee 11/2014 antoine.lizee@gmail.com. See the license included in the project.



# Initialization ----------------------------------------------------------

source("server/utilities.R", local = TRUE) # local = TRUE is actually not necessary here, just a bit cleaner (no need to go up to fetch the utilities for the global env.)

# Initialization of the db
con0 <- getCon()
if (!dbExistsTable(con0, userTableName)) {
  dbWriteTable(con0, name = userTableName, value = user0, row.names = F)
  dbGetQuery(con0, paste("DELETE FROM", userTableName, "WHERE Name = 'USER0'"))
}
closeCon(con0)
con <- getCon() # see line 30
# # Reset code:
# dbDisconnect(con)
# file.remove(userDBName)

# Server definition -------------------------------------------------------

shinyServer(function(input, output, session) {
  
  ### Session management ####################
  
  ## Geting the connection.
  # Three solution: 
  #   - sourcing the utilities here (non optimal launching of the session), or copying and attaching a "utilities" environment created earlier
  #   - using one connection for all (outside the server function), 
  #   - passing around the connection. 
  #   We use the second one for now, relying on the single-threaded operation of the app. (shiny-server non-pro)
  
  #   con <- getCon()
  #   sendDEBUG("Opening connection")
  #   browser()
  #   # Closing connection
  #   session$onSessionEnded(function(){
  #     sendDEBUG("Closing db connection")
  #     #     closeCon(con)
  #   })
  
  ## Timing & schedule (hardly necessary)
  values <- reactiveValues(starting = TRUE,
                           loadingProfile = FALSE
  )
  session$onFlushed(function() {
    values$starting <- FALSE
    values$loadingProfile <- FALSE
    #     sendDEBUG("loading Profile OFF")
  }, once = FALSE)
  
  
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
    #     #     Doesn't work as expected, because the three variables i_X are updated all at once with 
    #     #     3 calls to "updateSelectInput()" but are sent back to the server one by one. (Shiny BUG/limitation ?)
    #     #     see the stackoverflow relevant question     
    #     if (values$loadingProfile) {
    #             sendDEBUG("loading Profile still ON")
    #       #       invalidateLater(2000, session)
    #       "Loading Profile"      
    #     } else {
    #       draw radar chart
    user <- User()
    par(mar = c(0,0,0,0))
    radarchart(df = plotData(user), axistype = 0, seg = 5, 
               pcol = "black", pfcol = color(user))
    #     }
  })
  
  
  ### PROFILE  ####################
  
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
  output$LoginField <- renderUI({
    #     sendDEBUG("Rendering login field")
    selectizeInput(inputId = "s_Name", label = "Create a profile by typing a login into the box below or select an existing one to load the corresponding profile:", 
                   choices = c("Login" = "", getListOfUsers()), multiple = FALSE, 
                   options = list(create = "true", createOnBlur = "true", persist = "false", addPrecedence = "true"))
  })
  # Update the choices
  #   observe({
  #     input$b_Login
  #     updateSelectizeInput("s_Name", choices = c("Login" = "", getListOfUsers()))
  #   })
  
  output$textInvolvement <- reactive({
    c("A few hours", "Half of the time", "I'll be out a couple of hours", "I'll go home to rest at night", "I don't intend to sleep")[input$i_Involvement]
  })
  
  loggedInUI <- div(
    p(textOutput("LoginMessage", inline = TRUE), align = "center"),
    fluidRow(
      column(6, actionButton("b_Update", "Save Changes")),
      column(6, actionButton("b_Delete", "Delete Profile"))
    ))
  
  firstLoggingUI <- div(
    p(textOutput("LoginMessage"), align = "center"),
    fluidRow(
      column(6, actionButton("b_Create", "Save Profile"))
    ))
  
  
  ### Backend Listeners ##################################
  
  ## Main listener for the login and password field.
  output$LoginAction <- renderUI({
    
    if (values$starting) {
      return(helpText("Loading..."))
    } else {
      
      if (is.null(input$s_Name) || input$s_Name == "") { #Listen and check for the name inbox
        return(
          NULL)
      }
      
      # Check if user is in database
      if (!is.null(user <- getUser(input$s_Name))) {
        # Load the user data
        #         sendDEBUG("loading Profile ON")
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
        if (is.null(input$s_Password) || input$s_Password == "") {
          return(
              p("Create a weak, non-important password to be sure nobody edits your profile by mistake")
            )
        } else {
          output$LoginMessage <- renderText("Don't forget to save your profile!")
          # Reset personal info
          updateTextInput(session, inputId = "s_FirstName", value = "")
          updateTextInput(session, inputId = "s_LastName", value = "")
          return(firstLoggingUI)  
        }
      }
    }
    
  })
  
  ## Button listeners
  
  ## Clear data Listener
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
            output$LoginAction <- renderUI(loggedInUI)
            updateTextInput(session, "s_Password", value = "")
          }, error = function(e) {
            output$LoginMessage <- renderText(paste("ERROR: ", e$message))
          })
        }
      }
    }
  })
  
  # Fuction for profile modification
  modifyUserBackend <- function( noPasswordMessage, okayAction ) {
    # Check for missing info
    if (isolate(User()['Name']) == "") {
      output$LoginMessage <- renderText("ERROR: No pseudo provided.")
    } else if (isolate(input$s_Password == "")) {
      output$LoginMessage <- renderText(noPasswordMessage) #TODO put in red
    } else {
      user <- isolate(getUser(User()['Name']))
      # Check for non-existing user
      if (is.null(user)) {
        output$LoginMessage <- renderText("ERROR: The user with this pseudo doesn't exist yet.")
      } else if (user$Password != isolate(User()['Password'])){ # Check for matching password
        output$LoginMessage <- renderText("The password doesn't match our records... Try again?")
      } else {
        tryCatch({
          okayAction(user)
        },
        error = function(e) {
          output$LoginMessage <- renderText(paste("ERROR:", e$message))
          if (b_DEBUG) stop(e)
        })
      }
    }
  }
  
  # Listener for profile update
  observe({
    if (!is.null(input$b_Update) && input$b_Update != 0){ 
      modifyUserBackend( "Please provide the matching password to update your Profile.",
                         function(user) {
                           # Update the user data
                           updateUser(isolate(User()))
                           output$LoginMessage <- renderText("User data updated.")
                           updateTextInput(session, "s_Password", value = "")
                         })
    }  
  })
  
  # Listener for profile deletion
  observe({
    if (!is.null(input$b_Delete) && input$b_Delete != 0){ 
      modifyUserBackend( "Please type the password to delete this profile!",
                         function(user) {
                           # Delete the user data
                           deleteUser(user$Name)
                           output$LoginMessage <- renderText("User data Deleted.")
                           updateTextInput(session, inputId = "s_Name", value = "")
                           updateTextInput(session, inputId = "s_Password", value = "")
                         })
    }  
  })
  
  
  ### All Profiles pane ################################################
  
  reactivePlots <- reactiveValues()
  
  createPlots <- function(users) {
    for (i_user in 1:nrow(users)) {
      local({
        user <- users[i_user, ]
        reactivePlots[[user[['Name']]]] <- renderPlot({
          par(mar = c(0,0,0,0))
          radarchart(df = plotData(user), axistype = 0, seg = 5, 
                     pcol = "black", pfcol = color(user), vlabel = c("", "", ""))
        })
      })
    }
  }
  
  assignPlots <- function(plotPrefix) {
    for (name in names(reactivePlots)) {
      local({
        output[[paste0(plotPrefix, name)]] <- reactivePlots[[name]]
      })
    }
  }
  
  Users <- reactive({
    input$b_Create
    input$b_Save
    input$b_Delete
    createPlots(users)
    return(getUserTable())
  })
  
  output$AllProfiles1 <- renderUI({
    users <- Users()
    if (nrow(users) == 0) {
      return(h4("Nothing to show yet..."))
    }
    users <- users[order(users$Name),]
    assignPlots("plot")
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
        column(5,
               p(paste("BE:", paste(fromJSON(user[["BETags"]]), collapse = " - "))),
               p(paste("FE:", paste(fromJSON(user[["FETags"]]), collapse = " - "))),
               p(paste("DS:", paste(fromJSON(user[["DSTags"]]), collapse = " - ")))
        )
      ),
      hr()
      )
    }))
  })
  
  
  ## Teammates profil pane ###########################################################  
  
  createShortProfileUIs <- function(users, plotPrefix) {
    apply(users, 1, function(user) {
      fluidRow(fluidRow(
        column(4,
               h4(user['Name'], align = "center"),
               p(user['FirstName'],user['LastName'], align = "center")),
        column(4,
               plotOutput(paste0(plotPrefix, user['Name']), height = "80px")),
        column(2,
               p("Involvement:", align = "center"),
               p(user[["Involvement"]], align = "center"))
      )
      )
    })
  }
  
  output$OrderedProfiles <- renderUI({
    if (input$s_Name == "") {
      output$Team2 <- renderUI(helpText("Load or Create your profile first", align = "center"))
      return(h4("** Load or Create your profile first **", align = "center"))
    }
    users <- Users()
    if (nrow(users) == 0) {
      return(h4("Nothing to show yet..."))
    }
    users <- users[!users$Name %in% input$s_Name,]
    if (input$b_FilterDatasets) {
      myDatasets <- fromJSON(User()[["Datasets"]])
      data_index <- sapply(users$Datasets, function(udi) {
        length(intersect(myDatasets, fromJSON(udi))) != 0
      })
#       sendDEBUG(capture.output(summary(data_index)))
      users <- users[data_index,]
    }
    myProfile <- profile()
    profiles <- apply(users[c("DS", "BE", "FE")], 1, getProfile)
    distances <- abs(profiles[1,] - myProfile[1]) + abs(profiles[2,] - myProfile[2])
    users1 <- users[order(distances),]
    users2 <- users[order(-distances),]
    ## Define the graph outputs
#     t <- proc.time()
    createPlots(users)
#     sendDEBUG("created plots in:", ((t1 <- proc.time()) - t)[2])
    assignPlots("plot1")
    assignPlots("plot2")
#     sendDEBUG("assigned plots in:", ((t2 <- proc.time()) - t1)[2]) ## This is super quick, but evaluation is somewhere else...
    ## Define the other team representation
    return(
      fluidRow(column(6,
                      h4("Like-minded people", align = "center"),
                      createShortProfileUIs(users1, "plot1")),
               column(6,
                      h4("Complementary people", align = "center"),
                      createShortProfileUIs(users2, "plot2"))
      )
    )
  })
  
  
  
  
  ### Debugging #############################################
  
  if (b_DEBUG) {
    source("server/debugS.R", local = TRUE)
  }
  
})
