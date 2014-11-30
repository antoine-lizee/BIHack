# Hackathoner Profiler
# This script defines the debugging part of the server side function for the Shiny app.
# 
# Copyright Antoine Lizee 11/2014 antoine.lizee@gmail.com. See the license included in the project.

output$DEBUG <- renderPrint({
  #     print(profile())
  #     print(getListOfUsers())
  #     dput(User())
  print(User())
  #     if (!is.null(getUser(User()['Name'])))
  #       print(fromJSON(getUser(User()['Name'])[["DSTags"]]))
  #     print(users)
  #     users <- Users()
  #     myProfile <- profile()
  #     profiles <- apply(users[c("DS", "BE", "FE")], 1, getProfile)
  #     distances <- abs(profiles[1,] - myProfile[1]) + abs(profiles[2,] - myProfile[2])
  #     print(users)
  #     print(profiles)
  #     print(myProfile)
  #     print(distances)
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
  list(btnTags(),actionButton(inputId = "console", label = "console - DEBUG"))
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
