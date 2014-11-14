

library(shiny)
# options(shiny.trace=FALSE)
# options(shiny.error=NULL)
# library(fmsb)
# library(RSQLite)
# # library(jsonlite)
# library(rjson)

source("StaticVariables.R")

## UTILITIES
# 
# getCartCoord <- function(V) {
#   r <- V[1]
#   theta <- V[2]
#   r * c(cos(theta), sin(theta))
# }
# 
# getPolarCoord <- function(V) {
#   x <- V[1]
#   y <- V[2]
#   c(r = sqrt(x^2 + y^2), theta = atan2(y,x) %% (2*pi))
# }
# 
# getHex <- function(RGB) {
#   paste0("#", paste(sapply(RGB, function(i) as.character(as.hexmode(i))), collapse = ""))
# }
# 
# ## BACKEND - Database
# # Schema:
# user0 <- data.frame(Name = "USER0", Password = "765",
#                     DS = 0, BE = 3, FE = 2, 
#                     Datasets = toJSON(c(listOfDatasets$Education[1], listOfDatasets$Education[2])),
#                     Involvement = 2,
#                     stringsAsFactors = F)
# # Initialization
# userDBName <- "BIH_users.sqlite"
# userTableName <- "BIH_users"
# con <- RSQLite::dbConnect(SQLite(), userDBName, cache_size = 5000, synchronous = "full")
# if (!dbExistsTable(con, userTableName)) {
#   dbWriteTable(con, name = userTableName, value = user0[0,], row.names = F)
#   #   dbGetQuery(con, paste("DELETE FROM", userTableName, "WHERE Name = 'USER0'"))
# }
# # # Reset code:
# # dbDisconnect(con)
# # file.remove(userDBName)
# 
# getUser <- function(Name) {
#   user <- dbGetQuery(con, paste0("SELECT * FROM ", userTableName, " WHERE Name = '", Name, "'"))
#   if (nrow(user) == 0) {
#     return(NULL)
#   } else {
#     return(user)
#   }
# }
# 
# unpackDF <- function(User) {
#   stopifnot(nrow(User) == 1)
#   modDf <- sapply(User, 
#                   function(col) { 
#                     if(is.character(col)) 
#                       paste0("'", col, "'")
#                     else
#                       col
#                   })
#   return(paste0("( ", paste(modDf, collapse = ", "), ")"))
# }
# 
# createUser <- function(User) {
#   if (User$Name == "") {
#     warning("No name provided for the user")
#     return(FALSE)
#   }
#   dbSendQuery(con, paste0("INSERT INTO ", userTableName, " VALUES ", unpackDF(User)))
# }
# 
# updateUser <- function(User) {
#   if (User$Name == "") {
#     warning("No name provided for the user")
#     return(FALSE)
#   }
#   ## UGLY hack
#   dbGetQuery(con, paste("DELETE FROM", userTableName, "WHERE Name = '", User$Name,"'"))
#   dbSendQuery(con, paste0("INSERT INTO ", userTableName, " VALUES ", unpackDF(User)))
# }
# 
# getListOfUsers <- function() {
#   sqliteQuickColumn(con, userTableName, "Name")
# }


shinyServer(function(input, output, session) {
  
  ### DATA Unpacking
  DS <- reactive({input$i_DS})
  BE <- reactive({input$i_BE})
  FE <- reactive({input$i_FE})
  data <- reactive({
    data.frame(check.names = F,
               "Data Science" = c(5,0,DS()),
               "Back-End" = c(5,0, BE()),
               "Front-End" = c(5,0, FE()))
  })
  
  #   User <- reactive({
  #     user <- user0
  #     user$Name <- input$s_Name
  #     user$Password <- input$s_Password
  #     user$DS <- DS()
  #     user$BE <- BE()
  #     user$FE <- FE()
  #     user$Datasets <- toJSON(input$s_Datasets)
  #     user$Involvement <- input$i_Involvement
  #     return(user)
  #   })
  #   
  #   ### PLOT
  #   color.bckp <- reactive({
  #     topo.colors(3)[which.max(data()[3,])]
  #   })
  #   color <- reactive({
  #     getHex(floor( -(c(DS(), BE(), FE()) / 5)^1.5 * 150 + 250))
  #   })
  #   output$radarPlot <- renderPlot({
  #     # draw radar chart
  #     radarchart(df = data(), axistype = 0, seg = 5, 
  #                pcol = "black", pfcol = color())
  #   })
  #   
  #   ### PROFILE
  #   profile <- reactive({
  #     getPolarCoord(apply(
  #       apply(
  #         matrix(c(DS(), BE(), FE(), 0:2 * 2*pi/3 + pi/2), nrow = 2, byrow = 3),
  #         2, getCartCoord), 
  #       1, sum))
  #   })
  #   r <- reactive({profile()[1]})
  #   theta <- reactive({profile()[2]})
  #   output$Profile <- renderText({
  #     ifelse((r()/max(data()[3,]))<=1/3, "Balanced", 
  #            ifelse(theta() < 0, "ERROR", 
  #                   ifelse(theta() < pi/3, "Data Viz guy",
  #                          ifelse(theta() < 2*pi/3, "Data Scientist",
  #                                 ifelse(theta() < pi, "Data dude",
  #                                        ifelse(theta() < 4*pi/3, "Back-End geek", 
  #                                               ifelse(theta() < 5*pi/3,  "Full Stack dev", "Front-End dev")))))))
  #   })
  #   
  #   ### Updates to the UI
  #   #   
  #   #   output$LoginField <- renderUI({
  #   #     selectizeInput(inputId = "s_Name", label = NULL, choices = c(c("Login" = "", getListOfUsers()), multiple = FALSE, options = list(create = "true"))
  #   #   })
  #   
  #   output$textInvolvement <- reactive({
  #     c("A few hours", "Half of the time", "I'll be out a couple of hours", "All the time but when sleeping home")[input$i_Involvement]
  #   })
  #   
  #   ### Backend Listeners
  #   
  #   output$loginErrorCreate <- renderText({
  #     if (input$b_Create != 0){ # Try to create the record
  #       if (isolate(User()['Name']) == "") {
  #         return("ERROR: No pseudo provided.")
  #       }
  #       ## Try to retrieve the user data in a non-dependent fashion
  #       user <- isolate(getUser(User()['Name']))
  #       if (!is.null(user)) {
  #         return("ERROR: The user already exists... Try another one or load the existing user by also typing the password.")
  #       } else {
  #         createUser(isolate(User()))
  #       }
  #     }
  #   })
  #   
  #   output$loginErrorLoad <- renderText({
  #     if (input$b_Load != 0){ # Try to create the record
  #       ## Retrieve the user data in a non-dependent fashion
  #       user <- isolate(getUser(User()['Name']))
  #       if (is.null(user)) {
  #         "ERROR: The user with this pseudo doesn't exist yet."
  #       } else {
  #         if (user$Password != isolate(User()['Password']))
  #           "ERROR: The password doesn't match the records..."
  #         else {
  #           "User Loaded"
  #         }
  #       }
  #     }
  #   })
  
  ### Debugging
  output$DEBUG <- renderPrint({
    print(data())
    print(profile())
    print(color())
    print(User())
  })
})
