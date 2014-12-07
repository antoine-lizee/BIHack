# Hackathoner Profiler
# This script defines the global variables and other general conventions used throughout the app.
# 
# Copyright Antoine Lizee 11/2014 antoine.lizee@gmail.com. See the license included in the project.



# Switches ----------------------------------------------------------------

b_DEBUG = FALSE

# libraries ---------------------------------------------------------------

library(shiny)
# radarplot
library(fmsb)
# "back-end"
library(RSQLite)
# library(jsonlite) 
library(rjson) # less powerfule but does a better job for quick conversions (without classes)

# debuging
if (b_DEBUG) {
  options(shiny.trace=TRUE)
  options(shiny.error=browser)
}

# Static info -------------------------------------------------------------

listOfSkills <- list(BE = c("Python", "C#", "C++", "C", "go", "java", "Hadoop", "sysops", "Node.js", "PHP", "Django", "SQL", "noSQL", "ruby", "ROR"),
                     FE = c("js", "jQuery", "CSS", "D3.js", "Shiny", "Objective-C", "Android", "HTML", "HTML5", "Flash"),
                     DS = c("R", "Python", "Julia", "ML", "DataVis", "Algorithmics", "Stats", "MATLAB"))

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

# Schema ------------------------------------------------------------------

user0 <- data.frame(Name = "USER0", Password = "765",
                    FirstName = "Yo", LastName = "YO", 
                    DS = 0, BE = 3, FE = 2, 
                    DSTags = "json", BETags = "json", FETags = "json",
                    Datasets = toJSON(c(listOfDatasets$Education[1], listOfDatasets$Education[2])),
                    Involvement = 2,
                    stringsAsFactors = F)

userDBPath <- "data/BIH_users.sqlite"
dir.create(path = "data", showWarnings = F)
userTableName <- "BIH_users"


# Misc tool for the ui script ---------------------------------------------------------------

# Password field:
passwordInput <- function (inputId, label, value = "") 
{
  tagList(shiny:::`%AND%`(label, tags$label(label, `for` = inputId)), tags$input(id = inputId, 
                                                                                 type = "password", value = value))
}

