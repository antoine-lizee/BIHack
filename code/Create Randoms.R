# Hackathoner Profiler
# This script simply feed the database with random users.
# Thanks to the awesome, easy-to-use randomuser.me API.
# 
# Copyright Antoine Lizee 11/2014 antoine.lizee@gmail.com. See the license included in the project.


source("global.R")
source("server/utilities.R")
library(jsonlite)
randomNames <- fromJSON('http://api.randomuser.me/?results=25')
NR <- nrow(randomNames$results)
detach("package:jsonlite")

# Copy pasted from the "Schema"
flatListOfDatasets <- unlist(listOfDatasets, use.names = F)
randomUsers <- data.frame(Name = randomNames$results$user$username , Password = "1111",
                          FirstName = randomNames$results$user$name$first, LastName = randomNames$results$user$name$last, 
                          DS = sample(5, NR, replace = T), BE = sample(5, NR, replace = T), FE = sample(5, NR, replace = T), 
                          DSTags = replicate(NR, toJSON(listOfSkills$DS[sample(length(listOfSkills$DS),sample(3,1))])), 
                          BETags = replicate(NR, toJSON(listOfSkills$BE[sample(length(listOfSkills$BE),sample(3,1))])),  
                          FETags = replicate(NR, toJSON(listOfSkills$FE[sample(length(listOfSkills$FE),sample(3,1))])), 
                          Datasets = replicate(NR, toJSON(flatListOfDatasets[sample(length(flatListOfDatasets),sample(3,1))])),
                          Involvement = sample(5, NR, replace = T),
                          stringsAsFactors = F)

con <- RSQLite::dbConnect(SQLite(), userDBPath)
# getUserTable() has to be evaluated from the server sript
userTableFinal <- rbind(getUserTable(), randomUsers)
dbWriteTable(con, name = userTableName, value = userTableFinal, row.names = F, overwrite = T)
