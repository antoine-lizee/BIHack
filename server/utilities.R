# Hackathoner Profiler
# This script defines the several utilities that are used by the server.R part of the app.
# 
# Copyright Antoine Lizee 11/2014 antoine.lizee@gmail.com. See the license included in the project.


# Genereal utilities ------------------------------------------------------

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
# # Tested like that:
# ifelseFun(c(1,2,NA,3), 0, is.na)
# ifelseFun(c(1,2,NA,3), "", is.null)
# ifelseFun(NULL, "", is.null)

sendDEBUG <- function(message) {
  cat(sprintf("#### DEBUG: %s ############################\n", message), file = stderr())
}


# Backend -----------------------------------------------------------------

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
  ## UGLY but safer update hack
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



# Profile definition ------------------------------------------------------

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

getProfile <- function(DBF) {
  getPolarCoord(apply(
    apply(
      matrix(c(DBF, 0:2 * 2*pi/3 + pi/2), nrow = 2, byrow = 3),
      2, getCartCoord), 
    1, sum))
}


