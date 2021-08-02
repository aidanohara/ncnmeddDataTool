#https://pacha.dev/blog/2020/08/09/a-crash-course-on-postgresql-for-r-users/

library(RPostgres)
library(dplyr)


fun_connect <- function() {
  dbConnect(
    Postgres(),
    dbname = Sys.getenv("tutorial_db"),
    user = Sys.getenv("tutorial_user"),
    password = Sys.getenv("tutorial_pass"),
    host = Sys.getenv("tutorial_host")
  )
}

#Somewhat unneeded now...
#SQL connect/disconnect

#Creating Tables

#Accessing Tables

#Appending Tables
