library(RPostgreSQL)
pw <- {"pladias"}
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "dalibor",
                 host = "localhost", port = 7777,
                 user = "pladias", password = pw)