library(RPostgreSQL)
pw <- {"pladias"}
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "dalibor",
                 host = "localhost", port = 7777,
                 user = "pladias", password = pw)

sort(dbListTables(con)) 

# projects <- dbGetQuery(con, "SELECT * from atlas.projects")
# r.project_id<-projects$id
# 
# dataOutput <- dbGetQuery(con, "SELECT * from atlas.records")
# 
# pocty rec v projektech
# counts_rec <- dbGetQuery(con, "SELECT count(*), r.project_id FROM atlas.records r GROUP BY project_id ORDER BY project_id")
#  
# 
# # pocty rec pro jednotlive druhy
# counts_spec <- dbGetQuery(con, "SELECT count(*), taxon_id FROM atlas.records r GROUP BY taxon_id ORDER BY count(*) DESC")
# 
# # pocty rec pro jednotlive rody
# counts_spec <- dbGetQuery(con, "SELECT count(*), taxon_id FROM atlas.records r GROUP BY taxon_id ORDER BY count(*) DESC")
# 
# # pocty zaznamu v kvadrantech podle jednotlivych projektu
# counts_inquad <- dbGetQuery(con, "SELECT COUNT(*), r.project_id, q.quadrants_id FROM atlas.records r INNER JOIN atlas.records_quadrants q ON r.id=q.records_id")
