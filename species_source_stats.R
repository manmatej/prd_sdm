library(dplyr)
source("/home/matej/prd_sdm/server_link/secret_localdb.R") # connect to the database Ubuntu
dali<-dbGetQuery(con, "SELECT * from atlas.records INNER JOIN public.taxons ON atlas.records.taxon_id = public.taxons.id")
projects <- dbGetQuery(con, "SELECT * from atlas.projects")
rownames(projects)<-projects$id

dali<-dali[,-41] # duplicit column "id"
check<-"check_list.csv" # IUCN category, redl ist file

# lft:rght
# Bryophyta 2:3081
# Lichenes 3082:7521

l<-sum(dali$lft %in% c(3082:7521))
m<-sum(dali$lft %in% c(2:3081))
tot<-sum(counts_rec$count)

Check<-data.table::fread(check,stringsAsFactors = F,header=F,encoding = "Latin-1") # IUCN category, redl ist
mechy.red<-inner_join(mechy,Check,by=c("name_lat"="V9")) # red list category bryophytes
projs<-names(summary(as.factor(mechy.red$project_id)))

projects<-projects[projs,]

counts<-as.data.frame(table(mechy.red$name_lat[mechy.red$project_id==as.numeric(projects$id[1])]))
names(counts)<-c("spec",projects$abbrev[1])
for (i in 2:nrow(projects)){
  counts1<-as.data.frame(table(mechy.red$name_lat[mechy.red$project_id==projects$id[2]]))
  names(counts1)<-c("spec",projects$abbrev[i])
  counts<-merge(counts,counts1,by="spec",all=T)
}
  
counts$total<-apply(counts[,2:6],1,sum,na.rm=T)
counts<-merge(counts,Check[,c("V9","V13")],by.x ="spec",by.y="V9")
names(counts)[8]<-"RL_eval"
counts<-counts[order(counts$total,decreasing = T),]

write.table(counts,file = "Species_records_dalibor.csv",sep=";",na="0",row.names = F,col.names = T)
