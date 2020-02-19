

# library(knitr)
library(sf)
library(raster)
# library(dplyr)
# library(leaflet)
library(caret)
library(ranger)
library(snowfall)
library(doParallel)

task_params<-list(
  taxon_name="Lewinskya rupestris",
  includes_absence=FALSE,
  pseudoabsence_count=161,
  epsg=4326,
  area_type="cr",
  area_buffer=3000)

grids.path<-"/home/mman/czechgrids" # where are the data
grids.path1<-"/home/mman/czechgrids_local"

source("/home/mman/czechgrids_local/R_outputs/secret.R")

message("species data established")

crs=as.numeric(task_params$epsg)
body<-sf::st_as_sf(dali, coords = c("longitude", "latitude"), crs = crs ) # convert csv to spatial sf object
body$pa<-rep(1L,nrow(body))
body_4326<-sf::st_transform(body,4326) # transform point to wgs for mapping purpose
body_32633<-sf::st_transform(body_4326,32633) # transform points to utm for computation purpose

hrcr_4326<-st_read(paste0(grids.path1,"/hrcr_wgs.shp"),crs = 4326L)  # read shp CZ border
hrcr_4326<-st_geometry(hrcr_4326) # drop all information except geometry
hrcr_32633<-sf::st_transform(hrcr_4326,32633)


## define function for spatial enevelope
envelope<-function(type="buffer",dist=3000){
  if (type=="hull") {sa<-st_convex_hull(st_union(body_32633))} # convex hull envelope
  if (type=="box") {sa<-st_as_sfc(st_bbox(body_32633))} # bounding box envelope
  if (type=="buffer") {sa<-st_buffer(st_union(body_32633),dist)} # buffer of defined distance envelope
  if (type=="cr") {sa<-hrcr_32633}
  return(sa)
}
type=as.character(task_params$area_type)
dist=as.numeric(task_params$area_buffer)

sa<-envelope(type=type,dist=dist) # define envelope object with default settings (buffer 3 km)

spec_dat_typ<-as.character(ifelse(task_params$includes_absence, spec_dat_typ<-"pa", spec_dat_typ<-"po"))
n_samp<-as.numeric(task_params$pseudoabsence_count)

## define absences or pseudo-absences
p_a<-function(x){
if(spec_dat_typ == "pa"){
  presence_32633<-body_32633[body_32633$pa==1,]
  absence_32633<-body_32633[body_32633$pa==0,]
} else {
  presence_32633<-body_32633[complete.cases(body_32633$pa),]
  if(n_samp==161){
    n_pres<-nrow(presence_32633)
  } else {
    n_pres<-n_samp
  } 
  buf_pres_32633<-st_buffer(st_union(presence_32633),500)
  buf_abs_32633<-st_difference(sa,buf_pres_32633)
  absence_32633<-st_sample(buf_abs_32633,size=n_pres,type = "random")
} 
  return(list(presence_32633,absence_32633))
}

presence_32633<-st_geometry(p_a(x)[[1]])
absence_32633<-st_geometry(p_a(x)[[2]])

message("presence absence created")

### space check

precise_border<-paste0(grids.path,"/crdem5g_sgrd_32633_poly_01.shp")

hrcr_32633<-st_read(precise_border) # stric cutter precise polygon 
hrcr_32633_a<-st_transform(hrcr_32633,32633) # unify crs
hrcr_32633_b<-st_geometry(hrcr_32633_a) # unify crs
absence_32633_s<-st_intersection(hrcr_32633_b,absence_32633)
presence_32633_s<-st_intersection(hrcr_32633_b,presence_32633)

sa_s<-hrcr_32633_b # for whle cr
# sa_s<-st_intersection(hrcr_32633_b,sa) # for non whle cr !!edit

presence_4326<-(st_transform(presence_32633_s,4326L)) # transform presences to wgs
absence_4326<-st_transform(absence_32633_s,4326L) # transform absences to wgs
sa4326<-st_transform(sa_s,4326L) # transform spatial envelope to wgs

message("spatial check ok")


# helper functions to get lon, lat data from sf calass object
st_lat= function(x) st_coordinates(x)[,1]
st_lon = function(x) st_coordinates(x)[,2]


files<-list.files(grids.path,pattern = "*.tif$",full.names = T,recursive = F) # listtif files paths
files<-append(files,list.files(grids.path1,pattern = "*.tif$",full.names = T,recursive = F)) # listtif files paths
files<-append(files,list.files(grids.path1,pattern = "*.sdat$",full.names = T,recursive = F)) # listtif files paths


nice.predictors<-c("Converg_Index50m",
                   "Diurnal_anis_heat",
                   "Slope",
                   "Topo_pos_index50m",
                   "Topo_wet_index",
                   "Vert_dist_cha_netw",
                   "Elevation",
                   "CHMI_preci_25year_mean",
                   "CHMI_air_temp_25year_mean",
                   "CHMI_air_temp_25year_max",
                   "CHMI_air_temp_25year_min",
                   "CHMI_AOPK_biotop",
                   "EUROLST_air_temp_mean",
                   "EUROLST_air_temp_range",
                   "EUROLST_air_temp_max",
                   "UHUL_tree_species_sentinel")


st<-raster::stack(files) # stack envi rasters to R
names(st)<-nice.predictors
body_sfc<-c(presence_32633_s,absence_32633_s)

pa<-c(rep("presence",length(presence_32633_s)),rep("absence",length(absence_32633_s)))
body_pa<-st_as_sf(body_sfc,pa=pa)
body_32633<-sf::st_transform(body_pa,st@crs) # transform points to stack crs

# Pararelize the task
st.list <- unstack(st)
names(st.list) <- names(st)

sfInit(parallel=TRUE, cpus=parallel:::detectCores()-1)
sfLibrary(raster)
sfLibrary(sf)

e.body <- sfSapply(st.list, fun=extract, y=body_32633)
sfStop()

body_extract<-data.frame(e.body,pa=pa,stringsAsFactors = F)

message("rasters extracted")

set.seed(100) # defin fix starting number for "random" processes to be reproducible
body_extract<-body_extract[,c(nice.predictors,"pa")] # generate clera data set (omit repetitive columns)
response <- "pa"

# divide data to training and testing dataset
trainids <- createDataPartition(body_extract$pa,list=F,p=0.85) # define partition  
testDat <- as.data.frame(body_extract[-trainids,]) # training
trainDat <- as.data.frame(body_extract[trainids,]) # testing


ctrl.rf <- trainControl(method="repeatedcv", # resamping method repeated Cross Validation
                        number =10,# number of fold for fesampling
                        repeats = 10,
                        savePredictions = 'all',
                        classProbs = T)

ctrl.glm <- trainControl(method="repeatedcv", # resamping method repeated Cross Validation
                         number =10,# number of fold for fesampling
                         repeats = 10,
                         savePredictions = 'all',
                         classProbs = TRUE) 

set.seed(100) # defin fix starting number for "random" processes to be reproducible

message("glm computed") 

# # Pararelize the task, use 14 workers
cl <- makePSOCKcluster(14)
registerDoParallel(cl)

model.glm<- train(trainDat[complete.cases(trainDat[,nice.predictors]),nice.predictors],
                  trainDat[complete.cases(trainDat[,nice.predictors]),response],
                  method="glm",
                  metric="Kappa",
                  trControl=ctrl.glm,
                  preProcess = c( "center", "scale"))


model.rf<- train(trainDat[complete.cases(trainDat[,nice.predictors]),nice.predictors],
                 trainDat[complete.cases(trainDat[,nice.predictors]),response],
                 method="ranger",
                 metric="Kappa",
                 trControl=ctrl.rf,
                 importance="impurity")

stopCluster(cl)


message("rf computed")

# confusion matrix
glm.conf<-confusionMatrix(data = model.glm$pred$pred,
                          reference = model.glm$pred$obs,
                          positive = "presence")

rf.conf<-confusionMatrix(data = model.rf$pred$pred,
                          reference = model.rf$pred$obs,
                          positive = "presence")

png(filename = paste0("/home/mman/czechgrids_local/M_outputs/",task_params$taxon_name,"_glm_",format(Sys.time(),"%Y_%m_%d_%H"),".png"),
    width = 800, height = 800)
fourfoldplot(glm.conf$table,color = c("#B22222", "#2E8B57"))
text(-0.4,0.4, "TN", cex=1)
text(0.4, -0.4, "TP", cex=1)
text(0.4,0.4, "FN", cex=1)
text(-0.4, -0.4, "FP", cex=1)
dev.off()

png(filename = paste0("/home/mman/czechgrids_local/M_outputs/",task_params$taxon_name,"_rf_",format(Sys.time(),"%Y_%m_%d_%H"),".png"),
    width = 800, height = 800)
fourfoldplot(rf.conf$table,color = c("#B22223", "#2E8B58"))
text(-0.4,0.4, "TN", cex=1)
text(0.4, -0.4, "TP", cex=1)
text(0.4,0.4, "FN", cex=1)
text(-0.4, -0.4, "FP", cex=1)
dev.off()



glm.stat<-data.frame(rbind(glm.conf$byClass["Sensitivity"],
                     glm.conf$byClass["Specificity"],
                     glm.conf$byClass["Prevalence"],
                     glm.conf$overall["Kappa"],
                     performance=cbind(c(glm.conf$byClass["Sensitivity"]+glm.conf$byClass["Specificity"]-1
                     ))))

glm.stat<-round(glm.stat,4)

rownames(glm.stat)<-c("Sensitivity","Specificity","Prevalence","Kappa","TSS")
colnames(glm.stat)<-"Performance"
glm.stat


rf.stat<-data.frame(rbind(rf.conf$byClass["Sensitivity"],
                     rf.conf$byClass["Specificity"],
                     rf.conf$byClass["Prevalence"],
                     rf.conf$overall["Kappa"],
                     performance=cbind(c(rf.conf$byClass["Sensitivity"]+rf.conf$byClass["Specificity"]-1
                     ))))

rf.stat<-round(rf.stat,4)
rownames(rf.stat)<-c("Sensitivity","Specificity","Prevalence","Kappa","TSS")
colnames(rf.stat)<-"Performance"
rf.stat

glm.imp<-varImp(model.glm,scale = T)
rf.imp<-varImp(model.rf,scale = T)

png(filename = paste0("/home/mman/czechgrids_local/M_outputs/",task_params$taxon_name,"_imp_",format(Sys.time(),"%Y_%m_%d_%H"),".png"),
    width = 800, height = 1600)
par(mfrow=c(2,1))
plot(glm.imp,main="GLM - relative variable importnace")
plot(rf.imp,main="Random Forest - relative variable importnace")
dev.off()

message("pngs ploted")

## non whole cr !!edit
# spat_env32633<-st_transform(sa_s,st@crs)
# ext<-extent(st_bbox(spat_env32633)$xmin,
#             st_bbox(spat_env32633)$xmax,
#             st_bbox(spat_env32633)$ymin,
#             st_bbox(spat_env32633)$ymax)
# st_crop<-raster::crop(st,ext)
# spat_env32633_sp<-as(spat_env32633,"Spatial")
# st_clip<-raster::mask(st_crop,spat_env32633_sp)
# names(st_clip)<-nice.predictors


## whole cr
message(paste0("rf prediction started ",Sys.time()))
nam<-paste0("/home/mman/czechgrids_local/M_outputs/",task_params$taxon_name,"RF_predict",format(Sys.time(),"%Y_%m_%d_%H"),".tif")
raster::predict(st,model.rf,
                  filename=nam,
                  index=2,
                  type="prob",
                  overwrite=T)

message(paste0("rf prediction terminated ",Sys.time()),"DONE!")

# message(paste0("glm prediction started ",Sys.time()))
# nam<-paste0("/home/mman/czechgrids_local/M_outputs/",task_params$taxon_name,"GLM_predict",format(Sys.time(),"%Y_%m_%d_%H"),".tif")
# raster::predict(st,model.glm,
#                 filename=nam,
#                 index=2,
#                 type="prob",
#                 overwrite=T)
# message(paste0("glm prediction terminated ",Sys.time()))

