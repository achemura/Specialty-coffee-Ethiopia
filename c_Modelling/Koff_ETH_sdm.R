#R code for coffee suitability in Ethiopia
#Developed by Chemura Abel
#Chemura et al 



rm(list=ls())
gc()

library(dismo)  
library(raster)
library(rJava)  
library(maptools)
library(rasterVis)
library(ENMeval)
library(rgdal)
library(sdm)
library(sp)
library(corrplot)
library(Hmisc)

setwd("~/DATABANK/Ethiopia/Coffee/")

# Read file with presence points
points<- readOGR("~/DATABANK/Ethiopia/Coffee/Specialty/All_model_pnts.shp")
crop.points <- as.data.frame(points)

zw_hollow <- readOGR("~/DATABANK/Ethiopia/SHP/ETH_adm0.shp")
zw_prov <- readOGR("~/DATABANK/Ethiopia/SHP/ETH_adm2.shp")

crop.points <- crop.points[,-c(1:3)]
occurrence <- as.data.frame(rep(1, nrow(crop.points)))
colnames(occurrence) <- c("occurrence")

crop_val_all <- cbind(crop.points, occurrence)

#Convert to spatial points
xy <- crop_val_all[,c(1,2)]

crop.points.prj <- SpatialPointsDataFrame(coords = xy, data = crop_val_all,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
crop.points.prj <- crop.points.prj[, -(1:2)]

#Read layers 
bioclims <- list.files(path = "Layers/Current/", pattern=".asc$", full.names=T)
bio.var.all <- stack(bioclims)

plot(bio.var.all[[3]])
plot(crop.points.prj[crop.points.prj$occurrence == 1,], col='red', pch=19 , add = T)
#points(crop.points.prj[crop.points.prj$occurrence == 0,],col='red',pch=16)

#Variance inflation factor(VIF)  for variable selection
point.lyrs <- extract(bio.var.all, crop.points.prj)
point.lyrs <- as.data.frame(point.lyrs)
head(point.lyrs)

#Correlation 
cols <- colorRampPalette(c("darkorange", "white", "steelblue"))(20)
df2 <- cor(point.lyrs, use = "na.or.complete")
corrplot(df2, method="shade", shade.col= cols, tl.col="black", tl.srt=45,
         order="AOE", type="lower", cl.pos="b", col = cols)

source("~/DATABANK/RScripts/Bioclim/Ethiopia/Models/vif_func.R")

#run function for variable elimination based on VIF
VIF.layers <- vif_func(in_frame = point.lyrs, thresh=10, trace=T)
VIF.layers

bio.var <- subset(bio.var.all, VIF.layers, drop=T)
point.var <- extract(bio.var, crop.points.prj)
point.var <- as.data.frame(point.var)
head(point.var) 

point_type <- cbind(points@data$Type, point.var) 
colnames(point_type)[1] <- "Type"
head(point_type)

#write.csv(point_type, paste0(paste0("~/DATABANK/Ethiopia/Coffee/Model_out/"), 
#                                      "point_value_type.csv"))


d <- sdmData(train=crop.points.prj, predictors=bio.var, bg=list(n=10000))

#Model fitting and evaluation
sdm.model <- sdm(occurrence~., data=d, methods=c('rf', 'brt', 'svm'), 
          replicatin='sub', test.percent = 30, n=1)
sdm.model

# info on runs including modelID, whether they are successfully fitted and evaluated, e
getModelInfo(sdm.model) 

roc(sdm.model)
# the plots can be smoothes:
roc(sdm.model,smooth=TRUE)


eval_thresh <- getEvaluation(sdm.model, stat=c("Kappa","TSS"), opt=5) #show TSS and Kappa with the threshold that maximised Kappa

eval_thresh <- getEvaluation(sdm.model, opt=2) #Opt 2 = max[sensitivity + specificity] 
eval_thresh

eval_values <- getEvaluation(sdm.model, w=1:3, wtest='training', 
                             stat=c('sensitivity', 'specificity'), opt=2)
eval_values
sensitivity_values <- eval_values$sensitivity
sensitivity <- 1 - sensitivity_values
sensitivity
specificity_values <- eval_values$specificity
specificity <- 1 - specificity_values
specificity

sdm.model@models$occurrence

gui(sdm.model)
#Response curves 
rcurve(sdm.model, id = 1, mean=F, confidence = T) #Response curves 
getResponseCurve(sdm.model, id=1)

getVarImp(sdm.model, id = 1)

roc(sdm.model,smooth=TRUE)

#rf_roc<- as.data.frame(getRoc(sdm.model, 1))
#brt_roc<- as.data.frame(getRoc(sdm.model, 2))
#mx_roc<- as.data.frame(getRoc(sdm.model, 3))

# 
vi <- sdm::getVarImp(sdm.model, id=1:3) # the modelIDs of 1:10
plot(vi,'auc')


#Weighted forecasting
curr.s.tune <- ensemble(sdm.model, newdata = bio.var, #filename='e11.img', 
               setting=list(method='weighted', stat='AUC'), overwrite=TRUE) 
plot(curr.s.tune)
plot(zw_hollow, add = T)
plot(zw_prov, border = 'red', add = T)

threshold <- 0.42 #Max Spec + Sens
curr.s.cl = curr.s.tune
curr.s.cl[curr.s.cl  <    threshold]   <- 0  
curr.s.cl[curr.s.cl  >=  threshold]   <- 1 

#Suit_classes reclassifcation 
#class_vec_curr <- c(0,0.42,0,0.42,0.9999,1)
#class_vec <- c(0,0.42,0,0.42,0.9999,1)
#curr.s.cl <- reclassify(curr.s.tune, class_vec_curr)
curr.s.cl <- ratify(curr.s.cl)
plot(curr.s.cl)
plot(zw_hollow, add = T)


#####______________________________________________________________________________________________________________________________________________________________
######## 2030 SSP126
#BCCCSM2 
bio30.SSP126.BCCCSM2 <- list.files(path = "Layers/S2030/SSP126/BCC-CSM2/", pattern=".asc", full.names=T)
bio30.SSP126.BCCCSM2.var <- stack(bio30.SSP126.BCCCSM2)
bio30.SSP126.BCCCSM2.var <- subset(bio30.SSP126.BCCCSM2.var, VIF.layers, drop=T)
SSP126.s30.BCCCSM2 <- ensemble(sdm.model, newdata = bio30.SSP126.BCCCSM2.var, #filename='e11.img', 
                           setting=list(method='weighted', stat='AUC'), overwrite=TRUE) 
SSP126.s30.BCCCSM2.cl <- reclassify(SSP126.s30.BCCCSM2, class_vec)
SSP126.s30.BCCCSM2.cl <- ratify(SSP126.s30.BCCCSM2.cl)


#####_____________________________________________________________________________________________________
print("Inini Ndinonzi Inini")


