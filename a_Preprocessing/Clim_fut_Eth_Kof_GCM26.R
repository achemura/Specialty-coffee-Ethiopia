rm(list=ls()) # Cleaning the shit off

# load the library
library(pROC)
library(maptools)
library(raster)
library(rgdal)
library(RColorBrewer)
library(sp)
library(rasterVis)
library(graticule)

#Loading the environmental layers
setwd("~/DATABANK/Ethiopia/SHP")
eth_bound <- readOGR("Bound_Eth.shp")

#________________________________________________________________________________________________________________
#________________________________________________________________________________________________________________
#GFDL
setwd("~/DATABANK/BioClim/Future_2050/P26/GFDL/gd26pr50")

#Precheitation
list.files()
jan_p <- raster("gd26pr501.tif")
feb_p <- raster("gd26pr502.tif")
mar_p <- raster("gd26pr503.tif")
apr_p <- raster("gd26pr504.tif")
may_p <- raster("gd26pr505.tif")
jun_p <- raster("gd26pr506.tif")
jul_p <- raster("gd26pr507.tif")
aug_p <- raster("gd26pr508.tif")
sep_p <- raster("gd26pr509.tif")
oct_p <- raster("gd26pr5010.tif")
nov_p <- raster("gd26pr5011.tif")
dec_p <- raster("gd26pr5012.tif")

pr <- stack(jan_p, feb_p, mar_p, apr_p, may_p, jun_p, jul_p, aug_p, sep_p, oct_p, nov_p, dec_p)
pr <- crop(pr, eth_bound)
pr <- mask(pr, eth_bound)

prec_25 <- pr
prec_25 <- aggregate(pr, fact=4)
plot(prec_25[[11]])

#Precip layers
bio12 <-  overlay(prec_25[[1]], prec_25[[2]], prec_25[[3]], prec_25[[4]], prec_25[[5]], prec_25[[6]],
                  prec_25[[7]], prec_25[[8]], prec_25[[9]], prec_25[[10]],prec_25[[11]], prec_25[[12]], #Sum rainfall whole season,
                  fun=sum)
plot(bio12)

bio13 <- overlay(prec_25[[1]], prec_25[[2]], prec_25[[3]], prec_25[[4]], prec_25[[5]], fun=sum) #Sum rain sowing months
plot(bio13)

bio16 <- overlay(prec_25[[5]], prec_25[[6]], prec_25[[7]], #Rainfall growing season only
                 prec_25[[8]], prec_25[[9]],  
                 fun=sum)

stk.sn <- stack(prec_25[[5]], prec_25[[6]], prec_25[[7]], prec_25[[8]], prec_25[[9]])
std.stk_sn <- calc(stk.sn, fun=sd)
bio15 <- std.stk_sn/bio12*100

plot(bio15)

writeRaster(bio12, paste0("~/DATABANK/Ethiopia/LayersC/P26/GFDL/bio12.tif"), overwrite=TRUE)#, format = "EHdr")) 
writeRaster(bio13, paste0("~/DATABANK/Ethiopia/LayersC/P26/GFDL/bio13.tif"), overwrite=TRUE)#, format = "EHdr")) 
writeRaster(bio15, paste0("~/DATABANK/Ethiopia/LayersC/P26/GFDL/bio15.tif"), overwrite=TRUE)#, format = "EHdr")) 
writeRaster(bio16, paste0("~/DATABANK/Ethiopia/LayersC/P26/GFDL/bio16.tif"), overwrite=TRUE)#, format = "EHdr")) 
#  
# #Doing the same for temperature layers

#Diurnal temp range calculation
#TMAX
setwd("~/DATABANK/BioClim/Future_2050/P26/GFDL/gd26tx50")

#TMAX
list.files(pattern=".tif$",full.names=T)

jan_tx <- raster("gd26tx501.tif")
feb_tx <- raster("gd26tx501.tif")
mar_tx <- raster("gd26tx501.tif")
apr_tx <- raster("gd26tx501.tif")
may_tx <- raster("gd26tx501.tif")
jun_tx <- raster("gd26tx501.tif")
jul_tx <- raster("gd26tx501.tif")
aug_tx <- raster("gd26tx501.tif")
sep_tx <- raster("gd26tx501.tif")
oct_tx <- raster("gd26tx501.tif")
nov_tx <- raster("gd26tx501.tif")
dec_tx <- raster("gd26tx501.tif")

r_tx <- stack(jan_tx, feb_tx, mar_tx, apr_tx, may_tx, jun_tx, jul_tx, aug_tx, sep_tx, oct_tx, nov_tx, dec_tx)
r_tx <- crop(r_tx, eth_bound)
r_tx <- mask(r_tx, eth_bound)
plot(r_tx[[5]])

#resample to 25
res <- 0.16666667
tx_25 <- aggregate(r_tx, fact=4)
tx_25 <- tx_25/10
plot(tx_25[[11]])

tmax_av <- overlay(tx_25[[5]],tx_25[[6]],tx_25[[7]],tx_25[[8]], #Mean temperature whole season
                   tx_25[[9]],tx_25[[10]],
                   fun = mean)

#T_MIN
setwd("~/DATABANK/BioClim/Future_2050/P26/GFDL/gd26tn50")
#hen Temp
list.files(pattern=".tif$",full.names=T)

jan_tn <- raster("gd26tn501.tif")
feb_tn <- raster("gd26tn502.tif")
mar_tn <- raster("gd26tn503.tif")
apr_tn <- raster("gd26tn504.tif")
may_tn <- raster("gd26tn505.tif")
jun_tn <- raster("gd26tn506.tif")
jul_tn <- raster("gd26tn507.tif")
aug_tn <- raster("gd26tn508.tif")
sep_tn <- raster("gd26tn509.tif")
oct_tn <- raster("gd26tn5010.tif")
nov_tn <- raster("gd26tn5011.tif")
dec_tn <- raster("gd26tn5012.tif")


r_tn <- stack(jan_tn, feb_tn, mar_tn, apr_tn, may_tn, jun_tn, jul_tn, aug_tn, sep_tn, oct_tn, nov_tn, dec_tn)
r_tn <- crop(r_tn, eth_bound)
r_tn <- mask(r_tn, eth_bound)
plot(r_tn[[5]])


#resample to 25
res <- 0.16666667
tn_25 <- aggregate(r_tn, fact=4)
tn_25 <- tn_25/10
plot(tn_25[[12]])  
# 
tmin_av <- overlay(tn_25[[5]],tn_25[[6]],tn_25[[7]],tn_25[[8]], #Mean temperature whole season
                   tn_25[[9]],tn_25[[10]],
                    fun = mean)

#Caclculate monthly means
jan_tav <- overlay(tx_25[[1]], tn_25[[1]], fun=mean)
feb_tav <- overlay(tx_25[[2]], tn_25[[2]], fun=mean)
mar_tav <- overlay(tx_25[[3]], tn_25[[3]], fun=mean)
apr_tav <- overlay(tx_25[[4]], tn_25[[4]], fun=mean)
may_tav <- overlay(tx_25[[5]], tn_25[[5]], fun=mean)
jun_tav <- overlay(tx_25[[6]], tn_25[[6]], fun=mean)
jul_tav <- overlay(tx_25[[7]], tn_25[[7]], fun=mean)
aug_tav <- overlay(tx_25[[8]], tn_25[[8]], fun=mean)
sep_tav <- overlay(tx_25[[9]], tn_25[[9]], fun=mean)
oct_tav <- overlay(tx_25[[10]], tn_25[[10]], fun=mean)
nov_tav <- overlay(tx_25[[11]], tn_25[[11]], fun=mean)
dec_tav <- overlay(tx_25[[12]], tn_25[[12]], fun=mean)

#Cacluate the bios
bio2 <- overlay(tmax_av, tmin_av, fun=function(a,b){(a-b)})
plot(bio2)  

bio8 <- overlay(jan_tav, feb_tav, mar_tav, apr_tav, may_tav, jun_tav, jul_tav,
                aug_tav, sep_tav, oct_tav, nov_tav, dec_tav,
                fun=mean)
plot(bio8)

bio3 <- overlay(jan_tav, feb_tav, mar_tav, apr_tav, may_tav, jun_tav, jul_tav,
                fun=mean)
plot(bio3)


writeRaster(bio2, paste0("~/DATABANK/Ethiopia/LayersC/P26/GFDL/bio2.tif"), overwrite=TRUE)#, format = "EHdr")) 
writeRaster(bio3, paste0("~/DATABANK/Ethiopia/LayersC/P26/GFDL/bio3.tif"), overwrite=TRUE)#, format = "EHdr")) 
writeRaster(bio8, paste0("~/DATABANK/Ethiopia/LayersC/P26/GFDL/bio8.tif"), overwrite=TRUE)#, format = "EHdr")) 
#_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

#________________________________________________________________________________________________________________
#________________________________________________________________________________________________________________
#MIROC5
setwd("~/DATABANK/BioClim/Future_2050/P26/MIROC5/mi26pr50")

#Precheitation
list.files()
jan_p <- raster("mi26pr501.tif")
feb_p <- raster("mi26pr502.tif")
mar_p <- raster("mi26pr503.tif")
apr_p <- raster("mi26pr504.tif")
may_p <- raster("mi26pr505.tif")
jun_p <- raster("mi26pr506.tif")
jul_p <- raster("mi26pr507.tif")
aug_p <- raster("mi26pr508.tif")
sep_p <- raster("mi26pr509.tif")
oct_p <- raster("mi26pr5010.tif")
nov_p <- raster("mi26pr5011.tif")
dec_p <- raster("mi26pr5012.tif")

pr <- stack(jan_p, feb_p, mar_p, apr_p, may_p, jun_p, jul_p, aug_p, sep_p, oct_p, nov_p, dec_p)
pr <- crop(pr, eth_bound)
pr <- mask(pr, eth_bound)

prec_25 <- pr
prec_25 <- aggregate(pr, fact=4)
plot(prec_25[[11]])

#Precip layers
bio12 <-  overlay(prec_25[[1]], prec_25[[2]], prec_25[[3]], prec_25[[4]], prec_25[[5]], prec_25[[6]],
                  prec_25[[7]], prec_25[[8]], prec_25[[9]], prec_25[[10]],prec_25[[11]], prec_25[[12]], #Sum rainfall whole season,
                  fun=sum)
plot(bio12)

bio13 <- overlay(prec_25[[1]], prec_25[[2]], prec_25[[3]], prec_25[[4]], prec_25[[5]], fun=sum) #Sum rain sowing months
plot(bio13)

bio16 <- overlay(prec_25[[5]], prec_25[[6]], prec_25[[7]], #Rainfall growing season only
                 prec_25[[8]], prec_25[[9]],  
                 fun=sum)

stk.sn <- stack(prec_25[[5]], prec_25[[6]], prec_25[[7]], prec_25[[8]], prec_25[[9]])
std.stk_sn <- calc(stk.sn, fun=sd)
bio15 <- std.stk_sn/bio12*100

plot(bio15)

writeRaster(bio12, paste0("~/DATABANK/Ethiopia/LayersC/P26/MIROC5/bio12.tif"), overwrite=TRUE)#, format = "EHdr")) 
writeRaster(bio13, paste0("~/DATABANK/Ethiopia/LayersC/P26/MIROC5/bio13.tif"), overwrite=TRUE)#, format = "EHdr")) 
writeRaster(bio15, paste0("~/DATABANK/Ethiopia/LayersC/P26/MIROC5/bio15.tif"), overwrite=TRUE)#, format = "EHdr")) 
writeRaster(bio16, paste0("~/DATABANK/Ethiopia/LayersC/P26/MIROC5/bio16.tif"), overwrite=TRUE)#, format = "EHdr")) 
#  
# #Doing the same for temperature layers

#Diurnal temp range calculation
#TMAX
setwd("~/DATABANK/BioClim/Future_2050/P26/MIROC5/mi26tx50")

#TMAX
list.files(pattern=".tif$",full.names=T)

jan_tx <- raster("mi26tx501.tif")
feb_tx <- raster("mi26tx501.tif")
mar_tx <- raster("mi26tx501.tif")
apr_tx <- raster("mi26tx501.tif")
may_tx <- raster("mi26tx501.tif")
jun_tx <- raster("mi26tx501.tif")
jul_tx <- raster("mi26tx501.tif")
aug_tx <- raster("mi26tx501.tif")
sep_tx <- raster("mi26tx501.tif")
oct_tx <- raster("mi26tx501.tif")
nov_tx <- raster("mi26tx501.tif")
dec_tx <- raster("mi26tx501.tif")

r_tx <- stack(jan_tx, feb_tx, mar_tx, apr_tx, may_tx, jun_tx, jul_tx, aug_tx, sep_tx, oct_tx, nov_tx, dec_tx)
r_tx <- crop(r_tx, eth_bound)
r_tx <- mask(r_tx, eth_bound)
plot(r_tx[[5]])

#resample to 25
res <- 0.16666667
tx_25 <- aggregate(r_tx, fact=4)
tx_25 <- tx_25/10
plot(tx_25[[11]])

tmax_av <- overlay(tx_25[[5]],tx_25[[6]],tx_25[[7]],tx_25[[8]], #Mean temperature whole season
                   tx_25[[9]],tx_25[[10]],
                   fun = mean)

#T_MIN
setwd("~/DATABANK/BioClim/Future_2050/P26/MIROC5/mi26tn50")
#hen Temp
list.files(pattern=".tif$",full.names=T)

jan_tn <- raster("mi26tn501.tif")
feb_tn <- raster("mi26tn502.tif")
mar_tn <- raster("mi26tn503.tif")
apr_tn <- raster("mi26tn504.tif")
may_tn <- raster("mi26tn505.tif")
jun_tn <- raster("mi26tn506.tif")
jul_tn <- raster("mi26tn507.tif")
aug_tn <- raster("mi26tn508.tif")
sep_tn <- raster("mi26tn509.tif")
oct_tn <- raster("mi26tn5010.tif")
nov_tn <- raster("mi26tn5011.tif")
dec_tn <- raster("mi26tn5012.tif")


r_tn <- stack(jan_tn, feb_tn, mar_tn, apr_tn, may_tn, jun_tn, jul_tn, aug_tn, sep_tn, oct_tn, nov_tn, dec_tn)
r_tn <- crop(r_tn, eth_bound)
r_tn <- mask(r_tn, eth_bound)
plot(r_tn[[5]])


#resample to 25
res <- 0.16666667
tn_25 <- aggregate(r_tn, fact=4)
tn_25 <- tn_25/10
plot(tn_25[[12]])  
# 
tmin_av <- overlay(tn_25[[5]],tn_25[[6]],tn_25[[7]],tn_25[[8]], #Mean temperature whole season
                   tn_25[[9]],tn_25[[10]],
                   fun = mean)

#Caclculate monthly means
jan_tav <- overlay(tx_25[[1]], tn_25[[1]], fun=mean)
feb_tav <- overlay(tx_25[[2]], tn_25[[2]], fun=mean)
mar_tav <- overlay(tx_25[[3]], tn_25[[3]], fun=mean)
apr_tav <- overlay(tx_25[[4]], tn_25[[4]], fun=mean)
may_tav <- overlay(tx_25[[5]], tn_25[[5]], fun=mean)
jun_tav <- overlay(tx_25[[6]], tn_25[[6]], fun=mean)
jul_tav <- overlay(tx_25[[7]], tn_25[[7]], fun=mean)
aug_tav <- overlay(tx_25[[8]], tn_25[[8]], fun=mean)
sep_tav <- overlay(tx_25[[9]], tn_25[[9]], fun=mean)
oct_tav <- overlay(tx_25[[10]], tn_25[[10]], fun=mean)
nov_tav <- overlay(tx_25[[11]], tn_25[[11]], fun=mean)
dec_tav <- overlay(tx_25[[12]], tn_25[[12]], fun=mean)

#Cacluate the bios
bio2 <- overlay(tmax_av, tmin_av, fun=function(a,b){(a-b)})
plot(bio2)  

bio8 <- overlay(jan_tav, feb_tav, mar_tav, apr_tav, may_tav, jun_tav, jul_tav,
                aug_tav, sep_tav, oct_tav, nov_tav, dec_tav,
                fun=mean)
plot(bio8)

bio3 <- overlay(jan_tav, feb_tav, mar_tav, apr_tav, may_tav, jun_tav, jul_tav,
                fun=mean)
plot(bio3)


writeRaster(bio2, paste0("~/DATABANK/Ethiopia/LayersC/P26/MIROC5/bio2.tif"), overwrite=TRUE)#, format = "EHdr")) 
writeRaster(bio3, paste0("~/DATABANK/Ethiopia/LayersC/P26/MIROC5/bio3.tif"), overwrite=TRUE)#, format = "EHdr")) 
writeRaster(bio8, paste0("~/DATABANK/Ethiopia/LayersC/P26/MIROC5/bio8.tif"), overwrite=TRUE)#, format = "EHdr")) 
#_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

#HADGEM
setwd("~/DATABANK/BioClim/Future_2050/P26/HADGEM/he26pr50")

#Precheitation
list.files()
jan_p <- raster("he26pr501.tif")
feb_p <- raster("he26pr502.tif")
mar_p <- raster("he26pr503.tif")
apr_p <- raster("he26pr504.tif")
may_p <- raster("he26pr505.tif")
jun_p <- raster("he26pr506.tif")
jul_p <- raster("he26pr507.tif")
aug_p <- raster("he26pr508.tif")
sep_p <- raster("he26pr509.tif")
oct_p <- raster("he26pr5010.tif")
nov_p <- raster("he26pr5011.tif")
dec_p <- raster("he26pr5012.tif")

pr <- stack(jan_p, feb_p, mar_p, apr_p, may_p, jun_p, jul_p, aug_p, sep_p, oct_p, nov_p, dec_p)
pr <- crop(pr, eth_bound)
pr <- mask(pr, eth_bound)

prec_25 <- pr
prec_25 <- aggregate(pr, fact=4)
plot(prec_25[[11]])

#Precip layers
bio12 <-  overlay(prec_25[[1]], prec_25[[2]], prec_25[[3]], prec_25[[4]], prec_25[[5]], prec_25[[6]],
                  prec_25[[7]], prec_25[[8]], prec_25[[9]], prec_25[[10]],prec_25[[11]], prec_25[[12]], #Sum rainfall whole season,
                  fun=sum)
plot(bio12)

bio13 <- overlay(prec_25[[1]], prec_25[[2]], prec_25[[3]], prec_25[[4]], prec_25[[5]], fun=sum) #Sum rain sowing months
plot(bio13)

bio16 <- overlay(prec_25[[5]], prec_25[[6]], prec_25[[7]], #Rainfall growing season only
                 prec_25[[8]], prec_25[[9]],  
                 fun=sum)

stk.sn <- stack(prec_25[[5]], prec_25[[6]], prec_25[[7]], prec_25[[8]], prec_25[[9]])
std.stk_sn <- calc(stk.sn, fun=sd)
bio15 <- std.stk_sn/bio12*100

plot(bio15)

writeRaster(bio12, paste0("~/DATABANK/Ethiopia/LayersC/P26/HADGEM/bio12.tif"), overwrite=TRUE)#, format = "EHdr")) 
writeRaster(bio13, paste0("~/DATABANK/Ethiopia/LayersC/P26/HADGEM/bio13.tif"), overwrite=TRUE)#, format = "EHdr")) 
writeRaster(bio15, paste0("~/DATABANK/Ethiopia/LayersC/P26/HADGEM/bio15.tif"), overwrite=TRUE)#, format = "EHdr")) 
writeRaster(bio16, paste0("~/DATABANK/Ethiopia/LayersC/P26/HADGEM/bio16.tif"), overwrite=TRUE)#, format = "EHdr")) 
#  
# #Doing the same for temperature layers

#Diurnal temp range calculation
#TMAX
setwd("~/DATABANK/BioClim/Future_2050/P26/HADGEM/he26tx50")

#TMAX
list.files(pattern=".tif$",full.names=T)

jan_tx <- raster("he26tx501.tif")
feb_tx <- raster("he26tx501.tif")
mar_tx <- raster("he26tx501.tif")
apr_tx <- raster("he26tx501.tif")
may_tx <- raster("he26tx501.tif")
jun_tx <- raster("he26tx501.tif")
jul_tx <- raster("he26tx501.tif")
aug_tx <- raster("he26tx501.tif")
sep_tx <- raster("he26tx501.tif")
oct_tx <- raster("he26tx501.tif")
nov_tx <- raster("he26tx501.tif")
dec_tx <- raster("he26tx501.tif")

r_tx <- stack(jan_tx, feb_tx, mar_tx, apr_tx, may_tx, jun_tx, jul_tx, aug_tx, sep_tx, oct_tx, nov_tx, dec_tx)
r_tx <- crop(r_tx, eth_bound)
r_tx <- mask(r_tx, eth_bound)
plot(r_tx[[5]])

#resample to 25
res <- 0.16666667
tx_25 <- aggregate(r_tx, fact=4)
tx_25 <- tx_25/10
plot(tx_25[[11]])

tmax_av <- overlay(tx_25[[5]],tx_25[[6]],tx_25[[7]],tx_25[[8]], #Mean temperature whole season
                   tx_25[[9]],tx_25[[10]],
                   fun = mean)

#T_MIN
setwd("~/DATABANK/BioClim/Future_2050/P26/HADGEM/he26tn50")
#hen Temp
list.files(pattern=".tif$",full.names=T)

jan_tn <- raster("he26tn501.tif")
feb_tn <- raster("he26tn502.tif")
mar_tn <- raster("he26tn503.tif")
apr_tn <- raster("he26tn504.tif")
may_tn <- raster("he26tn505.tif")
jun_tn <- raster("he26tn506.tif")
jul_tn <- raster("he26tn507.tif")
aug_tn <- raster("he26tn508.tif")
sep_tn <- raster("he26tn509.tif")
oct_tn <- raster("he26tn5010.tif")
nov_tn <- raster("he26tn5011.tif")
dec_tn <- raster("he26tn5012.tif")


r_tn <- stack(jan_tn, feb_tn, mar_tn, apr_tn, may_tn, jun_tn, jul_tn, aug_tn, sep_tn, oct_tn, nov_tn, dec_tn)
r_tn <- crop(r_tn, eth_bound)
r_tn <- mask(r_tn, eth_bound)
plot(r_tn[[5]])


#resample to 25
res <- 0.16666667
tn_25 <- aggregate(r_tn, fact=4)
tn_25 <- tn_25/10
plot(tn_25[[12]])  
# 
tmin_av <- overlay(tn_25[[5]],tn_25[[6]],tn_25[[7]],tn_25[[8]], #Mean temperature whole season
                   tn_25[[9]],tn_25[[10]],
                   fun = mean)

#Caclculate monthly means
jan_tav <- overlay(tx_25[[1]], tn_25[[1]], fun=mean)
feb_tav <- overlay(tx_25[[2]], tn_25[[2]], fun=mean)
mar_tav <- overlay(tx_25[[3]], tn_25[[3]], fun=mean)
apr_tav <- overlay(tx_25[[4]], tn_25[[4]], fun=mean)
may_tav <- overlay(tx_25[[5]], tn_25[[5]], fun=mean)
jun_tav <- overlay(tx_25[[6]], tn_25[[6]], fun=mean)
jul_tav <- overlay(tx_25[[7]], tn_25[[7]], fun=mean)
aug_tav <- overlay(tx_25[[8]], tn_25[[8]], fun=mean)
sep_tav <- overlay(tx_25[[9]], tn_25[[9]], fun=mean)
oct_tav <- overlay(tx_25[[10]], tn_25[[10]], fun=mean)
nov_tav <- overlay(tx_25[[11]], tn_25[[11]], fun=mean)
dec_tav <- overlay(tx_25[[12]], tn_25[[12]], fun=mean)

#Cacluate the bios
bio2 <- overlay(tmax_av, tmin_av, fun=function(a,b){(a-b)})
plot(bio2)  

bio8 <- overlay(jan_tav, feb_tav, mar_tav, apr_tav, may_tav, jun_tav, jul_tav,
                aug_tav, sep_tav, oct_tav, nov_tav, dec_tav,
                fun=mean)
plot(bio8)

bio3 <- overlay(jan_tav, feb_tav, mar_tav, apr_tav, may_tav, jun_tav, jul_tav,
                fun=mean)
plot(bio3)


writeRaster(bio2, paste0("~/DATABANK/Ethiopia/LayersC/P26/HADGEM/bio2.tif"), overwrite=TRUE)#, format = "EHdr")) 
writeRaster(bio3, paste0("~/DATABANK/Ethiopia/LayersC/P26/HADGEM/bio3.tif"), overwrite=TRUE)#, format = "EHdr")) 
writeRaster(bio8, paste0("~/DATABANK/Ethiopia/LayersC/P26/HADGEM/bio8.tif"), overwrite=TRUE)#, format = "EHdr")) 
#_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
#______________________________________________________________________________________________________________________

#IPSL
setwd("~/DATABANK/BioClim/Future_2050/P26/IPSL/ip26pr50")

#Precheitation
list.files()
jan_p <- raster("ip26pr501.tif")
feb_p <- raster("ip26pr502.tif")
mar_p <- raster("ip26pr503.tif")
apr_p <- raster("ip26pr504.tif")
may_p <- raster("ip26pr505.tif")
jun_p <- raster("ip26pr506.tif")
jul_p <- raster("ip26pr507.tif")
aug_p <- raster("ip26pr508.tif")
sep_p <- raster("ip26pr509.tif")
oct_p <- raster("ip26pr5010.tif")
nov_p <- raster("ip26pr5011.tif")
dec_p <- raster("ip26pr5012.tif")

pr <- stack(jan_p, feb_p, mar_p, apr_p, may_p, jun_p, jul_p, aug_p, sep_p, oct_p, nov_p, dec_p)
pr <- crop(pr, eth_bound)
pr <- mask(pr, eth_bound)

prec_25 <- pr
prec_25 <- aggregate(pr, fact=4)
plot(prec_25[[11]])

#Precip layers
bio12 <-  overlay(prec_25[[1]], prec_25[[2]], prec_25[[3]], prec_25[[4]], prec_25[[5]], prec_25[[6]],
                  prec_25[[7]], prec_25[[8]], prec_25[[9]], prec_25[[10]],prec_25[[11]], prec_25[[12]], #Sum rainfall whole season,
                  fun=sum)
plot(bio12)

bio13 <- overlay(prec_25[[1]], prec_25[[2]], prec_25[[3]], prec_25[[4]], prec_25[[5]], fun=sum) #Sum rain sowing months
plot(bio13)

bio16 <- overlay(prec_25[[5]], prec_25[[6]], prec_25[[7]], #Rainfall growing season only
                 prec_25[[8]], prec_25[[9]],  
                 fun=sum)

stk.sn <- stack(prec_25[[5]], prec_25[[6]], prec_25[[7]], prec_25[[8]], prec_25[[9]])
std.stk_sn <- calc(stk.sn, fun=sd)
bio15 <- std.stk_sn/bio12*100

plot(bio15)

writeRaster(bio12, paste0("~/DATABANK/Ethiopia/LayersC/P26/IPSL/bio12.tif"), overwrite=TRUE)#, format = "EHdr")) 
writeRaster(bio13, paste0("~/DATABANK/Ethiopia/LayersC/P26/IPSL/bio13.tif"), overwrite=TRUE)#, format = "EHdr")) 
writeRaster(bio15, paste0("~/DATABANK/Ethiopia/LayersC/P26/IPSL/bio15.tif"), overwrite=TRUE)#, format = "EHdr")) 
writeRaster(bio16, paste0("~/DATABANK/Ethiopia/LayersC/P26/IPSL/bio16.tif"), overwrite=TRUE)#, format = "EHdr")) 
#  
# #Doing the same for temperature layers

#Diurnal temp range calculation
#TMAX
setwd("~/DATABANK/BioClim/Future_2050/P26/IPSL/ip26tx50")

#TMAX
list.files(pattern=".tif$",full.names=T)

jan_tx <- raster("ip26tx501.tif")
feb_tx <- raster("ip26tx501.tif")
mar_tx <- raster("ip26tx501.tif")
apr_tx <- raster("ip26tx501.tif")
may_tx <- raster("ip26tx501.tif")
jun_tx <- raster("ip26tx501.tif")
jul_tx <- raster("ip26tx501.tif")
aug_tx <- raster("ip26tx501.tif")
sep_tx <- raster("ip26tx501.tif")
oct_tx <- raster("ip26tx501.tif")
nov_tx <- raster("ip26tx501.tif")
dec_tx <- raster("ip26tx501.tif")

r_tx <- stack(jan_tx, feb_tx, mar_tx, apr_tx, may_tx, jun_tx, jul_tx, aug_tx, sep_tx, oct_tx, nov_tx, dec_tx)
r_tx <- crop(r_tx, eth_bound)
r_tx <- mask(r_tx, eth_bound)
plot(r_tx[[5]])

#resample to 25
res <- 0.16666667
tx_25 <- aggregate(r_tx, fact=4)
tx_25 <- tx_25/10
plot(tx_25[[11]])

tmax_av <- overlay(tx_25[[5]],tx_25[[6]],tx_25[[7]],tx_25[[8]], #Mean temperature whole season
                   tx_25[[9]],tx_25[[10]],
                   fun = mean)

#T_MIN
setwd("~/DATABANK/BioClim/Future_2050/P26/IPSL/ip26tn50")
#hen Temp
list.files(pattern=".tif$",full.names=T)

jan_tn <- raster("ip26tn501.tif")
feb_tn <- raster("ip26tn502.tif")
mar_tn <- raster("ip26tn503.tif")
apr_tn <- raster("ip26tn504.tif")
may_tn <- raster("ip26tn505.tif")
jun_tn <- raster("ip26tn506.tif")
jul_tn <- raster("ip26tn507.tif")
aug_tn <- raster("ip26tn508.tif")
sep_tn <- raster("ip26tn509.tif")
oct_tn <- raster("ip26tn5010.tif")
nov_tn <- raster("ip26tn5011.tif")
dec_tn <- raster("ip26tn5012.tif")


r_tn <- stack(jan_tn, feb_tn, mar_tn, apr_tn, may_tn, jun_tn, jul_tn, aug_tn, sep_tn, oct_tn, nov_tn, dec_tn)
r_tn <- crop(r_tn, eth_bound)
r_tn <- mask(r_tn, eth_bound)
plot(r_tn[[5]])


#resample to 25
res <- 0.16666667
tn_25 <- aggregate(r_tn, fact=4)
tn_25 <- tn_25/10
plot(tn_25[[12]])  
# 
tmin_av <- overlay(tn_25[[5]],tn_25[[6]],tn_25[[7]],tn_25[[8]], #Mean temperature whole season
                   tn_25[[9]],tn_25[[10]],
                   fun = mean)

#Caclculate monthly means
jan_tav <- overlay(tx_25[[1]], tn_25[[1]], fun=mean)
feb_tav <- overlay(tx_25[[2]], tn_25[[2]], fun=mean)
mar_tav <- overlay(tx_25[[3]], tn_25[[3]], fun=mean)
apr_tav <- overlay(tx_25[[4]], tn_25[[4]], fun=mean)
may_tav <- overlay(tx_25[[5]], tn_25[[5]], fun=mean)
jun_tav <- overlay(tx_25[[6]], tn_25[[6]], fun=mean)
jul_tav <- overlay(tx_25[[7]], tn_25[[7]], fun=mean)
aug_tav <- overlay(tx_25[[8]], tn_25[[8]], fun=mean)
sep_tav <- overlay(tx_25[[9]], tn_25[[9]], fun=mean)
oct_tav <- overlay(tx_25[[10]], tn_25[[10]], fun=mean)
nov_tav <- overlay(tx_25[[11]], tn_25[[11]], fun=mean)
dec_tav <- overlay(tx_25[[12]], tn_25[[12]], fun=mean)

#Cacluate the bios
bio2 <- overlay(tmax_av, tmin_av, fun=function(a,b){(a-b)})
plot(bio2)  

bio8 <- overlay(jan_tav, feb_tav, mar_tav, apr_tav, may_tav, jun_tav, jul_tav,
                aug_tav, sep_tav, oct_tav, nov_tav, dec_tav,
                fun=mean)
plot(bio8)

bio3 <- overlay(jan_tav, feb_tav, mar_tav, apr_tav, may_tav, jun_tav, jul_tav,
                fun=mean)
plot(bio3)


writeRaster(bio2, paste0("~/DATABANK/Ethiopia/LayersC/P26/IPSL/bio2.tif"), overwrite=TRUE)#, format = "EHdr")) 
writeRaster(bio3, paste0("~/DATABANK/Ethiopia/LayersC/P26/IPSL/bio3.tif"), overwrite=TRUE)#, format = "EHdr")) 
writeRaster(bio8, paste0("~/DATABANK/Ethiopia/LayersC/P26/IPSL/bio8.tif"), overwrite=TRUE)#, format = "EHdr")) 
#_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _


# print("Inini ndinonzi inini")



