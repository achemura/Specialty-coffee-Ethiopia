rm(list=ls()) # Cleaning the shit off

# load the library

library(maptools)
library(raster)
library(rgdal)
library(RColorBrewer)
library(sp)
library(rasterVis)

setwd("~/DATABANK/Ethiopia/")

#Loading the environmental layers
eth_country <- readOGR("~/DATABANK/Ethiopia/SHP/ETH_adm0.shp")
plot(eth_country)


setwd("~/DATABANK/ClimLayers/Curr_bios/")

bio1_raw<- raster("bio_1.bil")
bio1 <- crop(bio1_raw, eth_country)
plot(bio1)

bio2_raw<- raster("bio_2.bil")
bio2 <- crop(bio2_raw, eth_country)
plot(bio2)

bio3_raw<- raster("bio_3.bil")
bio3 <- crop(bio3_raw, eth_country)
plot(bio3)

bio4_raw<- raster("bio_4.bil")
bio4 <- crop(bio4_raw, eth_country)
plot(bio4)

bio5_raw<- raster("bio_5.bil")
bio5 <- crop(bio5_raw, eth_country)
plot(bio5)

bio6_raw<- raster("bio_6.bil")
bio6 <- crop(bio6_raw, eth_country)
plot(bio6)

bio7_raw<- raster("bio_7.bil")
bio7 <- crop(bio7_raw, eth_country)
plot(bio7)

bio8_raw<- raster("bio_8.bil")
bio8 <- crop(bio8_raw, eth_country)
plot(bio8)

bio9_raw<- raster("bio_9.bil")
bio9 <- crop(bio9_raw, eth_country)
plot(bio9)

bio10_raw<- raster("bio_10.bil")
bio10 <- crop(bio10_raw, eth_country)
plot(bio10)

bio11_raw<- raster("bio_11.bil")
bio11 <- crop(bio11_raw, eth_country)
plot(bio11)

bio12_raw<- raster("bio_12.bil")
bio12 <- crop(bio12_raw, eth_country)
plot(bio12)

bio13_raw<- raster("bio_13.bil")
bio13 <- crop(bio13_raw, eth_country)
plot(bio13)

bio14_raw<- raster("bio_14.bil")
bio14 <- crop(bio14_raw, eth_country)
plot(bio14)

bio15_raw<- raster("bio_15.bil")
bio15 <- crop(bio15_raw, eth_country)
plot(bio15)

bio16_raw<- raster("bio_16.bil")
bio16 <- crop(bio16_raw, eth_country)
plot(bio16)

bio17_raw<- raster("bio_17.bil")
bio17 <- crop(bio17_raw, eth_country)
plot(bio17)

bio18_raw<- raster("bio_18.bil")
bio18 <- crop(bio18_raw, eth_country)
plot(bio18)

bio19_raw<- raster("bio_19.bil")
bio19 <- crop(bio19_raw, eth_country)
plot(bio19)


soil_oc <- raster("~/DATABANK/Ethiopia/Layers/Soil/soil_ph_x.tif")
soil_oc <- crop(soil_oc, eth_country)
plot(soil_oc)
# soil_oc <- aggregate(soil_oc, fact=5.000001)
# plot(soil_oc)
# extent(soil_oc) <- extent(bio1)
# plot(soil_oc)


# elevat_raw <- raster("~/DATABANK/Ethiopia/Eth_dem/1km/ETH_ikmdem.tif")
# elevat <- crop(elevat_raw, eth_country)
# plot(elevat)
# elevat <- aggregate(elevat, fact=5.000001)
# extent(elevat) <- extent(bio1)
# plot(elevat)


# slope_aspect <- terrain(elevat, opt=c('slope', 'aspect'),  neighbors=8, unit='degrees')
# slope <- slope_aspect[[1]]
# plot(slope)
# aspect <- slope_aspect[[2]]
# plot(aspect)

bio1_raw<- raster("Coffee/Layers/Current/bio1.asc")
plot(bio1_raw)

crs1 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#Soil pH
soil_ph_raw <- raster("~/DATABANK/Ethiopia/Layers/Soil/soil_ph_x.tif")
eth_country_prj <- spTransform(eth_country, crs(soil_ph_raw))
soil_ph_raw_prj <- projectRaster(soil_ph_raw, crs = crs1)
soil_ph_res <- resample(soil_ph_raw_prj, bio1_raw)
soil_ph_fin <- crop(soil_ph_res, eth_country)
plot(soil_ph_fin)
plot(eth_country, add=T)
dim(soil_ph_fin)
dim(bio1_raw)

#Soil CEC
soil_cec_raw <- raster("~/DATABANK/Ethiopia/Layers/Soil/soil_cec_x.tif")
eth_country_prj <- spTransform(eth_country, crs(soil_cec_raw))
soil_cec_raw_prj <- projectRaster(soil_cec_raw, crs = crs1)
soil_cec_res <- resample(soil_cec_raw_prj, bio1_raw)
soil_cec_fin <- crop(soil_cec_res, eth_country)
plot(soil_cec_fin)
plot(eth_country, add=T)
dim(soil_cec_fin)
dim(bio1_raw)

#Soil BD
soil_bd_raw <- raster("~/DATABANK/Ethiopia/Layers/Soil/soil_bd_x.tif")
eth_country_prj <- spTransform(eth_country, crs(soil_bd_raw))
soil_bd_raw_prj <- projectRaster(soil_bd_raw, crs = crs1)
soil_bd_res <- resample(soil_bd_raw_prj, bio1_raw)
soil_bd_fin <- crop(soil_bd_res, eth_country)
plot(soil_bd_fin)
plot(eth_country, add=T)
dim(soil_bd_fin)
dim(bio1_raw)

writeRaster(bio1, paste0("~/DATABANK/Coffea/Vietnam/Layers/bio1.tif"), format="ascii", overwrite=TRUE)
writeRaster(bio2, paste0("~/DATABANK/Coffea/Vietnam/Layers/bio2.tif"), format="ascii", overwrite=TRUE)
writeRaster(bio3, paste0("~/DATABANK/Coffea/Vietnam/Layers/bio3.tif"), format="ascii", overwrite=TRUE)
writeRaster(bio4, paste0("~/DATABANK/Coffea/Vietnam/Layers/bio4.tif"), format="ascii", overwrite=TRUE)
writeRaster(bio5, paste0("~/DATABANK/Coffea/Vietnam/Layers/bio5.tif"), format="ascii", overwrite=TRUE)
writeRaster(bio6, paste0("~/DATABANK/Coffea/Vietnam/Layers/bio6.tif"), format="ascii", overwrite=TRUE)
writeRaster(bio7, paste0("~/DATABANK/Coffea/Vietnam/Layers/bio7.tif"), format="ascii", overwrite=TRUE)
writeRaster(bio8, paste0("~/DATABANK/Coffea/Vietnam/Layers/bio8.tif"), format="ascii", overwrite=TRUE)
writeRaster(bio9, paste0("~/DATABANK/Coffea/Vietnam/Layers/bio9.tif"), format="ascii", overwrite=TRUE)
writeRaster(bio10, paste0("~/DATABANK/Coffea/Vietnam/Layers/bio10.tif"), format="ascii", overwrite=TRUE)
writeRaster(bio11, paste0("~/DATABANK/Coffea/Vietnam/Layers/bio11.tif"), format="ascii", overwrite=TRUE)
writeRaster(bio12, paste0("~/DATABANK/Coffea/Vietnam/Layers/bio12.tif"), format="ascii", overwrite=TRUE)
writeRaster(bio13, paste0("~/DATABANK/Coffea/Vietnam/Layers/bio13.tif"), format="ascii", overwrite=TRUE)
writeRaster(bio14, paste0("~/DATABANK/Coffea/Vietnam/Layers/bio14.tif"), format="ascii", overwrite=TRUE)
writeRaster(bio15, paste0("~/DATABANK/Coffea/Vietnam/Layers/bio15.tif"), format="ascii", overwrite=TRUE)
writeRaster(bio16, paste0("~/DATABANK/Coffea/Vietnam/Layers/bio16.tif"), format="ascii", overwrite=TRUE)
writeRaster(bio17, paste0("~/DATABANK/Coffea/Vietnam/Layers/bio17.tif"), format="ascii", overwrite=TRUE)
writeRaster(bio18, paste0("~/DATABANK/Coffea/Vietnam/Layers/bio18.tif"), format="ascii", overwrite=TRUE)
writeRaster(bio19, paste0("~/DATABANK/Coffea/Vietnam/Layers/bio19.tif"), format="ascii", overwrite=TRUE)

writeRaster(soil_oc, paste0("~/DATABANK/Coffea/Vietnam/Layers/soil_oc.tif"), format="ascii", overwrite=TRUE)

#writeRaster(slope, paste0("~/DATABANK/Coffea/Vietnam/Layers/slope"), format="ascii", overwrite=TRUE)
#writeRaster(aspect, paste0("~/DATABANK/Coffea/Vietnam/Layers/aspect"), format="ascii", overwrite=TRUE)

#writeRaster(elevat, paste0("~/DATABANK/Coffea/Vietnam/Layers/elevat.tif"), format="ascii", overwrite=TRUE)

writeRaster(soil_ph_fin, paste0("~/DATABANK/Ethiopia/Coffee/Layers/Current/soil_ph"), format="ascii", overwrite=TRUE)
writeRaster(soil_cec_fin, paste0("~/DATABANK/Ethiopia/Coffee/Layers/Current/soil_cec"), format="ascii", overwrite=TRUE)
writeRaster(soil_bd_fin, paste0("~/DATABANK/Ethiopia/Coffee/Layers/Current/soil_bd"), format="ascii", overwrite=TRUE)

#
print("Inini ndinonzi inini")


