# --------------- GIS in R --------------- #
#### Vectorial maps ####

# download coastline global map:
# https://www.naturalearthdata.com/downloads/10m-physical-vectors/10m-coastline/

# How to read a vectorial map:
library(maptools)
library(rgdal)
library(sf)

#1
proj <- CRS('+proj=longlat +ellps=WGS84')
mapaSHP <- readShapeLines('~/Desktop/Batch maps/Global Maps/ne_10m_coastline/ne_10m_coastline.shp',
                          proj4string=proj) #need specify projection
#2
global_map_shp <- readOGR('~/Desktop/Batch maps/Global Maps/ne_10m_coastline/ne_10m_coastline.shp')

#3
global_map_shp2 <- st_read('~/Desktop/Batch maps/Global Maps/ne_10m_coastline/ne_10m_coastline.shp')

plot(global_map_shp)

#-------------------#
#### Raster maps ####

# download tavg 10m, tavg 30s, prec 10m, prec 30s
# https://www.worldclim.org/data/worldclim21.html

# Resolution can also be expressed in minutes or seconds
# 30 seconds ~1 km2; 10 min ~340km2

# How to read a raster map:
library(raster)
# ways of expressing resolution: km2, minutes/seconds
temp_10m <- raster("~/Desktop/Batch maps/WorldClim/wc2.1_10m_tavg/wc2.1_10m_tavg_01.tif")
temp_30s <- raster("~/Desktop/Batch maps/WorldClim/wc2.1_30s_tavg/wc2.1_30s_tavg_01.tif")
plot(temp_10m)

#------------------#
#### Projection ####
# Consulting projection
crs(temp_10m)
projection(temp_10m)
projection(global_map_shp)
crs(global_map_shp)

plot(temp_10m)
plot(global_map_shp, add=T)

# Reprojection in vectorial
global_shp_repr <- spTransform(global_map_shp,
                              CRS=CRS("+proj=merc +ellps=GRS80"))
plot(global_shp_repr)

# Reprojection in raster
temp_10m_mercator <- projectRaster(from=temp_10m, crs=crs(global_shp_repr))
plot(temp_10m_mercator)

#------------------#
#### Resolution ####
# Is showed in degrees
res(temp_10m)
res(temp_30s)

temp_ag <- aggregate(temp_10m, fact=4) #4 pixels now are 1
res(temp_ag)
res(temp_10m)
plot(temp_ag)

#### Crop function ####
EastUSA_10m <- crop(temp_10m, extent(-90,-40,20,50))
EastUSA_30s <- crop(temp_30s, extent(-90,-40,20,50))
EastUSA_agg <- crop(temp_ag, extent(-90,-40,20,50))
par(mfrow=c(3,1))
plot(EastUSA_10m)
plot(EastUSA_30s)
plot(EastUSA_agg)

#### Stack & Brick ####
mapFiles<- dir("~/Desktop/Batch maps/WorldClim/wc2.1_10m_tavg/", full.names=TRUE) #pattern=".tif",

# Same but different weights
maps_list<- lapply(mapFiles, raster)
maps_list[[6]]
tavg_stack<- stack(maps_list)
tavg_brick<- brick(maps_list)

# Apply calculations
start_time <- Sys.time()
stack_sd <- calc(tavg_stack, fun = sd)
end_time <- Sys.time()
end_time-start_time

start_time <- Sys.time()
brick_sd <- calc(tavg_brick, fun = sd)
end_time <- Sys.time()
end_time-start_time

# Write and save rasters/stacks/bricks
writeRaster(stack_sd, filename = "~/Desktop/test.grd") #help for formats
stack_sd <- calc(tavg_stack, fun = sd, filename="~/Desktop/test.grd")

# Convert df to points
# NP data is a db from foilar N and foliar P data from wooden plants. Units in % dry weight.
NPdata <- read.csv("~/Desktop/NPdata.csv")

# x is long and y is lat
NP_point <- SpatialPointsDataFrame(coords = NPdata[,c(5,4)], data = NPdata,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
NPdata <- subset(NPdata, !is.na(NPdata$Latitude))
NPdata <- subset(NPdata, !is.na(NPdata$Longitude))
NP_point <- SpatialPointsDataFrame(coords = NPdata[,c(5,4)], data = NPdata,
                                   proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

Pnt <- SpatialPoints(NPdata[,5:4])

plot(NP_point, pch=18)
plot(global_map_shp, add=TRUE)

# Pnt <- SpatialPoints(mfv[,4:3])

library(raster)
# Extract
Biopnt <- extract(temp_ag,Pnt)


