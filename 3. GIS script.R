# --------------- GIS in R --------------- #
#### 1. Vectorial maps ####

# download coastline global map:
# https://www.naturalearthdata.com/downloads/10m-physical-vectors/10m-coastline/

# How to read a vectorial map:
library(maptools)
library(rgdal)
library(sf)

#1
proj <- CRS('+proj=longlat +ellps=WGS84')
mapaSHP <- readShapeLines('~/Desktop/Batch maps/Global Maps/ne_50m_coastline/ne_50m_coastline.shp',
                          proj4string=proj) #need specify projection
#2 - Personal preference
global_map_shp <- readOGR('~/Desktop/Batch maps/Global Maps/ne_50m_coastline/ne_50m_coastline.shp')

#3
# global_map_shp2 <- st_read('~/Desktop/Batch maps/Global Maps/ne_50m_coastline/ne_50m_coastline.shp')

plot(global_map_shp)

#-------------------#
#### 2. Raster maps ####
#### .Read a raster ####
library(raster)
# ways of expressing resolution: km2, minutes/seconds
temp_10m <- raster("~/Desktop/Batch maps/WorldClim/wc2.1_10m_tavg/wc2.1_10m_tavg_01.tif")
plot(temp_10m)

#### .Time-series maps ####
library(rgdal)
library(ncdf4)
library(raster)
# source Yang and Tian 2022: https://data.isimip.org/10.48364/ISIMIP.759077.2
noy <- nc_open("~/Desktop/Batch maps/N dep/ndep-nhx_1901soc_monthly_1901_2021.nc")
plot(noy) #it doesn't work, no worries
names(noy$dim) # identify names

#getting the map structure
lon <- ncvar_get(noy, "lon")
lat <- ncvar_get(noy, "lat", verbose = F)
t <- ncvar_get(noy, "time")
noy.array <- ncvar_get(noy, "nhx") #get the content
fillvalue <- noy[["var"]][["nhx"]][["missval"]]
noy.array[noy.array == fillvalue] <- NA
nc_close(noy)

# transform into a raster
r <- raster(t(noy.array[,,1]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
plot(r)

# save raster
writeRaster(r, filename = "~/Desktop/MIT/Classes/Course material/ndep_map.tif") #Use .grd for grd format instead of .tif

#------------------#
#### 3. Projection ####
library(raster)
library(rgdal)
# Consulting projection
temp_10m <- raster("~/Desktop/Batch maps/WorldClim/wc2.1_10m_tavg/wc2.1_10m_tavg_01.tif")
global_map_shp <- readOGR('~/Desktop/Batch maps/Global Maps/ne_50m_coastline/ne_50m_coastline.shp')

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
soildata<- raster("~/Desktop/Batch maps/Global maps/SoilGrids/cec.map/cec_0-5cm_mean_1000.tif", full.names=TRUE, pattern= ".tif") #pattern=".tif",
plot(soildata)
soildata
temp_10m_reprojected <- projectRaster(temp_10m, soildata)
plot(temp_10m_reprojected)
crs(temp_10m_reprojected)

#------------------#
#### 4. Resolution ####
# download tavg 10m, tavg 30s (optional)
# https://www.worldclim.org/data/worldclim21.html

temp_10m <- raster("~/Desktop/Batch maps/WorldClim/wc2.1_10m_tavg/wc2.1_10m_tavg_01.tif")
temp_30s <- raster("~/Desktop/Batch maps/WorldClim/wc2.1_30s_tavg/wc2.1_30s_tavg_01.tif")
plot(temp_10m)

# Resolution can also be expressed in minutes or seconds, degrees or km2
# 30 seconds ~1 km2; 10 min ~340km2
# Is showed in degrees
res(temp_10m)
res(temp_30s)

temp_ag <- aggregate(temp_10m, fact=4) #4 pixels now are 1 LIGHTBOARD
res(temp_ag)
res(temp_10m)
plot(temp_ag)

#### 5. Stack & Brick ####
#load files in a folder all at once
mapFiles<- dir("~/Desktop/Batch maps/WorldClim/wc2.1_10m_tavg/", full.names=TRUE) #pattern=".tif",
maps_list<- lapply(mapFiles, raster)
maps_list[[6]]
plot(maps_list[[6]])

tavg_stack<- stack(maps_list) # they have to be in same extension and resolution
tavg_brick<- brick(maps_list) # they have to be in same extension and resolution

# Calculate time of a process
start_time <- Sys.time()
stack_sd <- calc(tavg_stack, fun = sd)
end_time <- Sys.time()
end_time-start_time
plot(stack_sd)

# Write and save rasters/stacks/bricks
writeRaster(stack_sd, filename = "~/Desktop/test.grd") #help for formats
stack_sd <- calc(tavg_stack, fun = sd, filename="~/Desktop/test.grd")
# IMPORTANT: For .grd files always geep the two files (.grd and .gri) in the same folder.

#### .Crop function ####
EastUSA_10m <- crop(temp_10m, extent(-90,-40,20,50))
plot(EastUSA_10m)
EastUSA_30s <- crop(tavg_stack, extent(-90,-40,20,50))

#### .Mask function ####
ndep <- raster("~/Desktop/MIT/Classes/Course material/ndep_map.tif")
plot(ndep)
temp_10m <- raster("~/Desktop/Batch maps/WorldClim/wc2.1_10m_tavg/wc2.1_10m_tavg_01.tif")
plot(temp_10m)

ground_dep <- mask(ndep, temp_10m) #it gives us an error, no worries

#how to solve the given error
ndep2 <- resample(ndep, temp_10m)
ground_dep <- mask(ndep2, temp_10m)

plot(ground_dep)

#### 6. Gap fill from maps ####
library(sp)
# Download data from: https://github.com/helenavallicrosa/Nuptake_NUE
Nup_data <- read.csv("~/Desktop/MIT/Classes/Course material/gapFilled_db_lats_ratios_ndep.csv")
# Nup data calculates plant N uptake based on field measurements.

# Convert latitude and longitude into spatial points. x is long and y is lat
colnames(Nup_data)
Nup_data <- subset(Nup_data, !is.na(Nup_data$Latitude)) #makes sure no NA on coordinates
Nup_data<- subset(Nup_data, !is.na(Nup_data$Longitude)) #makes sure no NA on coordinates

#Way 1
Nup_point <- SpatialPointsDataFrame(coords = Nup_data[,c(4,3)], data = Nup_data,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
#Way 2
Pnt <- SpatialPoints(Nup_data[,4:3])

plot(Pnt)
plot(Nup_point)

plot(Pnt, pch=18)
library(rgdal)
global_map_shp <- readOGR('~/Desktop/Batch maps/Global Maps/ne_50m_coastline/ne_50m_coastline.shp')

plot(global_map_shp, add=TRUE)

# Pnt <- SpatialPoints(mfv[,4:3])

library(raster)
# Extract info from data points
temp_10m <- raster("~/Desktop/Batch maps/WorldClim/wc2.1_10m_tavg/wc2.1_10m_tavg_01.tif")
Biopnt <- extract(temp_10m,Pnt)

#### EXERCISE ####

#### Step 1 ####
# Download 10m resolution average temperature maps from worldClim:
# https://www.worldclim.org/data/worldclim21.html
# Load the maps on your R session and calculate the average of all
# maps to obtain the yearly average temperature

mapFiles<- dir("~/Desktop/Batch maps/WorldClim/wc2.1_10m_tavg/", full.names=TRUE) #pattern=".tif",
maps_list<- lapply(mapFiles, raster)
tavg_stack<- stack(maps_list) 
mean_temperature <- calc(tavg_stack, fun=mean)
plot(mean_temperature)

#### Step 2 ####
# Download the Nuptake data from: 
# https://github.com/helenavallicrosa/Nuptake_NUE
# Create a new dataframe by subsetting the columns number 3,4 and 18 
# from the Nuptake data

Nup_data <- read.csv("~/Desktop/MIT/Classes/Course material/gapFilled_db_lats_ratios_ndep.csv")
Nup_db <- Nup_data[,c(3,4,18)]
  
#### Step 3 ####
# Create spatialpoints by using the coordinates of the subsetted 
# dataframe
# Extract the mean annual temperature of the data points you just 
# created from the map
# Include the new vector to the dataframe resulting in a 4 calumn 
# dataframe

Pnt <- SpatialPoints(Nup_data[,4:3])
MAT_points <- extract(mean_temperature, Pnt)

Nup_db$MAT_wc <- MAT_points

m1 <- lm(MAT ~ MAT_wc, data= Nup_db)
summary(m1)
plot(Nup_db$MAT, Nup_db$MAT_wc)

#### Step 4 ####
# Create a new column in your dataframe where you gapfill the MAT 
# variable with the map data you just extracted

for (i in 1:159){
  if (is.na(Nup_db$MAT[i])){
    Nup_db$MAT_new[i] <- Nup_db$MAT_wc[i]
  } else {
    Nup_db$MAT_new[i] <- Nup_db$MAT[i]
  }
}


#### 7.Plot a raster ####
temp_10m <- raster("~/Desktop/Batch maps/WorldClim/wc2.1_10m_tavg/wc2.1_10m_tavg_01.tif")
temp_coarse <- aggregate(temp_10m, fact=5) # optional
temp_df <- as.data.frame(temp_coarse, xy=T) # important xy=T
colnames(temp_df)[3] <- "temp"

library(maptools)
library(ggplot2)
library(viridis)

mapaSHP <- readOGR('~/Desktop/Batch maps/Global Maps/ne_50m_coastline/ne_50m_coastline.shp')

map_temp <- ggplot(temp_df) + 
  geom_raster(aes(x=x, y=y, fill=temp)) # most basic plot

map_temp <- ggplot(temp_df) + 
  geom_raster(aes(x, y, fill=temp)) +
  geom_polygon(data = mapaSHP, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  scale_fill_viridis(na.value=NA, direction = -1, option="G") +
  theme_minimal()+
  ylim(c(-60,90)) +
  labs(fill = "January temperature") +
  ggtitle("a) Example plot")+
  theme(legend.position = "left") +
  xlab("") +
  ylab("")

tiff(filename="~/yourRoute.tif",
     width=2100, height = 1000, res= 200)
map_temp
dev.off()
