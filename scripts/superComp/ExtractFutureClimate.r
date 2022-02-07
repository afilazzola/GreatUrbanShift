### Create a csv file for all possible future climate conditions
library(raster)
library(rgdal)
library(dismo)

## Set WD
setwd("~/projects/def-sapna/afila/GreatUrbanShift")

### Create a CSV with all the future climates
## Load cities to examine and add buffer
cities <- read.csv("data//CityList.csv")
coordinates(cities) <- ~lon + lat
proj4string(cities) <- CRS("+proj=longlat +datum=WGS84")

##  Load functions functions
sampleAround <- function(point, nsamples){
  cityBuffer <- circles(point, d=10000, lonlat=T, dissolve=F) ## 10 km radius (20 km buffer)
  cityBuffer <- cityBuffer@polygons
  crs(cityBuffer) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
  samplePoints <- spsample(cityBuffer, nsamples, sp=T, type="stratified")
  return(samplePoints)
}


## Sample points around city in buffer
allPoints <- lapply(1:nrow(cities), function(i)  {
  createdPoints <- sampleAround(cities[i,], 100)
  createdPoints <- SpatialPointsDataFrame(createdPoints, data=data.frame(City = rep(cities@data[i,1], length(createdPoints))))
}
)
cityPoints <- do.call(rbind, allPoints)

## Load NA polygon
NApoly <- readOGR("data//CanUSA.shp")


## List future climates
allClimate <- list.files("~/scratch/climateNA/", pattern = ".tif", recursive = T, full.names = T)
climateType <- gsub("/.*", "", gsub(".*climateNA//", "", allClimate))
gcmSSP <- gsub("_bioclim.zip", "", basename(allClimate))
gcms <- gsub("_.*", "", gcmSSP)
ssps <- gsub("^[^_]*_", "", gcmSSP)

climateFileDF <- data.frame(filepaths = allClimate,
    climateType, gcms, ssps)


allScenarios <- lapply(1:nrow(allClimate), function(j)  {
  
## Load and extract climate rasters
tempStack <- stack(climateFileDF[j, "filepaths"])
extractClimate <- extract(tempStack, cityPoints)

## labels
extractClimate[,"SSP"] <- climateFileDF[j, "ssps"]
extractClimate[,"GCM"] <- climateFileDF[j, "gcms"]
extractClimate[,"Climate"] <- climateFileDF[j, "climateType"]
extractClimate
})

allClimateExtracted <- do.call(rbind, allScenarios)