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

## List future climates
allClimate <- list.files("~/scratch/climateNA/", pattern = ".tif", recursive = T, full.names = T)
climateType <- gsub("/.*", "", gsub(".*climateNA//", "", allClimate))
gcmSSP <- gsub(".tif", "", basename(allClimate))
gcms <- gsub("_.*", "", gcmSSP)
ssps <- gsub("_.*", "", gsub("^[^_]*_", "", gcmSSP))
variableName <- gsub(".*2071_|.*2020_", "", gcmSSP)

climateFileDF <- data.frame(filepaths = allClimate,
    climateType, gcms, ssps, variables = variableName)

## Load one raster to transform climate data
crsSettingRaster <- raster(climateFileDF[1,"filepaths"])
cityPoints <- spTransform(cityPoints, crs(crsSettingRaster))

allScenarios <- lapply(1:nrow(climateFileDF), function(j)  {
  
## Load and extract climate rasters
tempStack <- raster(climateFileDF[j, "filepaths"])
cityObs <- data.frame(cityPoints)
cityObs[,"climateValue"] <- extract(tempStack, cityPoints)

## labels
cityObs[,"SSP"] <- climateFileDF[j, "ssps"]
cityObs[,"GCM"] <- climateFileDF[j, "gcms"]
cityObs[,"Climate"] <- climateFileDF[j, "climateType"]
cityObs[,"variable"] <- climateFileDF[j, "variables"]
cityObs
})

allClimateExtracted <- do.call(rbind, allScenarios)


### Split up climate into respective CSVs
library(dplyr)
library(tidyr)


currentClimate <- allClimateExtracted %>% 
  filter(Climate == "currentClimate") %>% 
  spread(variable, climateValue) %>% 
  write.csv(. , "data//currentClimate.csv")
  
futureClimate <- allClimateExtracted %>% 
  filter(Climate == "futureClimate") %>% 
  spread(variable, climateValue) %>% 
  write.csv(. , "data//futureClimate.csv")

futureGCMClimate <- allClimateExtracted %>% 
  filter(Climate == "futureGCMs") %>% 
  spread(variable, climateValue) %>% 
  write.csv(. , "data//futureGCMClimate.csv")
