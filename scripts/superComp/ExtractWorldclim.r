### Create a csv file for all possible future climate conditions
library(raster)
library(rgdal)
library(dismo)

## Set WD
setwd("~/projects/def-sapna/afila/GreatUrbanShift")

set.seed(11)

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
allClimate <- list.files("~/scratch/worldclim30/", pattern = ".tif", recursive = T, full.names = T)
GCMSSP <- gsub("_2081-2100.*", "", gsub(".*bioc_", "", allClimate))
climateType <- ifelse(grepl("bioc", basename(allClimate)), "futureClimate","currentClimate")
gcms <- ifelse(grepl("bioc", basename(allClimate)), gsub("_.*", "", GCMSSP), "current")
ssps <- ifelse(grepl("bioc", basename(allClimate)), gsub(".*_", "", GCMSSP), "current")


climateFileDF <- data.frame(filepaths = allClimate,
    climateType, gcms, ssps)

futureScenarios <- lapply(1:nrow(futureDF), function(j)  {
  
## Load and extract climate rasters
tempStack <- stack(futureDF[j, "filepaths"])
names(tempStack) <- paste0("bio", 1:19)
cityObs <- data.frame(cityPoints)
climateOut<- extract(tempStack, cityPoints)
cityClimate <- cbind(cityObs, climateOut)

## labels
cityClimate[,"SSP"] <- futureDF[j, "ssps"]
cityClimate[,"GCM"] <- futureDF[j, "gcms"]
cityClimate[,"Climate"] <- futureDF[j, "climateType"]
cityClimate
})

futureScenariosExtracted <- do.call(rbind, futureScenarios)

## Current climate
currentDF <- subset(climateFileDF, climateType == "currentClimate")
currentStack <- stack(currentDF[, "filepaths"])
names(currentStack) <- gsub("wc2.1_30s_", "", names(currentStack))
names(currentStack) <- gsub("_", "", names(currentStack) )

cityObs <- data.frame(cityPoints)
climateOut<- extract(currentStack, cityPoints)
CurrentCityClimate <- cbind(cityObs, climateOut)

## labels
CurrentCityClimate[,"SSP"] <- "currentClimate"
CurrentCityClimate[,"GCM"] <- "currentClimate"
CurrentCityClimate[,"Climate"] <- "currentClimate"


### Split up climate into respective CSVs
library(dplyr)
library(tidyr)


write.csv(CurrentCityClimate, "data//currentClimate.csv")
  
futureClimate <- futureScenariosExtracted %>% 
  group_by(City, x1, x2, optional, SSP, Climate) %>% 
  summarize_all(.funs = mean) %>%  
  data.frame() %>% 
  write.csv(. , "data//futureClimate.csv")

futureGCMClimate <- futureScenariosExtracted %>% 
  write.csv(. , "data//futureGCMClimate.csv")
