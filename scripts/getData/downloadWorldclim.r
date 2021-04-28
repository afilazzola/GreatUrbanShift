### Download all of worldClim Rasters
library(raster)
library(rgdal)


### List all model combinations
timeframe <- c("2041-2060","2081-2100")
GCM <- c("BCC-CSM2-MR", "MRI-ESM2-0","CNRM-CM6-1","CNRM-ESM2-1","CanESM5", "IPSL-CM6A-LR","MIROC-ES2L","MIROC6")
SSP <- c("ssp126", "ssp370","ssp585")
allModels <- expand.grid(timeframe=timeframe, GCM=GCM,SSP=SSP, stringsAsFactors = F)

## list filepaths

lapply(1:nrow(allModels), function(i){
downloadPath <- paste0("https://biogeo.ucdavis.edu/data/worldclim/v2.1/fut/2.5m/wc2.1_2.5m_bioc_",
                       allModels[i,"GCM"],"_", ## GCM
                       allModels[i,"SSP"],"_", ## SSP
                       allModels[i,"timeframe"],".zip")
destinationPath <-  paste0("~//scratch//futureClimate//",
                           allModels[i,"GCM"],"_", ## GCM
                           allModels[i,"SSP"],"_", ## SSP
                           allModels[i,"timeframe"],".zip")

download.file(url = downloadPath, destfile = destinationPath)
unzipPath <- gsub(".zip", "",destinationPath)
unzip(destinationPath, exdir=unzipPath)
})


#### average across the different GCMs

## List all downloaded rasters
bioclim <- list.files("~//scratch//futureClimate//", full.names = T, pattern=".tif", recursive = T)

## specify range of conditions to extract
selectedModels <- expand.grid(timeframe=timeframe, SSP=SSP, stringsAsFactors = F)
  
lapply(1:8, function(i){
modelIter <- paste0("(",selectedModels[i,"timeframe"],")(?:.+)(",selectedModels[i,"SSP"],")")

## Select all filepaths to 8 GCMs  
allGCMs <- bioclim[grep(modelIter, bioclim)]

## Load in all GCMs
GCMStack <- lapply(allGCMs, stack)

ensembleList <- lapply(1:19, function(j){
   mean(GCMStack[[1]][[j]],GCMStack[[2]][[j]])
})
ensembleModel <- do.call(stack, ensembleList)
names(ensembleModel) <- paste0("bio",1:19)

writeRaster(ensembleModel, paste0("~//scratch//futureClimate//",selectedModels[i,"timeframe"],"_",selectedModels[i,"SSP"],".tif"))
})



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
allFutureClimate <- list.files("data//futureClimate//", recursive = T, full.names = T)
RCPs <- c("ssp126","ssp370","ssp585")
window <- c("2041-2060","2081-2100")
allCombinations <- expand.grid(RCP=RCPs, Year=window, stringsAsFactors = F)


allScenarios <- lapply(1:6, function(j)  {
  
  ## Load climate raster 
  rasterPaths <- paste0("(?=.*",allCombinations[j,1],")(?=.*",allCombinations[j,2],")(?=.*)")
  futureClimate <- allFutureClimate[grep(rasterPaths, allFutureClimate, perl=T)]
  futureRaster <- stack(futureClimate) ## load raster
  names(futureRaster) <- paste0("bio",1:19) ## rename variables
  futureRaster <- futureRaster[[c("bio1","bio3","bio4","bio5","bio6","bio12","bio13","bio14","bio15")]]
  futureRaster <- futureRaster %>% crop(., NApoly) %>% mask(., NApoly) ## crop to study area
  
  ## Extract city climate
  cityClimate <- extract(futureRaster, cityPoints, df=T)
  cityClimate <- cbind(cityClimate, City = as.character(cityPoints@data[,1])) ## join city name
  cityClimate <- cityClimate[!is.na(cityClimate[,2]),] ## drop missing values
  cityClimate[,"SSP"] <- allCombinations[j,1]
  cityClimate[,"Year"] <- allCombinations[j,2]
  cityClimate
})
  
futureClimate <- do.call(rbind, allScenarios)
futureClimate <- futureClimate %>% dplyr::select(-ID)

write.csv(futureClimate, "data//futureClimate//futureClimate.csv", row.names=FALSE)