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
  