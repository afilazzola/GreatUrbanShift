###### SDM modelling

## Libraries
library(dplyr, quietly = T)
library(tidyr, quietly = T)
library(raster, quietly = T)
library(rgdal, quietly = T)
library(dismo, quietly = T)
library(rgeos, quietly = T)
library(rJava, quietly = T)
library(ENMeval, quietly = T)
library(rangeModelMetadata, quietly = T)

## Set WD
setwd("~/projects/def-sapna/afila/GreatUrbanShift")

## Load NA polygon
NApoly <- readOGR("data//CanUSA.shp")

source("scripts//SDMfunctions.r")

## Load Climate
climateFiles <- list.files("data//climateNA//", full.names = T)
climateRasters <- stack(climateFiles)  
names(climateRasters) <- gsub("Normal_1991_2020_", "", names(climateRasters) )
climateRasters <- dropLayer(climateRasters, 
  grep("MAR", names(climateRasters))) ## NA values in raster

## Load species
speciesFilepath <- commandArgs(trailingOnly = TRUE)
# speciesFilepath <- list.files("data//speciesOcc", pattern=".csv", full.names = T)[1]

## Load Master species list
sppList <- read.csv("data//cityData//UpdatedSpeciesList.csv")

## load in current and future climate
currentClimate <- read.csv("data//currentClimate.csv")
futureClimate <- read.csv("data//futureClimate.csv")
GCMclimate <- read.csv("data//futureGCMClimate.csv")
climateList <- list(currentClimate, futureClimate, GCMclimate)
climateList <- lapply(climateList, function(k) { ## Drop NA values
  k %>% 
    dplyr::select(-MAR) %>% 
    drop_na()
})

## Load cities to examine and add buffer
cities <- read.csv("data//CityList.csv")
coordinates(cities) <- ~lon + lat
proj4string(cities) <- CRS("+proj=longlat +datum=WGS84")

##### Spatial points processing  
  
## Load occurrences
sp1 <- read.csv(speciesFilepath, stringsAsFactors = F)
sp1 <- sp1[!is.na(sp1$decimalLongitude),] ## drop NA observations
sp1 <- sp1[!duplicated(sp1[c("decimalLongitude","decimalLatitude")]),] ## drop duplicate coordinates to speed processing
print(basename(speciesFilepath)) ## export species filepath to export file

## Get species info
speciesInfo <- sppList[sppList$species %in% unique(sp1$species),] %>% data.frame()
speciesName <- basename(speciesFilepath) %>% gsub(".csv", "", .) ## list species name


## Assign spatial coordinates
## systematic sampling as the most effect form of bias correct https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0097122
coordinates(sp1) <- ~decimalLongitude + decimalLatitude ## Transform occurrences to spdataframe
proj4string(sp1) <- CRS("+proj=longlat +datum=WGS84")
sp1 <- spTransform(sp1, crs(climateRasters))

## Spatial thinning to reduce bias
emptyThinningGrid <- MakeEmptyGridResolution(climateRasters[[1]],
  aggFactor = 5) ## convert to 5x5 km
sp1 <- ThinByGrid(sp1, emptyThinningGrid)

## Generate sample area for background points
samplearea <- raster::buffer(sp1, width = 100000, dissolve = T) ## 100 km buffer
crs(samplearea) <- crs(sp1)

## determine the number of background points
nbackgr <- ifelse(length(sp1) > 9999, length(sp1), 10000) ## 10k unless occurrences are more than 10k

####### Unable to use bias file method - restrict background points instead
samplearea <- mask(climateRasters[[1]], samplearea)
backgr <- randomPoints(samplearea, n=nbackgr, p =sp1)  %>% data.frame() ## sample points, exclude cells where presence occurs
coordinates(backgr) <- ~x+y ## re-assign as spatial points
proj4string(backgr)  <- crs(sp1) ## assign CRS
backgr <- ThinByGrid(backgr, emptyThinningGrid)

###### Reduce co-linearity between models - select best variables
collinearVariablesClimate <- FindCollinearVariables(occurrences = sp1,
  absences = backgr, climate = climateRasters)
bestClim <- climateRasters[[collinearVariablesClimate$selectVars]] 

###### Conduct species distribution modelling

## Select feature classes and regularization parameters for Maxent
tuneArgs <- list(fc = c("L", "Q", "P", "LQ","H","LQH","LQHP"), 
                  rm = seq(0.5, 3, 0.5))

### Run MaxEnt
max1 <- ENMevaluate(occ=collinearVariablesClimate$presClim,
                    bg.coords = collinearVariablesClimate$absClim,
                    tune.args = tuneArgs, 
                    taxon.name=speciesName, ## add species name
                    progbar=F, 
                    partitions = "block", # Conduct random spatial block CV https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.13107
                    quiet=T, ## silence messages but not errors
                    algorithm='maxent.jar')

## best model
bestMax <- which.max(max1@results$cbi.val.avg)
modelOut <- max1@results[bestMax,]
varImp <- max1@variable.importance[bestMax] %>% data.frame()
names(varImp) <- c("variable","percent.contribution","permutation.importance")

## find threshold
evalOut <- dismo::evaluate(collinearVariablesClimate$presClim[,c("longitude","latitude")], 
  collinearVariablesClimate$absClim[,c("longitude","latitude")],
  max1@models[[bestMax]], bestClim)
thresholdIdentified <- threshold(evalOut)


## predict species occurrences with each climate scenario
predictedCitiesList <- lapply(climateList, function(j) {
  cityValuesSummarized <- j %>% 
    dplyr::select(City, SSP, GCM, Climate) %>% 
    mutate(speciesOcc = predict(max1@models[[bestMax]], j,  type="logistic")) %>% 
    filter(speciesOcc > thresholdIdentified$spec_sens) %>%  ## drop observations above threshold
    group_by(City, SSP, GCM, Climate) %>% 
    summarize(meanProb = mean(speciesOcc, na.rm=T), sdProb = sd(speciesOcc, na.rm=T)) %>% 
    mutate(species = gsub("_", " ", speciesName)) %>% 
    data.frame()  
cityValuesSummarized
})

## Write predicted climates to CSVs
lapply(predictedCitiesList, function(j) {
  write.csv(j, 
  paste0("out//cityPredict//", unique(j$Climate), speciesName,".csv"), 
  row.names=FALSE)
})

## create output dataframe
modelData <- speciesInfo 
modelData[,"fileName"] <- basename(speciesFilepath)
modelData[,"AUCtest"] <- modelOut$auc.val.avg 
modelData[,"AUCdiff"] <- modelOut$auc.diff.avg 
modelData[,"CBItest"] <- modelOut$cbi.val.avg 
modelData[,"threshold"] <- thresholdIdentified$spec_sens
modelData[,"nobs"] <- length(sp1) ## number of presence points used overall
modelData[,"nthin"] <- nrow(max1@occs) ## number of presence points used after thinned
modelData[,"importantVars"] <- paste0(varImp$variable, collapse=";")
modelData[,"importantValue"] <- paste0(varImp$permutation.importance, collapse=";")
modelData[,"percentContribution"] <- paste0(varImp$percent.contribution, collapse=";")
modelData[,"Features"] <- as.character(modelOut[1,"fc"])
modelData[,"Regularization"] <- as.character(modelOut[1,"rm"])
modelData[,"AUCtrain"] <- as.character(modelOut[1,"auc.train"])


## save files
write.csv(modelData, paste0("out//models//Model",speciesName,".csv"), row.names=FALSE)

     
##  Generate SDM meta-data file
## https://onlinelibrary.wiley.com/doi/abs/10.1111/geb.12993
## https://onlinelibrary.wiley.com/doi/full/10.1111/ecog.04960
rmm <-  ENMeval::eval.rmm(max1)
## specify missing parameters
rmm$authorship$rmmName  <- paste0("CUEFilazzolaAlessandro_2022_Maxent",speciesName)
rmm$authorship$license  <- "CC BY-NC"
rmm$data$occurrence$yearMin  <- "2000"
rmm$data$occurrence$yearMax  <- "2021"
rmm$model$selectionRules <- "lowest AICc, break ties with AUC" ## selection criteria for model
rmm$model$finalModelSettings <- paste0(modelOut[1,"features"], modelOut[1,"rm"]) ## Best featureclass and RM
rmm$code$software$platform <- toBibtex(citation())
## save meta-data
rangeModelMetadata::rmmToCSV(rmm, filename=paste0("out//speciesDistro//RMM",speciesName,".csv")) ## export to CSV
      
