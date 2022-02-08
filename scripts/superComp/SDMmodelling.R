###### SDM modelling

## Libraries
library(dplyr)
library(tidyr)
library(raster)
library(rgdal)
library(dismo)
library(rgeos)
library(rJava)
library(rangeModelMetadata)

## Set WD
setwd("~/projects/def-sapna/afila/GreatUrbanShift")

## Load NA polygon
NApoly <- readOGR("data//CanUSA.shp")

## Load Climate
climateFiles <- list.files("data//climateNA//", full.names = T)
climateRasters <- stack(climateFiles)  
names(climateRasters) <- gsub("Normal_1991_2020_", "", names(climateRasters) )
climateRasters <- climateRasters %>% projectRaster(., crs = crs(NApoly)) %>% mask(., NApoly) 

## Load species
speciesFilepath <- commandArgs(trailingOnly = TRUE)

## Load Master species list
sppList <- read.csv("data//cityData//UpdatedSpeciesList.csv")

## load in current and future climate
CurrentcityClimate <- read.csv("data//currentCityClimate.csv")
futureClimate <- read.csv("data//futureClimate.csv")
GCMclimate <- read.csv("data//futureGCMClimate.csv")

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
speciesInfo <- sppList[sppList$species %in% unique(sp1$species),] %>% dplyr::select(-city) %>% data.frame()
speciesName <- basename(speciesFilepath) %>% gsub(".csv", "", .) ## list species name


## Assign spatial coordinates
coordinates(sp1) <- ~decimalLongitude + decimalLatitude ## Transform occurrences to spdataframe
proj4string(sp1) <- CRS("+proj=longlat +datum=WGS84")

## Generate sample area for background points
samplearea <- raster::buffer(sp1, width=100000, dissolve=T) ## 100 km buffer
crs(samplearea) <- crs(sp1)

## determine the number of background points
nbackgr <- ifelse(nrow(data.frame(sp1)) > 9999, nrow(data.frame(sp1)) ,10000) ## 10k unless occurrences are more than 10k

####### Unable to use bias file method - restrict background instead
## Select points
samplearea <- mask(climateRasters[[1]], samplearea)
backgr <- randomPoints(samplearea, n=nbackgr, p =sp1)  %>% data.frame() ## sample points, exclude cells where presence occurs
coordinates(backgr) <- ~x+y ## re-assign as spatial points
proj4string(backgr)  <- crs(sp1) ## assign CRS


### Conduct random spatial black CV https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.13107
## withhold 20% for sample testing
get.checkerboard1(occs = sp1, envs = climateRasters, bg = backgr, aggregation.factor=5)

fold.p <- kfold(sp1, k=5)
occtest.p <-sp1[fold.p == 4, ]
occtrain.p <- sp1[fold.p != 4, ]
fold.a <- kfold(backgr, k=5)
occtest.a <- backgr[fold.a == 4, ]
occtrain.a <- backgr[fold.a != 4, ]


###### Reduce co-linearity between models

## Extract climate data
pres <- extract(climateRasters, occtrain.p)
abs <- extract(climateRasters, occtrain.a)
allClim <- rbind(pres, abs) %>% data.frame()


## Check for covariance
colin <- usdm::vifcor(allClim[,-ncol(allClim)])
selectVars <-  colin@results$Variables


###### Conduct species distribution modelling

## Use best variables without colinearity issues
bestClim <- climateRasters[[selectVars]]


## Select feature classes and regularization parameters for Maxent
tuneArgs <- list(fc = c("L", "Q", "P", "LQ","H","LQH","LQHP"), 
                  rm = seq(0.5, 3, 0.5))

## Specify training dataset
trainDF <- data.frame(occtrain.p)
names(trainDF) <- c("x","y")

### Run MaxEnt
max1 <- ENMeval::ENMevaluate(occ=trainDF, envs = bestClim, bg = data.frame(occtrain.a),
                    tune.args = tuneArgs, 
                    taxon.name=speciesName, ## add species name
                    progbar=F, 
                    partitions = 'none',  ## fully withheld for better model optimization Soley-Guardia et al. 2019 https://jamiemkass.github.io/ENMeval/articles/ENMeval-2.0.0-vignette.html#user
                    quiet=T, ## silence messages but not errors
                    doClamp=F, ## remove clamping criteria which is not necessary unless extrapolating
                    algorithm='maxent.jar')

## best model
bestMax <- which(max1@results$delta.AICc==0)[1]
modelOut <- max1@results[bestMax,]
varImp <- max1@variable.importance[bestMax] %>% data.frame()
names(varImp) <- c("variable","percent.contribution","permutation.importance")

## evaluate
erf <- evaluate(occtest.p, occtest.a, max1@models[[bestMax]], bestClim)

# ## Compare against null model
# modNull <- ENMeval::ENMnulls(max1, 
#                              mod.settings = list(fc = as.character(modelOut[1,"fc"]), rm =  as.numeric(modelOut[1,"rm"])), 
#                              no.iter = 100)
# nullOut <- ENMeval::null.emp.results(modNull)


## predict species
CurrentcityClimate[,"speciesOcc"] <- predict(max1@models[[bestMax]], CurrentcityClimate,  type="logistic")
citySummary <- CurrentcityClimate %>% group_by(City) %>%  ##  average by city
  mutate(SSP = "currentClimate", Year = "currentClimate" ) %>% 
  summarize(meanProb = mean(speciesOcc, na.rm=T), sdProb = sd(speciesOcc, na.rm=T)) %>% data.frame()

citySummary[,"species"] <- speciesName %>% gsub("_", " ", .)

write.csv(citySummary, paste0("out//cityPredict//CurrentClimate",speciesName,".csv"), row.names=FALSE)

## create output dataframe
modelData <- speciesInfo 
modelData[,"fileName"] <- basename(speciesFilepath)
modelData[,"AUCtest"] <- erf@auc ## AUC value
modelData[,"nobs"] <- nrow(spLoad) ## number of presence points used overall
modelData[,"nthin"] <- nrow(sp1) ## number of presence points used after thinned
modelData[,"np"] <- erf@np ## number of presence points used for evaluation
modelData[,"na"] <- erf@na ## number of absence points used for evaluation
modelData[,"cor"] <- erf@cor ## correlation between test data and model
modelData[,"TPR"] <- mean(erf@TPR) ## True positive rate
modelData[,"TNR"] <- mean(erf@TNR) ## True negative rate
modelData[,"importantVars"] <- paste0(varImp$variable, collapse=";")
modelData[,"importantValue"] <- paste0(varImp$permutation.importance, collapse=";")
modelData[,"percentContribution"] <- paste0(varImp$percent.contribution, collapse=";")
modelData[,"Features"] <- as.character(modelOut[1,"fc"])
modelData[,"Regularization"] <- as.character(modelOut[1,"rm"])
modelData[,"AUCtrain"] <- as.character(modelOut[1,"auc.train"])


## save files
write.csv(modelData, paste0("out//models//Model",speciesName,".csv"), row.names=FALSE)


## predict future climate

      ## Load future climate data
      cityClimate <- futureClimate
      
      ## predict species
      cityClimate[,"speciesOcc"] <- predict(max1@models[[bestMax]], cityClimate,  type="logistic")
      citySummary <- cityClimate %>% group_by(City, SSP, Year) %>% summarize(meanProb = mean(speciesOcc, na.rm=T),
                                                                                  sdProb = sd(speciesOcc, na.rm=T)) %>% data.frame()
      citySummary[,"species"] <- speciesName %>% gsub("_", " ", .)
      write.csv(citySummary, paste0("out//cityPredict//FutureClimate",speciesName,".csv"), row.names=FALSE)
      


      
##  Generate SDM meta-data file
## https://onlinelibrary.wiley.com/doi/abs/10.1111/geb.12993
## https://onlinelibrary.wiley.com/doi/full/10.1111/ecog.04960
rmm <-  ENMeval::eval.rmm(max1)
## specify missing parameters
rmm$authorship$rmmName  <- paste0("CUEFilazzolaAlessandro_2021_Maxent",speciesName)
rmm$authorship$license  <- "CC BY-NC"
rmm$data$occurrence$yearMin  <- "2000"
rmm$data$occurrence$yearMax  <- "2021"
rmm$model$selectionRules <- "lowest AICc, break ties with AUC" ## selection criteria for model
rmm$model$finalModelSettings <- paste0(modelOut[1,"features"], modelOut[1,"rm"]) ## Best featureclass and RM
rmm$code$software$platform <- toBibtex(citation())
## save meta-data
rangeModelMetadata::rmmToCSV(rmm, filename=paste0("out//speciesDistro//RMM",speciesName,".csv")) ## export to CSV
      
