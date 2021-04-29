###### SDM modelling

## Libraries
library(tidyverse)
library(raster)
library(ncdf4)
library(rgdal)
library(dismo)
library(rgeos)
library(foreach)
require(adehabitatHR)
library(doParallel)
library(rJava)
options(java.parameters = "-Xmx1024m")

## Set WD
setwd("~/projects/def-sapna/afila/GreatUrbanShift")

##  Load functions functions
sampleAround <- function(point, nsamples){
cityBuffer <- circles(point, d=10000, lonlat=T, dissolve=F) ## 10 km radius (20 km buffer)
cityBuffer <- cityBuffer@polygons
crs(cityBuffer) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
samplePoints <- spsample(cityBuffer, nsamples, sp=T, type="stratified")
return(samplePoints)
}

## Load NA polygon
NApoly <- readOGR("data//CanUSA.shp")

## Load Climate
climateFiles <- list.files("data//climate//", full.names = T)
climateRasters <- stack(climateFiles) ## Drop MAR with missing values
names(climateRasters) <- paste0("bio",1:19)
climateRasters <- climateRasters[[c("bio1","bio3","bio4","bio5","bio6","bio12","bio13","bio14","bio15")]]
climateRasters <- climateRasters %>% crop(., NApoly) %>% mask(., NApoly) ## load sampling grid


## Load species
speciesFiles <- list.files("data//speciesOcc", full.names = T)
## Drop species that have already been processed
currentProcessed <- list.files("out//speciesDistro//")
if(length(currentProcessed) > 0){  ## if downloaded species exist, skip those species
currentProcessed <- currentProcessed %>% gsub(".tif", "", . ) %>% paste0(., collapse="|") ## create a list of species already processed
speciesFiles <- grep(currentProcessed,speciesFiles, value=T, invert=T) ## drop those species
} else { ## if no species exist, just use the regular species list
speciesFiles <- speciesFiles 
}

## load in future climate
futureClimate <- read.csv("data//futureClimate//futureClimate.csv")

## Load cities to examine and add buffer
cities <- read.csv("data//CityList.csv")
coordinates(cities) <- ~lon + lat
proj4string(cities) <- CRS("+proj=longlat +datum=WGS84")

## Sample points around city in buffer
allPoints <- lapply(1:nrow(cities), function(i)  {
  createdPoints <- sampleAround(cities[i,], 100)
  createdPoints <- SpatialPointsDataFrame(createdPoints, data=data.frame(City = rep(cities@data[i,1], length(createdPoints))))
}
)
cityPoints <- do.call(rbind, allPoints)

### Bias corrections
bias <- raster("data//biasFile.tif") ## Load bias file
gridThinning <- climateRasters[[1]]
gridThinning[!is.na(gridThinning)] <- 0

## Set up cluster
## specify number of cores available
cl <- makeCluster(20, type="FORK")
clusterEvalQ(cl, { library(MASS); RNGkind("L'Ecuyer-CMRG") })
clusterExport(cl, varlist=list("cityPoints","futureClimate","speciesFiles","NApoly","climateRasters","gridThinning"),
              envir = environment())
registerDoParallel(cl)

### Need multiple runs to improve Efficacy (n = 10)
## https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/j.2041-210X.2011.00172.x

foreach(i = 1:length(speciesFiles),   .packages=c("rJava","tidyverse","raster","dismo","doParallel","foreach","adehabitatHR","ncdf4","rgdal","rgeos")) %dopar% {

  
##### Spatial points processing  
          
## Load occurrences
spLoad <- read.csv(speciesFiles[i])
coordinates(spLoad) <- ~decimalLongitude + decimalLatitude ## Transform occurrences to spdataframe
proj4string(spLoad) <- CRS("+proj=longlat +datum=WGS84")

## Thin occurrences using observations per grid cell
## systematic sampling as the most effect form of bias correct https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0097122
sp1 <- gridSample(spLoad, gridThinning, n=1)  %>% data.frame()
coordinates(sp1) <- ~decimalLongitude + decimalLatitude ## Transform occurrences to spdataframe
proj4string(sp1) <- CRS("+proj=longlat +datum=WGS84")

## list species name
speciesName <- basename(speciesFiles[i]) %>% gsub(".csv", "", .) %>% gsub(" ", "_", .)

## Generate sample area for background points
samplearea <- mcp(sp1, percent=95)
crs(samplearea) <- crs(sp1)

## determine the number of background points
nbackgr <- ifelse(nrow(data.frame(sp1))*10 > 9999, nrow(data.frame(sp1)) ,10000) ## 10k unless occurrences are more than 10k
 
## generate background points based on biased observations
## https://github.com/jamiemkass/ENMeval/issues/26
biasCut <- mask(bias, samplearea) ## mask bias area to species extent
biasValues <- values(biasCut)
biasValues[is.na(biasValues)] <- 0
backgr <- xyFromCell(biasCut, sample(ncell(biasCut), nbackgr, prob=biasValues))


## withhold 20% for sample testing
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

## Use best variables
bestClim <- climateRasters[[selectVars]]


## Select bias weighting for random forest

max1 <- ENMeval::ENMevaluate(data.frame(occtrain.p), bestClim, bg.coords = data.frame(occtrain.a),
                    fc = c("L", "Q", "P", "LQ", "HQ", "QPH", "QPHT", "LQHP"), RMvalues=seq(0.5, 3, 0.5),
                    method="randomkfold", kfolds=10, progbar=F,
                    algorithm='maxent.jar')

## best model
bestMax <- which(max1@results$AICc==min(max1@results$AICc))[1]
modelOut <- max1@results[bestMax,]
predOut <-  raster::predict(bestClim, max1@models[[bestMax]],  type="logistic")

## evaluate
erf <- evaluate(occtest.p, occtest.a, max1@models[[bestMax]], bestClim)


## Extract current city climate
CurrentcityClimate <- extract(bestClim, cityPoints, df=T)
CurrentcityClimate <- cbind(CurrentcityClimate, City = as.character(cityPoints@data[,1])) ## join city name
CurrentcityClimate <- CurrentcityClimate[!is.na(CurrentcityClimate[,2]),] ## drop missing values


## predict species
CurrentcityClimate[,"speciesOcc"] <- predict(max1@models[[bestMax]], CurrentcityClimate)
citySummary <- CurrentcityClimate %>% group_by(City) %>% dplyr::select(-ID) %>%  ## drop ID column and average by city
  mutate(SSP = "currentClimate", Year = "currentClimate" ) %>% 
  summarize(meanProb = mean(speciesOcc, na.rm=T), sdProb = sd(speciesOcc, na.rm=T)) %>% data.frame()

citySummary[,"species"] <- speciesName %>% gsub("_", " ", .)

write.csv(citySummary, paste0("out//cityPredict//CurrentClimate",speciesName,".csv"), row.names=FALSE)

## create output dataframe
modelData <- data.frame(species =  speciesName %>% gsub("_", " ", .))
modelData[,"AUC"] <- erf@auc ## AUC value
modelData[,"np"] <- erf@np ## number of presence points used for evaluation
modelData[,"na"] <- erf@na ## number of absence points used for evaluation
modelData[,"cor"] <- erf@cor ## correlation between test data and model
modelData[,"TPR"] <- mean(erf@TPR) ## True positive rate
modelData[,"TNR"] <- mean(erf@TNR) ## True negative rate
modelData[,"SelectedVars"] <- paste(names(bestClim), collapse='; ')
modelData[,"Features"] <- as.character(modelOut[1,"features"])
modelData[,"Regularization"] <- as.character(modelOut[1,"rm"])


## save files
write.csv(modelData, paste0("out//models//Model",speciesName,".csv"), row.names=FALSE)

## write raster
writeRaster(predOut,  paste0("out//speciesDistro//",speciesName,".tif"), overwrite=T)

## predict future climate

      ## Load future climate data
      cityClimate <- futureClimate
      
      ## predict species
      cityClimate[,"speciesOcc"] <- predict(max1@models[[bestMax]], cityClimate)
      citySummary <- cityClimate %>% group_by(City, SSP, Year) %>% summarize(meanProb = mean(speciesOcc, na.rm=T),
                                                                                  sdProb = sd(speciesOcc, na.rm=T)) %>% data.frame()
      citySummary[,"species"] <- speciesName %>% gsub("_", " ", .)
      write.csv(citySummary, paste0("out//cityPredict//FutureClimate",speciesName,".csv"), row.names=FALSE)
      


## Memory clean-up to try and solve memory leak
rm(list= c("predOut", "modelData", "citySummary", "max1", "sp1", "spLoad","modelOut"))
gc()

}


