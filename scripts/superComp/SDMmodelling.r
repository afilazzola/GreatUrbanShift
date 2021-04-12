###### SDM modelling

## Libraries
library(tidyverse)
library(raster)
library(ncdf4)
library(rgdal)
library(randomForest)
library(dismo)
library(rgeos)
library(foreach)
require(usdm)
require(MuMIn)
require(adehabitatHR)
library(doParallel)
library(spThin)
library(rJava)
options(java.parameters = "- Xmx1024m")

## Set WD
setwd("D:/RStudio/UrbanClimateSensitivity/troubleshoot")

##  Load functions functions
sampleAround <- function(point, nsamples){
cityBuffer <- circles(point, d=25000, lonlat=F, dissolve=F) ## 25 km radius (50 km buffer)
cityBuffer <- cityBuffer@polygons
crs(cityBuffer) <- CRS("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +datum=WGS84")
samplePoints <- spsample(cityBuffer, nsamples, sp=T, type="stratified")
return(samplePoints)
}

## Load Climate
climateFiles <- list.files("data//climate//", full.names = T)
climateRasters <- stack(climateFiles) ## Drop MAR with missing values
crs(climateRasters) <- CRS("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +datum=WGS84")

## Load NA polygon
NApoly <- readOGR("data//EasternNA.shp")
NApoly <- spTransform(NApoly, CRS("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +datum=WGS84"))

## Load species
speciesFiles <- list.files("data//speciesOcc", full.names = T)
# speciesFiles <- list.files("data//gbifOcc", full.names = T)

## List future climates
allFutureClimate <- list.files("data//FutureClimate//", recursive = T, full.names = T)
GCMs <- c("GFDL","MPI","ENSEMBLE")
RCPs <- c("rcp45","rcp85")
window <- c("2055","2085")
allCombinations <- expand.grid(GCM=GCMs, RCP=RCPs, Year=window, stringsAsFactors = F)

## Load cities to examine and add buffer
cities <- read.csv("data//StudySites.csv")
coordinates(cities) <- ~lon + lat
proj4string(cities) <- CRS("+proj=longlat +datum=WGS84")
cities <- spTransform(cities, CRS("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +datum=WGS84"))
## Sample points around city in buffer
allPoints <- lapply(1:10, function(i)  {
  createdPoints <- sampleAround(cities[i,], 100)
  createdPoints <- SpatialPointsDataFrame(createdPoints, data=data.frame(City = rep(cities@data[i,1], length(createdPoints))))
}
)
cityPoints <- do.call(rbind, allPoints)

## Load bias file
bias <- raster("data//biasFile.tif")

## Set up cluster
## specify number of cores available
cl <- makeCluster(30, type="FORK")
clusterEvalQ(cl, { library(MASS); RNGkind("L'Ecuyer-CMRG") })
clusterExport(cl, varlist=list("cityPoints","allCombinations","allFutureClimate","speciesFiles","NApoly","climateRasters"),
              envir = environment())
registerDoParallel(cl)

### Need multiple runs to improve Efficacy (n = 10)
## https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/j.2041-210X.2011.00172.x

iterN <- foreach(i = 1:length(speciesFiles),  .combine=c,  .packages=c("tidyverse","raster","randomForest","dismo","usdm","MuMIn","doParallel","foreach","spThin","adehabitatHR","ncdf4","rgdal","rgeos"), .errorhandling = "remove") %dopar% {

  
##### Spatial points processing  
          
## Load occurrences
spLoad <- read.csv(speciesFiles[i])

## Thin occurrences using spthin
## https://onlinelibrary.wiley.com/doi/10.1111/ecog.01132 spThin package
## systematic sampling as the most effect form of bias correct https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0097122
speciesOcc <- thin(spLoad, lat.col="decimalLatitude", long.col = "decimalLongitude", spec.col="species",
                   thin.par = 10, reps=1, write.files=FALSE, locs.thinned.list.return=T) %>% data.frame() 


## Transform occurrences to spdataframe
coordinates(speciesOcc) <- ~Longitude + Latitude
proj4string(speciesOcc) <- CRS("+proj=longlat +datum=WGS84")
sp1 <- spTransform(speciesOcc,  CRS("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +datum=WGS84"))

## list species name
speciesName <- basename(speciesFiles[i]) %>% gsub(".csv", "", .) %>% gsub(" ", "_", .)

## Generate sample area for background points
samplearea <- mcp(sp1, percent=95)
crs(samplearea) <- crs(sp1)

## generate background points based on biased observations
## https://github.com/jamiemkass/ENMeval/issues/26
biasCut <- mask(bias, samplearea) ## mask bias area to species extent
biasValues <- values(biasCut)
biasValues[is.na(biasValues)] <- 0
backgr <- xyFromCell(biasCut, sample(ncell(biasCut), nrow(data.frame(speciesOcc))*10, prob=biasValues))


## withhold 20% for sample testing
fold.p <- kfold(sp1, k=5)
occtest.p <-sp1[fold.p == 4, ]
occtrain.p <- sp1[fold.p != 4, ]
fold.a <- kfold(backgr, k=5)
occtest.a <- backgr[fold.a == 4, ]
occtrain.a <- backgr[fold.a != 4, ]


###### Variable selection

## Extract climate data
pres <- extract(climateRasters, occtrain.p)
abs <- extract(climateRasters, occtrain.a)
allClim <- rbind(pres, abs) %>% data.frame()
allClim[,"occurrence"] <- c(rep(1, nrow(pres)),rep(0, nrow(abs)))
allClim <- allClim %>% filter(!is.na(allClim$AHM)) ## drop NAs

## Check for covariance
colin <- usdm::vifcor(allClim[,-ncol(allClim)])
dropVars <- paste0(colin@excluded, collapse = "|")
selectVars <-  allClim[grep(dropVars, names(allClim), invert = T, value=T)]

## Best subsets of variables
m1 <- glm(occurrence ~ . , data=selectVars, na.action="na.fail", family="binomial")
dredgeOut <- MuMIn::dredge(m1, rank="AICc")


## select top model
bestModel <- get.models(dredgeOut ,1 )
bestVars <- names(bestModel[[1]]$coefficients)
bestVars <- bestVars[-1] ## drop intercept

###### Conduct species distribution modelling

## Use best variables
bestClim <- climateRasters[[bestVars]]


## Select bias weighting for random forest

max1 <- ENMeval::ENMevaluate(data.frame(occtrain.p), bestClim, bg.coords = data.frame(occtrain.a),
                    fc = c("L", "Q", "P", "LQ", "H", "HQ", "T", "QPH", "QPHT", "LQHP"), RMvalues=seq(0.5, 2, 0.5),
                    method="randomkfold", kfolds=10, 
                    parallel=TRUE, numCores = 6,
                    algorithm='maxent.jar')

## best model
bestMax <- which(max1@results$AICc==min(max1@results$AICc))[1]
modelOut <- max1@results[bestMax,]
predOut <-  raster::predict(bestClim, max1@models[[bestMax]], progress="text", type="logistic")

## evaluate
erf <- evaluate(occtest.p, occtest.a, max1@models[[1]], bestClim)


## predict area
EasternClim <- crop(bestClim, NApoly)
sppmap <- raster::predict(EasternClim, max1)
sppmap2 <- raster::predict(EasternClim, rf2)
sppmap <- mask(sppmap, samplearea) ## trim by surveyed area

## Extract current city climate
CurrentcityClimate <- extract(bestClim, cityPoints, df=T)
CurrentcityClimate <- cbind(CurrentcityClimate, City = as.character(cityPoints@data[,1])) ## join city name
CurrentcityClimate <- CurrentcityClimate[!is.na(CurrentcityClimate[,2]),] ## drop missing values


## predict species
CurrentcityClimate[,"speciesOcc"] <- predict(rf1, CurrentcityClimate)
citySummary <- CurrentcityClimate %>% group_by(City) %>% summarize(meanProb = mean(speciesOcc, na.rm=T),
                                                                            sdProb = sd(speciesOcc, na.rm=T)) %>% data.frame()
citySummary[,"GCM"] <- "currentClimate"
citySummary[,"RCP"] <- "currentClimate"
citySummary[,"Year"] <- "currentClimate"
citySummary[,"species"] <- speciesName %>% gsub("_", " ", .)

write.csv(citySummary, paste0("out//modelOut//CurrentClimate",speciesName,".csv"), row.names=FALSE)

## create output dataframe
outdata <- data.frame(varcontribute)
outdata[,"AUC"] <- erf@auc ## AUC value
outdata[,"np"] <- erf@np ## number of presence points used for evaluation
outdata[,"na"] <- erf@na ## number of absence points used for evaluation
outdata[,"cor"] <- erf@cor ## correlation between test data and model
outdata[,"TPR"] <- mean(erf@TPR) ## True positive rate
outdata[,"TNR"] <- mean(erf@TNR) ## True negative rate
outdata[,"R2avg"] <- mean(rf1$rsq) ## average r2
outdata[,"SelectedVars"] <- paste(names(preds), collapse='; ')
outdata[,"species"] <- speciesName %>% gsub("_", " ", .)

## save files
write.csv(outdata, paste0("out//modelOut//Model",speciesName,".csv"), row.names=FALSE)

## write raster
writeRaster(sppmap,  paste0("out//speciesDistro//",speciesName,".tif"), overwrite=T)

## predict future climate
selectedVariables <- names(preds)

lapply(c(1,7), function(j)  {

      ## Load climate raster 
      rasterPaths <- paste0("(?=.*",allCombinations[j,1],")(?=.*",allCombinations[j,2],")(?=.*",allCombinations[j,3],")")
      futureClimate <- allFutureClimate[grep(rasterPaths, allFutureClimate, perl=T)]
      formatSelectVars <- paste0(selectedVariables, collapse="|") ## convert selected vars to regex
      futureRaster <- stack(futureClimate[grep(formatSelectVars, futureClimate)])
      crs(futureRaster) <- crs(cityPoints)
      
      ## Extract city climate
      cityClimate <- extract(futureRaster, cityPoints, df=T)
      cityClimate <- cbind(cityClimate, City = as.character(cityPoints@data[,1])) ## join city name
      cityClimate <- cityClimate[!is.na(cityClimate[,2]),] ## drop missing values
      cityClimate[,"GCM"] <- allCombinations[j,1] 
      cityClimate[,"RCP"] <- allCombinations[j,2]
      cityClimate[,"Year"] <- allCombinations[j,3]
      
      ## predict species
      cityClimate[,"speciesOcc"] <- predict(rf1, cityClimate)
      citySummary <- cityClimate %>% group_by(City, GCM, RCP, Year) %>% summarize(meanProb = mean(speciesOcc, na.rm=T),
                                                                                  sdProb = sd(speciesOcc, na.rm=T)) %>% data.frame()
      citySummary[,"species"] <- speciesName %>% gsub("_", " ", .)
      write.csv(citySummary, paste0("out//modelOut//FutureClimate",speciesName,".csv"), row.names=FALSE)
      
        })
print(i)
}

iterN

stopCluster(cl)

