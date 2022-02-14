#### SDM functions
## A list of functions to facilitate processing SDM 

### Thin observations
ThinByGrid <- function(occurrences, raster){
    tempRaster <- raster
    tempRaster[!is.na(tempRaster)] <- 0

    thinnedPoints <- gridSample(occurrences, tempRaster, n=1)  %>% data.frame()
    coordinates(thinnedPoints) <- ~decimalLongitude + decimalLatitude ## Transform occurrences to spdataframe
    proj4string(thinnedPoints) <- crs(occurrences)
    return(thinnedPoints)
}


### Function to use Kfold partioninig into 10 groups
kfoldPartitionData <- function(occurrences, absences){
fold.p <- kfold(occurrences, k=10)
occtest.p <- occurrences[fold.p == 1, ]
occtrain.p <- occurrences[fold.p != 1, ]
fold.a <- kfold(absences, k=10)
occtest.a <- absences[fold.a == 1, ]
occtrain.a <- absences[fold.a != 1, ]

returnList <- list(occtest.p, occtrain.p, occtest.a, occtrain.a)

names(returnList) <- c("testingPresence","trainingPresence","testingAbsence","trainingAbsence")
return(returnList)
}

### Extract predictors with coordinates into DF
ExtractWithCoordinates <- function(x, rasterstack){
coords <- data.frame(coordinates(x))
variables <- raster::extract(rasterstack, x)
return(cbind(coords,variables))
}


### find co-linear variables
FindCollinearVariables <- function(occurrences, absences, climate){

## Pull climate data out of coordinates
pres <- ExtractWithCoordinates(sp1, climateRasters)
names(pres)[1:2] <- c("longitude","latitude")
abs <- ExtractWithCoordinates(backgr, climateRasters)
names(abs)[1:2] <- c("longitude","latitude")
allClim <- rbind(pres, abs) %>% 
    dplyr::select(-longitude, -latitude) %>% 
    data.frame()

## Check for covariance
colin <- usdm::vifcor(allClim[,-ncol(allClim)])
selectVars <-  colin@results$Variables

## Drop collinear variables
pres <- pres %>% dplyr::select(all_of(c("longitude","latitude",selectVars)))
abs <- abs %>% dplyr::select(all_of(c("longitude","latitude",selectVars)))

return(list(presClim = pres, absClim = abs, selectVars = selectVars))
}

### Pull residuals from Maxent model
GetMaxEntResiduals <- function(occ, abs, model){
allpoints <- rbind(occ, abs)
allpoints[,"presence"] <- c(rep(1, nrow(occ)),rep(0, nrow(abs)))
allpoints <- allpoints[!is.na(rowSums(allpoints)),]
allpoints[,"predictedOccurrence"] <- predict(model, allpoints, type = "logistic")
allpoints[,"residuals"] <- allpoints$presence - allpoints$predictedOccurrence
return(allpoints)
}



### Get Moran's I value
require(ape)
GetSubsampledMoranI <- function(x, niter){
subSample <- dplyr::sample_n(x, niter)
residualMatrix <- subSample[,c("longitude","latitude")]
residualValues <- subSample$residuals
outDist <- as.matrix(dist(residualMatrix))

moranOut <- ape::Moran.I(residualValues, outDist, 
          alternative = "two.sided", scaled = T)
moranOutDF <- data.frame(moranOut)
names(moranOutDF) <- c("MoransObs","MoransExp","MoransSD","MoransPval")

return(data.frame(moranOutDF))
}
