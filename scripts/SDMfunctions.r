#### SDM functions
## A list of functions to facilitate processing SDM 

### Set raster grid for observations
MakeEmptyGridResolution <- function(rasterIn, aggFactor){
r1 <- terra::rast(nrows = nrow(rasterIn)/aggFactor,
    ncols =  ncol(rasterIn)/aggFactor,
    ext = terra::ext(rasterIn),
    crs = crs(rasterIn))
values(r1) <- 0
return(r1)

}

### Thin observations
ThinByGrid <- function(occurrences, raster){
    tempRaster <- raster
    tempRaster[!is.na(tempRaster)] <- 0

    thinnedPoints <- gridSample(occurrences, tempRaster, n=1)  %>% data.frame()
    names(thinnedPoints)[1:2] <- c("decimalLongitude","decimalLatitude")
    coordinates(thinnedPoints) <- ~decimalLongitude + decimalLatitude ## Transform occurrences to spdataframe
    proj4string(thinnedPoints) <- crs(occurrences)
    return(thinnedPoints)
}

### Extract predictors with coordinates into DF
ExtractWithCoordinates <- function(x, rasterstack){
coords <- terra::vect(x)
variables <- terra::extract(rasterstack, coords, xy = T)
return(data.frame(variables))
}


### find co-linear variables
FindCollinearVariables <- function(occurrences, absences, climate){

## Pull climate data out of coordinates
pres <- ExtractWithCoordinates(occurrences, climateRasters)
pres <- pres %>% dplyr::select(-ID, longitude = x, latitude = y)
abs <- ExtractWithCoordinates(absences, climateRasters)
abs <- abs %>% dplyr::select(-ID, longitude = x, latitude = y)
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




###### Legacy functions

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



## Standard error
se <- function(x) {
    sd(x, na.rm = T) / sqrt(length(x[!is.na(x)]))
}

### Pull residuals from Maxent model
GetMaxEntResiduals <- function(occ, abs, model){
allpoints <- rbind(occ, abs)
allpoints[,"presence"] <- c(rep(1, nrow(occ)),rep(0, nrow(abs)))
allpoints <- allpoints[!is.na(rowSums(allpoints)),]
allpoints[,"predictedOccurrence"] <- predict(model, allpoints, type = "logistic")
allpoints[,"residuals"] <- (allpoints$presence - allpoints$predictedOccurrence) / se(allpoints$predictedOccurrence)
return(allpoints)
}


### Conduct Moran's I on residuals
library(spdep, quietly = T)

GetSubsampledMoranI <- function(x, niter){
## Sumsample large DF to increase runtime
if(nrow(maxentResiduals) > 100000){
    residualDF <- dplyr::sample_n(maxentResiduals, 100000)
} else {
    residualDF <- maxentResiduals
}
residualDFSimplified <- residualDF %>% 
    dplyr::select(longitude, latitude, residuals) %>% 
    distinct(longitude, latitude, .keep_all = T)
coords <- coordinates(residualDFSimplified[,c("longitude","latitude")])

distances  <-  dnearneigh(coords, 5000, 100000) 
ResidualWeights <- nb2listw(distances, style="B")

moranOut <- moran.mc(residualDFSimplified$residuals, 
    listw = ResidualWeights, 
    nsim = niter)

moranOutDF <- data.frame(
    MoranObs = moranOut$statistic,
    MoransPval = moranOut$p.value
)
return(moranOutDF)
}


