### Cut species causing the errors


## Libraries
library(dplyr)
library(tidyr)
library(raster)
library(rgdal)
library(rgeos)
library(foreach)
library(doParallel)

## Set WD
setwd("~/projects/def-sapna/afila/GreatUrbanShift")

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
currentProcessed <- list.files("out//models//")
if(length(currentProcessed) > 0){  ## if downloaded species exist, skip those species
  currentProcessed <- currentProcessed %>% gsub("Model", "", . ) %>%  gsub("_", " ", . ) %>% paste0(., collapse="|") ## create a list of species already processed
  speciesFiles <- grep(currentProcessed,speciesFiles, value=T, invert=T) ## drop those species
} else { ## if no species exist, just use the regular species list
  speciesFiles <- speciesFiles 
}

## Load Master species list
sppList <- read.csv("data//cityData//UpdatedSpeciesList.csv")

### Bias corrections
# bias <- raster("data//biasFile.tif") ## Load bias file
gridThinning <- climateRasters[[1]]
gridThinning[!is.na(gridThinning)] <- 0

### Set up cluster
cl <- makeCluster(15, type="FORK")
clusterEvalQ(cl, { library(MASS); RNGkind("L'Ecuyer-CMRG") })
clusterExport(cl, varlist=list("sppList","speciesFiles","gridThinning"),
              envir = environment())
registerDoParallel(cl)

revisedSppList <- foreach(i = 1:length(speciesFiles), .errorhandling = "remove", .combine=c , .packages=c("dplyr","tidyr","raster","dismo","rgdal","rgeos")) %dopar% {

## Load occurrences
spLoad <- read.csv(speciesFiles[i], stringsAsFactors = F)

## Get species info
speciesInfo <- sppList[sppList$species %in% unique(spLoad$species),] %>% dplyr::select(-city) %>% data.frame()

## Assign spatial coordinates
coordinates(spLoad) <- ~decimalLongitude + decimalLatitude ## Transform occurrences to spdataframe
proj4string(spLoad) <- CRS("+proj=longlat +datum=WGS84")

## Thin occurrences using observations per grid cell
## systematic sampling as the most effect form of bias correct https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0097122
sp1 <- dismo::gridSample(spLoad, gridThinning, n=1)  %>% data.frame()
coordinates(sp1) <- ~decimalLongitude + decimalLatitude ## Transform occurrences to spdataframe
proj4string(sp1) <- CRS("+proj=longlat +datum=WGS84")

## Omit species with too few observations
speciesOut <- ifelse(length(sp1)<10, NA, speciesFiles[i])
speciesOut
}


write.csv(data.frame(RevisedSpecies = revisedSppList), "data//SpeciesListErrorFree.csv", row.names=FALSE)