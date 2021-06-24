
## Libraries
library(dplyr)
library(tidyr)
library(raster)
library(rgdal)
library(doParallel)
library(rgbif)

## Set wd
setwd("~/projects/def-sapna/afila/GreatUrbanShift")

## Load in master species list
masterList <- read.csv("data//cityData//UpdatedSpeciesList.csv", stringsAsFactors = F)

### bounding box polygon for North America
NApolyBB <- "POLYGON((-160 0,-160 65,-50 65,-50 0,-160 0))"


### Look up species that are missing
missing <- read.csv("data//FixSpecies.csv")
missingSpecies <- masterList[masterList$species %in% missing$species,]

currentProcessed <- list.files("data//speciesOcc//")
if(length(currentProcessed) > 0){  ## if downloaded species exist, skip those species
  currentProcessed <- currentProcessed %>% basename() %>%  gsub(".csv", "", . )  ## create a list of species already processed
  missingSpecies <- missingSpecies %>% filter(!(species %in% currentProcessed)) ## drop those species
} else { ## if no species exist, just use the regular species list
  missingSpecies <- missingSpecies 
}




### Download remaining species on at a time
for(i in length(missingSpecies$taxonKeys):1) {
  tryCatch({ ## catch species where download fails
sampleSpp <- occ_search(taxonKey = missingSpecies[i,"taxonKeys"],  hasCoordinate=T,   ## select plants, in Canada, with coordinates
                        fields = c("scientificName","acceptedScientificName","decimalLatitude","decimalLongitude","eventDate","verbatimEventDate","year","month","day","genus","species","kingdom","datasetKey"),
                        year= c("2000,2020"), limit=100000,  ## Extract recent observations only
                        geometry=NApolyBB,
                        hasGeospatialIssue=FALSE) ## specify for area around GTA - WKT polygon counter clockwise
sampleSpp <- data.frame(sampleSpp$data) ## convert to DF
write.csv(sampleSpp, paste0("data//speciesOcc//",missingSpecies[i,"species"],".csv"), row.names=FALSE)
print(paste0("Progress ", round(i / nrow(missingSpecies),3)*100," %"))}, error=function(e) print(paste0(missingSpecies[i,"species"], " Failed")))
}
