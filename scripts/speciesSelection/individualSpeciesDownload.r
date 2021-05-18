
## Libraries
library(tidyverse)
library(raster)
library(rgdal)
library(doParallel)
library(rgbif)

## Set wd
setwd("~/projects/def-sapna/afila/GreatUrbanShift")

## Load in master species list
masterList <- read.csv("data//cityData//UpdatedSpeciesList.csv", stringsAsFactors = F)

## Load species that have already been downloaded
currentDL <- list.files("data//speciesOcc//")
currentDL <- currentDL %>% gsub("_", " ", . ) %>% gsub(".csv", "", . ) 
masterList <- masterList %>% filter(!species %in% currentDL)

### bounding box polygon for North America
NApolyBB <- "POLYGON((-160 0,-160 65,-50 65,-50 0,-160 0))"


### Look up species that are missing
acceptedSpecies <- list.files("data//speciesOcc2//") %>% gsub(".csv", "", .) ## from bulk download
missingSpecies <- masterList[!masterList$species %in% acceptedSpecies,]


### Download remaining species on at a time
for(i in 1:length(missingSpecies$taxonKeys)) {
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
