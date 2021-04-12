## Load functions and libraries
library(tidyverse)
library(foreach)

### Set working directory
setwd("~/projects/def-sapna/afila/UrbanSensitivity")
source("scripts//functions.r")


### bounding box polygon for eastern North America
EasterNAwkt <- "POLYGON((-97.2348 24.836,-97.2348 63.637,-52.6592 63.637,-52.6592 24.836,-97.2348 24.836))"

getGBIFcsv <- function(species){
  
  taxonKeys <- species %>% 
    taxize::get_gbifid_(method="backbone")  %>% 
    imap(~ .x %>% mutate(original_sciname = .y)) %>% 
    bind_rows() %>% # combine all data.frames into one
    filter(matchtype == "EXACT" & status == "ACCEPTED") %>% 
    pull(usagekey) # get the gbif taxonkeys
  
  
  
  sampleSpp <- occ_search(taxonKey = taxonKeys,  hasCoordinate=T, country=c("CA;US"),  ## select plants, in Canada, with coordinates
                          fields = c("scientificName","acceptedScientificName","decimalLatitude","decimalLongitude","eventDate","verbatimEventDate","year","month","day","coordinateUncertaintyInMeters","genus","species","kingdom","datasetKey"),
                          year= c("2000,2020"), limit=10000,  ## Extract recent observations only
                          # geometry=EasterNAwkt,
                          hasGeospatialIssue=FALSE) ## specify for area around GTA - WKT polygon counter clockwise
  return(sampleSpp)
}


## specify bounding box of NA
# EasternNA <- readOGR("data//SDMdata//EasternNA.shp") 
# EasternNAbb <- EasternNA %>% bbox()
# studyArea <- as(raster::extent(EasternNAbb[1,1], EasternNAbb[1,2], EasternNAbb[2,1], EasternNAbb[2,2]), "SpatialPolygons")
# proj4string(studyArea) <- crs(EasternNA)
# wktPolyStudy <- wicket::sp_convert(studyArea)

sppFiles <- list.files("data//SDMdata//SiteLists//", full.names = T)
allSpecies <- data.frame()
for(i in 1:length(sppFiles)){
  tempSpecies <- read.csv(sppFiles[i], stringsAsFactors = F)
  allSpecies <- rbind(allSpecies, tempSpecies)
}
uniqueSppDF <- allSpecies[!duplicated(allSpecies$species),]
sppList <- unique(uniqueSppDF$species)


geographyPatterns <- foreach(j = 226:length(sppList), .combine=c,  .packages=c("tidyverse","raster","rgdal","rgbif"), 
                             .errorhandling = "remove") %do% {
                               
                               ### Download species list
                               sampleSpp <- getGBIFcsv(sppList[j])
                               sampleSpp <- data.frame(sampleSpp$data)
                               speciesName <- gsub(" ", "_", sppList[j])
                               write.csv(sampleSpp, paste0("out//speciesOcc//",speciesName,".csv"), row.names=FALSE)
                               print(j)
                             }