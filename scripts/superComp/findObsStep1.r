## Load functions and libraries
library(foreach)
library(tidyverse)

### Set working directory
setwd("~/projects/def-sapna/afila/UrbanSensitivity")
source("scripts//functions.r")


## Load all study sites
# sites <- read.csv("data//StudySites.csv")
# 
# for(i in 1:nrow(sites)){
# GTApoly <- makePoly( centroid.lon= sites[i,5], centroid.lat = sites[i,4], buffer=50) ## 50 km quadrat
# wktPoly <- wicket::sp_convert(GTApoly)
# sites[i,"WKTpoly"] <- wktPoly
# }
# 
# write.csv(sites, "data//StudySitesWKT.csv", row.names=FALSE)

## Get WKT polygons for study areas
sites <- read.csv("data//StudySitesWKT.csv", stringsAsFactors = F)

foreach(i = 1:nrow(sites), .combine=c,  .packages=c("tidyverse","raster","rgdal","rgbif","foreach"), 
        .errorhandling = "remove") %do% {
          siteSpecies <- occ_search(taxonKey = 1,  hasCoordinate=T, country=c("CA;US"),  ## select plants, in Canada, with coordinates
                                    start=1, 
                                    limit=100000, ## set  limit for downloading records
                                    fields = c("scientificName","acceptedScientificName","decimalLatitude","decimalLongitude","eventDate","verbatimEventDate","year","month","day","coordinateUncertaintyInMeters","genus","species","kingdom","datasetKey"),
                                    year= c("2000,2020"), ## Extract recent observations only
                                    geometry=sites[i,"WKTpoly"],
                                    hasGeospatialIssue=FALSE) ## specify for area around GTA - WKT polygon counter clockwise
          
          siteDF <- siteSpecies$data %>% group_by(species) %>% 
            mutate(obsLocation = paste0(decimalLongitude, "-",decimalLatitude)) %>% 
            summarize(nobs = length(unique(obsLocation)))
          topSpecies <- siteDF %>% filter(nobs>10) %>%  top_n(100) %>% filter(!is.na(species)) %>%  data.frame()
          topSpecies[,"city"] <- sites[i,1]
          
          write.csv(topSpecies, paste0("data//SDMdata//SiteLists//",sites[i,"Location"], "SpeciesLists.csv"), row.names=FALSE)
        }
