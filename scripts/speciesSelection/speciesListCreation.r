### Find most common species for cities in Canada and the US


## Libraries
library(tidyverse)
library(raster)
library(rgdal)
library(doParallel)
library(rgbif)
## Set wd
setwd("~/projects/def-sapna/afila/GreatUrbanShift")

## Useful functions to ID species
source("scripts//functions.r")

## City list
cities <- read.csv("data//CityList.csv")

# ## Create WKT polygon 20x20 km
# 
# for(i in 1:nrow(cities)){
# GTApoly <- makePoly( centroid.lon= cities[i,2], centroid.lat = cities[i,3], buffer=20) ## 2 km quadrat
# wktPoly <- wicket::sp_convert(GTApoly)
# cities[i,"WKTpoly"] <- wktPoly
# }
# 
# write.csv(cities, "data//Citylist.csv", row.names=FALSE)

## specify number of cores available
cl <- makeCluster(8, type="FORK")
clusterEvalQ(cl, { library(MASS); RNGkind("L'Ecuyer-CMRG") })
clusterExport(cl, varlist=list("cities"),
              envir = environment())
registerDoParallel(cl)



## Find most frequently observed species
foreach(i = 17:nrow(cities), .combine=c,  .packages=c("tidyverse","raster","rgdal","rgbif"), 
        .errorhandling = "remove") %dopar% {
          siteSpecies <- occ_search( hasCoordinate=T,  
                                    start=1, 
                                    limit=100000, ## set  limit for downloading records
                                    fields = c("scientificName","acceptedScientificName","decimalLatitude","decimalLongitude","eventDate","verbatimEventDate","year","month","day","coordinateUncertaintyInMeters","genus","species","kingdom","datasetKey","class","phylum"),
                                    year= c("2000,2020"), ## Extract recent observations only
                                    geometry=cities[i,"WKTpoly"],
                                    hasGeospatialIssue=FALSE) ## specify for area around GTA - WKT polygon counter clockwise
          
          
          siteDF <- siteSpecies$data %>% group_by(kingdom, phylum, class, species) %>% 
            mutate(obsLocation = paste0(decimalLongitude, "-",decimalLatitude)) %>% 
            summarize(nobs = length(unique(obsLocation)))
          topSpecies <- siteDF %>% filter(nobs>10) %>%  filter(!is.na(species)) %>%  data.frame()
          topSpecies[,"city"] <- cities[i,1]
          
          write.csv(topSpecies, paste0("data//SiteLists//",cities[i,"CityName"], "SpeciesLists.csv"), row.names=FALSE)
        }


### Generate species list

cityList <- list.files("data//cityData//speciesLists//", full.names=T)

allcities <- lapply(1:length(cityList), function(i){
  read.csv(cityList[i])
})
          
allcities <- do.call(rbind, allcities)

## drop duplicates
allcities <- allcities[!duplicated(allcities$species),]

allAnimals <- allcities %>% 
          filter(kingdom == "Animalia") %>% ## drop plants, fungi, and bacteria
         filter(!species %in% c("Apis mellifera","Felis catus","Sus scrofa","Gallus gallus")) ## drop domesticated animals
                   
write.csv(allAnimals, "data//cityData//masterSpeciesList.csv", row.names=FALSE)          
                   