## Download records for all species

## Libraries
library(tidyverse)
library(raster)
library(rgdal)
library(doParallel)
library(rgbif)

## Set wd
setwd("~/projects/def-sapna/afila/GreatUrbanShift")

## Load in master species list
masterList <- read.csv("data//cityData//masterSpeciesList.csv")

### bounding box polygon for North America
NApolyBB <- "POLYGON((-160 0,-160 65,-50 65,-50 0,-160 0))"

## Load species that have already been downloaded
currentDL <- list.files("data//speciesOcc//")
currentDL <- currentDL %>% gsub("_", " ", . ) %>% gsub(".csv", "", . ) 
masterList <- masterList %>% filter(!species %in% currentDL)

## Load function to download species
getGBIFcsv <- function(species){
  
  taxonKeys <- species %>% 
    taxize::get_gbifid_(method="backbone")  %>% 
    imap(~ .x %>% mutate(original_sciname = .y)) %>% 
    bind_rows() %>% # combine all data.frames into one
    filter(matchtype == "EXACT" & status == "ACCEPTED") %>% 
    pull(usagekey) # get the gbif taxonkeys
  
  
  
  sampleSpp <- occ_search(taxonKey = taxonKeys,  hasCoordinate=T,   ## select plants, in Canada, with coordinates
                          fields = c("scientificName","acceptedScientificName","decimalLatitude","decimalLongitude","eventDate","verbatimEventDate","year","month","day","coordinateUncertaintyInMeters","genus","species","kingdom","datasetKey"),
                          year= c("2000,2020"), limit=100000,  ## Extract recent observations only
                          geometry=NApolyBB,
                          hasGeospatialIssue=FALSE) ## specify for area around GTA - WKT polygon counter clockwise
  return(sampleSpp)
}

 
## specify number of cores available
cl <- makeCluster(3, type="PSOCK", outfile="")
clusterEvalQ(cl, { library(MASS); RNGkind("L'Ecuyer-CMRG") })
clusterExport(cl, varlist=list("masterList", "getGBIFcsv","NApolyBB"),
               envir = environment())
registerDoParallel(cl)



geographyPatterns <- foreach(j = 1:nrow(masterList), .combine=c,  .packages=c("tidyverse","raster","rgdal","rgbif","taxize"), .errorhandling = "remove") %dopar% {
                               
                               ### Download species list
                               sampleSpp <- getGBIFcsv(masterList[j,"species"])
                               sampleSpp <- data.frame(sampleSpp$data)
                               speciesName <- gsub(" ", "_", masterList[j,"species"])
                               write.csv(sampleSpp, paste0("data//speciesOcc//",speciesName,".csv"), row.names=FALSE)
                               print(j)
}





#### Create Download request directly from GBIF
listOut <- lapply(1:nrow(masterList), function(i) {
  tryCatch({
masterList[i,"taxonKeys"] <- as.character(masterList$species[i]) %>% 
  taxize::get_gbifid_(method="backbone")  %>% 
  imap(~ .x %>% mutate(original_sciname = .y)) %>% 
  bind_rows() %>% # combine all data.frames into one
  filter(matchtype == "EXACT" & status == "ACCEPTED") %>% 
  pull(usagekey) # get the gbif taxonkeys
} , error=function(e) NA)
}) ; beepr::beep(sound=6)
masterList["taxonKeys"] <- do.call(c, listOut) ## combine into one list of taxon keys
masterList <- masterList %>% filter(!is.na(taxonKeys)) ## drop species without GBIF matches

res <- occ_download(
pred_in("taxonKey", masterList[,"taxonKeys"] ),
pred("hasCoordinate", TRUE),
pred("hasGeospatialIssue", FALSE),
pred_in("year", 2000:2020),
pred_within(NApolyBB),
format = "SIMPLE_CSV",
user = "afilazzola", 
pwd = "LATyKeMxGUXnqY7",
email = "alex.filazzola@outlook.com"
)

occ_download_meta(res)
z <- occ_download_get(res)
df <- occ_download_import(z)