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
masterList <- read.csv("data//cityData//masterSpeciesList.csv", stringsAsFactors = F)

### bounding box polygon for North America
# NApolyBB <- "POLYGON((-160 0,-160 65,-50 65,-50 0,-160 0))"
NApolyBB <- "POLYGON((-160 0,-50 0, -50 65,-160 65,-160 0))" ## they expect counter-clockwise

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
                          fields = c("scientificName","acceptedScientificName","decimalLatitude","decimalLongitude","eventDate","verbatimEventDate","year","month","day","genus","species","kingdom","datasetKey"),
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



### Species needed to be manually downloaded 
# masterList <- data.frame(species = c("Cardinalis cardinalis","Vireo olivaceus","Setophaga coronata","Agelaius phoeniceus",
#   "Sturnus vulgaris","Spizella passerina","Turdus migratorius","Zenaida macroura","Passer domesticus",
#   "Junco hyemalis","Corvus brachyrhynchos","Cyanocitta cristata","Spinus tristis","Branta canadensis","Picoides pubescens",
#   "Poecile atricapillus","Quiscalus quiscula","Thryothorus ludovicianus","Baeolophus bicolor","Pieris rapae","Vespula vulgaris","Cicindela formosa"))


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
})
masterList["taxonKeys"] <- do.call(c, listOut) ## combine into one list of taxon keys
masterList <- masterList %>% filter(!is.na(taxonKeys)) ## drop species without GBIF matches

### Save taxon species list
write.csv(masterList, "data//cityData//masterSpeciesList.csv", row.names=F)


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


#### iterate through multiple downloads

iterStart <- seq(1, nrow(masterList), by=200)
iterEnd <- c(seq(200, nrow(masterList), by=200), nrow(masterList))

gbifKeyList <- lapply(10:12, function(i) {
speciesSelect <- seq(iterStart[i], iterEnd[i], by=1)
subsetList <- masterList[speciesSelect,]


res <- occ_download(
  pred_in("taxonKey", subsetList[,"taxonKeys"] ),
  pred("hasCoordinate", TRUE),
  pred("hasGeospatialIssue", FALSE),
  pred_in("year", 2000:2020),
  pred_within(NApolyBB),
  format = "SIMPLE_CSV",
  user = "afilazzola", 
  pwd = "LATyKeMxGUXnqY7",
  email = "alex.filazzola@outlook.com"
)
})

## Download from GBIF
lapply(1:3, function(j) {
occ_download_get(gbifKeyList[[j]], "data//scratch",  overwrite = T)
})
  
## Unzip files
zipFiles <- list.files("data//scratch", full.names = T)
lapply(1:3,  function(i) {
  unzip(zipFiles[i], exdir ="data//scratch")
})


### Look up species that are missing
missing <- read.csv("data//FixSpecies.csv")
missingSpecies <- masterList[masterList$species %in% missing$species,]

currentProcessed <- list.files("data//scratch//speciesRemaining//")
if(length(currentProcessed) > 0){  ## if downloaded species exist, skip those species
  currentProcessed <- currentProcessed %>% basename() %>%  gsub(".csv", "", . )  ## create a list of species already processed
  missingSpecies <- missingSpecies %>% filter(!(species %in% currentProcessed)) ## drop those species
} else { ## if no species exist, just use the regular species list
  missingSpecies <- missingSpecies 
}



iterStart <- c(1,101,201)
iterEnd <- c(100,200,304)

gbifKeyList <- lapply(1:3, function(i) {
  speciesSelect <- seq(iterStart[i], iterEnd[i], by=1)
  subsetList <- missingSpecies[speciesSelect,]
  
  
  res <- occ_download(
    pred_in("taxonKey", subsetList[,"taxonKeys"] ),
    pred("hasCoordinate", TRUE),
    pred("hasGeospatialIssue", FALSE),
    pred_in("year", 2000:2020),
    pred_within(NApolyBB),
    format = "SIMPLE_CSV",
    user = "afilazzola", 
    pwd = "LATyKeMxGUXnqY7",
    email = "alex.filazzola@outlook.com"
  )
})



### Download remaining species one at a time
for(i in 1:length(missingSpecies$taxonKeys)) {
  tryCatch({ ## catch species where download fails
sampleSpp <- occ_search(taxonKey = missingSpecies[i,"taxonKeys"],  hasCoordinate=T,   ## select plants, in Canada, with coordinates
                        fields = c("scientificName","acceptedScientificName","decimalLatitude","decimalLongitude","eventDate","verbatimEventDate","year","month","day","genus","species","kingdom","datasetKey"),
                        year= c("2000,2020"), limit=100000,  ## Extract recent observations only
                        geometry=NApolyBB,
                        hasGeospatialIssue=FALSE) ## specify for area around GTA - WKT polygon counter clockwise
sampleSpp <- data.frame(sampleSpp$data) ## convert to DF
write.csv(sampleSpp, paste0("data//scratch//speciesRemaining//",missingSpecies[i,"species"],".csv"), row.names=FALSE)
print(paste0("Progress ", round(i / nrow(missingSpecies),3)*100," %"))}, error=function(e) print(paste0(missingSpecies[i,"species"], " Failed")))
}

## Update species list
masterListUpdated <- masterList
masterListUpdated <- masterListUpdated %>% dplyr::select(-kingdom, -phylum, -class) ## drop old phylogeny columns
masterListUpdated <- masterListUpdated %>% filter(! taxonKeys %in% missingSpecies$taxonKeys) ## drop species unable to download

for(i in 1:nrow(masterList)) {
speciesInfo <- name_usage(key=masterList[i,"taxonKeys"])$data %>% data.frame()
### Check to make sure all columns are present
speciesInfo <- speciesInfo %>% mutate(class = ifelse("class" %in% names(.), class, NA),order = ifelse("order" %in% names(.), order, NA))
masterListUpdated[i,c("Kingdom","Phylum","Class","Order","Family","Genus")] <- speciesInfo[,c("kingdom","phylum","class","order","family","genus")]
}


write.csv(masterListUpdated, "data//cityData//UpdatedSpeciesList.csv", row.names = F)




