
#### download the IUCN stats for all the target species

## Libraries
library(tidyverse)

## load species data
sppList <- read.csv("data//cityData//UpdatedSpeciesList.csv")

## load IUCN status
IUCN <- read.csv("data//IUCN//assessments.csv", stringsAsFactors = F)
IUCNreduced <- IUCN %>% select(assessmentId, internalTaxonId, species = scientificName, redlistCategory, 
                               populationTrend,yearPublished )

## Join status to full species list
sppIUCN <- merge(sppList, IUCNreduced, all.x=T)

write.csv(sppIUCN, "data//IUCNspeciesList.csv", row.names=FALSE)