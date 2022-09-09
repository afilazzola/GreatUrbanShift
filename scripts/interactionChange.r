### Analyze results

## Libraries
library(tidyverse)
library(ggridges)
library(MASS)
library(emmeans)


## Look at the model results
models <- list.files("out//models//", full.names = T)

listModels <- lapply(1:length(models), function(i) {
  read.csv(models[i], stringsAsFactors = F)
})
allModels <- do.call(plyr::rbind.fill, listModels)



### Model checks to ensure none are poor qualifiers
hist(allModels$AUCtrain)
hist(allModels$AUCtest)
bestModels <- allModels %>%
  filter(AUCtest > 0.5) %>%
  filter(nobs > 9) ## Drop models where AUC < 0.7
ggplot(bestModels, aes(x = AUCtest, y = AUCtrain)) +
  geom_point()

## Correct missing taxa in Arachnids and filter out aquatic species
bestModels$Class <- ifelse(bestModels$Order == "Araneae" | ## Add arachnida Class to grouping where missing
  bestModels$Order == "Ixodida" |
  bestModels$Order == "Opiliones" |
  bestModels$Order == "Scorpiones" |
  bestModels$Order == "Trombidiformes", "Arachnida", bestModels$Class)
bestModels$Class <- ifelse(bestModels$Family == "Entomobryidae" | bestModels$Family == "Orchesellidae", "Entognatha", bestModels$Class)
terrestrial1 <- bestModels %>%
  filter(Phylum %in% c("Arthropoda", "Chordata")) %>%
  filter(!(Class %in% c("Actinopterygii", "Ascidiacea", "Elasmobranchii", "Merostomata", "Malacostraca", "Maxillopoda"))) %>%
  filter(!(Family %in% c("Otariidae", "Delphinidae", "Trichechidae", "Phocidae", "Balaenopteridae"))) %>%
  filter(species != "Enhydra lutris")
terrestrial2 <- bestModels %>%
  filter(Class %in% c("Gastropoda", "Clitellata")) %>%
  filter(Order %in% c("Stylommatophora", "Systellommatophora", "Cycloneritida", "Crassiclitellata")) %>%
  filter(!(Family %in% c("Neritopsidae", "Titiscaniidae", "Neritidae", "Phenacolepadidae")))
terrestrial <- rbind(terrestrial1, terrestrial2)

### Total number of observations
sum(terrestrial$nobs) / 1000000 ## total number of observations
median(terrestrial$nobs) ## average number of observations per species
sd(terrestrial$nobs) / sqrt(nrow(terrestrial)) ## Error
min(terrestrial$nobs)
max(terrestrial$nobs)
length(unique(terrestrial$species))

speciesList <- terrestrial[!duplicated(terrestrial$species), ]


head(terrestrial)


## Look at predictions for cities
allClimates <- list.files("out//cityPredict//", full.names = T, pattern = "futureClimate")

listFuture <- lapply(1:length(allClimates), function(i) {
  read.csv(allClimates[i], stringsAsFactors = F)
})
allFuture <- do.call(rbind, listFuture)
allFuture <- allFuture %>%
  group_by(City, SSP, Climate, species) %>%
  summarize(
    meanProb = mean(meanProb, na.rm = T),
    sdProb = mean(sdProb, na.rm = T)
  )

currentclimate <- list.files("out//cityPredict//", full.names = T, pattern = "currentClimate")

listCurrent <- lapply(1:length(currentclimate), function(i) {
  read.csv(currentclimate[i], stringsAsFactors = F)
})
allCurrent <- do.call(rbind, listCurrent)



## Combine future and current climate data frames
allClimate <- allCurrent %>%
  dplyr::select(City, species, currentProb = meanProb) %>%
  left_join(allFuture) %>%
  rename(futureProb = meanProb) %>%
  filter(!is.na(SSP)) %>%
  filter(species %in% speciesList$species) %>%
  filter(!(currentProb == 0 & futureProb == 0)) %>%
  mutate(
    currentOcc = ifelse(currentProb > 0, 1, 0),
    futureOcc = ifelse(futureProb > 0, 1, 0),
    loss = ifelse(currentOcc == 1 & futureOcc == 0, 1, 0),
    gain = ifelse(currentOcc == 0 & futureOcc == 1, 1, 0),
    noChange = ifelse(currentOcc == 1 & futureOcc == 1, 1, 0),
    speciesChange = futureOcc - currentOcc
  )

write.csv(allClimate, "testingInteractions.csv", row.names = FALSE)

allClimate <- read.csv("testingInteractions.csv")

## find the number of co-occurring species in each city

library(foreach)
library(doParallel)
library(tidyverse)
registerDoParallel(cores = 6)

allCityCooccur <- foreach(i = unique(allClimate$City), .combine = rbind, .packages=c("tidyverse")) %dopar% {
  currentSpeciesOverlap <- allClimate %>%
    filter(currentOcc == 1) %>%
    dplyr::select(City, species, currentOcc, SSP) %>%
    filter(SSP == "ssp585") %>%
    filter(City == i)

  coOcc <- crossprod(table(currentSpeciesOverlap[1:2]))
  coOccDF <- data.frame(coOcc)

  coOccDFLong <- coOccDF %>%
    rownames_to_column("species") %>%
    gather(speciesB, coOccCities, -species) %>%
    mutate(City = i)
  coOccDFLong
}

registerDoParallel(cores = 6)
futureCityCooccur <- foreach(i = unique(allClimate$City), .combine = rbind, .packages=c("tidyverse","foreach")) %do% {
  foreach(j = unique(allClimate$SSP), .combine = rbind, .packages=c("tidyverse")) %dopar% { 
  currentSpeciesOverlap <- allClimate %>%
    filter(futureOcc == 1) %>%
    dplyr::select(City, species, currentOcc, SSP) %>%
    filter(SSP == j) %>%
    filter(City == i)

  coOcc <- crossprod(table(currentSpeciesOverlap[1:2]))
  coOccDF <- data.frame(coOcc)

  coOccDFLong <- coOccDF %>%
    rownames_to_column("species") %>%
    gather(speciesB, coOccCities, -species) %>%
    mutate(City = i, SSP = j)
  coOccDFLong
}
}

futureCityCooccur %>% group_by(City, SSP) %>% summarize(length(unique(species)))

futureCityCooccur <- futureCityCooccur %>% rename(futureCoOcc = coOccCities) 


allInteractions <- foreach(i = unique(allClimate$SSP), .combine= rbind) %do% {

futureCityCooccurSSP <- futureCityCooccur  %>% filter(SSP == i)

allCoOccur <- allCityCooccur %>%
  full_join(futureCityCooccurSSP)
allCoOccur[,"coOccCities"] <- ifelse(is.na(allCoOccur$coOccCities), 0, 1)
allCoOccur[,"futureCoOcc"] <- ifelse(is.na(allCoOccur$futureCoOcc), 0, 1)

cityPatterns <- allCoOccur  %>% 
  mutate(interaction = futureCoOcc - coOccCities) %>%
  group_by(City) %>% 
  summarize(newInteraction = sum(interaction == 1, na.rm = T)/length(unique(species)), 
            lostInteraction = sum(interaction == -1, na.rm = T)/length(unique(species))) %>% 
  mutate(SSP = i)
  cityPatterns
}


write.csv(allInteractions, "data//AllSpeciesInteractions.csv", row.names = FALSE)
