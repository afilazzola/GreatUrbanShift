### Analyze results

## Libraries
library(tidyverse)
library(ggridges)


## Look at the model results
models <- list.files("out//models//", full.names = T)

listModels <- lapply(1:length(models), function(i)
  read.csv(models[i], stringsAsFactors = F)
)
allModels <- do.call(plyr::rbind.fill, listModels)



### Model checks to ensure none are poor qualifiers
hist(allModels$AUCtrain)
hist(allModels$AUCtest)
bestModels <- allModels %>% filter(AUCtest > 0.5) %>% filter(nobs > 9) ## Drop models where AUC < 0.7
ggplot(bestModels, aes(x=AUCtest, y=AUCtrain)) + geom_point()

## Correct missing taxa in Arachnids and filter out aquatic species
bestModels$Class <- ifelse(bestModels$Order == "Araneae" |  ## Add arachnida Class to grouping where missing
             bestModels$Order == "Ixodida" | 
             bestModels$Order == "Opiliones" |
             bestModels$Order == "Scorpiones" |
             bestModels$Order == "Trombidiformes", "Arachnida",bestModels$Class)
bestModels$Class <- ifelse(bestModels$Family == "Entomobryidae" | bestModels$Family == "Orchesellidae","Entognatha",bestModels$Class)
terrestrial1 <- bestModels %>%  
  filter(Phylum %in% c("Arthropoda","Chordata")) %>% 
  filter(!(Class %in% c("Actinopterygii","Ascidiacea","Elasmobranchii","Merostomata","Malacostraca","Maxillopoda"))) %>% 
  filter(!(Family %in% c("Otariidae","Delphinidae","Trichechidae","Phocidae","Balaenopteridae"))) %>% 
  filter(species != "Enhydra lutris")
terrestrial2 <- bestModels %>%  
  filter(Class %in% c("Gastropoda", "Clitellata")) %>% 
  filter(Order %in% c("Stylommatophora","Systellommatophora","Cycloneritida","Crassiclitellata")) %>% 
  filter(!(Family %in% c("Neritopsidae","Titiscaniidae","Neritidae","Phenacolepadidae")))
terrestrial <- rbind(terrestrial1, terrestrial2)

### Total number of observations
sum(terrestrial$nobs)/1000000 ## total number of observations
median(terrestrial$nobs) ## average number of observations per species
sd(terrestrial$nobs)/sqrt(nrow(terrestrial)) ## Error
min(terrestrial$nobs)
max(terrestrial$nobs)

## Look at predictions for cities
allClimates <- list.files("out//cityPredict//", full.names = T, pattern="futureClimate")

listFuture <- lapply(1:length(allClimates), function(i)
  read.csv(allClimates[i], stringsAsFactors = F)
)
allFuture <- do.call(rbind, listFuture)
allFuture <- allFuture %>% 
  group_by(City, SSP, Climate, species) %>% 
  summarize(meanProb = mean(meanProb, na.rm = T),
    sdProb = mean(sdProb, na.rm = T))

currentclimate <- list.files("out//cityPredict//", full.names = T, pattern="currentClimate")

listCurrent <- lapply(1:length(currentclimate), function(i)
  read.csv(currentclimate[i], stringsAsFactors = F)
)
allCurrent <- do.call(rbind, listCurrent)



## Combine future and current climate data frames
allClimate <- allCurrent %>% 
    dplyr::select(City, species, currentProb = meanProb) %>%
    left_join(allFuture) %>% 
    rename(futureProb = meanProb) %>% 
    filter(!is.na(SSP)) %>% 
    filter(!(currentProb == 0  & futureProb == 0 )) %>% 
    mutate(speciesChange = ifelse(currentProb > 0 & futureProb == 0 , -1, 
        ifelse(currentProb == 0 & futureProb > 0, 1, 0 ))) 

####### Plot map of general patterns
## Join city characteristics
cityStats <- read.csv("data//cityData//CityCharacteristics.csv") %>% 
  dplyr::select(City = CityName, ecozone = NA_L1NAME, bio1:bio15, lon, lat)


cityChange <- allClimate %>%
    left_join(cityStats) %>% 
    group_by(City, lat, lon, SSP) %>% 
    summarize(meanChange = mean(speciesChange )) 


### Patterns of differences among cities
ggplot(cityChange, aes(x=reorder(City,meanChange), y=meanChange, color=SSP)) + 
geom_point(size=2)  + 
  coord_flip() + theme_classic() + ylab("Change in predicted occurrence across all species") + xlab("") +
   scale_color_manual(values=c(RColorBrewer::brewer.pal(n=3, "Dark2"))) +
  geom_hline(yintercept=0, lty=2) + ylim(-0.8, 0.8)



##### Taxa plot
## Differences in taxa
taxaInfo <- terrestrial %>% distinct(Phylum, Class, Order, Family, species)
taxaClimate <- merge(allClimate, taxaInfo, by="species" )
# write.csv(taxaClimate, "out//allSpeciesData.csv", row.names=FALSE)


taxaClimateSimplified <- taxaClimate %>% 
    filter(SSP == "ssp585") %>% 
    group_by(Class, Order, Family, species) %>% 
    summarize(meanCityChange = mean(speciesChange))
taxaClimateSimplified <- taxaClimateSimplified %>% group_by(Order) %>% 
mutate(medianOrder = median(meanCityChange, na.rm=T), nOrder=length(meanCityChange)) %>% 
  ungroup()  %>%  group_by(Class) %>% mutate(medianClass = median(meanCityChange, na.rm=T), nClass=length(meanCityChange)) 

## Simple phylum plot
taxaPlot <- ggplot(taxaClimateSimplified,
      aes(y=Class, x= meanCityChange, fill=Class)) + 
  geom_density_ridges2(scale = 0.8, rel_min_height = 0.05) +
  theme_classic() + xlab("Change in number of cities (%)") +
  geom_hline(yintercept=0, lty=2) + xlim(-1,1 ) +
  scale_fill_manual(values=c(RColorBrewer::brewer.pal(n=12, "Paired"), "black")) +
  geom_vline(xintercept = 0, lty=2)  + theme(text = element_text(size=24), legend.position = "none")
taxaPlot
ggsave("Figure4.pdf", taxaPlot)



## N per grouping
currentSpecies <- taxaClimate %>%
    filter(currentProb > 0) %>% 
    filter(SSP == "ssp585") %>% 
    group_by(Class) %>% summarize(nSppCurrent = length(unique(species)))
futureSpecies <- taxaClimate %>%
    filter(futureProb > 0) %>% 
    filter(SSP == "ssp585") %>% 
    group_by(Class) %>% summarize(nSppCurrent = length(unique(species)))




### Extremes among taxa
extremeFamilies <- taxaClimateSimplified %>% 
  filter(Class %in% c("Reptilia","Mammalia","Amphibia", "Gastropoda")) %>% 
  group_by(Class, Family) %>% 
  summarize(taxaChange = mean(meanCityChange), nSpp = length(unique(species)))  %>% 
  filter(nSpp > 1) %>% 
  slice(which(taxaChange == max(taxaChange) | taxaChange == min(taxaChange) |  nSpp == max(nSpp))) 

extremeOrders <- taxaClimateSimplified %>% 
  filter(!(Class %in% c("Reptilia","Mammalia","Amphibia"))) %>% 
  group_by(Class, Order) %>% 
  summarize(taxaChange = round(mean(meanCityChange),3), nSpp = length(unique(species)))  %>% 
  filter(nSpp > 1) %>% 
  slice(which(taxaChange == max(taxaChange) | taxaChange == min(taxaChange) |  nSpp == max(nSpp))) 

taxaClimateSimplified %>% 
  filter(!(Class %in% c("Reptilia","Mammalia","Amphibia"))) %>% 
  group_by(Class, Order) %>% 
  summarize(taxaChange = round(mean(meanCityChange),3), nSpp = length(unique(species))) %>% 
  filter(Order == "Hymenoptera")
