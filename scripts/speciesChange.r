### Analyze results

## Libraries
library(tidyverse)
library(ggridges)
library(MASS)
library(emmeans)


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
length(unique(terrestrial$species))

speciesList <- terrestrial[!duplicated(terrestrial$species),]
write.csv(speciesList, "speciesListSuppelemental.csv.", row.names=FALSE)

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
    filter(species %in% speciesList$species) %>% 
    filter(!(currentProb == 0  & futureProb == 0 )) %>% 
    mutate(currentOcc = ifelse(currentProb > 0 , 1 , 0), 
      futureOcc = ifelse(futureProb > 0 , 1 , 0),
      loss = ifelse(currentOcc == 1 & futureOcc == 0 , 1, 0),
      gain = ifelse(currentOcc == 0 & futureOcc == 1 , 1, 0),
      noChange = ifelse(currentOcc == 1 & futureOcc == 1, 1, 0),
      speciesChange =  futureOcc - currentOcc)

####### Plot map of general patterns
## Join city characteristics
cityStats <- read.csv("data//cityData//CityCharacteristics.csv") %>% 
  dplyr::select(City = CityName, ecozone = NA_L1NAME, bio1:bio15, lon, lat)


### General plot of study sites
mp <- NULL
mapWorld <- borders("world", colour="black", fill="gray90") # create a layer of borders
mp <- ggplot() + theme_classic()+  mapWorld + xlim(-180,-30) + ylim(-20, 90)

mp <- mp+ geom_point(data=cityStats , aes(x=lon, y=lat) , fill="Grey60",  size=3, pch=21) +
 ylab("Latitude") + xlab("Longitude")
pdf("map.pdf", useDingbats = F)
mp
dev.off() 

cityChange <- allClimate %>%
    left_join(cityStats) %>% 
    group_by(City, lat, lon, SSP) %>% 
    summarize(gains = sum(gain), losses = sum(loss),
     noChanges = sum(noChange), currentSpp = sum(currentOcc)) %>% 
    mutate(perChange = (gains - losses) / currentSpp)

longCity <- cityChange %>% 
  mutate(losses = losses* -1) %>% 
  dplyr::select(City, SSP, gains, losses) %>% 
  gather(change, value, gains:losses) %>% 
  mutate(changeSPP = toupper(paste0(change, " - ", SSP)))


### Patterns of differences among cities
colours <- rev(c(RColorBrewer::brewer.pal(n=6, "RdYlBu")))
colours[1:3] <- rev(colours[1:3])
cityPlot <- ggplot(longCity, aes(x = reorder(City, value), y = value, fill = changeSPP)) + 
geom_point(shape = 21, color = "black", size = 2) + 
coord_flip()+ xlab("") +
   scale_fill_manual(values = colours) +
  geom_hline(yintercept=0, lty=2) + theme_classic() +
  ylab("Change in number of species")+
  theme(text = element_text(size=14), legend.position = c(0.86, 0.2))
cityPlot
ggsave("Figure1.pdf", cityPlot, width = 8, height = 10)

m1 <- glm.nb(value ~ SSP, data = longCity %>% filter(change == "gains"))
anova(m1)
m2 <- glm.nb(abs(value) ~ SSP, data = longCity %>% filter(change == "losses"))
anova(m2)

longCity %>% filter(City == "Toronto")

### Test against climate patterns
climate <- read.csv("data//currentClimate.csv")
meanClimate <- climate %>% 
  group_by(City) %>% 
  summarize(map = mean(MAP, na.rm = T),
    mat = mean(MAT, na.rm = T)) %>% 
    right_join(longCity) 

## Precipitation
m1Gain <- glm.nb(value ~ map * SSP, data= meanClimate %>% filter(change == "gains"))
anova(m1Gain, test="Chisq")
m1Gains <- effects::effect("map", m1Gain,
 xlevels = list(map = seq(100, 1600, by = 100)), 
 se = T) %>% data.frame()
m1Loss <- glm.nb(abs(value) ~ map * SSP, data= meanClimate %>% filter(change == "losses"))
anova(m1Loss, test="Chisq")
m1Losses <- effects::effect("map", m1Loss,
 xlevels = list(map = seq(100, 1600, by = 100)), 
 se = T) %>% data.frame()

precPlot <- ggplot(meanClimate, aes(x = map, y = value, fill=changeSPP)) +
geom_point(shape = 21, size = 3) + theme_classic() + geom_hline(yintercept = 0, lty = 2 )  + ylab("Change in species richness") +
 scale_fill_manual(values =  colours)  +
 xlab("Mean annual precipitation") + theme(text = element_text(size=16), legend.position = c(0.9, 0.85)) +
 geom_line(data = m1Gains, aes(x = map, y= fit, fill = NA), color = colours[3]) +
 geom_line(data = m1Losses, aes(x = map, y= fit , fill = NA), color = colours[6]) 
ggsave("PrecipitationPlot.pdf", precPlot)

## Temperature
m2Gain <- glm.nb(value ~ mat * SSP, data= meanClimate %>% filter(change == "gains"))
anova(m2Gain, test="Chisq")
m2Gains <- effects::effect("mat", m2Gain,
 xlevels = list(map = seq(3, 25, by = 1)), 
 se = T) %>% data.frame()
m2Loss <- glm.nb(abs(value) ~ mat * SSP, data= meanClimate %>% filter(change == "losses"))
anova(m2Loss, test="Chisq")
m2Losses <- effects::effect("mat", m2Loss,
 xlevels = list(mat = seq(3, 25, by = 1)), 
 se = T) %>% data.frame()

tempPlot <- ggplot(meanClimate, aes(x = mat, y = value, fill=changeSPP)) +
geom_point(shape = 21, size = 3) + theme_classic() + geom_hline(yintercept = 0, lty = 2 )  + ylab("Change in species richness") +
 scale_fill_manual(values =  colours)  +
 xlab("Mean annual temperature") + theme(text = element_text(size=16), legend.position = c(0.9, 0.85)) +
 geom_line(data = m2Gains, aes(x = mat, y= fit, fill = NA), color = colours[3]) +
 geom_line(data = m2Losses, aes(x = mat, y= fit , fill = NA), color = colours[6]) 
ggsave("TemperaturePlot.pdf", tempPlot)


##### Taxa plot
## Differences in taxa
taxaInfo <- terrestrial %>% distinct(Phylum, Class, Order, Family, species)
taxaClimate <- merge(allClimate, taxaInfo, by="species" )
# write.csv(taxaClimate, "out//allSpeciesData.csv", row.names=FALSE)


taxaClimateSimplified <- taxaClimate %>% 
    filter(SSP == "ssp585") %>% 
    group_by(Class, Order, Family, species) %>% 
    summarize(currentCities = sum(currentOcc), futureCities = sum(futureOcc), 
      netCities = futureCities - currentCities)
taxaClimateSimplified <- taxaClimateSimplified %>% group_by(Order) %>% 
mutate(medianOrder = median(netCities, na.rm=T), nOrder=length(netCities)) %>% 
  ungroup()  %>%  group_by(Class) %>% mutate(medianClass = median(netCities, na.rm=T), nClass=length(netCities)) 

## Simple phylum plot
taxaPlot <- ggplot(taxaClimateSimplified %>%  filter(Class != "Clitellata"),
      aes(y=Class, x= netCities, fill=Class)) + 
  geom_density_ridges2(scale = 0.8, rel_min_height = 0.03) +
  theme_classic() + xlab("Net change in number of cities") +
  geom_hline(yintercept=0, lty=2) + xlim(c(-30, 30)) +
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
  summarize(taxaChange = mean(netCities), nSpp = length(unique(species)), currentCity = mean(currentCities), futureCity = mean(futureCities))  %>% 
  filter(nSpp > 1) %>% 
  slice(which(taxaChange == max(taxaChange) | taxaChange == min(taxaChange) |  nSpp == max(nSpp))) %>% 
  mutate(propCityChange = taxaChange / currentCity)

extremeOrders <- taxaClimateSimplified %>% 
  filter(!(Class %in% c("Reptilia","Mammalia","Amphibia"))) %>% 
  group_by(Class, Order) %>% 
  summarize(taxaChange = round(mean(netCities),3), nSpp = length(unique(species)), currentCity = mean(currentCities))  %>% 
  filter(nSpp > 1) %>% 
  slice(which(taxaChange == max(taxaChange) | taxaChange == min(taxaChange) |  nSpp == max(nSpp))) %>% 
  mutate(propCityChange = taxaChange / currentCity)

taxaClimateSimplified %>% 
  filter(!(Class %in% c("Reptilia","Mammalia","Amphibia"))) %>% 
  group_by(Class, Order) %>% 
  summarize(taxaChange = round(mean(netCities),3), nSpp = length(unique(species)), currentCity = mean(currentCities)) %>% 
  filter(Order == "Hymenoptera")

taxaClimateSimplified %>%  filter(Class == "Clitellata")

classSummary <- taxaClimateSimplified %>% 
  group_by(Class)  %>% 
  summarize(taxaChange = mean(netCities), nSpp = length(unique(species)), currentCity = mean(currentCities))

## Number of no changes
speciesChanges <- taxaClimateSimplified %>% 
  mutate(noChange = currentCities == futureCities)
sum(speciesChanges$netCities < 0) / nrow(speciesChanges)
sum(speciesChanges$netCities > 0) / nrow(speciesChanges)


classSummary <- taxaClimateSimplified %>% 
  filter(Class %in% c("Aves","Insecta")) %>% 
  group_by(Class)  %>% 
  summarize(increases = sum(netCities > 0), losses = sum(netCities < 0))

BeesNA <- taxaClimateSimplified %>% filter(Order=="Hymenoptera") %>% 
  # filter(City=="Raleigh") %>%  
  filter(Family %in% c("Andrenidae","Apidae","Collectidae","Hallictidae","Megachilidae","Melittidae","Stenotritidae")) %>%  ## bee families
  summarize(avg=mean(netCities), currentCities = mean(currentCities), propChange = avg/currentCities)



cityChange <- allClimate %>%
left_join(taxaInfo, by="species" ) %>% 
filter(Order=="Hymenoptera") %>% 
filter(City=="Raleigh") %>%  
filter(Family %in% c("Andrenidae","Apidae","Collectidae","Hallictidae","Megachilidae","Melittidae","Stenotritidae")) %>% 
    left_join(cityStats) %>% 
    group_by(City, lat, lon, SSP) %>% 
    summarize(gains = sum(gain), losses = sum(loss),
     noChanges = sum(noChange), currentSpp = sum(currentOcc)) %>% 
    mutate(perChange = (gains - losses) / currentSpp)

##### IUCN Comparison

IUCNList <- read.csv("data//IUCNspeciesList.csv")


## join IUCN data with probability
IUCNclimate <- merge(allClimate, IUCNList)
redListcat <- data.frame(redlistCategory = c("Critically Endangered","Endangered","Vulnerable","Near Threatened","Least Concern","Data Deficient"),
                         redlistSimplified = c("At-risk","At-risk","At-risk","At-risk","Least Concern","Data Deficient"))
IUCNclimate <- merge(IUCNclimate, redListcat)

se <- function(x) { sd(x)/ sqrt(length(x))}
IUCNsummary <- IUCNclimate %>% 
  group_by(SSP, redlistSimplified, City) %>% 
  filter(!is.na(redlistCategory)) %>% 
  filter(redlistSimplified != "Data Deficient") %>% 
  summarize(gains = sum(gain), losses = sum(loss), noChanges = sum(noChange)) %>% 
  gather(change, value, gains:noChanges)

IUCNplot <- ggplot(IUCNsummary, aes(x = change, y = value, fill = SSP)) + 
geom_boxplot() + facet_wrap(~redlistSimplified, scale = "free") +
theme_classic() + scale_fill_manual(values =  c("#56B4E9","#999999", "#E69F00")) +
theme(text = element_text(size=16), legend.position = c(0.1, 0.85)) +
ylab("Number of species") + xlab("")
IUCNplot
ggsave("Figure3.pdf", IUCNplot, width = 10, height = 7)

### Models to test for differences


## At-risk
m1 <- glm.nb(value ~ change * SSP, 
  data = IUCNsummary %>% filter(redlistSimplified == "At-risk"))
summary(m1)
anova(m1)
pairwise1 <- emmeans(m1,  "change")
pairs(pairwise1)
## Common
m2 <- glm.nb(value ~ change * SSP, 
  data = IUCNsummary %>% filter(redlistSimplified == "Least Concern"))
summary(m2)
anova(m2)
pairwise2 <- emmeans(m2,  "change")
pairs(pairwise2)




#### Compare population size with change in occurrence
cityPop <- read.csv("data//cityPopulation.csv", stringsAsFactors = F)
popChange <- merge(cityPop, allClimate, by="City")

totalCityChange <- popChange %>%  group_by(SSP, City, Population) %>% 
  summarize(gains = sum(gain), losses = sum(loss), noChanges = sum(noChange))  %>% 
  mutate(cityCode = abbreviate(City, 6)) %>% 
  filter(SSP == "ssp585")


populationPlot <- ggplot(totalCityChange, aes(x = Population, y= noChanges, label=cityCode)) + 
  geom_errorbar(aes( ymax = noChanges+ gains, ymin = noChanges - losses)) +
  geom_label() + scale_x_log10() +
  theme_classic() + ylab("Predicted species richness") +
  xlab("Current city population") +
  theme(text = element_text(size=16))
populationPlot 
ggsave("Figure2.pdf", populationPlot, width = 12, height = 7)

cor.test(totalCityChange$Population, totalCityChange$gains)
cor.test(totalCityChange$Population, totalCityChange$losses)

#### Appendix 
comparisonToCurrent <- cityChange %>% 
  mutate(losses = losses * -1) %>% 
  dplyr::select(City, SSP, gains, losses, currentSpp) %>% 
  gather(change, value, gains:losses) %>% 
  mutate(changeSSP = paste0(change, " - ", SSP))

m1 <- glm.nb(value ~ currentSpp * SSP, data = comparisonToCurrent %>%  filter(change == "gains"))
summary(m1)
anova(m1)
predGains <- effects::effect("currentSpp", m1,
 xlevels = list(currentSpp = seq(100, 1400, by = 100)), 
 se = T) %>% data.frame()

m2 <- glm.nb(abs(value) ~ currentSpp * SSP, data = comparisonToCurrent %>%  filter(change == "losses"))
summary(m2)
anova(m2)
MuMIn::r.squaredGLMM(m2)
predLosses<- effects::effect("currentSpp", m2,
 xlevels = list(currentSpp = seq(100, 1400, by = 100)), 
 se = T) %>% data.frame() %>% 
 mutate(fit = fit* -1, lower = lower* -1, upper = upper* -1)

RelativeChangePlot <- ggplot(comparisonToCurrent , aes(x = currentSpp, y = value, fill = changeSSP)) +
 geom_point(shape = 21, size = 3) + theme_classic() + geom_hline(yintercept = 0, lty = 2 )  + ylab("Change in species richness") +
 scale_fill_manual(values =  colours)  +
 xlab("Historic species richness") + theme(text = element_text(size=16), legend.position = c(0.9, 0.85)) +
 geom_line(data = predGains, aes(x = currentSpp, y= fit, fill = NA), color = colours[3]) +
 geom_line(data = predLosses, aes(x = currentSpp, y= fit , fill = NA), color = colours[6]) +
 scale_x_continuous(breaks = seq(250,1500, 250) )
RelativeChangePlot
ggsave("Figure5.pdf", RelativeChangePlot, width = 9, height = 7)



### Appendix Table
ExtendedData2 <- cityChange %>% 
  ungroup() %>% 
  dplyr::select(SSP, City,  gains, losses, noChanges, HistoricSpecies = currentSpp)
write.csv(ExtendedData2, "AppendixTable2.csv", row.names=F)




#### Appendix Data for Toronto Star

SimplifiedCity <- taxaClimate  %>% 
  filter(SSP %in% c("ssp245", "ssp585")) %>%
  dplyr::select(City, SSP, Phylum, Class, Order, Family,Species = species, currentOcc, futureOcc) %>% 
  arrange(City) %>% 
  mutate(NetChange = futureOcc - currentOcc) %>%
  write.csv("appendix//AllSpeciesChange.csv", row.names = F)


canadianSpecies <- c("Cardinalis cardinalis", "Poecile atricapillus","Cyanocitta cristata", "Icterus galbula",
"Setophaga ruticilla", "Picoides pubescens", "Spinus tristis", "Progne subis", 
"Gavia immer", "Ardea herodias","Sciurus carolinensis","Sciurus niger", "Tamias minimus","Tamias striatus",
"Danaus plexippus","Papilio canadensis","Bombus impatiens","Bombus vagans","Bombus bimaculatus",
"Chrysemys picta","Chelydra serpentina","Anaxyrus hemiophrys","Ambystoma macrodactylum",
"Lithobates pipiens","Ambystoma maculatum", "Pseudacris maculata", "Lepus americanus")


taxaClimate %>% 
  filter(SSP %in% c("ssp245", "ssp585")) %>%
  filter(species %in% canadianSpecies) %>% 
  dplyr::select(City, SSP, Phylum, Class, Order, Family, Species = species, currentOcc, futureOcc) %>% 
  arrange(City) %>% 
  mutate(NetChange = futureOcc - currentOcc) %>%
  write.csv("appendix//CanadianSpeciesChange.csv", row.names = F)



### Get common names for species list
library(taxize)
speciesList <- read.csv("appendix//AllSpeciesChange.csv", stringsAsFactors = F)

latinNames <- unique(speciesList$Species)

commonName <- sci2comm(sci= latinNames, simplify = T)
commonName <- lapply(commonName, function(i) {
  ifelse(length(nchar(i)) == 0, NA, i)
})
commonNames <- as.vector(unlist(commonName))

allNames <- data.frame(Species = latinNames, commonNames)

speciesAll <- merge(speciesList, allNames, by="Species")

write.csv( allNames, "appendix//SpeciesList.csv", row.names = F)
speciesList <- write.csv(speciesAll, "appendix//AllSpeciesChangeCommon.csv")



### Compare native vs non-native

nativeStatus <- read.csv("data/nativeStatus.csv")

head(nativeStatus)

nativeStatusSelect <- nativeStatus %>% dplyr::select(species, NativeStatus)

allClimateNative <- left_join(allClimate, nativeStatusSelect, by="species") %>% 
  mutate(nativeStatus = ifelse(NativeStatus=="Exotic", "Exotic", "Native"))


exoticClimate <- allClimateNative %>% 
  group_by(SSP, nativeStatus, City) %>% 
  summarize(totalGain = sum(gain), totalLoss = sum(loss), totalNoChange = sum(noChange)) %>% 
  filter(nativeStatus == "Exotic") 

## need the bars to not stack, but to be beside each other
ggplot(aes(x = City, y = totalGain, fill = SSP), data = exoticClimate) + 
  geom_bar(stat = "identity", position="dodge")  + facet_wrap(~SSP) + theme_classic() +
  theme(text = element_text(size=16)) + ylab("Number of species") + xlab("")+ 
  scale_fill_manual(values =  c("#56B4E9","#999999", "#E69F00"))  +
  coord_flip()

ggplot(aes(x = City, y = totalLoss, fill = SSP), data = exoticClimate) + 
  geom_bar(stat = "identity", position="dodge")  + facet_wrap(~SSP) + theme_classic() +
  theme(text = element_text(size=16)) + ylab("Number of species") + xlab("")+ 
  scale_fill_manual(values =  c("#56B4E9","#999999", "#E69F00"))  +
  coord_flip()


exoticClimate %>% group_by(SSP) %>% summarize(meanGain = mean(totalGain), meanLoss = mean(totalLoss))

cityChange <- exoticClimate %>%
    left_join(cityStats) %>% 
    group_by(City, lat, lon, SSP) %>% 
    summarize(gains = sum(gain), losses = sum(loss),
     noChanges = sum(noChange), currentSpp = sum(currentOcc)) %>% 
    mutate(perChange = (gains - losses) / currentSpp)

longCity <- exoticClimate %>% 
  mutate(totalLoss = totalLoss* -1) %>% 
  dplyr::select(City, SSP, totalGain, totalLoss) %>% 
  gather(change, value, totalGain:totalLoss) %>% 
  mutate(changeSPP = toupper(paste0(change, " - ", SSP)))


### Patterns of differences among cities
colours <- rev(c(RColorBrewer::brewer.pal(n=6, "RdYlBu")))
colours[1:3] <- rev(colours[1:3])
cityPlot <- ggplot(longCity, aes(x = reorder(City, value), y = value, fill = changeSPP)) + 
geom_point(shape = 21, color = "black", size = 2) + 
coord_flip()+ xlab("") +
   scale_fill_manual(values = colours) +
  geom_hline(yintercept=0, lty=2) + theme_classic() +
  ylab("Change in number of species")+
  theme(text = element_text(size=14), legend.position = c(0.86, 0.2))
cityPlot