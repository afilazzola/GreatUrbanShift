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


cityChange <- allClimate %>%
    left_join(cityStats) %>% 
    group_by(City, lat, lon, SSP) %>% 
    summarize(gains = sum(gain), losses = sum(loss),
     noChanges = sum(noChange), currentSpp = sum(currentOcc)) %>% 
    mutate(perChange = (gains - losses) / currentSpp)

longCity <- cityChange %>% 
  mutate(losses = losses* -1) %>% 
  select(City, SSP, gains, losses) %>% 
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
  geom_density_ridges2(scale = 0.8, rel_min_height = 0.05) +
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
  summarize(taxaChange = mean(netCities), nSpp = length(unique(species)))  %>% 
  filter(nSpp > 1) %>% 
  slice(which(taxaChange == max(taxaChange) | taxaChange == min(taxaChange) |  nSpp == max(nSpp))) 

extremeOrders <- taxaClimateSimplified %>% 
  filter(!(Class %in% c("Reptilia","Mammalia","Amphibia"))) %>% 
  group_by(Class, Order) %>% 
  summarize(taxaChange = round(mean(netCities),3), nSpp = length(unique(species)))  %>% 
  filter(nSpp > 1) %>% 
  slice(which(taxaChange == max(taxaChange) | taxaChange == min(taxaChange) |  nSpp == max(nSpp))) 

taxaClimateSimplified %>% 
  filter(!(Class %in% c("Reptilia","Mammalia","Amphibia"))) %>% 
  group_by(Class, Order) %>% 
  summarize(taxaChange = round(mean(netCities),3), nSpp = length(unique(species))) %>% 
  filter(Order == "Hymenoptera")



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
library(MASS)
library(emmeans)
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
 geom_line(data = predLosses, aes(x = currentSpp, y= fit , fill = NA), color = colours[6])
RelativeChangePlot
ggsave("Figure5.pdf", RelativeChangePlot, width = 9, height = 7)
