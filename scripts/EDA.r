### Analyze results

## Libraries
library(tidyverse)
library(ggridges)

## Look at the model results
models <- list.files("out//models//", full.names = T)

listModels <- lapply(1:length(models), function(i)
  read.csv(models[i])
)
allModels <- do.call(plyr::rbind.fill, listModels)

### Total number of observations
sum(allModels$nobs)/1000000 ## total number of observations
mean(allModels$nobs) ## average number of observations per species
sd(allModels$nobs)/sqrt(nrow(allModels)) ## Error

### Model checks to ensure none are poor qualifiers
hist(allModels$AUCtrain)
hist(allModels$AUCtest)

## Drop models where AUC < 0.7
bestModels <- allModels %>% filter(AUCtest > 0.7)
ggplot(bestModels, aes(x=AUCtest, y=AUCtrain)) + geom_point()

## Look at predictions for cities
futureclimate <- list.files("out//cityPredict//", full.names = T, pattern="Future")

listFuture <- lapply(1:length(futureclimate), function(i)
  read.csv(futureclimate[i])
)
allFuture <- do.call(rbind, listFuture)

currentclimate <- list.files("out//cityPredict//", full.names = T, pattern="Current")

listCurrent <- lapply(1:length(currentclimate), function(i)
  read.csv(currentclimate[i])
)
allCurrent <- do.call(rbind, listCurrent)



## Combine future and current climate data frames
allClimate <- allCurrent %>% dplyr::select(City, species, currentProb = meanProb) %>% left_join(allFuture) %>% 
  mutate(changeProb =  log(meanProb/currentProb), changeProb= round(changeProb, 4)) %>%  ## log(future/current) = LRR change in predicted occurrence
  filter(!is.na(SSP)) %>% filter(currentProb > 0.05)
allClimate <- allClimate %>% filter(species %in% bestModels$species) ## remove species that had low model AUC


## Calculate species richness per 
meanRichness <- allClimate %>% group_by(SSP, Year, City) %>% 
  summarize(currentRichness = sum(currentProb >0.05), futureRichness = sum(meanProb >0.05)) %>% 
  mutate(diffRichness = (futureRichness - currentRichness)/currentRichness) %>% 
  data.frame() 


## plot the patterns in richness
ggplot(meanRichness %>% filter(SSP=="ssp126" & Year=="2041-2060"), aes(x=City, y=currentRichness, fill=City)) + 
  geom_bar(stat="identity") + 
  theme_classic() + coord_flip() + ylab("Number of species predicted to occur") + scale_fill_manual(values=rep(c("#E69F00", "#56B4E9"),30)) +
  theme(legend.position = "none") + xlab("")


## plot the change in richness for the future
ggplot(meanRichness , aes(x=City, y=diffRichness*100, fill=Year)) + 
  geom_bar(stat="identity", position="dodge2") + facet_grid(~SSP) + 
  theme_classic() + coord_flip() + ylab("Percent change in species richness") + scale_fill_manual(values=rep(c("#E69F00", "#56B4E9"),30)) +
  theme(legend.position = "top") + xlab("")


####### Define regions for cities that are most affected

cityStats <- read.csv("data//cityData//CityCharacteristics.csv") %>% dplyr::select(City = CityName, ecozone = NA_L1NAME, bio1:bio15, lon, lat)

modelClimateCity <- merge(allClimate, cityStats, by="City")


summarizedRegions <- modelClimateCity %>% group_by(ecozone, species, SSP, Year ) %>% summarize(diff=mean(changeProb))



ggplot(data=summarizedRegions, aes(x=diff, y= ecozone, fill=ecozone)) + geom_density_ridges(na.rm=T, stat="binline") + theme_ridges() + 
 theme_classic() + ylab("Change in Predicted Occurrence") + scale_fill_manual(values=RColorBrewer::brewer.pal(n=7, "Dark2")) +
  geom_hline(yintercept=0, lty=2) + geom_vline(xintercept = 0, lty=2) + facet_grid(Year~SSP)


### City map of globe
averageCity <- modelClimateCity %>% filter(!is.infinite(changeProb)) %>%  
  group_by(City, lat, lon,  SSP, Year ) %>% summarize(diff=mean(changeProb, na.rm=T))

## Take extreme year and discrete extremes
averageExtreme <- data.frame(averageCity) %>% filter(Year=="2081-2100")
averageExtreme[,"diffBin"] <- cut(averageExtreme$diff, breaks=c(1,0,-1,-2,-3))

mp <- NULL
mapWorld <- borders("world", colour="white", fill="gray75") # create a layer of borders
mp <- ggplot() + theme_classic()+  mapWorld + xlim(-180,-30) + ylim(-20, 90)
RColorBrewer::brewer.pal(n=8, "YlOrRd")

mp <- mp+ geom_point(data=averageExtreme , aes(x=lon, y=lat, fill=diffBin),  size=3, pch=21) +
 ylab("Latitude") + xlab("Longitude")  + scale_fill_manual(values=rev(c("#AED6F1", "#FFEDA0","#FEB24C","#B10026")))
mp

### Average across models and timeframes
ggplot(allClimate, aes(y=SSP, x=changeProb, fill=Year)) + geom_density_ridges(na.rm=T, stat="binline", alpha=0.5) + 
  theme_classic() + xlab("Change in Predicted Occurrence") + scale_fill_manual(values=c("#E69F00","#56B4E9")) +
  geom_hline(yintercept=0, lty=2) + geom_vline(xintercept = 0, lty=2) + ylab("") +
  scale_y_discrete(limits = rev(levels(allClimate$SSP)))



##### Taxa plot
## Differences in taxa
taxaInfo <- allModels %>% distinct(Phylum, Class, Order, Family, species)
taxaClimate <- merge(allClimate, taxaInfo, by="species" )


taxaClimateSimplified <- taxaClimate %>% filter(!(Phylum=="Platyhelminthes" | Phylum=="Cnidaria" | is.na(Order)| is.na(Class))) %>% 
  filter(Year == "2081-2100" & SSP == "ssp585") 
taxaClimateSimplified <- taxaClimateSimplified %>% group_by(Order) %>% mutate(medianOrder = median(changeProb, na.rm=T), nOrder=length(changeProb)) %>% 
  ungroup()  %>%  group_by(Class) %>% mutate(medianClass = median(changeProb, na.rm=T), nClass=length(changeProb)) 

### Complex phylum
ggplot(taxaClimateSimplified %>% filter(Phylum %in% c("Arthropoda","Chordata"))  , aes(y=reorder(Order, medianOrder), x= changeProb, fill=Class)) + 
  geom_density_ridges(stat="binline") + 
  theme_classic() + xlab("Change in Predicted Occurrence") +
  geom_hline(yintercept=0, lty=2) + facet_wrap(Phylum~., scales = "free_y") +
  scale_fill_manual(values=c(RColorBrewer::brewer.pal(n=12, "Paired"), "black")) +
  geom_vline(xintercept = 0, lty=2) 

### Other  phylum
ggplot(taxaClimateSimplified %>% filter(!(Phylum %in% c("Arthropoda","Chordata")))  , aes(y=reorder(Class, medianClass), x= changeProb)) + 
  geom_density_ridges(stat="binline", fill="black", alpha=0.5) + 
  theme_classic() + xlab("Change in Predicted Occurrence") +
  geom_hline(yintercept=0, lty=2) + 
  geom_vline(xintercept = 0, lty=2) + ylab("")



##### Analyze IUCN data

IUCNList <- read.csv("data//IUCNspeciesList.csv")


## join IUCN data with probability
IUCNclimate <- merge(allClimate, IUCNList)

IUCNsummary <- IUCNclimate %>% group_by(Year, SSP, redlistCategory) %>% filter(!is.infinite(changeProb)) %>% 
  filter(!is.na(redlistCategory)) %>% 
    summarize(diffProb = mean(changeProb, na.rm=T)) %>% arrange(diffProb) %>% 
  data.frame()

IUCNsummary$redlistCategory <- factor(IUCNsummary$redlistCategory, 
                                      levels=c("Critically Endangered","Endangered","Vulnerable","Near Threatened","Least Concern","Data Deficient"))


ggplot(IUCNsummary, aes(x=redlistCategory, y=diffProb, fill=Year)) + 
  geom_bar(stat="identity", position="dodge2") + facet_grid(~SSP) +
  coord_flip() + theme_classic() + 
  scale_fill_manual(values = c("#E69F00", "#56B4E9")) +
  geom_hline(yintercept=0, lty=2) +
  ylab("Change in predicted occurrence (LRR)") + xlab("")

