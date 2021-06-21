### Analyze results

## Libraries
library(tidyverse)
library(ggridges)

## Look at the model results
models <- list.files("out//models//", full.names = T)

listModels <- lapply(1:length(models), function(i)
  read.csv(models[i])
)
allModels <- do.call(rbind, listModels)

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




ggplot(allCurrent, aes(x=species, y= meanProb)) + geom_boxplot() +coord_flip()

## compare change
allClimate <- allCurrent %>% dplyr::select(City, species, currentProb = meanProb) %>% left_join(allFuture) %>% 
  mutate(changeProb = log(meanProb / currentProb))

## Drop occurrences with extremely low probability
allClimate <- allClimate %>% filter(meanProb <0.05)

## Differences in species
ggplot(allClimate, aes(x=species, y= changeProb, fill=Year)) + geom_boxplot() + coord_flip() +
  scale_fill_manual(values = c("#E69F00", "#56B4E9")) + theme_classic() + ylab("Change in Predicted Occurrence") +
  geom_hline(yintercept=0, lty=2) + facet_grid(~SSP)


## Differences in cities
ggplot(allClimate %>% filter(!is.na(SSP)), aes(x=City, y= changeProb, fill=Year)) + geom_boxplot() + coord_flip() +
  scale_fill_manual(values = c("#E69F00", "#56B4E9")) + theme_classic() + ylab("Change in Predicted Occurrence") +
  geom_hline(yintercept=0, lty=2) + facet_grid(~SSP)


## Differences in taxa
taxaInfo <- allModels %>% distinct(Phylum, Class, Order, Family, species)
taxaClimate <- merge(allClimate, taxaInfo, by="species" )


ggplot(taxaClimate, aes(x=Phylum, y= changeProb, fill=Year)) + geom_boxplot() + coord_flip() +
  scale_fill_manual(values = c("#E69F00", "#56B4E9")) + theme_classic() + ylab("Change in Predicted Occurrence") +
  geom_hline(yintercept=0, lty=2) + facet_grid(~SSP) 

ggplot(taxaClimate %>% filter(Phylum == "Chordata"), aes(x=Class, y= changeProb, fill=Year)) + geom_boxplot() + coord_flip() +
  scale_fill_manual(values = c("#E69F00", "#56B4E9")) + theme_classic() + ylab("Change in Predicted Occurrence") +
  geom_hline(yintercept=0, lty=2) + facet_grid(~SSP) 

ggplot(taxaClimate %>% filter(Phylum == "Arthropoda"), aes(x=Order, y= changeProb, fill=Year)) + geom_boxplot() + coord_flip() +
  scale_fill_manual(values = c("#E69F00", "#56B4E9")) + theme_classic() + ylab("Change in Predicted Occurrence") +
  geom_hline(yintercept=0, lty=2) + facet_grid(~SSP) 

ggplot(taxaClimate %>% filter(Class == "Aves"), aes(x=Order, y= changeProb, fill=Year)) + geom_boxplot() + coord_flip() +
  scale_fill_manual(values = c("#E69F00", "#56B4E9")) + theme_classic() + ylab("Change in Predicted Occurrence") +
  geom_hline(yintercept=0, lty=2) + facet_grid(~SSP) 



### Create Ridgeline plots
library(ggridges)



## Differences in cities
ggplot(data=allClimate %>%  filter(SSP=="ssp585"), aes(x=changeProb, y= City)) + geom_density_ridges(na.rm=T, stat="binline") + theme_ridges() + 
  scale_fill_manual(values = c("#E69F00", "#56B4E9")) + theme_classic() + ylab("Change in Predicted Occurrence") +
  geom_hline(yintercept=0, lty=2) 


ggplot(taxaClimate %>% filter(Year == "2081-2100")%>% filter(Class == "Aves"), aes(y=Order, x= changeProb)) + 
  geom_density_ridges(stat="binline") + 
  scale_fill_manual(values = c("#E69F00", "#56B4E9")) + theme_classic() + ylab("Change in Predicted Occurrence") +
  geom_hline(yintercept=0, lty=2) + facet_grid(~SSP) 


taxaClimateSimplified <- taxaClimate %>% filter(!(Phylum=="Platyhelminthes" | Phylum=="Cnidaria" | is.na(Order)| is.na(Class)))

ggplot(taxaClimateSimplified %>% filter(Year == "2081-2100" & SSP == "ssp585"), aes(y=Order, x= changeProb, fill=Class)) + 
  geom_density_ridges(stat="binline") + 
  theme_classic() + xlab("Change in Predicted Occurrence") +
  geom_hline(yintercept=0, lty=2) + facet_wrap(Phylum~., scales = "free_y") +
  scale_fill_manual(values=c(RColorBrewer::brewer.pal(n=12, "Paired"), "black")) +
  geom_vline(xintercept = 0, lty=2)




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
averageExtreme[,"diffBin"] <- cut(averageExtreme$diff, breaks=c(-1,-2,-3,-4,-5))

mp <- NULL
mapWorld <- borders("world", colour="white", fill="gray75") # create a layer of borders
mp <- ggplot() + theme_classic()+  mapWorld + xlim(-180,-30) + ylim(-20, 90)
RColorBrewer::brewer.pal(n=8, "YlOrRd")

mp <- mp+ geom_point(data=averageExtreme , aes(x=lon, y=lat, fill=diffBin),  size=3, pch=21) +
 ylab("Latitude") + xlab("Longitude")  + scale_fill_manual(values=rev(c("#FFEDA0","#FEB24C","#FC4E2A","#B10026")))
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
taxaClimateSimplified <- taxaClimateSimplified %>% group_by(Order) %>% mutate(medianOrder = median(changeProb, na.rm=T)) %>% 
  ungroup()  %>%  group_by(Class) %>% mutate(medianClass = median(changeProb, na.rm=T))

### Complex phylum
ggplot(taxaClimateSimplified %>% filter(Phylum %in% c("Arthropoda","Chordata"))  , aes(y=reorder(Order, medianOrder), x= changeProb, fill=Class)) + 
  geom_density_ridges(stat="binline") + 
  theme_classic() + xlab("Change in Predicted Occurrence") +
  geom_hline(yintercept=0, lty=2) + facet_wrap(Phylum~., scales = "free_y") +
  scale_fill_manual(values=c(RColorBrewer::brewer.pal(n=12, "Paired"), "black")) +
  geom_vline(xintercept = 0, lty=2) 

### Other phlyum phylum
ggplot(taxaClimateSimplified %>% filter(!(Phylum %in% c("Arthropoda","Chordata")))  , aes(y=reorder(Class, medianClass), x= changeProb)) + 
  geom_density_ridges(stat="binline", fill="black", alpha=0.5) + 
  theme_classic() + xlab("Change in Predicted Occurrence") +
  geom_hline(yintercept=0, lty=2) + 
  geom_vline(xintercept = 0, lty=2) + ylab("")




###### Compare with city characteristics

population <- read.csv("data//cityData//populationData.csv", stringsAsFactors = F)
taxaClimateSimplified$City <- as.character(taxaClimateSimplified$City)

## City patterns
cityMean <- taxaClimateSimplified %>% group_by(Phylum, City) %>% 
  filter(!is.infinite(changeProb)) %>% 
  summarize(meanChange = mean(changeProb, na.rm=T )) %>% 
  left_join(population) %>% data.frame()



ggplot(cityMean, aes(x=log(Population), y= log(meanChange+6), color=Phylum)) + 
  geom_point() + geom_smooth( method="lm", se=F) + theme_classic() +
  ylab("Change in Predicted Occurrence") +
  scale_color_manual(values=c(RColorBrewer::brewer.pal(n=5, "Dark2"))) 




pollution <- read.csv("data//cityData//pollutionData.csv", stringsAsFactors = F)


citypollution <- taxaClimateSimplified %>% group_by(Phylum, City) %>% 
  filter(!is.infinite(changeProb)) %>% 
  summarize(meanChange = mean(changeProb, na.rm=T )) %>% 
  left_join(pollution) %>% data.frame()


plot2 <- ggplot(citypollution, aes(x=green_and_parks_quality , y= meanChange, color=Phylum)) + 
  geom_point() + geom_smooth( method="lm", se=F) + theme_classic() + ylab("Change in Predicted Occurrence") +
  scale_color_manual(values=c(RColorBrewer::brewer.pal(n=5, "Dark2"))) 
  

