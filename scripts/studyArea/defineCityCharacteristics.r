### Define city characteristics

## Load libraries
library(rgdal)
library(raster)
library(vegan)
library(tidyverse)

### Load in city data
cities <- read.csv("data//CityList.csv")
coordinates(cities) <- ~lon + lat ## Transform occurrences to spdataframe
proj4string(cities) <- CRS("+proj=longlat +datum=WGS84")

## plot cities

require(ggmap)
require(maps)
###  Start with base map of world
mp <- NULL
mapWorld <- borders("world", colour="white", fill="gray75") # create a layer of borders
mp <- ggplot() + theme_classic()+  mapWorld + xlim(-180,-30) + ylim(-20, 90)

mp <- mp+ geom_point(data=data.frame(cities) , aes(x=lon, y=lat), size=3, pch=21, fill="orange")  + ylab("Latitude") + xlab("Longitude") 
mp



## Load in climate data
CityClimate <- read.csv("data//currentCityClimate.csv")
climateSummary <- CityClimate %>% group_by(CityName = City) %>% dplyr::select(-City) %>%  
  summarize_all(.funs=mean) %>% data.frame()

## Load in polygon for ecoregions
ecoregion <- readOGR("data//NA_CEC_Eco_Level1.shp")
ecoregion <- spTransform(ecoregion, CRS= crs(cities))

## Extract ecoregions for cities
cityRegion <- raster::extract(ecoregion, cities)
cityData <- cbind(cities@data, cityRegion)
cityData[cityData$CityName=="Halifax","NA_L1NAME"] <- "NORTHERN FORESTS"

## Merge climate with ecoregion data
climateCityRegion <- merge(cityData, climateSummary, by="CityName")

## Merge with coordinates
cityCoords <- cities %>% data.frame() %>% dplyr::select(CityName, lon ,lat)
climateCityRegion <- merge(climateCityRegion, cityCoords)
# write.csv(climateCityRegion, "data//cityData//CityCharacteristics.csv")

## ecoregion representation
ggplot(climateCityRegion, aes(x=NA_L1NAME, fill=NA_L1NAME)) + geom_bar() + 
  coord_flip() + xlab("") + theme_classic() + scale_fill_manual(values=RColorBrewer::brewer.pal(n=7, "Dark2"))

## Prepare data for ordination
scaledClimate <- decostand(climateCityRegion[,paste0("bio",c(1,3,4,5,6,12,13,14,15))], method="standardize")
row.names(scaledClimate) <- climateCityRegion$CityName
names(scaledClimate) <- c("Mean Temp", "Isothermality","Temp Stdev","Hottest Temp","Coldest Temp","Annual Prec","Highest Prec","Lowest Prec","Prec CV")

## Run PCA
pca1 <- rda(scaledClimate)
summary(pca1)
plot(pca1)


## Plot the Ordination
cbPalette <- RColorBrewer::brewer.pal(n=7, "Dark2")
climateCityRegion$NA_L1NAME <- as.character(climateCityRegion$NA_L1NAME)
colourDF <- data.frame(NA_L1NAME= unique(climateCityRegion$NA_L1NAME),cbPalette, stringsAsFactors = F)
ColourOut <- merge(climateCityRegion, colourDF, sort = F) %>% arrange(CityName)
ColourOut$cbPalette <- as.character(ColourOut$cbPalette)

par(mar = c(4.5, 4.5, 0.5, 0.5))
plot(pca1, type="n", xlab="PC1 (47.8%)", ylab="PC2 (29.5%)")
orditorp(pca1, display = "species", cex = 1, col = "black", air=0.5)
orditorp(pca1, display = "sites", cex = 0.7, col = ColourOut$cbPalette, air=0.1)

