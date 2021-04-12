### Pick cities with different climates and socio-economic backgrounds for the analysis

## Libraries
library(raster)
library(tidyverse)
library(rgdal)

## Load city polygons
canadaCity <- readOGR("data//cityData//CanadaMajorCities//CanadaMajorCities.shp")
USACity <- readOGR("data//cityData//USAMajorCities//USA_Major_Cities.shp")

## Select cities with at least 500,000 people
USAtop <- subset(USACity, POPULATION > 400000) ## USA
canadalist <- c("Toronto","Montréal","Vancouver","Calgary","Ottawa - Gatineau","Edmonton",
                "Winnipeg","Québec","Halifax", "Hamilton")
CanadaTop <- subset(canadaCity, PCNAME %in% canadalist)
CanadaTop <- unionSpatialPolygons(CanadaTop, CanadaTop$PCNAME) ## drop duplicate of Ottawa


## City names combined
USAcentroid <- rgeos::gCentroid(USAtop,byid=TRUE)
Cancentroid <- rgeos::gCentroid(CanadaTop,byid=TRUE)
AllCities <- data.frame( CityName = c(USAtop$NAME, names(CanadaTop)), 
                         lon = c(coordinates(USAtop)[,1],coordinates(CanadaTop)[,1]),
                         lat = c(coordinates(USAtop)[,2],coordinates(CanadaTop)[,2]))

## Download all the population variables for cities
AllCities[,2:3]  <- round(AllCities[,2:3] ,2 ) ## need less specific coordinates to work


lapply(1:nrow(AllCities), function(i){
URL <- paste0("https://www.numbeo.com/api/city_pollution?api_key=gvwemy27fipv0e&query=",AllCities[i,3],",",AllCities[i,2])
download.file(URL, paste0(destfile="data//cityData//numero//",AllCities[i,1],".json"))
})

jsonFiles <- list.files("data//cityData//numero", full.names = T)

cityOut <- lapply(1:length(jsonFiles), function(i){
table <- jsonlite::fromJSON(jsonFiles[i]) %>% unlist %>% t() %>%  data.frame()
table
})
              
## Pollution Data
pollution <- do.call(plyr::rbind.fill, cityOut)
pollution[,"City"] <- AllCities$CityName
write.csv(pollution, "data//cityData//pollutionData.csv", row.names = F)



## Climate data
coordinates(AllCities) <- ~lon+lat
proj4string(AllCities) <- crs("+proj=longlat +datum=WGS84 +no_defs")

climateFiles <- list.files("data//climate//", full.names = T)
climateRasters <- stack(climateFiles)

climateOut <- raster::extract(climateRasters, AllCities) %>% data.frame()
climateOut[,"City"] <- AllCities$CityName

write.csv(climateOut, "data//cityData//climateData.csv", row.names = F)




### Analyze city data
pop <- read.csv("data//cityData//pollutionData.csv")
clim <- read.csv("data//cityData//climateData.csv")
names(clim) <- names(clim) %>% gsub("wc2.1_2.5m_", "", .)

both <- merge(pop, clim, by="City")

library(vegan)

selectedVariables <- both %>% select(MAT = bio_1, MAP=bio_12, WarmestTemp = bio_5, ColdestTemp = bio_6, 
                                     WinterPrecip = bio_19, SummerPrecip = bio_18,
                                     Greenspace = green_and_parks_quality, AirQuality = air_quality,
                                     NoiseLightPoll = noise_and_light_pollution, WaterPoll = water_pollution)
scaledVars <- decostand(selectedVariables, method="standardize")
row.names(scaledVars) <- both$City

pca1 <- rda(scaledVars)

plot(pca1)



selectedVariables <- both %>% select(MAT = bio_1, MAP=bio_12, WarmestTemp = bio_5, ColdestTemp = bio_6, 
                                     WinterPrecip = bio_19, SummerPrecip = bio_18)
scaledVars <- decostand(selectedVariables, method="standardize")
row.names(scaledVars) <- both$City

pca1 <- rda(scaledVars)
plot(pca1)
