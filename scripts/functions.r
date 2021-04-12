### Functions

##Libraries
library(raster)
library(rgdal)
library(tidyverse)
library(rgbif)



### Get species from GBIF

getGBIFcsv <- function(species, nrecord){

taxonKeys <- species %>% 
  taxize::get_gbifid_(method="backbone")  %>% 
  imap(~ .x %>% mutate(original_sciname = .y)) %>% 
  bind_rows() %>% # combine all data.frames into one
  filter(matchtype == "EXACT" & status == "ACCEPTED") %>% 
  pull(usagekey) # get the gbif taxonkeys


sampleSpp <- occ_search(taxonKey = taxonKeys,  hasCoordinate=T, country=c("CA;US"),  ## select plants, in Canada, with coordinates
                        start=1, 
                        limit=nrecord, ## set  limit for downloading records
                        fields = c("scientificName","acceptedScientificName","decimalLatitude","decimalLongitude","eventDate","verbatimEventDate","year","month","day","coordinateUncertaintyInMeters","genus","species","kingdom","datasetKey"),
                        year= c("2000,2020"), ## Extract recent observations only
                        geometry=wktPoly,
                        hasGeospatialIssue=FALSE) ## specify for area around GTA - WKT polygon counter clockwise
return(sampleSpp)
}


## Spatial conversions
deg2rad <- function(deg) {(deg * pi) / (180)} ## function converts decimal degrees to radians
lat2km <- function(lat){ cos(deg2rad(lat)) * 111.32 } ## cosine of latitude in radius multiplied by the size of the decimal at the equator

### Make a polygon around specific coordinates
makePoly <- function(centroid.lon, centroid.lat, buffer){
  ## determine buffer for longitude    
  latinkm <-  lat2km(centroid.lat)
  res <- 1/latinkm*buffer
  xmin = centroid.lon - res
  xmax = centroid.lon + res
  ymin = centroid.lat - 1/111*buffer
  ymax = centroid.lat + 1/111*buffer
  cornerCoords <- matrix(c(xmin, ymin, ## South-west Corner
                           xmin, ymax, ## North-west Corner
                           xmax, ymax, ## North-East Corner
                           xmax, ymin, ## South-east Corner
                           xmin, ymin), ## South-west Corner Close
                         ncol=2, byrow=T)
  polytemp <- Polygon(cornerCoords)
  polytemp <- Polygons(list(polytemp), ID="A")
  Ps1 <- SpatialPolygons(list(polytemp), proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
  return(Ps1)
}


### calculate geographic patterns of coordinates
calcDistribution <- function(speciesTest, centroid){ ## need a lat and lon column

## List Libraries
require(adehabitatHR) 
require(sf)
require(raster)
  
## List arguments
center <- centroid


### Convert Centroid to proper CRS
coordinates(center) <- ~lon+lat
proj4string(center) <- CRS("+proj=longlat +datum=WGS84")
center <- spTransform(center,  CRS("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +datum=WGS84"))

## Create minimum convext polygon
sppMCP <- mcp(speciesTest, percent = 99)

# ## Visualizations of plot
# plot(sampleSpp)
# plot(sppMCP, add=T)
# plot(center, add=T, pch=21)


###  Latitude patterns
sampleSppGPS <- speciesTest %>% spTransform(CRS("+proj=longlat +datum=WGS84")) %>%  coordinates() %>% data.frame()
minLat <- min(sampleSppGPS[,2])
medLat <- median(sampleSppGPS[,2])
maxLat <- max(sampleSppGPS[,2])

## Distance Shortest Edge 
shortEdge <- st_geometry(obj = st_as_sf( sppMCP)) %>% st_cast(to = 'LINESTRING') %>% st_distance(y = st_as_sf(center)) %>%  as.numeric()
shortEdge <- shortEdge/1000 ## Km away from edge of MCP

## Average distances to other points
distToToronto <- pointDistance(center, speciesTest, lonlat=F)/1000
minDist <- min(distToToronto)
medianDist <- median(distToToronto)
maxDist <- max(distToToronto)

## Proportion of points North
propNorth <- sum(sampleSppGPS$decimalLatitude>43.66)/nrow(sampleSppGPS)

## Data Out
measureOut <- data.frame(species = unique(speciesTest$species),
                         minLat=minLat, medLat=medLat, maxLat=maxLat,
                         shortEdge=shortEdge,propNorth,
                         minDist,medianDist,maxDist)
centroidInfo <- centroid %>% distinct(centroid, lat, lon)
measureOut <- cbind(centroid, measureOut)
return(measureOut)
}

# toronto <- data.frame( centroid = "Toronto", lat = 43.66, lon = -79.56)
# 
# calcDistribution(speciesTest=sampleSpp, centroid = toronto)
