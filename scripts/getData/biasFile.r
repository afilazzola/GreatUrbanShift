## Create bias file

## Load species occurrences
speciesFiles <- list.files("data//speciesOcc", full.names = T)

SpeciesList <- lapply(1:length(speciesFiles), function(i) {
  species <- read.csv(speciesFiles[i])
  species
})
allSpecies <- do.call( rbind, SpeciesList) ## combine all into one CSV

## Convert species to CRS of Climate NA
coordinates(allSpecies) <- ~decimalLongitude+decimalLatitude
crs(allSpecies) <- CRS("+proj=longlat +datum=WGS84")
allSpecies <- spTransform(allSpecies, CRS("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +datum=WGS84")) 
allSpecies <- data.frame(allSpecies) ## convert back to dataframe

### Load climate raster
climateFiles <- list.files("data//climate//", full.names = T)
climateRasters <- raster(climateFiles[1])
crs(climateRasters) <- CRS("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +datum=WGS84")


## Conduct point density estimates
xband <- MASS::bandwidth.nrd(allSpecies$decimalLongitude) ## Find bandwidths for x of raster
yband <- MASS::bandwidth.nrd(allSpecies$decimalLatitude) ## Find bandwidths for y of raster
dens <- KernSmooth::bkde2D(allSpecies[,c("decimalLongitude","decimalLatitude")], bandwidth=c(xband,yband), gridsize=c(ncol(climateRasters),nrow(climateRasters))) ## density function based on raster cells to create bias raster
dens.ras <- raster(list(x=dens$x1,y=dens$x2,z=dens$fhat))
crs(dens.ras) <- crs(climateRasters)

## Normalize the raster points to make interpretation easier
normalize <- function(x){
  (x - min(x, na.rm=T))/(min(x, na.rm=T) + max(x, na.rm=T))
}
dens.ras2 <- calc(dens.ras, normalize)


## Write Raster
writeRaster(dens.ras2, "data//biasFile.tif", overwrite=T)

