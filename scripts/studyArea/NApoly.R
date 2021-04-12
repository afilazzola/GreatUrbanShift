## Get polygon for states and provinces
Can <-getData('GADM', country='CAN', level=1)
USA <- getData('GADM', country='USA', level=1)

CanEast <- Can[Can$NAME_1 %in% c("Ontario", "Québec","New Brunswick"),]
USAEast <- USA[USA$NAME_1 %in% c("Connecticut","Delaware","District of Columbia","Illinois","Indiana",
                                 "Kentucky","Maine","Maryland","Massachusetts","Michigan",
                                 "New Hampshire","New Jersey","New York","North Carolina","Ohio","Pennsylvania","Rhode Island",
                                 "Tennessee","Vermont","Virginia","West Virginia","Wisconsin"),]
NAEast <- rbind(CanEast, USAEast)
plot(NAEast)
NAEastSimplified <- rgeos::gSimplify(NAEast, tol = 0.1)

## Re-join data
NAdf <- NAEast@data
NAEastSimplified <- SpatialPolygonsDataFrame(NAEastSimplified, NAdf)

writeOGR(NAEastSimplified, "data", "EasternNA", driver="ESRI Shapefile", overwrite_layer = T) 






### Generate Larger study area
CanEast <- Can[Can$NAME_1 %in% c("Ontario", "Québec","New Brunswick","Newfoundland and Labrador","Nova Scotia","Prince Edward Island"),]
USAEast <- USA[USA$NAME_1 %in% c("Connecticut","Delaware","District of Columbia", "Georgia","Illinois","Indiana","Florida","Kentucky","Louisiana",
                                 "Maine","Maryland","Massachusetts","Michigan","Minnesota","Iowa","Missouri","Arkansas",
                                 "New Hampshire","New Jersey","New York","North Carolina", "South Carolina", "Ohio","Pennsylvania","Rhode Island",
                                "Alabama","Mississippi", "Tennessee","Vermont","Virginia","West Virginia","Wisconsin"),]
NAEast <- rbind(CanEast, USAEast)
plot(NAEast)
NAEastSimplified <- rgeos::gSimplify(NAEast, tol = 0.1)

## Re-join data
NAdf <- NAEast@data
NAEastSimplified <- SpatialPolygonsDataFrame(NAEastSimplified, NAdf)

EasternNAbb <- bbox(NAEastSimplified)


writeOGR(NAEastSimplified, "data//SDMdata", "EasternNA", driver="ESRI Shapefile", overwrite_layer = T) 



### Create Polygon for Canada and US

Can <-getData('GADM', country='CAN', level=1)
USA <- getData('GADM', country='USA', level=1)

USA <- USA[!USA$NAME_1 == "Hawaii",] ## drop Hawaii
USA <- crop(USA, extent(-170,-52,24,72.7)) ## drop Eastern territories 


CanUSA <- rbind(Can, USA)
CanUSAsimple <- rgeos::gSimplify(CanUSA, tol = 0.1)
CanUSAsimpleDF<-  SpatialPolygonsDataFrame(CanUSAsimple, CanUSA@data)

writeOGR(CanUSAsimpleDF, "data", driver="ESRI Shapefile", "CanUSA")
