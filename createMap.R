

# load necessary packages
# ggmap - mapping
# maptools - might need it. provides conversions for geospatial data
# raster - google suggests it could be useful?

library(maps)
library(ggmap) # loads ggplot2

##### read and merge data #####
collection <- read.csv("data/collectiondata.csv")
collection$collGenus[collection$collGenus == "Artrocarpus"] <- c("Artocarpus", "Artocarpus")
collection$collGenus[collection$collGenus == "Artocarpus "] <- "Artocarpus"

geospatial <- read.csv("data/geospatial.csv")

# put the data together. i happen to want genus and species for this.
latinName <- with(collection, data.frame(ID, collGenus, collSpecies))
mapping <- merge(latinName, geospatial, by.x="ID", by.y="fk_geospatial_collection_ID")

mapping <- mapping[mapping$collGenus == "Artocarpus",] #remove non-Artocarpus

##### build maps #####
mapCenter <- geocode("Sandakan, Malaysia")
mapSize <- 4 # 3-21, continent to zoom, 10 is city.

distMap <- get_map(location=c(lon=mapCenter$lon, lat=mapCenter$lat), zoom=mapSize, maptype="satellite")
printMap <- ggmap(distMap) + 
               geom_point(data=geospatial, aes(x=Longitude, y=Latitude))
printMap # i guess the default method is to print a plot.
