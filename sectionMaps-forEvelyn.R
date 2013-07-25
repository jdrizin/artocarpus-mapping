# evelyn needs some graphs
library(ggmap)

#read in the data
arto <- read.csv("data/jarrett-specific.csv", stringsAsFactors=F)

##### build maps #####
mapCenter <- geocode("Sandakan, Malaysia") # this is about right
mapSize <- 4 # 3-21, continent to zoom, 10 is city.

#Artocarpus
distMap <- get_map(location=c(lon=mapCenter$lon, lat=mapCenter$lat), zoom=mapSize, maptype="satellite")
printMap <- ggmap(distMap) + 
	geom_point(data=arto[arto$section == "Artocarpus",], aes(x=LOClongitude, y=LOClatitude, color=series)) +
	ggtitle("Section Artocarpus") + xlab("longitude") + ylab("latitude")

printMap 

#Duricarpus
printMap <- ggmap(distMap) + 
	geom_point(data=arto[arto$section == "Duricarpus",], aes(x=LOClongitude, y=LOClatitude, color=series)) +
	ggtitle("Section Duricarpus") + xlab("longitude") + ylab("latitude")
	
	printMap 

#Pseudojaca
printMap <- ggmap(distMap) + 
	geom_point(data=arto[arto$section == "Pseudojaca",], aes(x=LOClongitude, y=LOClatitude, color=series)) +
	ggtitle("Section Pseudojaca") + xlab("longitude") + ylab("latitude")
	
	printMap 