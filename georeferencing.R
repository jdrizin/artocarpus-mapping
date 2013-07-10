#some data munging for figuring this junk out. 

#### load packages ####
library(ggmap)

#### load data ####
arto <- read.csv("data/jarrett.csv", as.is=T)
arto$index <- 1:1628

#deal with some of the smaller islands. some of this will be done by hand on the file directly
#especially those with few records.

arto <- arto[arto$Country != "Cultivated", ] # remove the cultivated records

#### try to geocode ####

looplen  <- length(arto$LOCALITY) # this is about 1600. you have 2500 from google
dummyvec <- rep(0, looplen)

arto$LOClatitude  <- dummyvec # populate the df
arto$LOClongitude <- dummyvec

for(i in 1:looplen) {
	loc <- arto$LOCALITY[i]
	geoloc <- geocode(loc)
	arto$LOClongitude[i] <- geoloc$lon
	arto$LOClatitude[i]  <- geoloc$lat
	Sys.sleep(.25) #sleep for 250ms
}

#### let's look at a map ####
# see also https://github.com/jdrizin/artocarpus-mapping/blob/master/createMap.R
library(maps)
library(ggmap)

mapSize <- 3 # 3-21, continent to zoom, 10 is city.
mapCenter <- geocode("Sandakan, Malaysia") # this is about right

distMap <- get_map(location=c(lon=mapCenter$lon, lat=mapCenter$lat), zoom=mapSize, maptype="satellite")
printMap <- ggmap(distMap) +
	geom_point(data=arto, aes(x=LOClongitude, y=LOClatitude, colour=series)) # might be nice to add some cities too
printMap # i guess the default method is to print a plot.

#### let's try a somewhat more clever approach to geocoding ####
# just countries
dummyvec <- rep(0, length(arto$index) ) #make a dummy vector
arto$countryLat <- dummyvec             #add it as latlongs
arto$countryLon <- dummyvec
countries <- unique(sort(arto$Country)) #looks like we have 20 countries
countryLocs <- data.frame(countries, lat=rep(0,20), lon=rep(0,20), stringsAsFactors=F) #fill out an empty data frame

for(i in 1:length(countries)){
	loc <- geocode(countryLocs$countries[i]) 
	countryLocs$lat[i] <- loc$lat
	countryLocs$lon[i] <- loc$lon
}

