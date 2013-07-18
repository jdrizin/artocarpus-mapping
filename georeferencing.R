#some data munging for figuring this junk out. 

#### load packages ####
library(ggmap)

#### load data ####
arto <- read.csv("data/jarrett.csv", as.is=T)
arto$index <- 1:1628
arto <- arto[arto$Country != "Cultivated", ] # remove the cultivated records

#### try to geocode more specific stuff.####
looplen  <- length(arto$LOCALITY) # this is about 1600. you have 2500 from google
dummyvec <- rep(0, looplen)

arto$LOClatitude  <- dummyvec # populate the df
arto$LOClongitude <- dummyvec

for(i in 1:looplen) {
	loc <- paste(arto$more.specific[i], arto$Country[i])
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
countries <- unique(sort(arto$Country)) #looks like we have 20 countries
countryLocs <- data.frame(countries, lat=rep(0,20), lon=rep(0,20), stringsAsFactors=F) #fill out an empty data frame

for(i in 1:length(countries)){
	loc <- geocode(countryLocs$countries[i]) 
	countryLocs$lat[i] <- loc$lat
	countryLocs$lon[i] <- loc$lon
}


##### build test map: how did it do? #####
mapCenter <- geocode("Sandakan, Malaysia") # this is about right
mapSize <- 3 # 3-21, continent to zoom, 10 is city.

distMap <- get_map(location=c(lon=mapCenter$lon, lat=mapCenter$lat), zoom=mapSize, maptype="satellite")
printMap <- ggmap(distMap) + 
	geom_point(data=countryLocs, aes(x=lon, y=lat)) +
	geom_text (data=countryLocs, aes(x=lon, y=lat, label=countries))
	# might be nice to add some cities too
printMap # no out-of-bounds points.

# ok, so countryLocs now seems to have good mappings for countries -> latlong
# now it's time to populate the original table with this.
# begin merge()
names(countryLocs)[2] <- "countryLat" #fix the names
names(countryLocs)[3] <- "countryLon"
arto <- merge(arto, countryLocs, by.x="Country", by.y="countries")

#all merged. let's write the data to a new file
write.csv(arto, file="data/jarret-country.csv")

#### locality attempts ####
arto <- read.csv("data/jarret-country.csv", stringsAsFactors=F)
arto$LOCALITY[arto$LOCALITY==""] <- NA

# download your data from the following url
# http://download.geonames.org/export/dump/
# I unzipped MY.zip into data/
# the data doesn't include a header. see the readme.txt for more details about the data
MY <- read.delim(file="data/MY.txt", sep='\t', header=F, stringsAsFactors=F)
names(MY) <- c("geonameid","name","asciiname","alternatenames","latitude","longitude","feature_class","feature_code","country_code","cc2","admin1_code","admin2_code","admin3_code","admin4_code","population","elevation","dem","timezone","modification")

# i'm going to be matching on certain patterns, so we're going to do some fuzzy matching and hope for the best
# so we'll create word[1].+word[2]
MY$matchname <- gsub(" ", ".+", MY$name)

library(geonames)

artoLength <- length(arto$X)
dummyvec <- rep(NA, artoLength)
arto$localeLat <- dummyvec
arto$localeLon <- dummyvec

#i think something funny goes on with this function and NAs. strip them out
artoTrim <- arto[complete.cases(arto$LOCALITY),]

#what I need to do is assign modern countries and chop down dataframes to make this faster
#otherwise my ugly loop will take days to run
oldcountries <- unique(sort(arto$Country))
country <- data.frame(old=oldcountries, new=oldcountries, stringsAsFactors=F) #dummy

#add country codes
country$new[c(1,7, 15)] <- "IN" #andaman and nicobar are indian too
country$new[c(2,4, 9, 10, 13, 14,20)] <- "ID" #most of borneo is indonesia, and celebes, java, sundra, moluccas, new guinea, sumatra
country$new[19] <- "SB" #solomon islands
country$new[18] <- "TH" #siam is thailand now
country$new[c(16,17)] <- "PH" #phillippines
country$new[c(3, 11)] <- "MM" #burma is now myanmar
country$new[12] <- "MY" #i guess malaya is malaysia?
country$new[5] <- "LK" #ceylon is sri lanka
country$new[6] <- "CN"
country$new[4] <- "ID"
#i know i missed indochina. i don't know what to do about it.

#now smoosh them together
artoTrim <- merge(artoTrim, country, by.x="Country", by.y="old")

for(i in 1:length(artoTrim$new)){
	GNsearch(name=artoTrim$LOCALITY[i], country=artoTrim$new[i])
}

#### figure out if it's in the right country ####
#code snagged from https://stat.ethz.ch/pipermail/r-sig-geo/2010-December/010354.html
library(rgdal) #needs gdal-dev and libproj-dev

setwd(tempdir())
download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/cultural/ne_110m_admin_0_countries.zip",destfile = "temp")
unzip(zipfile = "temp")
w <- readOGR(tempdir(), 'ne_110m_admin_0_countries')
locs <- data.frame(arto$LOClongitude, arto$LOClatitude)
locs <- locs[complete.cases(locs),]
pointSP <- SpatialPoints(coords=locs, proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

countries <- over(pointSP, w)
countries <- with(countries, data.frame(sovereignt, admin, name_long, continent))
locs <- data.frame(locs, countries)
locs <- droplevels.data.frame(locs) #wipe out extraneous levels. makes str() nicer
#ok, now we should look for weird stuff!
