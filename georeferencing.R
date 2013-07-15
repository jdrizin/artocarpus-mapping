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
countries <- unique(sort(arto$Country)) #looks like we have 20 countries
countryLocs <- data.frame(countries, lat=rep(0,20), lon=rep(0,20), stringsAsFactors=F) #fill out an empty data frame

for(i in 1:length(countries)){
	loc <- geocode(countryLocs$countries[i]) 
	countryLocs$lat[i] <- loc$lat
	countryLocs$lon[i] <- loc$lon
}

#there are some weird points around

loc <- geocode("thailand") #listed as siam, but seemed to be somewhere weird 
countryLocs$lat[18] <- loc$lat
countryLocs$lon[18] <- loc$lon

loc <- geocode("java, indonesia") #listed as java, but seemed to be somewhere weird 
countryLocs$lat[9] <- loc$lat
countryLocs$lon[9] <- loc$lon

loc <- geocode("philippines") #philippine islands doesn't work. oooooook.
countryLocs$lat[16] <- loc$lat
countryLocs$lon[16] <- loc$lon

loc <- geocode("peninsular malaysia") #and i had to decide where malaya means.
countryLocs$lat[12] <- loc$lat
countryLocs$lon[12] <- loc$lon

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
arto <- read.csv("data/jarret-country.csv")
# download your data from the following url
# http://download.geonames.org/export/dump/
# I unzipped MY.zip into data/
# the data doesn't include a header. see the readme.txt for more details about the data
MY <- read.delim(file="data/MY.txt", sep='\t', header=F, stringsAsFactors=F)
names(MY) <- c("geonameid","name","asciiname","alternatenames","latitude","longitude","feature_class","feature_code","country_code","cc2","admin1_code","admin2_code","admin3_code","admin4_code","population","elevation","dem","timezone","modification")

# i'm going to be matching on certain patterns, so we're going to do some fuzzy matching and hope for the best
# so we'll create word[1].+word[2]
MY$matchname <- gsub(" ", ".+", MY$name)

artoLength <- length(arto$X)
dummyvec <- rep(NA, artoLength)
arto$localeLat <- dummyvec
arto$localeLon <- dummyvec

for(i in 1:length(MY$name)){
	matches <- agrep(MY$matchname[i], arto$LOCALITY)
	for(j in 1:length(matches)){
		eval(parse(text=paste("arto$localeLat[" , matches[j], "] <- MY$latitude[i]", sep="")))
		eval(parse(text=paste("arto$localeLon[" , matches[j], "] <- MY$longitude[i]", sep="")))
		print(j)
	}
	print(i)
}
