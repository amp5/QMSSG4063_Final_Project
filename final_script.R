########################################
#### Libraries Needed #######
########################################
install.packages("RCurl")
install.packages("rjson")
install.packages("streamR")
install.packages("RColorBrewer")
install.packages("wordcloud")
install.packages("NLP")
install.packages("tm")
install.packages("ggplot2")
install.packages("maps")
install.packages("maptools")
install.packages("rworldmap")

### stopped installing here
install.packages("grid")
install.packages("sp")
install.packages("plyr")
install.packages("Rstem")
install.packages("stringr")




library(RCurl)
library(bitops)
library(rjson)
library(streamR)

library(RColorBrewer)
library(wordcloud)

library(NLP)
library(tm)

library(ggplot2)
library(maps)
library(maptools)
library(rworldmap)


# stopped loading libraries here. 
library(grid)

library(sp)

library(plyr)

library(Rstem)
library(stringr)
gpclibPermit()

####################################################
######## Loading and creating subsets of data ######
####################################################
load("/Users/amp2261/Desktop/wetransfer-684680/tweets.21_25.Rda")

tweets_all <- rbind(tweets.03.21.2016, 
                    tweets.03.22.2016, 
                    tweets.03.23.2016, 
                    tweets.03.24.2016, 
                    tweets.03.25.2016)

HC_all <- subset (tweets_all, grepl(pattern =  "Clinton | clinton | Hillary | hillary | Hillaryclinton | hillaryclinton | Hillary Clinton | hillary clinton" , 
                                    tweets_all$text, ignore.case = TRUE))
BS_all <- subset (tweets_all, grepl(pattern =  "Berniesanders | berniesanders | Bernie Sanders  | bernie sanders | Bernie | bernie | Sensanders | sensanders" , 
                                    tweets_all$text, ignore.case = TRUE))
TC_all <-  subset (tweets_all, grepl(pattern =  "Cruz | cruz | Ted | ted | Tedcruz | tedcruz | Ted Cruz | ted cruz" , 
                                     tweets_all$text, ignore.case = TRUE))
DT_all <- subset (tweets_all, grepl(pattern =  "Donaldtrump  | donaldtrump | Donald Trump | donald trump | Trump | trump | Donald | donald | Trumpf | trumpf" , 
                                    tweets_all$text, ignore.case = TRUE))
MR_all <- subset (tweets_all, grepl(pattern =  "Marcorubio | marcorubio | Marco Rubio | marco rubio" , 
                                    tweets_all$text, ignore.case = TRUE))

useful_info <- c("text", "id_str", "created_at", "screen_name", "place_lat", "place_lon",  "lat", "lon", "country_code", "retweeted", "retweet_count")
HC <-  HC_all[useful_info]
BS <- BS_all[useful_info]
TC <- TC_all[useful_info]
DT <- DT_all[useful_info]
MR <- MR_all[useful_info]
all_filtered <- tweets_all[useful_info]

save (HC, file= 'HC.Rdata')
save (BS, file= 'BS.Rdata')
save (TC, file= 'TC.Rdata')
save (DT, file= 'DT.Rdata')
save (MR, file= 'MR.Rdata')
save (all_filtered, file= 'all_filtered.Rdata')


###########################################
####### Creating Word Cloud ########
###########################################

#making word cloud function
# adding in the name of the dataframe into this function will result in a worldcloud being generated
wc <- function(filename){
  filename$text <- sapply(filename$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
  TweetCorpus<-paste(unlist(filename$text), collapse =" ")
  TweetCorpus <- Corpus(VectorSource(TweetCorpus))
  TweetCorpus <- tm_map(TweetCorpus, removePunctuation)
  TweetCorpus <- tm_map(TweetCorpus, removeWords, stopwords("english"))
  #TweetCorpus <- tm_map(TweetCorpus, stemDocument)
  TweetCorpus <- tm_map(TweetCorpus, content_transformer(tolower),lazy=TRUE)
  TweetCorpus <- tm_map(TweetCorpus, PlainTextDocument)
  TweetCorpus <- tm_map(TweetCorpus, removeWords, c("https",
                                                    "https...",
                                                    "via",
                                                    "use",
                                                    "just",
                                                    "think",
                                                    "say",
                                                    "that",
                                                    "its",
                                                    "like",
                                                    "this",
                                                    "will",
                                                    "the",
                                                    "lol", 
                                                    "now", 
                                                    "one", 
                                                    "still", 
                                                    "whi",
                                                    "amp",
                                                    "let",
                                                    "ill",
                                                    "come",
                                                    "shit",
                                                    "and",
                                                    "realli",
                                                    "your",
                                                    "you",
                                                    "fuck",
                                                    "last",
                                                    "for",
                                                    "much",
                                                    "see",
                                                    "got",
                                                    "can",
                                                    "get"
  ))
  return(wordcloud(TweetCorpus, min.freq = 900,  max.words = 500, random.order = FALSE, colors = brewer.pal(4, "Dark2")))
}


# creating a tweetcorpus, just insert the db filename into function, will receive TweetCorpus as a result
# don't forget to set this to a var name
#ex. bs_tc <- tweet_corp(BS)
tweet_corp <- function(filename){
  filename$text <- sapply(filename$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
  TweetCorpus<-paste(unlist(filename$text), collapse =" ")
  TweetCorpus <- Corpus(VectorSource(TweetCorpus))
  TweetCorpus <- tm_map(TweetCorpus, removePunctuation)
  TweetCorpus <- tm_map(TweetCorpus, removeWords, stopwords("english"))
  #TweetCorpus <- tm_map(TweetCorpus, stemDocument)
  TweetCorpus <- tm_map(TweetCorpus, content_transformer(tolower),lazy=TRUE)
  TweetCorpus <- tm_map(TweetCorpus, PlainTextDocument)
  TweetCorpus <- tm_map(TweetCorpus, removeWords, c("https",
                                                    "https...",
                                                    "via",
                                                    "use",
                                                    "just",
                                                    "think",
                                                    "say",
                                                    "that",
                                                    "its",
                                                    "like",
                                                    "this",
                                                    "will",
                                                    "the",
                                                    "lol", 
                                                    "now", 
                                                    "one", 
                                                    "still", 
                                                    "whi",
                                                    "amp",
                                                    "let",
                                                    "ill",
                                                    "come",
                                                    "shit",
                                                    "and",
                                                    "realli",
                                                    "your",
                                                    "you",
                                                    "fuck",
                                                    "last",
                                                    "for",
                                                    "much",
                                                    "see",
                                                    "got",
                                                    "can",
                                                    "get"
  ))
  return(TweetCorpus)
}


######
# calling all the functions here
######

wc(HC)
tc <- tweet_corp(BS)
# wordcloud(tc, min.freq = 900,  max.words = 500, random.order = FALSE, colors = brewer.pal(4, "Dark2"))

###########################################
##### Building Map #######
###########################################

map.data <- map_data("state")

map_tweets <- function(filename){
  points <- data.frame(x = as.numeric(filename$lon), y = as.numeric(filename$lat))
  points <- points[points$y > 25, ]
  ggplot(map.data) + geom_map(aes(map_id = region), map = map.data, fill = "#fdf9f9", 
                              color = "#9d9595", size = 0.25) + expand_limits(x = map.data$long, y = map.data$lat) + 
    theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
          axis.title = element_blank(), panel.background = element_blank(), panel.border = element_blank(), 
          panel.grid.major = element_blank(), plot.background = element_blank(), 
          plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines")) + geom_point(data = points, 
                                                                                   aes(x = x, y = y), size = 1, alpha = 1/5, color = "#CC6666") 
}

map_tweets(HC)


points <- data.frame(x = as.numeric(tweets.df$lon), y = as.numeric(tweets.df$lat))
points <- points[points$y > 25, ]
## How can I get this to separate between Hillary and Sanders
ggplot(map.data) + geom_map(aes(map_id = region), map = map.data, fill = "#fdf9f9", 
                            color = "#9d9595", size = 0.25) + expand_limits(x = map.data$long, y = map.data$lat) + 
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
        axis.title = element_blank(), panel.background = element_blank(), panel.border = element_blank(), 
        panel.grid.major = element_blank(), plot.background = element_blank(), 
        plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines")) + geom_point(data = points, 
                                                                                 aes(x = x, y = y), size = 1, alpha = 1/5, color = "#CC6666") 

###########################################
####### Building a Social Network #########
###########################################


### This one is tbd.......


###########################################
####### More Mapping #########
###########################################


data <- geo_tagged_tweets

filteredData <- data[!(is.na(data$lat)) | !(is.na(data$place_lat)),]
save (filteredData, file= 'geo_filtered_data.Rdata')








world <- map_data("world")
US_states <- map_data("state")
ggplot()+ geom_polygon( data=world, aes(x=long, y=lat, group = group),colour="white", fill="grey10" )

map_gen_w <-function(filename){
  ggplot(world) + geom_map(aes(map_id = region), map = world, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = world$long, y = world$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = filename, aes(x = place_lon, y = place_lat), size = 1, alpha = 1/5, color = "blue")
}

map_gen_w(HC)

map_gen_s <-function(){}


#Clinton
ggplot(world) + geom_map(aes(map_id = region), map = world, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = world$long, y = world$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = HC, aes(x = place_lon, y = place_lat), size = 1, alpha = 1/5, color = "blue")
ggplot(US_states) + geom_map(aes(map_id = region), map = US_states, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = US_states$long, y = US_states$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = HC, aes(x = place_lon, y = place_lat), size = 1, alpha = 1/5, color = "blue")
#Sanders
ggplot(world) + geom_map(aes(map_id = region), map = world, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = world$long, y = world$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = BS, aes(x = place_lon, y = place_lat), size = 1, alpha = 1/5, color = "blue")
ggplot(US_states) + geom_map(aes(map_id = region), map = US_states, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = US_states$long, y = US_states$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = BS, aes(x = place_lon, y = place_lat), size = 1, alpha = 1/5, color = "blue")
#Cruz
ggplot(world) + geom_map(aes(map_id = region), map = world, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = world$long, y = world$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = TC, aes(x = place_lon, y = place_lat), size = 1, alpha = 1/5, color = "red")
ggplot(US_states) + geom_map(aes(map_id = region), map = US_states, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = US_states$long, y = US_states$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = TC, aes(x = place_lon, y = place_lat), size = 1, alpha = 1/5, color = "red")
#Rubio
ggplot(world) + geom_map(aes(map_id = region), map = world, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = world$long, y = world$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = MR, aes(x = place_lon, y = place_lat), size = 1, alpha = 1/5, color = "red")
ggplot(US_states) + geom_map(aes(map_id = region), map = US_states, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = US_states$long, y = US_states$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = MR, aes(x = place_lon, y = place_lat), size = 1, alpha = 1/5, color = "red")
#Trump
ggplot(world) + geom_map(aes(map_id = region), map = world, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = world$long, y = world$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = DT, aes(x = place_lon, y = place_lat), size = 1, alpha = 1/5, color = "red")
ggplot(US_states) + geom_map(aes(map_id = region), map = US_states, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = US_states$long, y = US_states$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = DT, aes(x = place_lon, y = place_lat), size = 1, alpha = 1/5, color = "red")

#class(HC)


#install.packages("rworldmap") 




useful_info <- c("text", "id_str", "created_at", "screen_name", "place_lat", "place_lon",  "lat", "lon", "country_code")
HC_us <- HC[HC$country_code=='US',] 
BS_us <- BS[BS$country_code=='US',]
TC_us <- TC[TC$country_code=='US',]
DT_us <- DT[DT$country_code=='US',]
MR_us <- MR[MR$country_code=='US',]

ggplot(US_states) + geom_map(aes(map_id = region), map = US_states, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = US_states$long, y = US_states$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = HC_us, aes(x = place_lon, y = place_lat), size = 1, alpha = 1/5, color = "blue")
ggplot(US_states) + geom_map(aes(map_id = region), map = US_states, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = US_states$long, y = US_states$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = BS_us, aes(x = place_lon, y = place_lat), size = 1, alpha = 1/5, color = "blue")
ggplot(US_states) + geom_map(aes(map_id = region), map = US_states, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = US_states$long, y = US_states$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = TC_us, aes(x = place_lon, y = place_lat), size = 1, alpha = 1/5, color = "red")
ggplot(US_states) + geom_map(aes(map_id = region), map = US_states, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = US_states$long, y = US_states$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = MR_us, aes(x = place_lon, y = place_lat), size = 1, alpha = 1/5, color = "red")
ggplot(US_states) + geom_map(aes(map_id = region), map = US_states, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = US_states$long, y = US_states$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = DT_us, aes(x = place_lon, y = place_lat), size = 1, alpha = 1/5, color = "red")

#############################
# Part 2 - Counting by State
#############################



# The single argument to this function, pointsDF, is a data.frame in which:
#   - column 1 contains the longitude in degrees (negative in the US)
#   - column 2 contains the latitude in degrees

latlong2state <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per state (plus DC, minus HI & AK)
  states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, states_sp)
  # Return the state names of the Polygons object containing each point
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
}


geo_pts <- c("place_lon", "place_lat")
HC_pt <-  HC_us[geo_pts]
BS_pt <- BS_us[geo_pts]
TC_pt <- TC_us[geo_pts]
DT_pt <- DT_us[geo_pts]
MR_pt <- MR_us[geo_pts]

all_pt <- rbind(HC_pt, BS_pt, TC_pt, DT_pt, MR_pt)

#ggplot(US_states) + geom_map(aes(map_id = region), map = US_states, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = US_states$long, y = US_states$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = HC_pt, aes(x = place_lon, y = place_lat), size = 1, alpha = 1/5, color = "blue")


HC_pt$state <- latlong2state(HC_pt)
filteredHC <- HC_pt[!(is.na(HC_pt$state)),]
count(filteredHC, "state")
state_HC <- count(filteredHC, "state")

BS_pt$state <- latlong2state(BS_pt)
filteredBS <- BS_pt[!(is.na(BS_pt$state)),]
count(filteredBS, "state")
state_BS <- count(filteredBS, "state")

TC_pt$state <- latlong2state(TC_pt)
filteredTC <- TC_pt[!(is.na(TC_pt$state)),]
count(filteredTC, "state")
state_TC <- count(filteredTC, "state")

DT_pt$state <- latlong2state(DT_pt)
filteredDT <- DT_pt[!(is.na(DT_pt$state)),]
count(filteredDT, "state")
state_DT <- count(filteredDT, "state")

MR_pt$state <- latlong2state(MR_pt)
filteredMR <- MR_pt[!(is.na(MR_pt$state)),]
count(filteredMR, "state")
state_MR <- count(filteredMR, "state")



mapUSA <- map('state',  fill = TRUE,  plot = FALSE)
nms <- sapply(strsplit(mapUSA$names,  ':'),  function(x)x[1])
USApolygons <- map2SpatialPolygons(mapUSA,  IDs = nms,  CRS('+proj=longlat'))
idx <- match(unique(nms),  state_HC$state)
dat2 <- data.frame(value = state_HC$freq[idx], state = unique(nms))
row.names(dat2) <- unique(nms)
USAsp <- SpatialPolygonsDataFrame(USApolygons,  data = dat2)
spplot(USAsp['value'], col.regions= rainbow(100, start = 3/6, end = 4/6 ))
View(state_HC)

mapUSA <- map('state',  fill = TRUE,  plot = FALSE)
nms <- sapply(strsplit(mapUSA$names,  ':'),  function(x)x[1])
USApolygons <- map2SpatialPolygons(mapUSA,  IDs = nms,  CRS('+proj=longlat'))
idx <- match(unique(nms),  state_BS$state)
dat2 <- data.frame(value = state_BS$freq[idx], state = unique(nms))
row.names(dat2) <- unique(nms)
USAsp <- SpatialPolygonsDataFrame(USApolygons,  data = dat2)
spplot(USAsp['value'], col.regions= rainbow(100, start = 3/6, end = 4/6 ))
View(state_BS)


mapUSA <- map('state',  fill = TRUE,  plot = FALSE)
nms <- sapply(strsplit(mapUSA$names,  ':'),  function(x)x[1])
USApolygons <- map2SpatialPolygons(mapUSA,  IDs = nms,  CRS('+proj=longlat'))
idx <- match(unique(nms),  state_TC$state)
dat2 <- data.frame(value = state_TC$freq[idx], state = unique(nms))
row.names(dat2) <- unique(nms)
USAsp <- SpatialPolygonsDataFrame(USApolygons,  data = dat2)
spplot(USAsp['value'], col.regions= rainbow(100, start = 3/6, end = 4/6 ))
View(state_TC)


mapUSA <- map('state',  fill = TRUE,  plot = FALSE)
nms <- sapply(strsplit(mapUSA$names,  ':'),  function(x)x[1])
USApolygons <- map2SpatialPolygons(mapUSA,  IDs = nms,  CRS('+proj=longlat'))
idx <- match(unique(nms),  state_DT$state)
dat2 <- data.frame(value = state_DT$freq[idx], state = unique(nms))
row.names(dat2) <- unique(nms)
USAsp <- SpatialPolygonsDataFrame(USApolygons,  data = dat2)
# rev(rainbow.....)
spplot(USAsp['value'], col.regions= rainbow(100, start = 3/6, end = 4/6 ))
View(state_DT)


mapUSA <- map('state',  fill = TRUE,  plot = FALSE)
nms <- sapply(strsplit(mapUSA$names,  ':'),  function(x)x[1])
USApolygons <- map2SpatialPolygons(mapUSA,  IDs = nms,  CRS('+proj=longlat'))
idx <- match(unique(nms),  state_MR$state)
dat2 <- data.frame(value = state_MR$freq[idx], state = unique(nms))
row.names(dat2) <- unique(nms)
USAsp <- SpatialPolygonsDataFrame(USApolygons,  data = dat2)
# rev(rainbow.....)
spplot(USAsp['value'], col.regions= rainbow(100, start = 3/6, end = 4/6 ))
View(state_MR)

useful_info <- c("text", "id_str", "created_at", "screen_name", "place_lat", "place_lon",  "lat", "lon", "country_code")
HC_us <- HC[HC$country_code=='US',] 
BS_us <- BS[BS$country_code=='US',]
TC_us <- TC[TC$country_code=='US',]
DT_us <- DT[DT$country_code=='US',]
MR_us <- MR[MR$country_code=='US',]

ggplot(US_states) + geom_map(aes(map_id = region), map = US_states, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = US_states$long, y = US_states$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = HC_us, aes(x = place_lon, y = place_lat), size = 1, alpha = 1/5, color = "blue")
ggplot(US_states) + geom_map(aes(map_id = region), map = US_states, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = US_states$long, y = US_states$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = BS_us, aes(x = place_lon, y = place_lat), size = 1, alpha = 1/5, color = "blue")
ggplot(US_states) + geom_map(aes(map_id = region), map = US_states, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = US_states$long, y = US_states$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = TC_us, aes(x = place_lon, y = place_lat), size = 1, alpha = 1/5, color = "red")
ggplot(US_states) + geom_map(aes(map_id = region), map = US_states, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = US_states$long, y = US_states$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = MR_us, aes(x = place_lon, y = place_lat), size = 1, alpha = 1/5, color = "red")
ggplot(US_states) + geom_map(aes(map_id = region), map = US_states, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = US_states$long, y = US_states$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = DT_us, aes(x = place_lon, y = place_lat), size = 1, alpha = 1/5, color = "red")

geo_pts <- c("place_lon", "place_lat")
HC_pt <-  HC_us[geo_pts]
ggplot(US_states) + geom_map(aes(map_id = region), map = US_states, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = US_states$long, y = US_states$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = HC_pt, aes(x = place_lon, y = place_lat), size = 1, alpha = 1/5, color = "blue")




# The single argument to this function, pointsDF, is a data.frame in which:
#   - column 1 contains the longitude in degrees (negative in the US)
#   - column 2 contains the latitude in degrees

latlong2state <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per state (plus DC, minus HI & AK)
  states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, states_sp)
  
  # Return the state names of the Polygons object containing each point
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
}

# Test the function using points in Wisconsin and Oregon.
#testPoints <- data.frame(x = c(-90, -120), y = c(44, 44))
#latlong2state(testPoints)

latlong2state(HC_pt)


HC_pt$state <- latlong2state(HC_pt)
filteredHC <- HC_pt[!(is.na(HC_pt$state)),]


count(filteredHC, "state")
state_HC <- count(filteredHC, "state")
View(state_HC)
write.csv(state_HC, file = "state_HC.csv",row.names=FALSE)


colwise(class)(state_HC)

#library(rgeos)
#library(maptools)
#library(gpclib)  # may be needed, may not be

# MAP
#us_state <- readShapeSpatial("/Users/alexandraplassaras/Desktop/Columbia_Courses/Spring_2016/QMSS_G4063/QMSS_G4063_Data_Visualization/assignment3/cb_2013_us_state_500k (1)/cb_2013_us_state_500k.shp")
# VERIFY IT LOADED PROPERLY
#plot(us_state)



#library(ggplot2)
#np_dist <- fortify(np_dist, region = "NAME_3")
#np_dist$id <- toupper(np_dist$id)  #change ids to uppercase
#ggplot() + geom_map(data = edu63, aes(map_id = District, fill = PASS.PERCENT), 
#                    map = np_dist) + expand_limits(x = np_dist$long, y = np_dist$lat)

#library(maps)
#library(maptools)
#library(ggplot2)
#library(ggmap)
#map.text("state", regions=c(state_HC$state), labels=as.character(state_HC$freq))





# maps per candidate by population
population <-read.csv("VotingPopulation.csv", header = TRUE, sep = ",", quote = "\"")

HC_pop <- merge(population,state_HC)
HC_pop$freq <- HC_pop$freq/HC_pop$pop
View(HC_pop)

BS_pop <- merge(population,state_BS)
BS_pop$freq <- BS_pop$freq/BS_pop$pop
View(BS_pop)

TC_pop <- merge(population,state_TC)
TC_pop$freq <- TC_pop$freq/TC_pop$pop
View(TC_pop)

MR_pop <- merge(population,state_MR)
MR_pop$freq <- MR_pop$freq/MR_pop$pop
View(MR_pop)

DT_pop <- merge(population,state_DT)
DT_pop$freq <- DT_pop$freq/DT_pop$pop
View(DT_pop)

# map time
mapUSA <- map('state',  fill = TRUE,  plot = FALSE)
nms <- sapply(strsplit(mapUSA$names,  ':'),  function(x)x[1])
USApolygons <- map2SpatialPolygons(mapUSA,  IDs = nms,  CRS('+proj=longlat'))
idx <- match(unique(nms),  HC_pop$state)
dat2 <- data.frame(value = HC_pop$freq[idx], state = unique(nms))
row.names(dat2) <- unique(nms)
USAsp <- SpatialPolygonsDataFrame(USApolygons,  data = dat2)
spplot(USAsp['value'], col.regions = rainbow(100, start = 3/6, end = 4/6 ))        


mapUSA <- map('state',  fill = TRUE,  plot = FALSE)
nms <- sapply(strsplit(mapUSA$names,  ':'),  function(x)x[1])
USApolygons <- map2SpatialPolygons(mapUSA,  IDs = nms,  CRS('+proj=longlat'))
idx <- match(unique(nms),  BS_pop$state)
dat2 <- data.frame(value = BS_pop$freq[idx], state = unique(nms))
row.names(dat2) <- unique(nms)
USAsp <- SpatialPolygonsDataFrame(USApolygons,  data = dat2)
spplot(USAsp['value'], col.regions = rainbow(100, start = 3/6, end = 4/6 ))        


mapUSA <- map('state',  fill = TRUE,  plot = FALSE)
nms <- sapply(strsplit(mapUSA$names,  ':'),  function(x)x[1])
USApolygons <- map2SpatialPolygons(mapUSA,  IDs = nms,  CRS('+proj=longlat'))
idx <- match(unique(nms),  TC_pop$state)
dat2 <- data.frame(value = TC_pop$freq[idx], state = unique(nms))
row.names(dat2) <- unique(nms)
USAsp <- SpatialPolygonsDataFrame(USApolygons,  data = dat2)
spplot(USAsp['value'], col.regions = rainbow(100, start = 3/6, end = 4/6 ))        


mapUSA <- map('state',  fill = TRUE,  plot = FALSE)
nms <- sapply(strsplit(mapUSA$names,  ':'),  function(x)x[1])
USApolygons <- map2SpatialPolygons(mapUSA,  IDs = nms,  CRS('+proj=longlat'))
idx <- match(unique(nms),  MR_pop$state)
dat2 <- data.frame(value = MR_pop$freq[idx], state = unique(nms))
row.names(dat2) <- unique(nms)
USAsp <- SpatialPolygonsDataFrame(USApolygons,  data = dat2)
spplot(USAsp['value'], col.regions = rainbow(100, start = 3/6, end = 4/6 ))        


mapUSA <- map('state',  fill = TRUE,  plot = FALSE)
nms <- sapply(strsplit(mapUSA$names,  ':'),  function(x)x[1])
USApolygons <- map2SpatialPolygons(mapUSA,  IDs = nms,  CRS('+proj=longlat'))
idx <- match(unique(nms),  DT_pop$state)
dat2 <- data.frame(value = DT_pop$freq[idx], state = unique(nms))
row.names(dat2) <- unique(nms)
USAsp <- SpatialPolygonsDataFrame(USApolygons,  data = dat2)
spplot(USAsp['value'], col.regions = rainbow(100, start = 3/6, end = 4/6 ))        









######################
#Extra Credit Section
######################

latlong2county <- function(pointsDF) {
  states <- map('county', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=wgs84"))
  pointsSP <- SpatialPoints(pointsDF,
                            proj4string=CRS("+proj=longlat +datum=wgs84"))
  indices <- over(pointsSP, states_sp)
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
}

#import county list
data(county.fips)

################
################
################
################
################
################
county <- latlong2county(HC_pt)
View(all_pt)

#convert county names to fip code
fips<-with(county.fips, fips[match(county, polyname)])
US_pt1 <- US_pt
US_pt1$fips <- fips 

all_pt
#count fips
US_pt <- count(all_pt,"fips")

#Transfer into a tab seperated value file to be used in D3
write.table(US_pt1, file='UStweets.tsv', quote=FALSE, sep='\t')


##############################################
########## Sentiment Analysis ################
##############################################





setwd("/Users/alexandraplassaras/Desktop/Columbia_Courses/Spring_2016/QMSS_G4063/QMSS_G4063_Data_Visualization/assignment4")
load("ParsedTweets.Rdata")

tweets_all <- rbind(t02092016_df, 
                    t02202016_df, 
                    t02232016_df, 
                    t02272016_df, 
                    t03012016_df, 
                    t03052016_df, 
                    t03062016_df, 
                    t03082016_df, 
                    t03152016_df, 
                    t03212016_df)

tweets_all$text <- sapply(tweets_all$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
#twts <- tweets_all$text
#class(twts)
#View(twts)


#Clinton, Cruz, Rubio, Sanders, Trump
HC_all <- subset (tweets_all, grepl(pattern =  "Clinton | 
                                    clinton | Hillary | 
                                    hillary | Hillaryclinton | 
                                    hillaryclinton | Hillary Clinton | 
                                    hillary clinton" , 
                                    tweets_all$text, ignore.case = TRUE))

BS_all <- subset (tweets_all, grepl(pattern =  "Berniesanders | berniesanders | 
                                    Bernie Sanders  | bernie sanders | Bernie | 
                                    bernie | Sensanders | sensanders" , 
                                    tweets_all$text, ignore.case = TRUE))

TC_all <-  subset (tweets_all, grepl(pattern =  "Cruz | cruz | Ted | ted | 
                                     Tedcruz | tedcruz | Ted Cruz | ted cruz" , 
                                     tweets_all$text, ignore.case = TRUE))

DT_all <- subset (tweets_all, grepl(pattern =  "Donaldtrump  | donaldtrump | 
                                    Donald Trump | donald trump | Trump | trump | 
                                    Donald | donald | Trumpf | trumpf" , 
                                    tweets_all$text, ignore.case = TRUE))



dem_all <- rbind(HC_all, BS_all)
rep_all <- rbind(TC_all, DT_all)

#View(dem_all)

useful_info <- c("text", "created_at", "screen_name")
## First section - separate by political party
all_twts <- tweets_all[useful_info]

dem <-  dem_all[useful_info]
rep <- rep_all[useful_info]

# Second section - separate by political candidate
HC <- HC_all[useful_info]
BS <- BS_all[useful_info]
TC <- TC_all[useful_info]
DT <- DT_all[useful_info]



save (HC, file= 'HC.Rdata')
save (BS, file= 'BS.Rdata')
save (TC, file= 'TC.Rdata')
save (DT, file= 'DT.Rdata')
save (dem, file= 'dem.Rdata')
save (rep, file= 'rep.Rdata')
save (all_twts, file= 'alltwts.Rdata')


########### After R crahses, start here:
# processing all these very large files. Will not use in future will just load old folders
load("alltwts.Rdata")
load("HC.Rdata")
load("BS.Rdata")
load("TC.Rdata")
load("DT.Rdata")
load("dem.Rdata")
load("rep.Rdata")


lexicon <- read.csv("lexicon_ps.csv", stringsAsFactors=F)
econ.words <- lexicon$word[lexicon$polarity=="economy"]
imm.words <- lexicon$word[lexicon$polarity=="immigration"]
health.words <- lexicon$word[lexicon$polarity=="health_care"]
military.words <- lexicon$word[lexicon$polarity=="military"]
gun.words <- lexicon$word[lexicon$polarity=="gun_control"]
china.words <- lexicon$word[lexicon$polarity=="china"]
trade.words <- lexicon$word[lexicon$polarity=="trade"]
race.words <- lexicon$word[lexicon$polarity=="race"]
climate.words <- lexicon$word[lexicon$polarity=="climate_change"]
religion.words <- lexicon$word[lexicon$polarity=="religion"]


### All Tweet Corpus
TweetCorpus <- paste(unlist(tweets_all$text), collapse =" ") #to get all of the tweets together
TweetCorpus <- Corpus(VectorSource(TweetCorpus))
TweetCorpus <- tm_map(TweetCorpus, PlainTextDocument)
TweetCorpus <- tm_map(TweetCorpus, removePunctuation)
TweetCorpus <- tm_map(TweetCorpus, removeWords, stopwords('english'))
TweetCorpus <- tm_map(TweetCorpus, content_transformer(tolower),lazy=TRUE)
TweetCorpus <- tm_map(TweetCorpus, PlainTextDocument)
wordcloud(TweetCorpus, max.words = 100, random.order = FALSE)

### HC Tweet Corpus
TweetCorpusHC <- paste(unlist(HC$text), collapse =" ") #to get all of the tweets together
TweetCorpusHC <- Corpus(VectorSource(TweetCorpusHC))
TweetCorpusHC <- tm_map(TweetCorpusHC, PlainTextDocument)
TweetCorpusHC <- tm_map(TweetCorpusHC, removePunctuation)
TweetCorpusHC <- tm_map(TweetCorpusHC, removeWords, stopwords('english'))
TweetCorpusHC <- tm_map(TweetCorpusHC, content_transformer(tolower),lazy=TRUE)
TweetCorpusHC <- tm_map(TweetCorpusHC, PlainTextDocument)
wordcloud(TweetCorpusHC, max.words = 100, random.order = FALSE)

### BS Tweet Corpus
TweetCorpusBS <- paste(unlist(BS$text), collapse =" ") #to get all of the tweets together
TweetCorpusBS <- Corpus(VectorSource(TweetCorpusBS))
TweetCorpusBS <- tm_map(TweetCorpusBS, PlainTextDocument)
TweetCorpusBS <- tm_map(TweetCorpusBS, removePunctuation)
TweetCorpusBS <- tm_map(TweetCorpusBS, removeWords, stopwords('english'))
TweetCorpusBS <- tm_map(TweetCorpusBS, content_transformer(tolower),lazy=TRUE)
TweetCorpusBS <- tm_map(TweetCorpusBS, PlainTextDocument)
wordcloud(TweetCorpusBS, max.words = 100, random.order = FALSE)


### TC Tweet Corpus
TweetCorpusTC <- paste(unlist(TC$text), collapse =" ") #to get all of the tweets together
TweetCorpusTC <- Corpus(VectorSource(TweetCorpusTC))
TweetCorpusTC <- tm_map(TweetCorpusTC, PlainTextDocument)
TweetCorpusTC <- tm_map(TweetCorpusTC, removePunctuation)
TweetCorpusTC <- tm_map(TweetCorpusTC, removeWords, stopwords('english'))
TweetCorpusTC <- tm_map(TweetCorpusTC, content_transformer(tolower),lazy=TRUE)
TweetCorpusTC <- tm_map(TweetCorpusTC, PlainTextDocument)
wordcloud(TweetCorpusTC, max.words = 100, random.order = FALSE)


### DT Tweet Corpus
TweetCorpusDT <- paste(unlist(DT$text), collapse =" ") #to get all of the tweets together
TweetCorpusDT <- Corpus(VectorSource(TweetCorpusDT))
TweetCorpusDT <- tm_map(TweetCorpusDT, PlainTextDocument)
TweetCorpusDT <- tm_map(TweetCorpusDT, removePunctuation)
TweetCorpusDT <- tm_map(TweetCorpusDT, removeWords, stopwords('english'))
TweetCorpusDT <- tm_map(TweetCorpusDT, content_transformer(tolower),lazy=TRUE)
TweetCorpusDT <- tm_map(TweetCorpusDT, PlainTextDocument)
wordcloud(TweetCorpusDT, max.words = 100, random.order = FALSE)


### DEM Tweet Corpus
TweetCorpusD <- paste(unlist(dem$text), collapse =" ") #to get all of the tweets together
TweetCorpusD <- Corpus(VectorSource(TweetCorpusD))
TweetCorpusD <- tm_map(TweetCorpusD, PlainTextDocument)
TweetCorpusD <- tm_map(TweetCorpusD, removePunctuation)
TweetCorpusD <- tm_map(TweetCorpusD, removeWords, stopwords('english'))
TweetCorpusD <- tm_map(TweetCorpusD, content_transformer(tolower),lazy=TRUE)
TweetCorpusD <- tm_map(TweetCorpusD, PlainTextDocument)
wordcloud(TweetCorpusD, max.words = 100, random.order = FALSE)



### REP Tweet Corpus
TweetCorpusR <- paste(unlist(rep$text), collapse =" ") #to get all of the tweets together
TweetCorpusR <- Corpus(VectorSource(TweetCorpusR))
TweetCorpusR <- tm_map(TweetCorpusR, PlainTextDocument)
TweetCorpusR <- tm_map(TweetCorpusR, removePunctuation)
TweetCorpusR <- tm_map(TweetCorpusR, removeWords, stopwords('english'))
TweetCorpusR <- tm_map(TweetCorpusR, content_transformer(tolower),lazy=TRUE)
TweetCorpusR <- tm_map(TweetCorpusR, PlainTextDocument)
wordcloud(TweetCorpusR, max.words = 100, random.order = FALSE)



all_econ <- sum(str_count(TweetCorpus, econ.words))
all_imm <- sum(str_count(TweetCorpus, imm.words))
all_health <- sum(str_count(TweetCorpus, health.words))
all_military <- sum(str_count(TweetCorpus, military.words))
all_gun <- sum(str_count(TweetCorpus, gun.words))
all_china <- sum(str_count(TweetCorpus, china.words))
all_trade <- sum(str_count(TweetCorpus, trade.words))
all_race <- sum(str_count(TweetCorpus, race.words))
all_climate <- sum(str_count(TweetCorpus, climate.words))
all_religion <- sum(str_count(TweetCorpus, religion.words))



HC_econ <- sum(str_count(TweetCorpusHC, econ.words))
HC_imm <- sum(str_count(TweetCorpusHC, imm.words))
HC_health <- sum(str_count(TweetCorpusHC, health.words))
HC_military <- sum(str_count(TweetCorpusHC, military.words))
HC_gun <- sum(str_count(TweetCorpusHC, gun.words))
HC_china <- sum(str_count(TweetCorpusHC, china.words))
HC_trade <- sum(str_count(TweetCorpusHC, trade.words))
HC_race <- sum(str_count(TweetCorpusHC, race.words))
HC_climate <- sum(str_count(TweetCorpusHC, climate.words))
HC_religion <- sum(str_count(TweetCorpusHC, religion.words))

HC_df = data.frame(HC_econ, 
                   HC_imm, 
                   HC_health, 
                   HC_military, 
                   HC_gun, 
                   HC_china,
                   HC_trade,
                   HC_race, 
                   HC_climate, 
                   HC_religion)
write.csv(HC_df, file = "HC_topics.csv")


BS_econ <- sum(str_count(TweetCorpusBS, econ.words))
BS_imm <- sum(str_count(TweetCorpusBS, imm.words))
BS_health <- sum(str_count(TweetCorpusBS, health.words))
BS_military <- sum(str_count(TweetCorpusBS, military.words))
BS_gun <- sum(str_count(TweetCorpusBS, gun.words))
BS_china <- sum(str_count(TweetCorpusBS, china.words))
BS_trade <- sum(str_count(TweetCorpusBS, trade.words))
BS_race <- sum(str_count(TweetCorpusBS, race.words))
BS_climate <- sum(str_count(TweetCorpusBS, climate.words))
BS_religion <- sum(str_count(TweetCorpusBS, religion.words))

BS_df = data.frame(BS_econ, 
                   BS_imm, 
                   BS_health, 
                   BS_military, 
                   BS_gun, 
                   BS_china,
                   BS_trade,
                   BS_race, 
                   BS_climate, 
                   BS_religion)
write.csv(BS_df, file = "BS_topics.csv")

TC_econ <- sum(str_count(TweetCorpusTC, econ.words))
TC_imm <- sum(str_count(TweetCorpusTC, imm.words))
TC_health <- sum(str_count(TweetCorpusTC, health.words))
TC_military <- sum(str_count(TweetCorpusTC, military.words))
TC_gun <- sum(str_count(TweetCorpusTC, gun.words))
TC_china <- sum(str_count(TweetCorpusTC, china.words))
TC_trade <- sum(str_count(TweetCorpusTC, trade.words))
TC_race <- sum(str_count(TweetCorpusTC, race.words))
TC_climate <- sum(str_count(TweetCorpusTC, climate.words))
TC_religion <- sum(str_count(TweetCorpusTC, religion.words))

TC_df = data.frame(TC_econ, 
                   TC_imm, 
                   TC_health, 
                   TC_military, 
                   TC_gun, 
                   TC_china,
                   TC_trade,
                   TC_race, 
                   TC_climate, 
                   TC_religion)
write.csv(TC_df, file = "TC_topics.csv")


DT_econ <- sum(str_count(TweetCorpusDT, econ.words))
DT_imm <- sum(str_count(TweetCorpusDT, imm.words))
DT_health <- sum(str_count(TweetCorpusDT, health.words))
DT_military <- sum(str_count(TweetCorpusDT, military.words))
DT_gun <- sum(str_count(TweetCorpusDT, gun.words))
DT_china <- sum(str_count(TweetCorpusDT, china.words))
DT_trade <- sum(str_count(TweetCorpusDT, trade.words))
DT_race <- sum(str_count(TweetCorpusDT, race.words))
DT_climate <- sum(str_count(TweetCorpusDT, climate.words))
DT_religion <- sum(str_count(TweetCorpusDT, religion.words))

DT_df = data.frame(DT_econ, 
                   DT_imm, 
                   DT_health, 
                   DT_military, 
                   DT_gun, 
                   DT_china,
                   DT_trade,
                   DT_race, 
                   DT_climate, 
                   DT_religion)
write.csv(DT_df, file = "DT_topics.csv")

D_econ <- sum(str_count(TweetCorpusD, econ.words))
D_imm <- sum(str_count(TweetCorpusD, imm.words))
D_health <- sum(str_count(TweetCorpusD, health.words))
D_military <- sum(str_count(TweetCorpusD, military.words))
D_gun <- sum(str_count(TweetCorpusD, gun.words))
D_china <- sum(str_count(TweetCorpusD, china.words))
D_trade <- sum(str_count(TweetCorpusD, trade.words))
D_race <- sum(str_count(TweetCorpusD, race.words))
D_climate <- sum(str_count(TweetCorpusD, climate.words))
D_religion <- sum(str_count(TweetCorpusD, religion.words))


D_df = data.frame(D_econ, 
                  D_imm, 
                  D_health, 
                  D_military, 
                  D_gun, 
                  D_china,
                  D_trade,
                  D_race, 
                  D_climate, 
                  D_religion)
write.csv(D_df, file = "D_topics.csv")







R_econ <- sum(str_count(TweetCorpusR, econ.words))
R_imm <- sum(str_count(TweetCorpusR, imm.words))
R_health <- sum(str_count(TweetCorpusR, health.words))
R_military <- sum(str_count(TweetCorpusR, military.words))
R_gun <- sum(str_count(TweetCorpusR, gun.words))
R_china <- sum(str_count(TweetCorpusR, china.words))
R_trade <- sum(str_count(TweetCorpusR, trade.words))
R_race <- sum(str_count(TweetCorpusR, race.words))
R_climate <- sum(str_count(TweetCorpusR, climate.words))
R_religion <- sum(str_count(TweetCorpusR, religion.words))

R_df = data.frame(R_econ, 
                  R_imm, 
                  R_health, 
                  R_military, 
                  R_gun, 
                  R_china,
                  R_trade,
                  R_race, 
                  R_climate, 
                  R_religion)
write.csv(R_df, file = "R_topics.csv")


###### After running rediculously

HC_df = read.csv("HC_topics.csv")
BS_df = read.csv("BS_topics.csv")
TC_df = read.csv("TC_topics.csv")
DT_df = read.csv("DT_topics.csv")
dem_df = read.csv("D_topics.csv")
rep_df = read.csv("R_topics.csv")


dem_compared = read.csv("dem_compare.csv") 

ggplot(data=dem_compared, aes(x=term, y=rate, fill=name)) +
  geom_bar(stat="identity", position=position_dodge())  +
  scale_fill_brewer() +
  ggtitle("Rate of Topics Per Candidate")



rep_compared = read.csv("rep_compare.csv") 

ggplot(data=rep_compared, aes(x=term, y=rate, fill=name)) +
  geom_bar(stat="identity", position=position_dodge())  +
  scale_fill_manual(values=c("#FF9999", "#FF6666")) +
  ggtitle("Rate of Topics Per Candidate")


party_compared = read.csv("party_compare.csv") 

ggplot(data=party_compared, aes(x=term, y=rate, fill=party)) +
  geom_bar(stat="identity", position=position_dodge())  +
  scale_fill_manual(values=c("#6699FF", "#FF6666")) +
  ggtitle("Rate of Topics Per Candidate")


