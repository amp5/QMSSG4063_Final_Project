# personal comp wd
setwd("/Users/alexandraplassaras/Desktop/Columbia_Courses/Spring_2016/QMSS_G4063/QMSS_G4063_Data_Visualization/Final")

# school comp wd
setwd("/Users/amp2261/Desktop/Final_Project")


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
install.packages("sp")
install.packages("maps")
install.packages("maptools")
install.packages("rworldmap")

### stopped installing here
install.packages("Rstem")


library(bitops)
library(RCurl)
library(rjson)
library(streamR)

library(RColorBrewer)
library(wordcloud)

library(NLP)
library(tm)

library(ggplot2)
library(sp)
library(maps)
library(maptools)
library(rworldmap)

library(grid)
library(stringr)
library(plyr)


# stopped loading libraries here. 
library(Rstem)
gpclibPermit()

####################################################
######## Loading and creating subsets of data ######
####################################################
load("/Users/amp2261/Desktop/wetransfer-684680/tweets.21_25.Rda")


load("tweets.27-29Feb.Rda")
load("tweets.22-26Feb.Rda")
load("tweets.01-05April.RData")
load("tweets.21-25March.Rda")
load("tweets.16-20March.Rda")
load("tweets.15-21Feb.Rda")
load("tweets.12-14Feb.Rda")
load("tweets.11-15March.RData")
load("tweets.07-11Feb.Rda")
load("tweets.06-15April.RData")
load("tweets.06-10March.Rda")
load("tweets.01-05March.Rda")


tweets_all <- rbind(tweets.02.12.2016, 
                    tweets.02.13.2016, 
                    tweets.02.14.2016, 
                    tweets.02.15.2016, 
                    tweets.02.18.2016, 
                    tweets.02.19.2016, 
                    tweets.02.20.2016, 
                    tweets.02.21.2016,
                    tweets.02.22.2016, 
                    tweets.02.23.2016, 
                    tweets.02.24.2016, 
                    tweets.02.25.2016,
                    tweets.02.26.2016, 
                    tweets.02.27.2016, 
                    tweets.02.28.2016, 
                    tweets.02.29.2016, 
                    tweets.03.11.2016,
                    tweets.03.12.2016,
                    tweets.03.13.2016,
                    tweets.03.14.2016,
                    tweets.03.15.2016,
                    tweets.03.16.2016,
                    tweets.03.17.2016,
                    tweets.03.18.2016,
                    tweets.03.19.2016,
                    tweets.03.20.2016,
                    tweets.03.21.2016, 
                    tweets.03.22.2016, 
                    tweets.03.23.2016, 
                    tweets.03.24.2016, 
                    tweets.03.25.2016, 
                
                    tweets.04.01.2016,
                    tweets.04.02.2016,
                    tweets.04.03.2016,
                    tweets.04.04.2016,
                    tweets.04.05.2016,
                    
                    
                    )


useful_info <- c("text", "id_str", "created_at", "screen_name", "place_lat", "place_lon",  "lat", "lon", "country_code", "retweeted", "retweet_count")
all_filtered <- tweets_all[useful_info]


HC <- subset (all_filtered, grepl(pattern =  "Clinton | clinton | Hillary | hillary | Hillaryclinton | hillaryclinton | Hillary Clinton | hillary clinton" , 
                                all_filtered$text, ignore.case = TRUE))
BS <- subset (all_filtered, grepl(pattern =  "Berniesanders | berniesanders | Bernie Sanders  | bernie sanders | Bernie | bernie | Sensanders | sensanders" , 
                                all_filtered$text, ignore.case = TRUE))
TC <-  subset (all_filtered, grepl(pattern =  "Cruz | cruz | Ted | ted | Tedcruz | tedcruz | Ted Cruz | ted cruz" , 
                                 all_filtered$text, ignore.case = TRUE))
DT <- subset (all_filtered, grepl(pattern =  "Donaldtrump  | donaldtrump | Donald Trump | donald trump | Trump | trump | Donald | donald | Trumpf | trumpf" , 
                                all_filtered$text, ignore.case = TRUE))
MR <- subset (all_filtered, grepl(pattern =  "Marcorubio | marcorubio | Marco Rubio | marco rubio" , 
                                all_filtered$text, ignore.case = TRUE))


# also by party
dem <- rbind(HC, BS)
rep <- rbind(TC, DT)

save (HC, file= 'HC.Rdata')
save (BS, file= 'BS.Rdata')
save (TC, file= 'TC.Rdata')
save (DT, file= 'DT.Rdata')
save (MR, file= 'MR.Rdata')
save(dem, file='dem.Rdata')
save(rep, file='rep.Rdata')
save (all_filtered, file= 'all_filtered.Rdata')

##### starting here.....after force quit .... as a sample....
#setwd("/Users/amp2261/Desktop")
load("HC.Rdata")
load("BS.Rdata")





###########################################
####### Creating Word Cloud ########
###########################################
# making word cloud function
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

###########################################
##### Building Map #######
###########################################

##### UNABLE TO GET THIS FUNCTION WORKING..........................................................................................................
#### problem installing grid package.....

map.data <- map_data("state")

map_tweets <- function(filename){
  only_coords <- filename[complete.cases(filename) ,]
  us_coords <- only_coords[only_coords$country_code == 'US',]
  points <- data.frame(x = as.numeric(us_coords$place_lon), y = as.numeric(us_coords$place_lat))
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


points <- data.frame(x = as.numeric(HC$place_lon), y = as.numeric(HC$place_lat))
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

#### functions

## TODO: GET THIS SECTION OF THE CODE (OR YOUR CODE FROM HW 3 WORKING IN GENERAL) TO WORK! THEN MAKE IT INTO A FUNCTION. 
world <- map_data("world")
US_states <- map_data("state")
ggplot()+ geom_polygon( data=world, aes(x=long, y=lat, group = group),colour="white", fill="grey10" )

##### Problem with ggplot and mapping
#### getting a formatting error....

map_gen_w <-function(filename){
  filtered_file <- filename[complete.cases(filename) ,]
  ggplot(world) + geom_map(aes(map_id = region), map = world, fill = "grey90", color = "grey50", size = 0.25) + 
    expand_limits(x = as.numeric(world$long), y = as.numeric(world$lat)) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + 
    theme_minimal() + geom_point(data = filtered_file, aes(x = as.numeric(lon), y = as.numeric(lat)), size = 1, alpha = 1/5, color = "blue")
}

map_gen_s <-function(filename){
  filtered_file <- filename[complete.cases(filename) ,]
  filtered_file <- filtered_file[filtered_file$country_code=='US',] 
  ggplot(US_states) + geom_map(aes(map_id = region), map = US_states, fill = "grey90", color = "grey50", size = 0.25) + 
    expand_limits(x = as.numeric(US_states$long), y = as.numeric(US_states$lat)) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + 
    theme_minimal() + geom_point(data = filtered_file, aes(x = as.numeric(place_lon), y = as.numeric(place_lat)), size = 1, alpha = 1/5, color = "blue")
  
}

# Counting by State
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

as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

state_mp_cnt <- function(filename){
  filtered_file <- filename[complete.cases(filename) ,]
  filtered_file <- filtered_file[filtered_file$country_code=='US',] 
  geo_pts <- c("place_lon", "place_lat")
  df_pt <-  filtered_file[geo_pts]
  df_pt$place_lon <- as.numeric.factor(df_pt$place_lon)
  
  df_pt$state <- latlong2state(df_pt)
  filtered_df <- df_pt[!(is.na(df_pt$state)),]
  count(filtered_df, "state")
  state_df <- count(filtered_df, "state")
  print("two")
  
  mapUSA <- map('state',  fill = TRUE,  plot = FALSE)
  nms <- sapply(strsplit(mapUSA$names,  ':'),  function(x)x[1])
  USApolygons <- map2SpatialPolygons(mapUSA,  IDs = nms,  CRS('+proj=longlat'))
  idx <- match(unique(nms),  state_df$state)
  dat2 <- data.frame(value = state_df$freq[idx], state = unique(nms))
  row.names(dat2) <- unique(nms)
  USAsp <- SpatialPolygonsDataFrame(USApolygons,  data = dat2)
  print("three")
  spplot(USAsp['value'], col.regions= rainbow(100, start = 3/6, end = 4/6 ))
}
##############################################
########## Sentiment Analysis ################
##############################################

lexicon <- read.csv("lexicon.csv", stringsAsFactors=F)
positive.words <- lexicon$word[lexicon$polarity=="positive"]
negative.words <- lexicon$word[lexicon$polarity=="negative"]

sentiment <- function(filename, name_csv){
  tc <- tweet_corp(filename)
  pos_count <- sum(str_count(tc, positive.words))
  neg_count <- sum(str_count(tc, negative.words))
  polarity_df = data.frame(pos_count, 
                        neg_count)
  write.csv(polarity_df, file = name_csv)

}



plot_polarity <- function(filename){
  ggplot(data=filename, aes(x=polarity, y=rate)) +
    geom_bar(stat="identity", position=position_dodge())  +
    scale_fill_brewer() +
    ggtitle("Polarity of Candidate")
}





#############################################################
# Calling functions from above here


# wordclouds
wc(HC)


# generating social network
## TBD

# building general map
## TBD

# generating maps
## First two don't work....
map_gen_w(BS)
map_gen_s(HC)
state_mp_cnt(HC)

# sentiment analysis
sentiment(HC, "HC_polarity.csv")
########
## before this we have to create rates by hand on csv file........
HC_p<- read.csv("HC_polarity_e.csv")
plot_polarity(HC_p)
