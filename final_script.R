# personal comp wd
setwd("/Users/alexandraplassaras/Desktop/Columbia_Courses/Spring_2016/QMSS_G4063/QMSS_G4063_Data_Visualization/Final")

# school comp wd
setwd("/Users/amp2261/Desktop/Final_Project")

########################################
#### Libraries Needed #######
########################################
#install.packages("RCurl")
#install.packages("rjson")
#install.packages("streamR")
#install.packages("RColorBrewer")
#install.packages("wordcloud")
#install.packages("NLP")
#install.packages("tm")
#install.packages("ggplot2")
#install.packages("sp")
#install.packages("maps")
#install.packages("maptools")
#install.packages("rworldmap")
### stopped installing here
#install.packages("Rstem")


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
load("tweets_all_sent_mapped_r.Rdata")
tweetsUS <- tweets_all_sent_mapped

subset_tw <- function(filename){
  filename$text <- sapply(filename$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
  HC <- subset (filename, grepl(pattern =  "Clinton | clinton | Hillary | hillary | Hillaryclinton | hillaryclinton | Hillary Clinton | hillary clinton" , 
                                filename$text, ignore.case = TRUE))
  BS <- subset (filename, grepl(pattern =  "Berniesanders | berniesanders | Bernie Sanders  | bernie sanders | Bernie | bernie | Sensanders | sensanders" , 
                                filename$text, ignore.case = TRUE))
  TC <-  subset (filename, grepl(pattern =  "Cruz | cruz | Ted | ted | Tedcruz | tedcruz | Ted Cruz | ted cruz" , 
                                 filename$text, ignore.case = TRUE))
  DT <- subset (filename, grepl(pattern =  "Donaldtrump  | donaldtrump | Donald Trump | donald trump | Trump | trump | Donald | donald | Trumpf | trumpf" , 
                                filename$text, ignore.case = TRUE))
  MR <- subset (filename, grepl(pattern =  "Marcorubio | marcorubio | Marco Rubio | marco rubio" , 
                                filename$text, ignore.case = TRUE))
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
}

subset_tw(tweetsUS)


load("HC.Rdata")
load("BS.Rdata")
load("TC.Rdata")
load("DT.Rdata")
load("dem.Rdata")
load("rep.Rdata")

testHC <- head(HC, 1000)
testBS <- head(BS, 1000)
testTC <- head(TC, 1000)
testDT <- head(DT, 1000)
testdem <- head(dem, 1000)
testrep <- head(rep, 1000)



###########################################
####### Creating Word Cloud ########
###########################################
# creating a tweetcorpus, just insert the db filename into function, will receive TweetCorpus as a result
# don't forget to set this to a var name
#ex. bs_tc <- tweet_corp(BS)
tc <- function(filename){
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

wc_t <- function(filename){
  return(wordcloud(tweet_corp(filename), min.freq = 900,  max.words = 500, random.order = FALSE, colors = brewer.pal(4, "Dark2")))
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
  points <- data.frame(x = as.numeric(as.character(us_coords$place_lon)), y = as.numeric(as.character(us_coords$place_lat)))
  points <- points[points$y > 25, ]
  ggplot(map.data) + geom_map(aes(map_id = region), map = map.data, fill = "#fdf9f9", 
                              color = "#9d9595", size = 0.25) + expand_limits(x = map.data$long, y = map.data$lat) + 
    theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
          axis.title = element_blank(), panel.background = element_blank(), panel.border = element_blank(), 
          panel.grid.major = element_blank(), plot.background = element_blank(), 
          plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines")) + geom_point(data = points, 
                                                                                   aes(x = x, y = y), size = 1, alpha = 1/5, color = "#CC6666") 
}

###########################################
####### Building a Social Network #########
###########################################
Force_clinton <- read.csv("force_clinton.csv", header = TRUE, sep = ",", quote = "\"",
                          dec = ".", fill = TRUE, comment.char = "")

Force_bernie <- read.csv("force_sanders.csv", header = TRUE, sep = ",", quote = "\"",
                         dec = ".", fill = TRUE, comment.char = "")

Force_trump <- read.csv("force_trump.csv", header = TRUE, sep = ",", quote = "\"",
                        dec = ".", fill = TRUE, comment.char = "")

Force_cruz <- read.csv("force_cruz.csv", header = TRUE, sep = ",", quote = "\"",
                       dec = ".", fill = TRUE, comment.char = "")

output$simple_Clinton <- renderSimpleNetwork({
  src <- Force_clinton$source
  target <- Force_clinton$target
  networkData <- data.frame(src, target) 
  simpleNetwork(networkData)
  
})

output$simple_Bernie <- renderSimpleNetwork({
  src <- Force_bernie$source
  target <- Force_bernie$target
  networkData <- data.frame(src, target) 
  simpleNetwork(networkData)
  
})

output$simple_Cruz <- renderSimpleNetwork({
  src <- Force_cruz$source
  target <- Force_cruz$target
  networkData <- data.frame(src, target) 
  simpleNetwork(networkData)
  
})

output$simple_Trump <- renderSimpleNetwork({
  src <- Force_trump$source
  target <- Force_trump$target
  networkData <- data.frame(src, target) 
  simpleNetwork(networkData)
  
})


###########################################
####### More Mapping #########
###########################################
world <- map_data("world")
US_states <- map_data("state")

map_gen_w <-function(filename){
  filtered_file <- filename[complete.cases(filename) ,]
  ggplot(world) + geom_map(aes(map_id = region), map = world, fill = "grey90", color = "grey50", size = 0.25) + 
    expand_limits(x = as.numeric(as.character(world$long)), y = as.numeric(as.character(world$lat))) + 
    scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + 
    geom_point(data = filtered_file, aes(x = as.numeric(as.character(lon)), y = as.numeric(as.character(lat))), size = 1, alpha = 1/5, color = "blue")
}

map_gen_s <-function(filename){
  filtered_file <- filename[complete.cases(filename) ,]
  filtered_file <- filtered_file[filtered_file$country_code=='US',] 
  ggplot(US_states) + geom_map(aes(map_id = region), map = US_states, fill = "grey90", color = "grey50", size = 0.25) + 
    expand_limits(x = as.numeric(as.character(US_states$long)), y = as.numeric(as.character(US_states$lat))) + 
    scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + 
    geom_point(data = filtered_file, aes(x = as.numeric(as.character(place_lon)), y = as.numeric(as.character(place_lat))), size = 1, alpha = 1/5, color = "blue")
  
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
# key terms lexicon
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

tc_countT <- function(filename, fname, person){
  econ <- sum(str_count(filename, econ.words))
  imm <- sum(str_count(filename, imm.words))
  health <- sum(str_count(filename, health.words))
  military <- sum(str_count(filename, military.words))
  gun <- sum(str_count(filename, gun.words))
  china <- sum(str_count(filename, china.words))
  trade <- sum(str_count(filename, trade.words))
  race <- sum(str_count(filename, race.words))
  climate <- sum(str_count(filename, climate.words))
  religion <- sum(str_count(filename, religion.words))
  
  fn_df = data.frame(econ, 
                     imm, 
                     health, 
                     military, 
                     gun, 
                     china,
                     trade,
                     race, 
                     climate, 
                     religion)
  write.csv(fn_df, file = fname)
  return(cnvrt_df(fn_df, person))
}

cnvrt_df <- function(filename, nameC){
  filename$X <- NULL
  filename <-t(filename)
  filename <- data.frame(filename)
  names(filename)[1]<-paste("num")
  filename$term <- rownames(filename)
  filename$name <- nameC
  filename$rate <- filename$num / sum(filename$num)
  return(filename)
}

term_plots <- function(data, title, color){
  ggplot(data=data, aes(x=term, y=rate, fill=name)) +
    geom_bar(stat="identity", position=position_dodge())  +
    scale_fill_manual(values=color) +
    ggtitle(title)
}

#positivity key terms lexicon
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

########################### Calling functions ##################################

# wordclouds
wc_t(testHC)
wc_t(testBS)
wc_t(testTC)
wc_t(testDT)
wc_t(testdem)
wc_t(testrep)

# generating social network
## TBD

# building general map
map_tweets(testHC)
map_tweets(testBS)
map_tweets(testTC)
map_tweets(testDT)
map_tweets(testdem)
map_tweets(testrep)


# generating maps
# world map not that useful for us....
map_gen_w(testHC)

map_gen_s(testHC)
map_gen_s(testBS)
map_gen_s(testTC)
map_gen_s(testDT)
map_gen_s(testdem)
map_gen_s(testrep)



#problem with this function
state_mp_cnt(testHC)

# sentiment analysis

# for terms
final_countHC <- tc_countT(tc(testHC) , "HC_topics.csv", "Hillary Clinton")
final_countBS <- tc_countT(tc(testBS) , "BS_topics.csv", "Bernie Sanders")
final_countTC <- tc_countT(tc(testTC) , "TC_topics.csv", "Ted Cruz")
final_countDT <- tc_countT(tc(testDT) , "DT_topics.csv", "Donald Trump")
final_count_dem <- tc_countT(tc(testdem) , "dem_topics.csv", "Democrats")
final_count_rep <- tc_countT(tc(testrep) , "rep_topics.csv", "Republican")

final_count_dems <- rbind(final_countHC, final_countBS)
final_count_reps <- rbind(final_countTC, final_countDT)
final_count_parties <- rbind(final_count_dem, final_count_rep)

#dems
color1 <- c("#99CCFF", "#003399")
#reps
color2 <- c("#FF9999", "#FF6666")
#both parties
color3 <- c("#6699FF", "#FF6666")

term_plots(final_count_dems, "Rate of Topics per Democratic Candidate", color1)
term_plots(final_count_reps, "Rate of Topics per Republican Candidate", color2)
term_plots(final_count_parties, "Rate of Topics per Political Party", color3)



###### fix this function part....
# for neg and pos
sentiment(HC, "HC_polarity.csv")
########
## before this we have to create rates by hand on csv file........
HC_p<- read.csv("HC_polarity_e.csv")
plot_polarity(HC_p)
