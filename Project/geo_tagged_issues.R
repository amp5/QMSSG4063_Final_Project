## package management
packages <- c("RCurl", "rjson", "streamR", "RColorBrewer", "wordcloud", 
              "NLP", "tm", "ggplot2", "sp", "maps", "maptools", 
              "rworldmap","Rstem", "dplyr", "stringr")

sapply(packages, function(package) if (!(package %in% row.names(installed.packages()))){
  install.packages(package, dependencies = TRUE)
})

lapply(packages, require, character.only = TRUE)

## loading in the other groups data 
load("data/tweets_all_sent_mapped_r.Rdata")

## retrieving some subsetting functions from AP's github
## https://github.com/amp5/QMSSG4063_Final_Project/blob/master/final_script.R

## substting the big dataset based on candidates and party
subset_tw <- function(filename){
  ## changing encodings of text to ASCII
  filename$text <- sapply(filename$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
  HC <- subset(filename, grepl(pattern =  "Clinton | clinton | Hillary | hillary | Hillaryclinton | hillaryclinton | Hillary Clinton | hillary clinton" , 
                               filename$text, ignore.case = TRUE))
  BS <- subset(filename, grepl(pattern =  "Berniesanders | berniesanders | Bernie Sanders  | bernie sanders | Bernie | bernie | Sensanders | sensanders" , 
                               filename$text, ignore.case = TRUE))
  TC <-  subset(filename, grepl(pattern =  "Cruz | cruz | Ted | ted | Tedcruz | tedcruz | Ted Cruz | ted cruz" , 
                                filename$text, ignore.case = TRUE))
  DT <- subset(filename, grepl(pattern =  "Donaldtrump  | donaldtrump | Donald Trump | donald trump | Trump | trump | Donald | donald | Trumpf | trumpf" , 
                               filename$text, ignore.case = TRUE))
  MR <- subset(filename, grepl(pattern =  "Marcorubio | marcorubio | Marco Rubio | marco rubio" , 
                               filename$text, ignore.case = TRUE))
  # also by party
  dem <- rbind(HC, BS)
  rep <- rbind(TC, DT)
  
  HC$Date <- trunc(as.POSIXct(HC$created_at, format="%a %b %d %H:%M:%S %z %Y"), "day")
  BS$Date <- trunc(as.POSIXct(BS$created_at, format="%a %b %d %H:%M:%S %z %Y"), "day")
  TC$Date <- trunc(as.POSIXct(TC$created_at, format="%a %b %d %H:%M:%S %z %Y"), "day")
  DT$Date <- trunc(as.POSIXct(DT$created_at, format="%a %b %d %H:%M:%S %z %Y"), "day")
  dem$Date <- trunc(as.POSIXct(dem$created_at, format="%a %b %d %H:%M:%S %z %Y"), "day")
  rep$Date <- trunc(as.POSIXct(rep$created_at, format="%a %b %d %H:%M:%S %z %Y"), "day")
  
  
  save(HC, file= 'data/HC.Rdata')
  save(BS, file= 'data/BS.Rdata')
  save(TC, file= 'data/TC.Rdata')
  save(DT, file= 'data/DT.Rdata')
  save(MR, file= 'data/MR.Rdata')
  save(dem, file= 'data/dem.Rdata')
  save(rep, file= 'data/rep.Rdata')
}

## function to save all the candidate and party tweets seperately
subset_tw(tweets_all_sent_mapped)

## removing the big tweets file to improve speed of R session 
rm(tweets_all_sent_mapped)

##tweet corpus function
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
  TweetCorpus <- tm_map(TweetCorpus, content_transformer(tolower),lazy=TRUE)
  TweetCorpus <- tm_map(TweetCorpus, PlainTextDocument)
  return(TweetCorpus)
}

##doing some sentiment analysis 
issues <- function(filename, name_csv){
  lexicon <- read.csv("lexicon/Issue_Lexicon.csv", colClasses = rep("character", 15))
  issues_scores <- data.frame()
  
  Dates <- unique(filename$Date)
  
  for(i in 1:length(Dates)){
    tc <- tc(filename[filename$Date == Dates[i], ])
    for (issue in names(lexicon)){
      issues_scores[as.character(Dates[i]), issue] <- sum(str_count(tc, lexicon[,issue]), na.rm = T)
    }
  } 
  
  write.csv(issues_scores, file = name_csv) 
}

## getting candidate level numbers
load("data/BS.Rdata")
issues(BS, "issues_BS.csv")

load("data/DT.Rdata")
issues(DT, "issues_DT.csv")

load("data/HC.Rdata")
issues(HC, "issues_HC.csv")

load("data/TC.Rdata")
issues(TC, "issues_TC.csv")

## Combining the candidate level dataframes
issues_BS <- read.csv("issues_BS.csv")
issues_BS$Candidate <- "Sanders"
issues_DT <- read.csv("issues_DT.csv")
issues_DT$Candidate <- "Trump" 
issues_HC <- read.csv("issues_HC.csv")
issues_HC$Candidate <- "Clinton"
issues_TC <- read.csv("issues_TC.csv")
issues_TC$Candidate <- "Cruz"

issues <- rbind(issues_BS, issues_HC, issues_DT, issues_TC)
write.csv(issues, file = "issues.csv")
