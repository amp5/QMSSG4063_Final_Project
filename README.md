# QMSSG4063_Final_Project
Final Project Looking at Tweets During Presidential Primaries

The final paper with additional information and visulizations can be found in this repo uner 'QMSSFinalReport.pdf'

## Background
This data consists of Tweets with specific hashtags collected from February 7th, 2016 to April 2nd, 2016 that referred to the presidential primaries that were taking place earlier this year. Below are links to Data Processing and Data Visualization’s github page as well as a link to the original JSON data containing all tweets scraped. 

-	https://github.com/hassanpour/QMSS_G4063  
-	https://www.dropbox.com/sh/zyy9tsvibrl4d63/AAAQ6D3h0Kksxb8EeVH2RSSAa/tweets_geo_all.json?dl=0# 

After filtering out only Tweets sent in the US the data set is approximately 1.37 GB worth of tweets were collected during this time period.  Of the tweets that were collected, 1,816,475 were captured that contained geo-location data. The geo-location attribute will be useful to filter out all tweets that were not sent from the United States. The tweets used in this study were scraped via Twitter’s Streaming API during a 56 day period and information regarding both meta data about the tweets as well as the tweet and user data were collected. A total of 50 unique variables associated for each tweet have been collected but this research will only look at a small subset of these variables. 
The tweets were selected from Twitter’s API based on their reference to one of any of the following candidates – Hillary Clinton, Bernie Sanders, Ted Cruz, Donald Trump, and Marco Rubio. Nicknames and references to particular candidates were also included such as Trumpf, Hillary and Cruz.  

#### Breakdown of Identifiers for each Candidate
*Hillary Clinton:*	Clinton, clinton, Hillary, hillary, Hillaryclinton, hillaryclinton, Hillary Clinton, hillary clinton
*Bernie Sanders:*	Berniesanders, berniesanders, Bernie Sanders, bernie sanders, Bernie, Bernie, Sensanders, sensanders
*Ted Cruz:* Cruz, cruz, Ted, ted, Tedcruz, tedcruz, Ted Cruz, ted cruz
*Donald Trump:*	Donaldtrump, donaldtrump, Donald Trump, donald trump, Trump, trump, Donald, Donald, Trumpf, trumpf
*Marco Rubio:*	Marcorubio, marcorubio, Marco Rubio, marco rubio


## Vizualizations
Various vizualizations that were created for this project can be seen on the website below. In addition, see the giphys here to view short demonstrations.

#### Issues Per Candidate
![](https://cloud.githubusercontent.com/assets/5368361/22612886/ad9734e0-ea41-11e6-81fc-8ce47a4db1ca.gif)

#### Topics Over Time
![](https://cloud.githubusercontent.com/assets/5368361/22612883/a8f3de7a-ea41-11e6-817a-307476e71709.gif)

#### Topics by Political Party
![](https://cloud.githubusercontent.com/assets/5368361/22612879/a16f9a86-ea41-11e6-9fe9-23db0c28b60d.gif)

#### Topics Over Time (Candidate)
![](https://cloud.githubusercontent.com/assets/5368361/22612881/a4cf1238-ea41-11e6-80b6-dd5a4251bc66.gif)


## Link to the website
http://www.columbia.edu/~amp2261/D3/index.html
