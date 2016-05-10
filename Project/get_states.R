install.packages(c("sp", "maps", "maptools", "dplyr"))
## CODE Obtained from stack overflow

library(sp)
library(maps)
library(maptools)
library(dplyr)

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

## reading in data
load("tweets_all_sent_mapped_r.Rdata")

## checking the origin of tweets 
tweets_geo_points <- data.frame(x = tweets_all_sent_mapped$X, y = tweets_all_sent_mapped$Y)

tweets_states <- latlong2state(tweets_geo_points)
tweets_all_sent_mapped$State <- tweets_states

table(tweets_states)
## checking the umber nas
sum(is.na(tweets_states))

save(tweets_all_sent_mapped, file = "tweets_all_sent_mapped_state.Rdata")
