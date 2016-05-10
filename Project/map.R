## installing required packages

packages <- c("ggmap",             
              "rgdal", 
              "rgeos", 
              "maptools", 
              "dplyr", 
              "tidyr", 
              "tmap", 
              "maps")

new_packages <- packages[!packages %in% installed.packages()]

if (length(new_packages) > 0){
  install.packages(new_packages, dependencies = TRUE)
}

library(maps)
library(maptools)
##Alexandra's plot function modified

load("sentiment.rda")

##Alexandra's plot function modified

plot_map <- function(data_set, Candidate){
  max_score <- max(data_set$score)
  min_score <- abs(min(data_set$score))
  print(min_score)
  print(max_score)
  print(3/6 + (1/12*(1-min_score)))
  print(4/6  - (1/12*(1-max_score)))      
  mapUSA <- map('state',  fill = TRUE,  plot = FALSE)
  nms <- sapply(strsplit(mapUSA$names,  ':'),  function(x)x[1])
  USApolygons <- map2SpatialPolygons(mapUSA,  IDs = nms,  CRS('+proj=longlat'))
  idx <- match(unique(nms),  data_set$State)
  dat2 <- data.frame(value = data_set$score[idx], state = unique(nms))
  row.names(dat2) <- unique(nms)
  USAsp <- SpatialPolygonsDataFrame(USApolygons,  data = dat2)
  ##spplot(USAsp['value'], col.regions= rainbow(100, start = 3/6 , end = 4/6)) 
  spplot(USAsp['value'], col.regions= rainbow(100, start = 3/6 + (1/12*(1-min_score)), end = 4/6  - (1/12 * (1-max_score))), main=list(label=Candidate,cex=3)) 
}

BS_plot <- plot_map(sentiment[sentiment$Candidate == "Sanders", ], "Sanders")
HC_plot <- plot_map(sentiment[sentiment$Candidate == "Clinton", ], "Clinton")
DT_plot <- plot_map(sentiment[sentiment$Candidate == "Trump", ], "Trump")
TC_plot <- plot_map(sentiment[sentiment$Candidate == "Cruz", ], "Cruz")

grid.arrange(BS_plot,HC_plot, DT_plot, TC_plot, ncol = 2)
plot_map(sentiment)
