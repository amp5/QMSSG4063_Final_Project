## importing the required libraries
library(shiny)
library(reshape)
library(ggplot2)
library(dplyr)
library(scales)

## reading in the data about sentiment scores
data <- read.csv("Data.csv", header = TRUE, sep = ",", quote = "\"")
df2 <- melt(data = data, id.vars = "Days")

## generating an important event data frame
Primaries <- data.frame(day = c(3,14,21,24,28,29,31,38,45,49), type = "primary", col = "black")
Dem_Debates <- data.frame(day = c(5,29,32), type = "dem_debate", col = "blue")
Rep_Debates <- data.frame(day = c(7,19,26,33,44), type = "rep_debate", col = "red")

important_events <- rbind(Primaries, Dem_Debates, Rep_Debates)


## Define a server for the Shiny app
shinyServer(function(input, output) {
  
  # Fill in the spot we created for a plot
  output$countPlot <- renderPlot({
    
    subset <- filter(df2, variable %in% input$checkGroup)
    subset$Days <- as.Date(subset$Days, format = "%m/%d/%Y")
    ggplot(data = subset, aes(x = Days, y = value, colour = variable)) + 
            geom_line(size=1, linetype = 1, alpha = 0.8) +
      #Primaries
      geom_vline(size=0.5, alpha = 0.5, 
                 xintercept = as.numeric(subset$Days[filter(important_events, type %in% input$checkGroup2)$day])) +
      #Republican Debates
      #geom_vline(color = "red", size=0.5, alpha = 0.5, 
      #           xintercept = as.numeric(subset$Days[c(7,19,26,33,44)])) +
      #Democratic Debates
      #geom_vline(color = "blue", size=0.5, alpha = 0.5, 
      #           xintercept = as.numeric(subset$Days[c(5,29,32)])) +
      
      geom_point(shape = 5) + 
      #scale_x_datetime("Dates", breaks= date_breaks("1 day"), labels = date_format("%d/%m")) + 
      scale_x_date("Dates", breaks=date_breaks("1 day"), labels = date_format("%d-%b")) + 
      scale_y_continuous("Sentiment Ratio Score") + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
  })
})  

