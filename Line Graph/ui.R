library(shiny)
# Rely on the 'WorldPhones' dataset in the datasets
# package (which generally comes preloaded).
library(datasets)

tags$style(type="text/css",
           ".shiny-output-error { visibility: hidden; }",
           ".shiny-output-error:before { visibility: hidden; }"
)

# Define the overall UI
shinyUI(
  
  # Use a fluid Bootstrap layout
  fluidPage(    
    
    
    # Give the page a title
    titlePanel("Sentiment Count by Day"),
    
    
    checkboxGroupInput("checkGroup", label = h3("Choose your candidate"),
                       choices = list("Hillary Clinton" = "Clinton", "Bernie Sanders" = "Bernie", "Donald Trump" = "Trump", "Ted Cruz" = "Cruz"),
                       selected = "Hillary Clinton"),
    checkboxGroupInput("checkGroup2", label = h4("Highlight event"), 
                       choices = list("Primary" = "primary", "Democratic Debate" = "dem_debate", "Republican Debate" = "rep_debate"),
                       selected = 1),
    hr(),
    
    #Create a spot for the line plot
    mainPanel(
      plotOutput("countPlot")  
     )
    
  ))
    
   