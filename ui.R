library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Explore Economic Hardship & Drug Related Death in America"),
  
  inputPanel(
    selectInput("region", 
                "Choose a region:", 
                choices = c("All Regions", "Midwest", "Northeast", "South", "West"),
                selected = "All Regions"),
    selectInput("year", 
                "Choose a year:", 
                choices = c("All Years", sapply(1999:2016, as.character)),
                selected = "All Years")
  ),
  plotOutput("plot", height = 600)
)
)
