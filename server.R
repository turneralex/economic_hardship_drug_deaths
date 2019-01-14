library(shiny)
library(readr)
library(tidyr)
library(dplyr)
library(forcats)
library(lubridate)
library(ggplot2)
library(magrittr)

# import data
drug_eco <- read_csv("drug_eco.csv") # data on drug-related death and unemployment rate
usa_region <- read_csv("usa_region.csv") # data on USA census regions

# convert variables to factors
drug_eco[,1:2] <- drug_eco[,1:2] %>% lapply(factor)
usa_region[,1:4] <- usa_region[,1:4] %>% lapply(factor)

# add column that indexes drug related deaths to total state population
drug_eco <- drug_eco %>% 
  mutate(drug_deaths_per_100k = round(drug_deaths/(population/100000), 1))

# convert column names to lower case for join
colnames(usa_region) <- colnames(usa_region) %>% tolower()

# join data frames
drug_eco <- drug_eco %>%  
  left_join(usa_region[,c(1,3)], by = "state")

# convert data frame to long format
drug_eco_long <- drug_eco %>% 
  select(c(1:2,5:7)) %>% 
  gather(variable, value, -c(1:2,5))

# convert "variable" column to factor & add clean labels for plotting
drug_eco_long$variable <- drug_eco_long$variable %>% 
  factor(levels = c("drug_deaths_per_100k", "unemployment_rate"),
         labels = c("Drug Related Deaths per 100k Population", "Unemployment Rate %"))

# create data frame for fixing plot axis
fix_y <- data.frame(
  year = as.Date(c("1999-01-01", "2016-01-01")),
  value = c(0, 55, 0, 15),
  variable = c(rep("Drug Related Deaths per 100k Population", 2),
               rep("Unemployment Rate %", 2)),
  state = rep("group", 4)
)

shinyServer(function(input, output) {
  
  line_df <- reactive({
    if (input$region == "All Regions") {
      temp_1 <- drug_eco_long
    } else {
      temp_1 <- filter(drug_eco_long, region == input$region)
    }
  })
  
  point_df <- reactive({
    if (input$region == "All Regions" & input$year == "All Years") {
      temp_2 <- drug_eco_long
    } else if (input$region != "All Regions" & input$year == "All Years") {
      temp_2 <- filter(drug_eco_long, region == input$region)
    } else if (input$region == "All Regions" & input$year != "All Years") {
      temp_2 <- filter(drug_eco_long, year == input$year) 
    } else {
      temp_2 <- filter(drug_eco_long, region == input$region & year == input$year)
    }
  })
  
  output$plot <- renderPlot({
    p1 <- ggplot(line_df(), aes(ymd(year, truncated = 2), value, group = state, colour = variable)) +
      geom_line(size = 1) +
      scale_colour_brewer(palette = "Set1") +
      facet_wrap(~variable, scales = "free") +
      geom_blank(data = fix_y) +
      annotate("rect", ymin = 0, ymax = Inf, xmin = as.Date("2008-01-01"), xmax = as.Date("2012-01-01"), alpha = 0.2) +
      annotate("label", label = "Global Financial Crisis", size = 4, x = as.Date("2010-01-01"), y = 0) +
      scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
      labs(title = "Variable Rates Over Time",
           subtitle = "Each line represents a state",
           x = "Year",
           y = "Variable Rate",
           caption = "Data Source: CDC & BLS") +
      theme(legend.position = "none", plot.caption = element_text(size = 10))
    
    p2 <- ggplot(point_df(), 
                 aes(fct_reorder2(state, -variable, -value), value, colour = variable)) +
      geom_point(size = 3, alpha = 0.8) +
      scale_colour_brewer(palette = "Set1") +
      scale_y_continuous(limits = c(0, 50)) +
      coord_flip() + 
      labs(title = "Variable Rates by State",
           subtitle = "Each point represents a year", 
           x = "State (inc. DC)", 
           y = "Variable Rate", 
           colour = "Variable",
           caption = "Data Source: CDC & BLS") +
      theme(legend.position = "bottom", plot.caption = element_text(size = 10)) +
      guides(colour = guide_legend(title.position = "top", title.hjust = 0.5))
    
    cowplot::plot_grid(p1, p2)
  })
})
