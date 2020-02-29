############
# Tidy Tuesday Week 8 2020: Food Consumption and CO2 emissions
# February 18, 2020
# Jennifer Truong
############

#####
# 1. Attach packages
#####

library(tidyverse)
library(janitor)
library(here)
library(rvest)

#####
# 2. Read in data
#####

food_consumption <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')

# Units for Consumption (kg/person/year)
# Units for	Co2 Emission (Kg CO2/person/year)

#####
# 3. Wrangle data
#####

# Sum up total CO2 emissions per country

total_co2_table <- food_consumption %>% 
  group_by(country) %>% # Group by country
  summarize(
    sum_co2 = sum(co2_emmission)
  ) # Add total emissions

# Create subset for top 10 countries for CO2 emissions

top_10_co2 <- total_co2_table %>% 
  arrange(-sum_co2) %>% # Arrange by total emissions in descending order
  top_n(10) # Keep top 10
  
# Create subset only focusing on poultry, fish, pork, and beef 

ppfb_table <- food_consumption %>% 
  filter(food_category %in% c("Poultry", "Fish", "Pork", "Beef")) %>% 
  filter(country %in% top_10_co2$country) # Filter by country from top 10 subset dataframe

#####
# 4. Time to create bar graph!
#####

# Create bar graphs
ggplot(ppfb_table,
       aes(x = food_category,
           y = consumption)) +
  geom_col(aes(fill = food_category,
               color = food_category), # Color bars by food category
           show.legend = FALSE) + # Don't show legend 
  scale_y_continuous(expand = c(0,0)) + # Expand y-axis to avoid weird gap below 0
  facet_wrap( ~ country, # Facet by country
              nrow = 2, # 2 rows
              ncol = 5) + # 5 columns
  labs(x = "Animal Protein Types",
       y = "Consumption (kg/person/year)",
       title = "Animal Protein Consumption of Top 10 Carbon-Emitting Countries") + # Add axis labels and title
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5) # Center plot title
  )
