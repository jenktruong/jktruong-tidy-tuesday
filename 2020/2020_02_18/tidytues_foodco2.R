############
# Tidy Tuesday Week 7 2020: Food Consumption and CO2 emissions
# February 18, 2020
# Jennifer Truong
############

# Attach packages

library(tidyverse)
library(janitor)
library(here)

# Read in data

food_consumption <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')
