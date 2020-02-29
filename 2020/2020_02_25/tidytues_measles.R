############
# Tidy Tuesday Week 9 2020: Food Consumption and CO2 emissions
# February 25, 2020
# Jennifer Truong
############

# 1. Attach packages ----------------

library(tidyverse)

# 2. Read in data -------------------

measles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-25/measles.csv')

# 3. Wrangle data -------------------

# Find average MMR rate and total enrollment by state