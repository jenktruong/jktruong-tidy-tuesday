# ----
# Preliminary Stuff
# ----

# Attach packages

library(tidyverse)
library(here)
library(pryr)
library(visdat)
library(skimr)
library(lubridate)
library(leaflet)
library(janitor)
library(tidyr)

# Read in data

sf_trees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv')

# Clean data

sf_trees_df <- sf_trees %>%
  drop_na()

# ----
# 1. Find 5 most common tree species in SF
# ----

sf_trees_top5 <- sf_trees_df %>% 
  group_by(species) %>%  # Group by species
  count() %>%  # Count occurences for each species, goes in ascending order
  arrange(-n) %>%  # Arrange in descending order
  head(5) # Keep top 5 rows (has the most observations)

# 2. Graph location

# 3. Graph abuncance
