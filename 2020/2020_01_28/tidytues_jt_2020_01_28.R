# ----
# Preliminary Stuff
# ----

# Attach packages

library(tidyverse)
library(here)
library(janitor)
library(tidyr)
library(sf)
library(transformr)
library(gridExtra)

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

# Go back to data frame and select only rows with those 5 species

sf_trees_select <- sf_trees_df %>% 
  filter(species %in% c("Lophostemon confertus :: Brisbane Box",
                        "Tristaniopsis laurina :: Swamp Myrtle",
                        "Arbutus 'Marina' :: Hybrid Strawberry Tree",
                        "Platanus x hispanica :: Sycamore: London Plane",
                        "Magnolia grandiflora :: Southern Magnolia"))

# ----
# 2. Graph location
# ----

# Make sure R reads longitude and latitude data as spatial data

sf_trees_spatial <- st_as_sf(sf_trees_select, 
                             coords = c("longitude","latitude")) %>% 
  select(species,geometry)

# Plot points - just to see what it's like!

plot(sf_trees_spatial)

# Read in SF neighborhood boundary shapefile

sf_border <- read_sf(here::here("SF Find Neighborhoods", "ca_state_border"), layer = "CA_State_TIGER2016")

plot(ca_border)

# Specify projection

# 3. Graph abuncance
