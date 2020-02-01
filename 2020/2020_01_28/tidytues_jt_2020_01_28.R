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
# 2. Graph location of trees in SF by creating a map
# ----

# Make sure R reads longitude and latitude data as spatial data

sf_trees_spatial <- st_as_sf(sf_trees_select, 
                             coords = c("longitude","latitude")) %>% 
  select(species,geometry)

plot(sf_trees_spatial)

st_crs(sf_trees_spatial) <- 4326

# Read in SF neighborhood boundary shapefile

sf_border <- read_sf(here::here("2020", "2020_01_28", "SF_Find_Neighborhoods"),
                     layer = "geo_export_9263930d-f53b-40e2-a384-89952e0799c6")

plot(sf_border)

st_crs(sf_border) # View projection - Projection is 4326!

# Then plot them together with ggplot2!

ggplot() +
  geom_sf(data = sf_trees_spatial)

ggplot() +
  geom_sf(data = sf_border)

# Combine: 
sf_tree_map <- ggplot() +
  geom_sf(data = sf_border) +
  geom_sf(data = sf_trees_spatial,
          aes(color = species),
          alpha = 0.5) +
  theme_minimal() +
  theme(
    axis.text = element_blank(), # Remove axis text
    panel.grid = element_blank() # Remove graticule behind map
  )

sf_tree_map

# ----
# 3. Graph abundance over time
# ----

sf_tree_line <- ggplot(data = sf_trees_select,
                       aes(x = date)) +
  geom_density(aes(color = species,
                   fill = species),
               size = 1,
               alpha = 0.7,
               show.legend = FALSE) +
  facet_wrap(species ~ .) +
  theme_minimal()

sf_tree_line

# ----
# 4. Combine graphs together
# ----