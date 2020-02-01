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
library(ggpubr)

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

# Change species variable in data frame to reordered factor

sf_trees_select$species <- factor(sf_trees_select$species,
                                  levels = c("Lophostemon confertus :: Brisbane Box",
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

st_crs(sf_trees_spatial) <- 4326 # Specify projection!

# Read in SF neighborhood boundary shapefile

sf_border <- read_sf(here::here("2020", "2020_01_28", "SF_Find_Neighborhoods"),
                     layer = "geo_export_9263930d-f53b-40e2-a384-89952e0799c6")

plot(sf_border)

st_crs(sf_border) # View projection - Projection is 4326!

# Then plot them together with ggplot2!

ggplot() +
  geom_sf(data = sf_trees_spatial)

ggplot() +
  geom_sf(data = sf_border,
          fill = "white")

# Combine: 
sf_tree_map <- ggplot() +
  geom_sf(data = sf_border,
          fill = "white") +
  geom_sf(data = sf_trees_spatial,
          aes(color = species,
              fill = species), # Color points by species
          alpha = 0.5) +
  labs(fill = "Tree Species",
       color = "Tree Species") + # Change legend title
  theme_minimal() +
  theme(
    axis.text = element_blank(), # Remove axis text
    panel.grid = element_blank(), # Remove graticule behind map
    legend.position = "bottom", # Move legend below map
    legend.direction = "vertical", # Place items in legend vertically
    legend.title.align = 0.5 # Center legend title
  )

sf_tree_map

# ----
# 3. Graph abundance over time
# ----

sf_tree_line <- ggplot(data = sf_trees_select,
                       aes(x = date)) +
  geom_density(aes(color = species, # color line by species
                   fill = species),
               size = 1,
               alpha = 0.7,
               show.legend = FALSE) +
  labs(x = "Year",
       y = "Density") + # Change axis titles
  facet_wrap(species ~ ., # facet by species vertically
             ncol = 1, # Keep to one column
             scales = "free") + # axis scales resize based on data
  theme_minimal()

sf_tree_line

# ----
# 4. Combine graphs together
# ----

# Create grobs for title, caption, and credits

sf_tree_title <- text_grob("The Five Most Recorded Tree Species in San Francisco",
                          just = "center", hjust = 0.5, vjust = NULL,
                          rot = 0, color = "green4", face = "bold", size = 18)

sf_tree_caption <- ggparagraph("The following five tree species - Brisbane Box, Swamp Myrtle, Hybrid Strawberry Tree, Sycamore 'London Plane', and Southern Magnolia - have the most observations in the city. Here, we show their distribution through space and time.",
                               color = "green4", size = 10, face = NULL,
                               family = NULL, lineheight = NULL)

sf_tree_credits <- text_grob("Data: DataSF\n Viz: @jktruong1\n #TidyTuesday",
                             just = "right", hjust = 1, vjust = 0,
                             rot = 0, color = "green4", face = NULL, size = 8)

# Set up layout matrix

lay <- rbind(c(1,1,1,1),
             c(2,2,2,2),
             c(3,3,4,4),
             c(3,3,4,4),
             c(5,5,5,5))

sf_tree_combine <- grid.arrange(sf_tree_title,
                                sf_tree_caption,
                                sf_tree_map, 
                                sf_tree_line,
                                sf_tree_credits,
                                layout_matrix = lay) # Specify that you want the two graphs in columns

sf_tree_combine
