# Attach packages

library(tidyverse)
library(here)
library(lubridate)

# Read in data

sf_trees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv')