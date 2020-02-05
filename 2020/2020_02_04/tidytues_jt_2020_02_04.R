############
# Tidy Tuesday Week 5 2020: Super Bowl
# February 4, 2020
# Jennifer Truong
############

# ---
# Preliminary Stuff
# ---

# Attach packages

library(tidyverse)

# Read in Super Bowl data

attendance <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/attendance.csv')
standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/standings.csv')
games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/games.csv')

# Join attendance and standings data frames together

sb_join <- dplyr::left_join(attendance,
                            standings,
                            by = c("year", "team_name", "team"))

# Clean data

sb_join_clean <- sb_join %>% 
  drop_na() %>%  # Drop NA values
  rename("total_attendance" = "total") %>%  # Rename "total" column to reflect attendance
  filter(playoffs == "Playoffs") # Filter to only teams that made it to playoffs

