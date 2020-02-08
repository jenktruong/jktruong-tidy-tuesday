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
library(ggpubr)
library(gridExtra)

# -----
# Read in Super Bowl data
# -----

attendance <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/attendance.csv') %>% 
  pivot_wider(names_from = week, values_from = weekly_attendance) # Clean data by pivoting wider by weekly attendance
standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/standings.csv')
games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/games.csv')

# -----
# Join attendance and standings data frames together
# -----

sb_join <- dplyr::left_join(attendance,
                            standings,
                            by = c("year", "team_name", "team"))

# ----
# Filter data to Super Bowl winners
# -----

sb_join_clean <- sb_join %>% 
  rename("total_attendance" = "total") %>%  # Rename "total" column to reflect attendance
  filter(sb_winner == "Won Superbowl") %>%  # Filter to only teams that made it to playoffs
  unite("full_team_name", team:team_name, sep = " ") # Combine team city and team name into one column

# -----
# Make two bar graphs for offensive and defensive rankings
# -----

sb_off <- ggplot(sb_join_clean,
                 aes(x = full_team_name,
                     y = offensive_ranking)) +
  geom_col(fill = "firebrick1") + # Color bars red
  geom_hline(yintercept=0, color="red4", size=1) + # Draw abline to emphasize y = 0
  labs(x = "Team",
       y = "Offensive Ranking") + # Rename axis labels
  theme_light() +
  theme(
    axis.text = element_text(size = 8) # Adjust axis label font size
  )

sb_def <- ggplot(sb_join_clean,
                 aes(x = full_team_name,
                     y = defensive_ranking)) +
  geom_col(fill = "steelblue3") + # Color bars blue
  geom_hline(yintercept=0, color="blue4", size=1) + # Draw abline to emphasize y = 0
  labs(x = "Team",
       y = "Defensive Ranking") + # Rename axis labels
  theme_light() +
  theme(
    axis.text = element_text(size = 8), # Adjust axis label font size
  )

# -----
# Place both bar graphs together
# -----

sb_margins_graph <- grid.arrange(sb_off,
                                 sb_def,
                                 nrow = 2)
