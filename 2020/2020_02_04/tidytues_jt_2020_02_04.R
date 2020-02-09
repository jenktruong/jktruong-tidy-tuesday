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
library(stringr)
library(here)

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
  unite("full_team_name", team:year, sep = " ") # Combine team city and team name into one column

# -----
# Make two bar graphs for offensive and defensive rankings
# -----

# Offensive ranking bar graph

sb_off <- ggplot(sb_join_clean,
                 aes(x = full_team_name,
                     y = offensive_ranking)) +
  geom_col(fill = "firebrick1") + # Color bars red
  geom_hline(yintercept=0, color="red4", size=1) + # Draw abline to emphasize y = 0
  labs(x = "Team",
       y = "Offensive Ranking") + # Rename axis labels
  geom_text(aes(y=ifelse(offensive_ranking<0, # Condition for negative rankings
                         offensive_ranking-1, # Move values one unit below bar
                         offensive_ranking+1.5), # For positive values, move values one unit above bar
                         label = offensive_ranking), 
            check_overlap = T, # Make sure labels don't overlap
            size = 3) + # Add data labels
  scale_x_discrete(labels = str_wrap(sb_join_clean$full_team_name,
                   width = 9)) + # Wrap axis tick labels
  scale_y_continuous(lim = c(-4.0,14.0), # Have y-axis start at -4 and end at 14
                     expand = c(0,0)) + # Extend limits of y axis
  theme_light() +
  theme(
    axis.text = element_text(size = 7), # Adjust axis label font size
    axis.title = element_text(size = 10,
                              face = "bold"), # Adjust axis title font size
    axis.title.x = element_blank() # Remove y-axis title to avoid redundancy
  )

# Defensive ranking bar graph 

sb_def <- ggplot(sb_join_clean,
                 aes(x = full_team_name,
                     y = defensive_ranking)) +
  geom_col(fill = "steelblue3") + # Color bars blue
  geom_hline(yintercept=0, color="blue4", size=1) + # Draw abline to emphasize y = 0
  labs(x = "Team and Season Year",
       y = "Defensive Ranking") + # Rename axis labels
  geom_text(aes(y=ifelse(defensive_ranking<0, # Condition for negative rankings
                         defensive_ranking-1, # Move values one unit below bar
                         defensive_ranking+1.5), # For positive values, move values one unit above bar
                label = defensive_ranking), 
            check_overlap = T, # Make sure labels don't overlap
            size = 3) + # Add data labels
  scale_x_discrete(labels = str_wrap(sb_join_clean$full_team_name,
                                     width = 9)) + # Wrap axis tick labels
  scale_y_continuous(lim = c(-4.0,12.0), # Have y-axis start at -4 and end at 12
                     expand = c(0,0)) + # Extend limits of y axis
  theme_light() +
  theme(
    axis.text = element_text(size = 7), # Adjust axis label font size
    axis.title = element_text(size = 10,
                              face = "bold") # Adjust axis title font size
  )

# -----
# Place both bar graphs together
# -----

# Create grobs for title, paragraph, and credits

sb_title <- text_grob("Offensive and Defensive Rankings of Super Bowl Winners",
                           just = "center", hjust = 0.5, vjust = NULL,
                           rot = 0, face = "bold", size = 14)

sb_caption <- ggparagraph("Here we examine the offensive and defensive rankings of each Super Bowl winner in the past 20 years. Rankings were measured as team offensive/defensive quality relative to average (0.0) using SRS (Simple Ranking System).",
                               size = 8, face = NULL,
                               family = NULL, lineheight = NULL)

sb_credits <- text_grob("Data: Pro Football Reference || Viz: @jktruong1 || #TidyTuesday",
                             just = "center", hjust = 1, vjust = 0,
                             rot = 0, face = NULL, size = 8)

# Set up layout matrix

lay <- rbind(c(1,1,1,1,1,1),
             c(NA,2,2,2,2,NA),
             c(3,3,3,3,3,3),
             c(3,3,3,3,3,3),
             c(3,3,3,3,3,3),
             c(4,4,4,4,4,4),
             c(4,4,4,4,4,4),
             c(4,4,4,4,4,4),
             c(NA,5,5,5,5,NA))


# Combine all grobs and graphs together

grid.arrange(sb_title,
             sb_caption,
             sb_off,
             sb_def,
             sb_credits,
             layout_matrix = lay)

# Save as graph
g <- arrangeGrob(sb_title,
                 sb_caption,
                 sb_off,
                 sb_def,
                 sb_credits,
                 layout_matrix = lay) #generates g

ggsave(here("2020", "2020_02_04","tidytuesday_superbowl.png"), g,
       width = 9,
       height = 5,
       units = "in",
       dpi = 300) #saves graph as png
