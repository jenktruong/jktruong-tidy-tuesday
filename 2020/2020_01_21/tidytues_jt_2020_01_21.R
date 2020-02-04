##########
# Tidy Tuesday Week 3 2020: Spotify Playlists
# January 21, 2020
# Jennifer Truong
##########

# Attach packages

library(tidyverse)
library(ggbeeswarm)
library(here)

# Read in TidyTuesday data

spotify_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv') %>% 
  drop_na()

# Group by key and filter to just pop songs

spotify_pop <- spotify_songs %>% 
  filter(playlist_genre=="pop" & track_popularity != 0)

# Graph comparing track popularity with tempo per sub genre

ggplot(spotify_pop,
       aes(x = tempo, y = track_popularity)) +
  geom_hex() +
  labs(x = "Tempo (BPM)",
       y = "Popularity",
       title = "What makes the most popular pop sub-genre on Spotify?",
       caption = "Data: Spotify via spotifyr package\n Viz: @jktruong1\n #TidyTuesday") +
  facet_wrap(~playlist_subgenre) +
  theme_minimal()

# Save graph

ggsave(here("2020", "2020_01_21","tidytuesday_spotify.png"))
