############
# Tidy Tuesday Week 8 2020: Food Consumption and CO2 emissions
# February 18, 2020
# Jennifer Truong
############

#####
# 1. Attach packages
#####

library(tidyverse)
library(janitor)
library(here)
library(rvest)

#####
# 2. Read in data
#####

food_consumption <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')

# Units for Consumption (kg/person/year)
# Units for	Co2 Emission (Kg CO2/person/year)

#########
# Cleaning Script - Copied and pasted from RforDataScience Github
#########

# Credit to Kasia and minorly edited to create output file and test plot
# Blog post at https://r-tastic.co.uk/post/from-messy-to-tidy/

url <- "https://www.nu3.de/blogs/nutrition/food-carbon-footprint-index-2018"

# scrape the website
url_html <- read_html(url)

# extract the HTML table
whole_table <- url_html %>% 
  html_nodes('table') %>%
  html_table(fill = TRUE) %>%
  .[[1]]

table_content <- whole_table %>%
  select(-X1) %>% # remove redundant column
  filter(!dplyr::row_number() %in% 1:3) # remove redundant rows

raw_headers <- url_html %>%
  html_nodes(".thead-icon") %>%
  html_attr('title')

tidy_bottom_header <- raw_headers[28:length(raw_headers)]
tidy_bottom_header[1:10]

raw_middle_header <- raw_headers[17:27]
raw_middle_header

tidy_headers <- c(
  rep(raw_middle_header[1:7], each = 2),
  "animal_total",
  rep(raw_middle_header[8:length(raw_middle_header)], each = 2),
  "non_animal_total",
  "country_total")

tidy_headers

combined_colnames <- paste(tidy_headers, tidy_bottom_header, sep = ';')
colnames(table_content) <- c("Country", combined_colnames)
glimpse(table_content[, 1:10])

long_table <- table_content %>%
  # make column names observations of Category variable
  tidyr::pivot_longer(cols = -Country, names_to = "Category", values_to = "Values") %>%
  # separate food-related information from the metric
  tidyr::separate(col = Category, into = c("Food Category", "Metric"), sep = ';')

glimpse(long_table)

tidy_table <- long_table %>%
  tidyr::pivot_wider(names_from = Metric, values_from = Values) %>%
  janitor::clean_names('snake')

glimpse(tidy_table)

final_table <- tidy_table %>%
  rename(consumption = 3,
         co2_emmission = 4) %>%
  filter(!stringr::str_detect(food_category, "total"))

clean_table <- final_table %>% 
  mutate_at(vars(consumption, co2_emmission), parse_number)

clean_table %>% 
  write_csv(here::here("2020/2020_02_18", "food_consumption.csv"))

clean_table %>% 
  ggplot(aes(x = fct_reorder(food_category, consumption), y = consumption, color = country)) +
  geom_jitter() +
  theme(legend.position = "none") +
  coord_flip()

#######
# END Cleaning Script
#######

#####
# 3. Wrangle data
#####

# Sum up total CO2 emissions per country

total_co2_table <- clean_table %>% 
  group_by(country) %>% # Group by country
  summarize(
    sum_co2 = sum(co2_emmission)
  ) # Add total emissions

# Create subset for top 10 countries for CO2 emissions

top_10_co2 <- total_co2_table %>% 
  arrange(-sum_co2) %>% # Arrange by total emissions in descending order
  top_n(10) # Keep top 10

# Create subset only focusing on poultry, fish, pork, and beef 

ppfb_table <- clean_table %>% 
  filter(food_category %in% c("Poultry", "Fish", "Pork", "Beef")) %>% 
  filter(country %in% top_10_co2$country) # Filter by country from top 10 subset dataframe

#####
# 4. Time to create bar graph!
#####

# Create bar graphs
ggplot(ppfb_table,
       aes(x = food_category,
           y = consumption)) +
  geom_col(aes(fill = food_category,
               color = food_category), # Color bars by food category
           show.legend = FALSE) + # Don't show legend 
  scale_y_continuous(expand = c(0,0)) + # Expand y-axis to avoid weird gap below 0
  facet_wrap( ~ country, # Facet by country
              nrow = 2, # 2 rows
              ncol = 5) + # 5 columns
  labs(x = "Animal Protein Types",
       y = "Consumption (kg/person/year)",
       title = "Animal Protein Consumption of Top 10 Carbon-Emitting Countries") + # Add axis labels and title
  theme_light()
