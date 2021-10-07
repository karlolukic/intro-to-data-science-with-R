# Title: Solution Script - Explore UFO Sightings
# Author: Karlo Lukic
# Date: 2021-10-08

# packages
library(tidyverse)
library(scales)
library(gganimate)
library(plotly)
library(corrr)
library(ggtext)
library(leaflet)
library(mapview)

# data
# note: dataset contains over 80,000 reports of UFO sightings over the last century
# note: each row represents a sighting (unit of obs.)
ufos_df <- read_csv(file = "01_data/ufo_sightings.csv") 
# View(ufos_df)
glimpse(ufos_df)


# 1) n sightings per year
n_sightings_per_year_df <- ufos_df %>% 
  group_by(year) %>% 
  summarize(n_sightings = n())

ggplot(n_sightings_per_year_df, aes(x = year, y = n_sightings)) + 
  geom_line() +
  scale_y_continuous(labels = label_comma()) +
  labs(y = "Number of UFO Sightings", x = "", 
       title = "Number of UFO Sightings", subtitle = "Per Year") + 
  theme_bw()

# 2) n sightings per year and country
ufos_df %>% 
  group_by(country_name) %>% 
  summarize(n_sightings = n()) %>% 
  arrange(desc(n_sightings))

n_sightings_per_year_and_country_df <- ufos_df %>% 
  group_by(year, country_name) %>% 
  summarize(n_sightings = n())

p <- ggplot(n_sightings_per_year_and_country_df, 
            aes(x = year, y = n_sightings, 
                color = fct_relevel(country_name, 
                                    unique(n_sightings_per_year_and_country_df$country_name)))) + 
  geom_line() +
  scale_y_continuous(labels = label_comma()) +
  labs(y = "Number of UFO Sightings", x = "", color = "Country:",
       title = "Number of UFO Sightings", subtitle = "Per Year and Country") + 
  theme_bw()
p

# dynamic plot?
# ggplotly(p)

# 3) n sightings per year and type of UFO
ufos_df %>% # too many types to plot!
  distinct(shape) %>% 
  print(n = Inf)

ufos_df %>%
  group_by(shape) %>% 
  filter(shape != "Unknown") %>% 
  summarize(n_obs = n()) %>% 
  mutate(prop = n_obs / sum(n_obs)) %>% 
  arrange(-prop) %>% 
  ggplot(., aes(x = reorder(shape, prop), y = prop, fill = shape)) + 
  geom_col(show.legend = F) + 
  scale_y_continuous(label = label_percent()) +
  coord_flip() + 
  labs(title = "UFO Shapes", subtitle = "By Number of Sightings", 
       x = "", y = "") + 
  theme_bw()

five_major_shapes_of_ufos <- c("Light", "Triangle", "Circle", "Fireball", "Other")

n_sightings_per_year_and_ufo_type_df <- ufos_df %>% 
  filter(shape %in% five_major_shapes_of_ufos) %>% 
  group_by(year, shape) %>% 
  summarize(n_sightings = n()) %>% 
  drop_na()

ggplot(n_sightings_per_year_and_ufo_type_df, 
       aes(x = year, y = n_sightings, 
           color = fct_relevel(shape, five_major_shapes_of_ufos))) + 
  geom_line() +
  scale_y_continuous(labels = label_comma()) +
  labs(y = "Number of UFO Sightings", x = "", color = "UFO Shape:",
       title = "Number of UFO Sightings", subtitle = "Per Year and UFO Shape") + 
  theme_bw() 

# 4) correlation between UFO sightings and number of flights
us_sightings_df <- ufos_df %>% 
  filter(country == "US") %>% 
  filter(year >= 1990) %>% 
  group_by(year) %>% 
  summarize(n_sightings = n())

flights_df <- read_csv("01_data/us_flights_per_year.csv")
glimpse(flights_df)

us_sightings_flights_df <- left_join(x = us_sightings_df, y = flights_df, by = "year") 
correlation_coef <- us_sightings_flights_df %>% 
  select(n_sightings, n_flights_in_thousands) %>%
  correlate() %>% 
  slice(2) %>% 
  select(n_sightings) %>% 
  pull() %>% 
  round(digits = 2)

us_sightings_flights_df %>% 
  pivot_longer(cols = c("n_sightings", "n_flights_in_thousands")) %>% 
  mutate(name = recode(name, 
                       "n_sightings" = "UFO Sightings",
                       "n_flights_in_thousands" = "Domestic Flights (in thousands)")) %>% 
  ggplot(., aes(x = year, y = value, color = name)) + 
  geom_line() +
  scale_y_continuous(labels = label_comma()) +
  labs(y = "Total Number", x = "", color = "Variable:",
       title = "Correlation Between UFO Sigthings and Domestic Flights in US", 
       subtitle = "Per Year") + 
  geom_richtext(x = 2002, y = 4000, 
                label = paste0("_r_ = ", correlation_coef), color = "black") +
  theme_bw() # + # animate?
# gganimate::transition_reveal(year)


# 5) mapped sightings in US
us_sightings_areas_df <- ufos_df %>% 
  filter(country == "US") %>%
  group_by(state) %>%
  summarize(n_sightings = n())

map_of_us <- read_csv("01_data/us_map.csv")
glimpse(map_of_us)

us_sightings_areas_map_df <- left_join(us_sightings_areas_df, map_of_us, 
                                       by = "state")

ggplot(us_sightings_areas_map_df, 
       aes(x = state_long, y = state_lat, group = state_group)) +
  geom_polygon(aes(fill = n_sightings), color = "white") +
  scale_fill_viridis_c(option = "C") + 
  labs(x = "lattitude", y = "longitude",
       title = "Number of UFO Sightings in USA",
       subtitle = "Per USA State",
       fill = "Number of UFO Sightings")

# 6) dynamic mapped sightings in US
leaflet(options = leafletOptions(preferCanvas = TRUE)) # faster html viewing
dynamic_map <- leaflet()
dynamic_map <- addTiles(dynamic_map)
my_icon <- makeIcon(
  iconUrl = "https://upload.wikimedia.org/wikipedia/commons/thumb/3/35/Circle-icons-ufo.svg/120px-Circle-icons-ufo.svg.png",
  iconWidth = 38, iconHeight = 35)
dynamic_map <- addMarkers(dynamic_map, 
                lat = ufos_df$latitude, 
                lng = ufos_df$longitude, 
                icon = my_icon,
                popup = paste0("Country: ", ufos_df$country_name, 
                               "<br>", 
                               "State: ", ufos_df$state, 
                               "<br>",
                               "City: ", ufos_df$city, 
                               "<br>",
                               "Date: ", ufos_df$year_month_day,
                               "<br>",
                               "UFO Type: ", ufos_df$shape,
                               "<br>",
                               "Encounter Duration: ", ufos_df$duration_hours_min,
                               "<br>",
                               "UFO Description: ", ufos_df$comments
                ),
                clusterOptions = markerClusterOptions()) # faster html viewing
dynamic_map

# export .html file?
# mapshot(dynamic_map, url = "04_misc/map_of_ufo_sightings.html")
