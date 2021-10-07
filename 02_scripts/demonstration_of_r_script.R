# Title: Demonstration of R Script 
# Author: Karlo Lukic
# Date: 2021-10-08

# packages
library(tidyverse)
library(naniar)
library(lubridate)
library(scales)
library(gganimate)
library(plotly)
library(corrr)
library(ggtext)
library(leaflet)

# data
# note: each row represents a sighting (unit of obs.)
# note: dataset contains over 80,000 reports of UFO sightings over the last century
ufos_df <- read_csv(file = "01_data/scrubbed.csv") %>% 
  janitor::clean_names()
# View(ufos_df)
glimpse(ufos_df)

# check error rows
any_na(ufos_df)
ufos_df %>% 
  slice(27823, 35693, 43783) %>% 
  select(duration_seconds, latitude)

# drop rows with NAs
ufos_df <- ufos_df %>% 
  drop_na()
any_na(ufos_df)

# 1) n sightings per year
ufos_df <- ufos_df %>% 
  mutate(year_month_day = as.Date(datetime, format = "%d/%m/%Y")) %>% 
  mutate(year = year(year_month_day))

n_sightings_per_year_df <- ufos_df %>% 
  group_by(year) %>% 
  summarize(n_sightings = n()) %>% 
  drop_na()

ggplot(n_sightings_per_year_df, aes(x = year, y = n_sightings)) + 
  geom_line() +
  scale_y_continuous(labels = label_comma()) +
  labs(y = "Number of UFO Sightings", x = "", 
       title = "Number of UFO Sightings", subtitle = "Per Year") + 
  theme_bw()

# 2) n sightings per year and country
country_names_df <- tribble(
  ~country, ~country_name,
  "us", "United States",
  "ca", "Canada",
  "au", "Australia",
  "gb", "Great Britain"
)
ufos_df <- left_join(x = ufos_df, y = country_names_df, by = "country")

ufos_df %>% 
  group_by(country_name) %>% 
  summarize(n_sightings = n()) %>% 
  arrange(desc(n_sightings))

n_sightings_per_year_and_country_df <- ufos_df %>% 
  group_by(year, country_name) %>% 
  summarize(n_sightings = n()) %>% 
  drop_na()

p <- ggplot(n_sightings_per_year_and_country_df, 
       aes(x = year, y = n_sightings, 
           color = fct_relevel(country_name, country_names_df$country_name))) + 
  geom_line() +
  scale_y_continuous(labels = label_comma()) +
  labs(y = "Number of UFO Sightings", x = "", color = "Country:",
       title = "Number of UFO Sightings", subtitle = "Per Year and Country") + 
  theme_bw()
p

# dynamic plot?
ggplotly(p)


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

# 4) correlation between UFO sightings and number of flights in US?
us_sightings_df <- ufos_df %>% 
  filter(country == "us") %>% 
  filter(year >= 1990) %>% 
  group_by(year) %>% 
  summarize(n_sightings = n())

flights_df <- read_csv("01_data/International_Report_Departures.csv") %>% 
  janitor::clean_names() %>% 
  filter(year <= 2014) # max = 2020

flights_per_year_df <- flights_df %>% 
  group_by(year) %>% # period 1990 - 2014
  summarize(n_flights = sum(total),
            mean_n_fllights = mean(total))

us_sightings_flights_df <- left_join(x = us_sightings_df, y = flights_per_year_df, by = "year") %>% 
  mutate(n_thousand_flights = n_flights / 1000)
correlation_coef <- us_sightings_flights_df %>% 
  select(n_sightings, n_thousand_flights) %>%
  correlate() %>% 
  slice(2) %>% 
  select(n_sightings) %>% 
  pull() %>% 
  round(digits = 2)

us_sightings_flights_df %>% 
  pivot_longer(cols = c("n_sightings", "n_thousand_flights")) %>% 
  mutate(name = recode(name, 
                       "n_sightings" = "UFO Sightings",
                       "n_thousand_flights" = "Domestic Flights (in thousands)")) %>% 
  ggplot(., aes(x = year, y = value, color = name)) + 
  geom_line() +
  scale_y_continuous(labels = label_comma()) +
  labs(y = "Total Number", x = "", color = "Variable:",
       title = "Correlation Between UFO Sigthings and Domestic Flights in US", 
       subtitle = "Per Year") + 
  geom_richtext(x = 2002, y = 2000, 
                label = paste0("_r_ = ", correlation_coef), color = "black") +
  theme_bw() # + # animate?
  # gganimate::transition_reveal(year)


# 5) mapped sightings in US
# a) via ggplot2
us_sightings_areas_df <- ufos_df %>% 
  filter(country == "us") %>%
  group_by(state) %>%
  summarize(n_sightings = n()) %>% 
  rename(state_code = "state")

us_states <- data.table::fread("01_data/states.csv") %>% 
  janitor::clean_names() %>% 
  mutate(state = str_to_lower(state)) %>%
  mutate(state_code = str_to_lower(state_code))

us_sightings_areas_df <- left_join(x = us_sightings_areas_df, y = us_states, 
                                   by = "state_code")
map_of_us <- map_data("state") %>% 
  select(region, group, lat, long)
us_sightings_areas_map_df <- left_join(us_sightings_areas_df, map_of_us, 
                                       by = c("state" = "region"))

ggplot(us_sightings_areas_map_df, aes(long, lat, group = group))+
  geom_polygon(aes(fill = n_sightings), color = "white")+
  scale_fill_viridis_c(option = "C")


# b) via leaflet
ufos_formatted_df <- ufos_df %>% 
  mutate(city = str_to_title(city),
         state = str_to_upper(state),
         shape = str_to_title(shape),
         comments = str_to_sentence(comments)) 

leaflet(options = leafletOptions(preferCanvas = TRUE)) # add this line for faster html viewing
m <- leaflet()
m <- addTiles(m)
my_icon <- makeIcon(
  iconUrl = "https://upload.wikimedia.org/wikipedia/commons/thumb/3/35/Circle-icons-ufo.svg/120px-Circle-icons-ufo.svg.png",
  iconWidth = 38, iconHeight = 35)
m <- addMarkers(m, 
                lat = ufos_df$latitude, 
                lng = ufos_df$longitude, 
                icon = my_icon,
                popup = paste0("Country: ", ufos_formatted_df$country_name, 
                               "<br>", 
                               "State: ", ufos_formatted_df$state, 
                               "<br>",
                               "City: ", ufos_formatted_df$city, 
                               "<br>",
                               "Date: ", ufos_formatted_df$year_month_day,
                               "<br>",
                               "UFO Type: ", ufos_formatted_df$shape,
                               "<br>",
                               "Encounter Duration: ", ufos_formatted_df$duration_hours_min,
                               "<br>",
                               "UFO Description: ", ufos_formatted_df$comments
                ),
                clusterOptions = markerClusterOptions()) # add this argument for faster html viewing
m


# export visualization as .html and .jpg to desired location
mapshot(m, url = "./IP_geo_map_0.1_sample.html")



