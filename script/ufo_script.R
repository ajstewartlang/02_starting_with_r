# RUM Virtual Meeting 18/03/2020
# Working with the Tidy Tuesday UFO dataset

library(tidyverse) # behold all things tidy
library(openintro) # useful functions such as abb2state
library(ggrepel) # needed to ensure text labels don't overlap
library(patchwork) # needed to combine our 4 plots at the end

# read in data 
ufo_sightings <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-25/ufo_sightings.csv")

# bar plot of top 10 US states with number of sightings in each state
# the openintro::abbr2state() function converts state abbreviations to full names
plot1 <- ufo_sightings %>%
  mutate(state = abbr2state(state)) %>%
  filter(!is.na(state)) %>%
  group_by(state) %>%
  tally() %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(state, n), y = n, fill = state)) +
  geom_col() + 
  coord_flip() +
  guides(fill = FALSE) + 
  labs(title = "Top 10 States for UFO Sightings",
       x = NULL, 
       y = NULL) +
  ylim(0, 11000) +
  theme_minimal() +
  theme(text = element_text(size = 15))

# work out the top 10 states with UFO sightings
top_states <- ufo_sightings %>%
  mutate(state = abbr2state(state)) %>%
  filter(!is.na(state)) %>%
  group_by(state) %>%
  tally() %>%
  top_n(10) %>% 
  pull(state)

# work out states within lat and long limits (i.e., exclude Alaska)
tidied_ufo <- ufo_sightings %>%
  filter(country == "us") %>%
  filter(latitude > 24 & latitude < 50) %>%
  mutate(state = abbr2state(state))

# plot all sightings on a map of the US, with 10 top states coloured
plot2 <- tidied_ufo %>%
  ggplot(aes(x = longitude, y = latitude)) + 
  geom_point(size = .5, alpha = .25) +
  theme_void() +
  coord_cartesian() +
  labs(title = "Sites of UFO Sightings in the US") +
  guides(colour = FALSE) +
  guides(fill = FALSE)

# bar plot of top 10 UFO shapes spotted in California
plot3 <- tidied_ufo %>%
  filter(state == "California") %>%
  filter(ufo_shape != "other") %>%
  filter(ufo_shape != "unknown") %>%
  group_by(ufo_shape) %>%
  tally() %>%
  top_n(10) %>%
  mutate(ufo_shape = str_to_title(ufo_shape)) %>%
  ggplot(aes(x = reorder(ufo_shape, n), y = n, fill = ufo_shape)) +
  geom_col() + 
  coord_flip() +
  guides(fill = FALSE) + 
  labs(title = "Top 10 UFO Shapes spotted in California",
       x = NULL, 
       y = NULL) +
  theme_minimal() +
  theme(text = element_text(size = 15))

# Plot 4
# read in dataset which contains lat and long of US states and cities
uscities <- read_csv("data/uscities.csv")

# extract data just about states
states <- map_data("state")

# create a data frame of the top 10 most populous cities in California
big_cal <- uscities %>%
  filter(state_name == "California") %>%
  arrange(-population) %>%
  top_n(10, population) 

# plot UFO sightings in California and add points representing the 10 largest cities
plot4 <- tidied_ufo %>%
  filter(state == "California") %>%
  ggplot(aes(x = longitude, y = latitude)) + 
  geom_polygon(data = filter(states, region == "california"), 
               aes(x = long, y = lat, fill = region, group = group), alpha = .2, color = "#D89000") +
  geom_point(size = 2, alpha = .5, colour = "#D89000") +
  geom_text_repel(data = big_cal, aes(x = lng, y = lat, label = city), size = 5) +
  geom_point(data = big_cal, aes(x = lng, y = lat), size = 2, alpha = .5) +
  guides(fill = FALSE) +
  theme_void() +
  coord_cartesian() +
  labs(title = "Sites of UFO Sightings in California") +
  labs(caption = "A subset of the Tidy Tuesday UFO Sightings around the world dataset")

# Put plots together
my_plot <- (plot1 + plot3) / (plot2 + plot4)

ggsave("images/ufo_plot.jpg", plot = my_plot, width = 15, height = 10)
