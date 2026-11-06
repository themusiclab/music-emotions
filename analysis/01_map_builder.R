# Build map of participants and nhs song locations -----------------

rm(list = ls())

library(pacman) # install.packages(pacman)
p_load(
  tidyverse,
  here
)

set.seed(1018)

processed_data <- read_csv(here("data", "tml_wwl_data.csv.gz"))

## Figure: participant country map --------------------------------

count_per_country <- processed_data %>%
  # temporarily replace hong kong with china for visualisation only
  mutate(country = case_when(country == "Hong Kong" ~ "China", T ~ country)) %>%
  group_by(country) %>%
  summarise(count = n_distinct(user_id))

# data to build map
world <- map_data("world")
world <- subset(world, region != "Antarctica")
world <- filter(world, !long > 180)

ppt_map <- ggplot(count_per_country) +
  geom_map(
    dat = world,
    map = world,
    aes(map_id = region),
    fill = "white",
    color = "gray80",
    linewidth = 0.1
  ) +
  geom_map(map = world, aes(map_id = country, fill = count), linewidth = 0.25) +
  expand_limits(x = world$long, y = world$lat) +
  coord_map("moll") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.position = "left",
    legend.text = element_text(angle = 45, hjust = 0, size = 6),
    legend.title = element_text(vjust = .8, size = 8),
    legend.margin = margin(0, 0, 0, 0)
  ) +
  scale_fill_gradient(
    low = "white",
    high = "#6F4070FF",
    trans = "log10",
    name = "Sample size",
    labels = c("0", "10", "100", "1000", "10000"),
    breaks = c(1, 10, 100, 1000, 10000)
  )

## Figure: NHS song map ---------------------------------------

# read in metadata (locations of song recordings etc.)
nhs_metadata <- read_csv(here(
  "data",
  "NHSDiscography_Metadata.csv"
))

nhs_map <- ggplot() +
  geom_polygon(
    data = world,
    aes(x = long, y = lat, group = group),
    fill = "gray80",
    color = "gray80",
    alpha = 0.3,
    linewidth = .2
  ) +
  # Add dots at lat/long info
  geom_point(
    data = nhs_metadata,
    alpha = .8,
    aes(x = longitude, y = latitude),
    size = 1,
    colour = "#6F4070FF"
  ) +
  theme_void() +
  coord_map("moll")

# Save results -------------------------------------------

save(ppt_map, nhs_map, file = here("results", "01_maps.RData"))
