
# set workding directory
setwd("~/GitHub-repo/tidytuesday-entries/2023/w27_20230704_historical_markers")

# load libraries
library(tidyverse)
library(spData)
library(sf)


# load data
historical_markers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-04/historical_markers.csv')
historical_markers <- historical_markers |> filter(longitude_minus_w > -130, latitude_minus_s > 20)
no_markers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-04/no_markers.csv')
us_map <- spData::us_states

historical_markers |> glimpse()


# plotting
ggplot() +
  geom_sf(
    data = us_map,
    fill = "#60AD9F",
    color = "#204231"
  ) +
  geom_point(
    data = historical_markers,
    aes(
      x = longitude_minus_w,
      y = latitude_minus_s
    ),
    size = 0.05,
    alpha = 0.5
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_blank()
  )

# saving plot with transparent background
ggsave(filename = "us_marker_map_distribution.png", 
       bg = "transparent", 
       height = 5,
       width = 7,
       units = "in",
       dpi = 400
       )


# Plotting historical markers

count_marker <- historical_markers |> count(state_or_prov, sort = TRUE)

count_marker |> 
  mutate(id = 1:nrow(count_marker)) |> 
  head(10) |> 
  mutate(state_or_prov = fct_reorder(state_or_prov, n)) |> 
  ggplot() +
  geom_col(
    aes(
      x = n,
      y = state_or_prov,
    ),
    fill = "#60AD9F",
    width = 0.8
  ) +
  geom_text(
    aes(
      label = n,
      x = n,
      y = state_or_prov
    ),
    hjust = 1.2,
    color = "white"
  ) +
  theme_minimal() +
  theme(
    plot.margin = margin(rep(20, 4)),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_text(size = 15, color = "#204231"),
    axis.text.x = element_blank()
  )

ggsave(
  filename = "us_marker_count_state.png",
  bg = "transparent",
  width = 7,
  height = 5,
  dpi = 400
)






