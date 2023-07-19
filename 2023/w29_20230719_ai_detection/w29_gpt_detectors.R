
# setting working directory
setwd("~/GitHub-repo/tidytuesday-entries/2023/w29_20230719_ai_detection")

# acknowledgement
## some codes were adopted from Frank Hanel at https://www.frankhaenel.de/tidytuesday/2023/week_29.html

# libraries
library(tidyverse)
library(patchwork)

# data management
tuesdata <- tidytuesdayR::tt_load(2023, week = 29)
detectors <- tuesdata$detectors

## data for ai vs human
ai_vs_human <- 
  detectors |> 
  mutate(det_cor = kind == .pred_class) |> 
  count(kind, detector, det_cor) |> 
  group_by(kind, detector) |> 
  mutate(total = sum(n)) |>
  filter(det_cor) |> 
  mutate(pct = round(n / total, 2))

## data for native vs non-native
native_vs_not <- 
  detectors |> 
  filter(kind == "Human") |> 
  mutate(det_cor = kind == .pred_class,
         native = if_else(native == "Yes", "Native", "Non-native")
  ) |> 
  group_by(kind, native, detector) |> 
  count(det_cor) |> 
  rename("true" = n) |> 
  mutate(total = sum(true)) |> 
  filter(det_cor) |> 
  mutate(pct = round(true / total, 2))


## colors and captions
blue1 <- "#1e6091"
blue2 <- "#073b4c"
orange1 <- "#f4a261"
orange2 <- "#d8572a"
title1 <- " GPT detector accuracy whether essay was written by a Human or AI"
title2 <- str_wrap(" GPT detector accuracy whether essay was writen by a native or non-native speakers", 70)


# plotting
## ai vs human detection
plt1 <- 
  ai_vs_human |> 
  ggplot(
    aes(detector, kind, fill = pct)
  ) +
  geom_tile() +
  geom_text(
    aes(label = pct), size = 4
  ) +
  scale_fill_gradient(low = "white", high = orange1) +
  theme_minimal() +
  theme(
    plot.margin = margin(rep(20, 4)),
    plot.background = element_rect(fill = "transparent", color = NA),
    plot.title = element_text(color = orange2, size = 14, hjust = 0),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 14)
  ) +
  labs(
    title = title1,
    fill = "% accuracy"
  )

## native vs non-native

plt2 <- 
  native_vs_not |> 
  ggplot(
    aes(detector, native, fill = pct)
  ) +
  geom_tile() +
  geom_text(
    aes(label = pct), size = 4
  ) +
  scale_fill_gradient(low = "white", high = blue1) +
  theme_minimal() +
  theme(
    plot.margin = margin(rep(20, 4)),
    plot.background = element_rect(fill = "transparent", color = NA),
    plot.title = element_text(color = blue2, size = 14, hjust = 0),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 14)
  ) +
  labs(
    title = title2,
    fill = "% accuracy"
  )

## combining plots
plt_gpt_detector <- plt1 / plot_spacer() /  plt2 + plot_layout(heights = c(2, 0.5, 2)) & theme(plot.background = element_rect(fill = "transparent", color = NA))

ggsave(plot = plt_gpt_detector,
       filename = "gpt_detector_accuracy.png",
       bg = "transparent",
       dpi = 400,
       width = 9,
       height = 5
       )
