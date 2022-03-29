#TidyTuesday week 13: Babynames

# Libraries
library(tidyverse)
library(glue)


# Reading data and wrangle
tuesdata <- tidytuesdayR::tt_load(2022, week = 13)
sports <- tuesdata$sports


partic_sports <- 
  sports %>% 
  mutate(total_partic_menwomen = sum_partic_men + sum_partic_women,
         pct_male = sum_partic_men / total_partic_menwomen,
         pct_female = sum_partic_women / total_partic_menwomen) %>% 
  ungroup() %>%
  group_by(sports) %>% 
  summarise(mean_partic = mean(total_partic_menwomen, na.rm = TRUE),
            mpct_male = mean(pct_male, na.rm = TRUE) * -1,
            mpct_female = mean(pct_female, na.rm = TRUE)) %>% 
  na.omit() %>% 
  mutate(num = 1:nrow(.),
         sports = fct_reorder(sports, mpct_female)) %>% 
  filter(!sports %in% c("All Track Combined", "Track and Field, X-Country"))

arrow1 <- 
tibble(x1 = "Wrestling",
       x2 = "Baseball",
       y1 = 0.7,
       y2 = 0.2)

arrow2 <- 
  tibble(x1 = "Rowing",
         x2 = "Softball",
         y1 = -0.6,
         y2 = -0.2)


# Plot: participation

partic_sports %>%
  ggplot() + 
  geom_col(aes(x = sports, y = mpct_female), fill = "#ee6c4d", width = 0.9, alpha = 0.8) +
  geom_col(aes(x = sports, y = mpct_male), fill = "#3d5a80", width = 0.9, alpha = 0.8) +
  geom_hline(yintercept = c(-0.5, 0.5), lty = "dashed", color = "#98c1d9") +
  geom_text(aes(label = sports, x = sports, y = 0), vjust = 0.4, color = "#e0fbfc", angle = 90) + 
  scale_y_continuous(labels = c(1, 0.5, 0, 0.5, 1), limits = c(-1, 1)) +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(rep(15, 4)),
        plot.background = element_rect(fill = "#293241"),
        plot.title = element_text(color = "#ee6c4d", size = 25),
        plot.subtitle = element_text(color = "#ee6c4d", margin = margin(b=20), size = 12),
        plot.caption = element_text(color = "#ee6c4d", size = 12),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "#293241"),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = "#e0fbfc"),
        axis.ticks = element_blank()
        ) +
  labs(title = "Proportion of men and women participation in sports",
       subtitle = "Sports occupying within the dashed lines have nearly equal proportion of men and women participation.",
       caption = "Data: Equity in Athletics Data Analysis | Plot: @Topenomics") +
  annotate(geom = "text", x = "Archery", y = -0.6, hjust = 0,
           label = "Sports played mostly by women.",
           color = "#ee6c4d") +
  geom_curve(data = arrow2, aes(x = x1, xend = x2, y = y1, yend = y2),
             arrow = arrow(length = unit(0.5, "cm")),
             size = 1,
             curvature = 0.5,
             color = "#ee6c4d") +
  annotate(geom = "text", x = "Wrestling", y = 0.75, hjust = 0,
           label = "Sports played mostly by men.",
           color = "#669bbc") +
  geom_curve(data = arrow1, aes(x = x1, xend = x2, y = y1, yend = y2),
             arrow = arrow(length = unit(0.5, "cm")),
             size = 1,
             curvature = 0.5,
             color = "#669bbc")
  










