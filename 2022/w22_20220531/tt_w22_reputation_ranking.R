
# Libraries
library(tidyverse)
library(tidytext)
library(ggbump)

# Data
tuesdata <- tidytuesdayR::tt_load(2022, week = 22)
reputation <- tuesdata$reputation
poll <- tuesdata$poll


# Transform data to long format
## Adapted from: Ansgar Wolsing | Twitter: @_ansgar
food_beverage <- 
  poll %>% 
  mutate(year = 2022) %>% 
  distinct(company, industry, year, rank = `2022_rank`, rq = `2022_rq`) %>% 
  bind_rows(select(poll, company, industry, year, rank, rq)) %>% 
  arrange(company, year) %>% 
  group_by(company) %>% 
  mutate(change = rank - lag(rank, default = NA)) %>% 
  ungroup() %>% 
  filter(industry == "Food & Beverage") %>% 
  mutate(company = str_remove_all(company, "The | Company| Corporation")) %>% 
  filter(!company %in% c("Hershey", "Subway", "Kraft Heinz", "Wendy's"))



# Colors
color <- c("#277da1", "#577590", "#4d908e", "#43aa8b", "#90be6d",
           "#f9c74f", "#f9844a")


# Plotting
food_beverage %>% 
  filter(!is.na(rank)) %>% 
  ggplot(aes(year, rank, col = company)) +
  geom_bump(size = 5) +
  geom_point(shape = 21, size = 8, stroke = 1.25, fill = "white") +
  geom_text(aes(label=rank), size = 3.5) +
  geom_text(data = food_beverage %>% filter(year == 2017),
            aes(label = company, x = year-0.01, y = rank),
            size = 4,
            hjust = 1.2) + 
  geom_text(data = food_beverage %>% filter(year == 2022),
            aes(label = company, x = year+0.01, y = rank),
            size = 4,
            hjust = -0.2) +
  scale_x_continuous(position = "top") +
  scale_y_reverse() + 
  coord_cartesian(clip = "off", expand = TRUE) +
  scale_color_manual(values = color) +
  theme_minimal() +
  theme(plot.margin = margin(l=70, r=70, t=20, b=20),
        plot.background = element_rect(fill = "white", color = "White"),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 24, hjust = 0.5, color = "#3d405b"),
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b=20), 
                                     size = 12, color = "grey40"),
        plot.caption = element_text(size = 10, color = "#3d405b", margin = margin(t = 10)),
        plot.caption.position = "panel",
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.line.x.top = element_line(color = "grey70", size = 1),
        axis.title = element_blank(),
        legend.position = "none") +
  labs(title = "Food & Beverage Reputation Rankings",
       subtitle = "Reputation ranking is based on the Axios Harris Poll 100.\nThe 100 'most visible companies' are rated across seven key dimensions of reputation to determine the ranking.",
       caption = "#TidyTuesday week 22\n Data source: Axios Harris Poll 100 | @Topenomics")


ggsave("2022/w22_20220531/tt_w22_reputation_ranking.jpeg")


  