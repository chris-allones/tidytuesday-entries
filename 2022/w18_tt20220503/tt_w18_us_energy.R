

library(tidyverse)
library(MetBrewer)

capacity <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/capacity.csv')

# Aes
showtext_auto()
font_add_google("Quicksand")
font_add_google("Fira Sans Condensed", "Fira")

capacity %>% 
  filter(type != "Other") %>% 
  mutate(type = fct_reorder(type, total_gw, .desc = TRUE)) %>% 
  mutate(log_total_gw = log2(total_gw + 1),
         lab_gw = round(log_total_gw, 0)) %>%
  ggplot() +
  geom_col(aes(x = year, y = log2(total_gw + 1), fill = as.factor(year)), width = 0.7) +
  geom_text(data = data.frame(x = 2013, y = c(0, 5, 10, 12)), 
            aes(x = x, y = y, label = y), hjust = 0.2, vjust = 0, color = "#264653") +
  coord_polar() +
  scale_fill_manual(values=met.brewer("Cross", 7)) +
  scale_color_manual(values = met.brewer("Cross", 7)) +
  ylim(-5, 10) +
  xlim(2013, 2021) +
  facet_wrap(~type, strip.position = "bottom") +
  theme(plot.margin = margin(20, 10, 20, 10),
        plot.background = element_rect(fill = "#f1faee"),
        plot.title = element_text(family = "Fira", face = "bold", size = 30,
                                  margin = margin(b=5), color = "#264653"),
        plot.subtitle = element_text(family = "Fira", size = 12,
                                     margin = margin(b=30), color = "#264653"),
        plot.caption = element_text(size = 12, color = "#264653", 
                                    margin = margin(t = 20)),
        panel.grid = element_line(color = "#457b9d", size = 0.2, linetype = "dotted"),
        panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = "#f1faee"),
        strip.text = element_text(family="Fira", face = "bold", size=14, lineheight = 0.3, vjust =1,
                                  color = "#264653", margin = margin(rep(4, 4))),
        strip.background = element_rect(fill = "#f1faee"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "#f1faee"),
        legend.justification = c(0,0)
  ) +
  guides(fill = guide_legend(nrow = 1,label.position = "top", )) +
  labs(title = "US Energy Capacity",
       subtitle = "Values are in log of the total gigawatts.",
       caption = "US Solar-Wind Data by Berkeley Lab | Plot: @Topenomics",
       fill = element_blank())

ggsave("2022/w18_tt20220503/us_energy.png", height = 8, width = 7, units = "in", dpi = 600)
