#TidyTuesday week 24: Drought conditions in the USA

# Libraries
library(tidyverse)
library(lubridate)
library(janitor)
library(ggnewscale)
library(geofacet)

# Reading data and wrangling
tuesdata <- tidytuesdayR::tt_load(2022, week = 24)
drought <- tuesdata$drought %>% 
  clean_names() %>% 
  mutate(date = str_remove(date, "d_"),
         date = ymd(date),
         year = year(date)) %>% 
  mutate(state = str_to_title(state)) %>% 
  filter(year >= 2000)

df_drought <- 
  drought %>% 
  select(state, state_abb, year, d0:d4) %>% 
  pivot_longer(!state:year)

df_wet <- 
  drought %>% 
  select(state, state_abb, year, w0:w3) %>% 
  pivot_longer(!state:year) %>% 
  mutate(value = value * -1)



# plot

fill_scale <- RColorBrewer::brewer.pal(6, 'YlOrRd')[c(2:6)]
sysfonts::font_add_google("Oswald", "oswald")

ggplot(data = df_drought) +
  geom_col(data = df_drought, aes(year, value, fill = name), position = "identity") +
  scale_fill_manual(values = fill_scale, labels = c("Abnormally dry","Moderate drought","Severe drought","Extreme drought","Exceptional drought"), name = NULL) +
  guides(fill = guide_legend(nrow = 1,
                             label.theme = element_text(color = "#dc2f02", size = 10),
                             label.position = "top",
                             title.hjust = 0.5,
                             keywidth = unit(5, "line"),
                             keyheight = unit(0.7, "line"))) +
  new_scale_fill() +
  geom_col(data = df_wet, aes(year, value, fill = name),
           position = "identity") +
  scale_fill_brewer("",palette = "Blues", labels=c("Abnormally wet","Moderate wet","Severe wet","Extreme wet","Exceptional wet")) +
  guides(fill = guide_legend(nrow = 1,
                             label.theme = element_text(color = "#005f73", size = 10),
                             label.position = "top",
                             title.hjust = 0.5,
                             keywidth = unit(5, "line"),
                             keyheight = unit(0.7, "line"),
                             )) +
  facet_wrap(~ state, strip.position = "bottom") +
  theme_minimal() +
  theme(plot.margin = margin(rep(20, 4)),
        plot.title = element_text(size = 28, hjust = 0.5, family = "oswald", face = "bold", color = "grey40"),
        plot.caption = element_text(size = 10, color = "gray40"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 10, margin = margin(b=10), color = "gray30"),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.box.margin = margin(b = 15),
        legend.position = "top") +
  labs(title = "Drought and wet conditions in the USA - 2000 to 2022",
       caption = "#TidyTuesday week 24\n Data source: National Integrated Drought Information System | @Topenomics")
  
# Saving plot
ggsave("2022/w24_20220614/drought.jpeg",  width = 11, height = 7)
