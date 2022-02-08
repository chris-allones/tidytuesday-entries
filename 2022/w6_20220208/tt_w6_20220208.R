# TidyTuesday week 6: Tuskegee Airmen

# Library
library(tidyverse)
library(janitor)
library(sf)
library(lubridate)
library(ggbump)
theme_set(theme_minimal())


# Reading data and wrangle

tuesdata <- tidytuesdayR::tt_load(2022, week = 6)
airmen <- tuesdata$airmen


## Major events in world war 2
ww2_event <- 
  tibble(x = c(1939, 1941, 1941.5, 1942, 1943, 1944, 1945, 1946, 1947),
         y = c(10, 80, 120, 170, 200, 250, 160, 120, 30),
         label = c(
           "Germany invades Poland, Britain and France declare war on Germany.",
           "Germany invades the Soviet Union.",
           "Japan attacks Pearl Harbor and the US enters the war.",
           "Battle of Midway.",
           "Soviet Union defeats Germany at Battle of Stalingard.",
           "Allied Powers carry out D-Day Invasion.",
           "FDR dies. Truman becomes next US president.",
           "Germany surrenders.",
           "Us drops atomic bombs in Japan. Japan surrenders, ending the war."
         )) %>% 
  mutate(label = str_wrap(label, width = 30))


# Fonts
sysfonts::font_add_google("Oswald", "os")
sysfonts::font_add_google("Montserrat", "mont")
showtext::showtext_auto()


# Plot

airmen2 %>% 
  group_by(year_grad) %>% 
  summarise(n = n()) %>% 
  na.omit() %>% 
  ungroup() %>% 
  ggplot(aes(x = year_grad, y = n)) +
  geom_bump(size = 1.3, color = "#74c69d", smooth = 6, alpha = 0.9)+
  geom_point(size = 3, color = "#52b788") +
  geom_point(size = 4.5, stroke = 1.1, color = "#b7e4c7", shape = 1) +
  geom_text(aes(label = n), vjust = 2, size = 4, color = "grey60") +
  geom_point(data = ww2_event, aes(x=x, y=y), size = 2, color = "grey60") +
  geom_segment(data = ww2_event, aes(x=x, xend=x, y=0, yend=y), color = "grey40", linetype = "dashed") +
  geom_text(data = ww2_event %>% subset(x %in% c(1941, 1941.5, 1942, 1943)),
            aes(label = label, x = x, y = y), 
            size = 3.2, vjust = -0.4, hjust = 1, color = "grey50") +
  geom_text(data = ww2_event %>% subset(!x %in% c(1941, 1941.5, 1942, 1943)),
            aes(label = label, x = x, y = y),
            size = 3.2, vjust = -0.4, hjust = 0, color = "grey50") +
  scale_x_continuous(breaks = seq(1939, 1949, 1), limits = c(1939, 1949)) +
  scale_y_continuous(limits = c(0, 400)) +
  theme(plot.margin = margin(rep(15, 4)),
        panel.grid = element_blank(),
        plot.title = element_text(family = "os", size = 24),
        plot.subtitle = element_text(family = "mont"),
        plot.caption = element_text(family = "mont", color = "grey40", margin = margin(t = 15)),
        axis.title = element_blank(),
        axis.text.x = element_text(size = 11, color = "grey60"),
        axis.text.y = element_blank()) +
  labs(title = "Tuskegee Airmen graduates and major events of World War II",
       subtitle = "Data source: Commemorative Air Force | WW II events source: Britannica",
       caption = "#TidyTuesday week 6\n Data source: Commemorative Air Force (CAF) | @Topenomics")
