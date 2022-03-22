#TidyTuesday week 12: Babynames

# Libraries
library(tidyverse)
library(tidytuesdayR)
library(glue)


# Reading data and wrangle
tuesdata <- tidytuesdayR::tt_load(2022, week = 12)
babynames <- tuesdata$babynames

babynames %>% 
  filter(name == "Christopher") %>% 
  ggplot(aes(x = year, y = prop)) +
  geom_line() +
  facet_wrap(~ sex)

# font

sysfonts::font_add_google("Comfortaa", "comfort")
showtext::showtext_auto()

# Annotate

description <- glue("Christopher is a classic regularly used and",
                    "has been in the top 500 US names since 1900.",
                    "The per cent share in the total yearly childbirth", 
                    "is at peak around the 70s to 80s but drops ",
                    "in the succeeding years.")


# plotting

christopher %>% 
  ggplot(aes(x = year, y = pct_total)) +
  geom_line(size  = 1.5, color = "#ffe8d6") +
  geom_vline(xintercept = 1984, lty = "dashed", color = "white", size = 0.8) + 
  scale_y_continuous(breaks = seq(0, 0.03, 0.005), limits = c(0, 0.02), labels = scales::percent_format()) + 
  scale_x_continuous(breaks = seq(1880, 2020, 10)) +
  theme_minimal() +
  theme(plot.margin = margin(rep(15, 4)),
        plot.background = element_rect(fill = "#22333b"),
        panel.grid = element_blank(),
        plot.caption = element_text(size = 12, color = "#ffe8d6", margin = margin(t = 10)),
        axis.text = element_text(color = "white", size = 11,),
        axis.title.y = element_text(color = "white", margin = margin(r = 10), size = 12)
        ) +
  labs(x = element_blank(),
       y = "Percent share in the total count of child's name",
       caption = "Data: H. Wickham | Plot: @Topenomics") +
  annotate(geom = "point", x = 1984, y = max(christopher$pct_total + 0.0002), 
           size = 5, color = "#ee6c4d") +
  annotate(geom = "text", x = 1883, y = 0.015, 
           label = "How popular is the\nname Christopher?", 
           size = 9, family = "comfort", hjust = 0, vjust = 0,
           lineheight = 0.8, color = "#ffe8d6") +
  annotate(geom = "text", x = 1994, y = max(christopher$pct_total), 
           label = "Peak year \nat 1984", 
           size = 5,
           lineheight = 0.8, color = "#ffe8d6") +
  annotate(geom = "text", x = 1883, y = 0.0114, 
           label = " Christopher is a classic regularly used and\n has been in the top 500 US names since 1900.\n The percent share in the total yearly childbirth\n is at peak around the 70s to 80s but drops in\n the succeeding years.", 
           size = 4.5, hjust = 0, vjust = 0,
           lineheight = 0.9, color = "#ffe8d6")

  
