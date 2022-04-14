# TidyTuesday week 15: Indoo Air Pollution

# Libraries
library(tidyverse)
library(janitor)
library(ggrepel)
library(scales)
theme_set(theme_light())


# Import TidyTuesday data
tuesdata <- tidytuesdayR::tt_load(2022, week = 15)
fuel_gdp <- tuesdata$fuel_gdp
indoor_pollution <- tuesdata$indoor_pollution

fuel_gdp_clean <- 
  fuel_gdp %>%
  clean_names() %>%
  rename(acess_clean_fuel = starts_with("access_to"),
         gdp_pc = starts_with("gdp_per"),
         pop = starts_with("population"),
         country = entity)

indoor_pollution_clean <- 
  indoor_pollution %>% 
  clean_names() %>% 
  rename(country = entity, 
         death = starts_with("deaths"))

fuel_indoor_pollution <- 
  fuel_gdp_clean %>% 
  left_join(indoor_pollution_clean)


# Fonts
sysfonts::font_add_google("Anton", "anton")
sysfonts::font_add_google("Inter", "inter")
showtext::showtext_auto()




#Plotting

fuel_indoor_pollution %>% 
  filter(year == 2016) %>%
  ggplot(aes(x = death, y = acess_clean_fuel, color = country)) +
  geom_smooth( lty = "dashed", se = FALSE, 
               color = "grey80", size = 0.7,
               span = 0.9) +
  geom_point(position = position_dodge(width = 2), 
             alpha = 0.3, aes(size = gdp_pc)) +
  geom_text_repel(aes(label = country)) +
  scale_y_continuous(limits = c(0, 100),
                     labels = function(y) paste0(y, "%")) +
  scale_x_continuous(limits = c(0, 25),
                     labels = function(x) paste0(x, "%")) +
  scale_size(range = c(0.1, 20)) +
  coord_cartesian(clip = "off") +
  scale_color_viridis_d(option = "H") +
  theme(plot.margin = margin(rep(20, 4)),
        plot.title = element_text(color = "#264653", 
                                  family = "anton", size = 25),
        plot.subtitle = element_text(margin = margin(b=20), 
                                     family = "inter", size = 14,
                                     color = "#14213d"),
        plot.title.position = "plot",
        plot.caption = element_text(size = 12, 
                                    color = "#264653", 
                                    margin = margin(t = 10)),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title.x = element_text(margin = margin(t=15), 
                                    size = 12,
                                    family = "inter",
                                    color = "#14213d"),
        axis.title.y = element_text(margin = margin(r=15), 
                                    size = 12,
                                    family = "inter",
                                    color = "#14213d"),
        legend.position = "none") +
  labs(title = "Access to clean fuels for cooking vs indoor air pollution death rates, 2016",
       subtitle = "Note: Bubble size represents the GDP per capita.",
       y = "Share of population with access to clean fuels for cooking",
       x = "Share of indoor pollution death rate",
       caption = "Data: Our World in Data | Plot: @Topenomics") +
  annotate(geom = "text", 
           label = "Countries with the highest death rate from indoor pollution\nhave low access to clean fuels. Also, using GDP per capita, \nlow-income households have lower access to cleaner fuels\nthan high-income households, either unavailable or expensive.",
           x = 10, y = 85,
           hjust = 0,
           size = 5,
           family = "inter",
           color = "#264653",
           alpha = 0.6, 
           lineheight = 1)
