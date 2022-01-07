# TidyTuesday Week 1: Bring you own data
# Data source: https://ourworldindata.org/covid-vaccinations


# Libraries

library(tidyverse)
library(janitor)
library(lubridate)
library(zoo)


# Reading data and wrangling

covid <- read_csv("2022/w1_20220106/owid-covid-data.csv") %>% 
  clean_names() %>% 
  remove_empty("cols")

selected_country <- c("Singapore", "Thailand", "Vietnam", "Malaysia",
                      "Cambodia", "Philippines", "Indonesia", "Brunei",
                      "Timor", "Laos", "Myanmar")

highlight_country <- c("Thailand", "Philippines", "Brunei", "Malaysia")


asian_covid <- covid %>% 
  filter(continent == "Asia") %>% 
  select(continent,
         location,
         date,
         new_cases_per_million,
         new_tests_per_thousand,
         positive_rate, 
         reproduction_rate) %>% 
  filter(location %in% selected_country) %>% 
  mutate(date = dmy(date)) %>% 
  pivot_longer(cols = new_cases_per_million:reproduction_rate,
               names_to = "param",
               values_to = "values") %>% 
  filter(values != is.na(values)) %>%
  group_by(param) %>% 
  mutate(values_rolling = rollmean(values, k = 7, fill = NA, align = "right")) %>% 
  ungroup() %>% 
  filter(values_rolling != is.na(values)) %>% 
  mutate(param_label = case_when(param == "new_cases_per_million" ~ "New cases (per 1M)",
                                 param == "new_tests_per_thousand" ~ "New tests (per 100)",
                                 param == "positive_rate" ~ "Positive test rate (%)",
                                 param == "reproduction_rate" ~ "Reproduction rate (%)",
                                 TRUE ~ param)) %>% 
  mutate(location_label = case_when(!location %in% highlight_country ~ "Other",
                                    TRUE ~ location),
         location = fct_reorder(location, values_rolling))


# Plot

asian_covid %>% 
  ggplot(aes(y = values_rolling, x = date, group = location)) +
  geom_line(alpha = 0.5,
            linetype = "dashed",
            color = "grey60") +
  geom_line(data = . %>% filter(location_label != "Other"),
            aes(color = location_label),
            size = 0.5) +
  scale_x_date(date_breaks = '4 months', 
               date_labels = "%b %Y") +
  scale_color_viridis_d(option = "D") +
  facet_wrap(~ param_label, scales = "free") +
  theme_light() +
  theme(plot.margin = margin(rep(15, 4)),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "grey90",
                                          linetype = "dashed"),
        panel.border = element_blank(),
        plot.title = element_text(size = 22,
                                  color = "gray40",
                                  face = "bold",
                                  hjust = 0),
        plot.subtitle = element_text(size = 10,
                                     color = "gray40",
                                     margin = margin(b = 10),
                                     hjust = 0),
        plot.caption = element_text(margin = margin(t = 20),
                                    size = 10,
                                    color = "grey40"),
        strip.background = element_blank(),
        strip.text = element_text(color = "gray20", 
                                  size = 12,
                                  hjust = 0,
                                  margin = margin(t = 20, b = 10)),
        legend.position = "top",
        legend.text = element_text(size = 12)) +
  guides(color = guide_legend(nrow = 1, override.aes = list(size  = 5))) +
  labs(title = "COVID-19 cases, tests, positive rate, and reproduction rate",
       subtitle = "Note: 7-day rolling average. Dues to limited testing, the number of confirmed cases is lower than the true number of infections",
       caption = "Source: Johns Hopkins University CSSE COVID-19 Data",
       x = element_blank(),
       y = element_blank(),
       color = element_blank())


# Saving plot
ggsave("2022/w1_20220106/asian_covid.jpg", units = "in", width = 10, height = 8)
