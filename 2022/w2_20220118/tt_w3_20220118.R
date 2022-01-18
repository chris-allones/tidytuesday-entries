# TidyTueday Week 2: Chocolate ratings


# Libraries----------------------------------------------------
library(tidyverse)
library(tidytext)
library(widyr)
library(ggridges)
library(ggwordcloud)
theme_set(theme_minimal())


# Reading data and wrangling----------------------------------

chocolate <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv')

choco <- chocolate %>% 
  select(ref, company_location, country_of_bean_origin, cocoa_percent, ingredients, most_memorable_characteristics, rating) %>% 
  rename(id = ref, origin = country_of_bean_origin, cocoa_pct = cocoa_percent, features = most_memorable_characteristics) %>% 
  mutate(cocoa_pct = parse_number(cocoa_pct))



# Fonts--------------------------------------------------------
showtext::showtext_auto()
sysfonts::font_add_google("Anton", "anton")
sysfonts::font_add_google("Caveat", "cav")
sysfonts::font_add_google("Lobster", "lobs")




# Plot ---------------------------------------------------------

choco_tokenized <- choco %>% 
  unnest_tokens("word", features, drop = FALSE) %>%
  filter(origin != "Blend", word != "cocoa") %>%
  mutate(word = str_replace(word, "fruit", "fruity"),
         word = str_replace(word, "grits", "gritty")) %>% 
  mutate(company_location = fct_lump(company_location, 11, other_level = "Other location")) %>% 
  group_by(company_location, word) %>% 
  summarise(rating = mean(rating),
            cocoa_pct = mean(cocoa_pct),
            n = n()) %>% 
  arrange(desc(n)) %>% 
  top_n(5, n) %>% 
  ungroup() %>% 
  mutate(word = reorder_within(word, n, company_location))



choco_tokenized %>% 
  ggplot(aes(n, word)) +
  geom_col(width = 0.6, fill = "#644632") +
  # scale_fill_gradient(low = "#0466c8" ,high = "#001233") +
  scale_y_reordered() +
  facet_wrap(~ company_location, scales = "free") +
  labs(title = "Top chocolate bar characteristics across country of manufacturer",
       caption = "#TidyTuesday Week 3 | Data source: Flavors of cacao by Georgios and Kelsey\n @Topenomics",
       x = element_blank(),
       y = element_blank()) +
  theme_void() +
  theme(plot.margin = margin(rep(0.5, 4), unit = "cm"),
        plot.background = element_rect(fill = "white"),
        plot.title = element_text(size = 27, margin = margin(b = 15), family = "lobs", face = "bold", color = "#351303", hjust = 0.5),
        plot.caption = element_text(margin = margin(t = 10)),
        strip.text = element_text(size = 16, color = "#110703", face = "bold"),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_blank()
  )

