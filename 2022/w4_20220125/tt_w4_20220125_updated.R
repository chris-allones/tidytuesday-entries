# TidyTuesday Week 4: Board Games

# Libraries
library(tidyverse)
library(janitor)

# Reading data and wrangling

# ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/ratings.csv')
# details <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/details.csv')

tuesdata <- tidytuesdayR::tt_load('2022-01-25')
details <- tuesdata$details
ratings <- tuesdata$ratings
boardgames <- ratings %>% left_join(details, by = 'id') %>% clean_names()

boardgames_publisher <- boardgames %>% 
  select(id, yearpublished, boardgamepublisher, thumbnail, average) %>% 
  filter(yearpublished > 1900, yearpublished < 2022) %>% 
  separate_rows(boardgamepublisher, sep = ",") %>% 
  mutate(boardgamepublisher = str_remove_all(boardgamepublisher, "[[:punct:]]"),
         boardgamepublisher = str_trim(boardgamepublisher)) %>% 
  filter(!is.na(boardgamepublisher)) %>% 
  filter(!boardgamepublisher %in% c("Inc", "Ltd")) %>% 
  group_by(boardgamepublisher) %>% 
  summarise(rating = mean(average, na.rm = TRUE),
            n = n()) %>% 
  arrange(desc(n)) %>% 
  ungroup() %>%
  mutate(above_avg_rating = rating > avg_rating) %>% 
  mutate(boardgamepublisher = fct_reorder(boardgamepublisher, rating)) %>% 
  head(40)


# Fonts

sysfonts::font_add_google("Dancing Script", "dance")
sysfonts::font_add_google("Abel", "abel")
showtext::showtext_auto()




# Plot
avg_rating <- mean(boardgames$average, na.rm = TRUE)

arrows <- tibble(x1 = c("LLC", "Mayfair Games"),
                 y1 = c(5.9, 7),
                 x2 = c("Arclight", "Ravensburger"),
                 y2 = c(avg_rating - 0.02, avg_rating + 0.02),
                 above_avg_rating = c(TRUE, FALSE))

boardgames_publisher_sub_up <- boardgames_publisher %>% subset(rating > avg_rating)
boardgames_publisher_sub_down <- boardgames_publisher %>% subset(rating < avg_rating)

boardgames_publisher %>% 
  ggplot(aes(boardgamepublisher, rating, color = above_avg_rating)) +
  geom_point(size = 4) +
  geom_segment(aes(x = boardgamepublisher, xend = boardgamepublisher, 
                   y = avg_rating, yend = rating),
               size = 0.8) +
  geom_hline(yintercept = avg_rating, size = 0.5, color = "gray60", linetype = "dashed") +
  geom_text(data = boardgames_publisher_sub_up, aes(label = boardgamepublisher,  y = 0),
            hjust = 1, nudge_y = 6.4, size = 4, family = "abel") +
  geom_text(data = boardgames_publisher_sub_down, aes(label = boardgamepublisher,  y = 0),
            hjust = 0, nudge_y = 6.44, size = 4, family = "abel") +
  coord_flip() +
  theme(plot.margin = margin(rep(17, 4)),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(size = 44, family = "dance", hjust = 0.5, color = "cyan4"),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = 16, margin = margin(b = 20), hjust = 0.5, family = "abel", color = "firebrick1"),
        plot.caption = element_text(color = "grey40", size = 12),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none") +
  labs(x = element_blank(),
       y = element_blank(),
       title = "Top rated boardgame publishers",
       subtitle = "Above and below average ratings of boardgame publishers (1900-2021)",
       caption = "#TidyTuesday Week 4\nData source: Kaggle / Board Game Geeks | @Topenomics") +
  annotate("text", x = "Broadway Toys LTD", y = 5.7, 
           label = glue::glue("Boardgame publishers\nwhose ratings are\n above average rating ({round(avg_rating, 2)})"),
           color = "cyan4", lineheight = 0.9, size = 4.5) +
  annotate("text", x = "Piatnik", y = 7.2, 
           label = glue::glue("Boardgame publishers\nwhose ratings are\n below average rating ({round(avg_rating, 2)})"),
           color = "firebrick1", lineheight = 0.9, size = 4.5)

