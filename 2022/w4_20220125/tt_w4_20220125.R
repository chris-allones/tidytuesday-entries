# TidyTuesday Week 4: Board Games

# Libraries


# Reading data and wrangling

# ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/ratings.csv')
# details <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/details.csv')

tuesdata <- tidytuesdayR::tt_load('2022-01-25')
details <- tuesdata$details
ratings <- tuesdata$ratings
boardgames <- ratings %>% left_join(details, by = 'id') %>% clean_names()

boardgames_publisher <- boardgames %>% 
  select(id, yearpublished, boardgamepublisher, average) %>% 
  filter(yearpublished > 1900, yearpublished < 2022) %>% 
  separate_rows(boardgamepublisher, sep = ",") %>% 
  mutate(boardgamepublisher = str_remove_all(boardgamepublisher, "[[:punct:]]"),
         boardgamepublisher = str_trim(boardgamepublisher)) %>% 
  filter(!is.na(boardgamepublisher))



# Fonts


# Plot
## Try to get mean rating by decades

boardgames_publisher %>% 
  filter(!boardgamepublisher %in% c("Inc", "Ltd")) %>% 
  group_by(boardgamepublisher) %>% 
  summarise(rating = mean(average, na.rm = TRUE),
            n = n()) %>% 
  arrange(desc(n)) %>% 
  ungroup() %>%
  mutate(boardgamepublisher = fct_reorder(boardgamepublisher, rating)) %>% 
  head(40) %>% 
  ggplot(aes(boardgamepublisher, rating)) +
  geom_point(size = 4) +
  geom_segment(aes(x = boardgamepublisher, xend = boardgamepublisher, 
                   y = mean(boardgames_publisher$average), yend = rating),
               size = 0.8) +
  geom_hline(yintercept = mean(boardgames_publisher$average),
             size = 0.5, 
             color = "gray60",
             linetype = "dashed") +
  coord_flip() +
  theme(plot.margin = margin(rep(15, 4)),
        panel.grid = element_blank(),
        plot.title = element_text(size = 20, 
                                  hjust = -0.8),
        plot.subtitle = element_text(size = 12, 
                                     hjust = -0.5, 
                                     margin = margin(b = 20)),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10)) +
  labs(x = element_blank(),
       y = element_blank(),
       title = "Top rated boardgame publishers (1900-2021)",
       subtitle = "Above and below above ratings of boardgame publishers")