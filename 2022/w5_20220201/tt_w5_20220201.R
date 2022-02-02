# TidyTuesday week 5: dog breeds


# Libraries
library(tidyverse)
library(janitor)
library(tidytext)
library(ggbump)
library(ggimage)

#fonts
sysfonts::font_add_google("Oswald", "oswald")
sysfonts::font_add_google("Arimo", "arimo")
showtext::showtext_auto()


# Reading data and wrangle -----------------------------------------------------------------

# breed_traits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv')
# trait_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/trait_description.csv')
# breed_rank_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv')

breed_traits <- read_csv("data/breed_traits.csv")
trait_description <- read_csv("data/trait_description.csv")
breed_rank_all <- read_csv("data/breed_rank_all.csv") %>% clean_names()



breed_top <- breed_rank_all %>% 
  pivot_longer(cols = -c("breed", "links", "image"),
               names_to = "year",
               values_to = "rank") %>% 
  filter(rank < 11) %>% 
  mutate(year = parse_number(year))


traits <- breed_traits %>% 
  select(
    Breed,
    Family='Affectionate With Family',
    Children='Good With Young Children',
    Dogs='Good With Other Dogs',
    Shedding='Shedding Level',
    Coat='Coat Grooming Frequency',
    Drooling='Drooling Level',
    Strangers='Openness To Strangers',
    Watchdog='Watchdog/Protective Nature',
    Adaptability='Adaptability Level',
    Trainability='Trainability Level',
    Energy='Energy Level',
    Barking='Barking Level',
    Mental='Mental Stimulation Needs',
    Playfulness = 'Playfulness Level' ) %>% 
  pivot_longer(cols = -Breed,
               names_to = "traits", 
               values_to = "rating") %>% 
  clean_names()


ope_stranger_dogs <- traits %>% 
  filter(traits == "Strangers") %>% 
  group_by(breed) %>% 
  summarise(rating = mean(rating, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(breed = str_trim(breed, side = "both"))

top_ranking_dog <- breed_top %>% count(breed)

dog_rank_stranger <- top_ranking_dog %>% 
  left_join(ope_stranger_dogs, by = "breed") %>% 
  mutate(rating = case_when(breed == "French Bulldogs" ~ 5,
                            breed == "German Shepherd Dogs" ~ 3,
                            breed == "Pembroke Welsh Corgis" ~ 4,
                            breed == "Pointers (German Shorthaired)" ~ 4,
                            breed == "Retrievers (Golden)" ~ 5,
                            breed == "Retrievers (Labrador)" ~ 5,
                            breed == "Yorkshire Terriers" ~ 5,
                            TRUE ~ rating)) %>% 
  select(-n)



# Plot --------------------------------------------------------------------------------

breed_top %>% 
  left_join(dog_rank_stranger, by = "breed") %>% 
  ggplot(aes(year, rank)) +
  geom_bump(aes(group = breed, color = factor(rating)), 
            size = 12, 
            smooth = 5,
            alpha = 0.7) +
  scale_color_manual("Openness to strangers level (1-5)",
                     values=c("3" = "#ef233c",
                              "4" = "#fb8500",
                              "5" = "#184e77"),
                     aesthetics = c("color", "fill")) +
  scale_y_reverse() +
  scale_x_continuous(breaks = seq(2013, 2020, 1))  +
  geom_text(data = breed_top %>% filter(year == 2018, rank %in% c(1, 2, 3, 4, 5, 6, 7, 8)),
            aes(label = breed), family = "oswald", size = 5.5, color = "#f5f3f4") +
  geom_text(data = breed_top %>% filter(year == 2019, !rank %in% c(1, 2, 3, 4, 5, 6, 7, 8)),
            aes(label = breed), family = "oswald", size = 5.5, color = "#f5f3f4") +
  theme_void() +
  theme(plot.margin = margin(rep(15, 4)),
        plot.title = element_text(size = 24, family = "arimo", color = "#3d405b", face = "bold"),
        plot.subtitle = element_text(size = 18, color = "#3d405b", margin = margin(b = 20, t = 10)),
        plot.caption = element_text(size = 12, color = "#3d405b", margin = margin(t = 10)),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 12, face = "bold"),
        legend.position = "none",
        # legend.justification = "left",
        # legend.box.margin = margin(t=2, b=-2),
        # legend.text = element_text(size = 16, color = "#3d405b"),
        # legend.title = element_text(size = 16, color = "#3d405b")
        ) +
  labs(x = "",
       y = "",
       title = "Top 10 most popular dog breed and level of openness to strangers",
       subtitle = "Popularity of dog breeds by AKC registration statistics",
       caption = "#TidyTuesday week 5\n Data source: AKC courtecy of KKakey | @Topenomics") +
  geom_image(data = breed_top %>% filter(year == min(year)),
           aes(image = image, x = year - 0.4), asp = 2) +
  geom_image(data = breed_top %>% filter(year == max(year)),
             aes(image = image, x = year + 0.4), asp = 2)
  
