# TidyTuesday week 2: Bee colonies

# Libraries ----

library(tidytuesdayR)
library(tidyverse)
library(janitor)
library(sf)
library(grid)
library(cowplot)
library(showtext)


# Reading data and wrangling ----

colony <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/colony.csv') %>% 
  clean_names() %>% 
  filter(year != "6/") %>% 
  select(year, months, state, colony_lost_pct) %>% 
  group_by(year, months, state) %>% 
  summarise(colony_lost_pct = mean(na.omit(colony_lost_pct))) %>% 
  ungroup

us_counties <- st_as_sf(maps::map(database = "state", plot = FALSE, fill = TRUE)) %>% 
  mutate(state  = str_to_title(ID))



# Plot ----

blue_pal <- c("#013a63", "#2a6f97", "#90caf9", "#f0efeb")
na_pal <- "#f7fff7"
max <- max(na.omit(colony$colony_lost_pct))
sysfonts::font_add_google("Lobster", "lobs")
sysfonts::font_add_google("Caveat", "cav")
showtext_auto()


bee_plot <- function(yrs, month) {
  
  sub <- colony %>% 
    filter(year == yrs) %>% 
    filter(months == month)
  
  bee_sub <- us_counties %>% 
    left_join(sub)
  
  bee_map <- ggplot(bee_sub) +
    geom_sf(aes(fill = colony_lost_pct), color = "white", size = 0.2) +
    scale_fill_gradientn(
      colors = rev(blue_pal), 
      na.value = na_pal,
      limits = c(0, max),
      breaks = c(10, 20, 30, 40, 50),
      labels = c("10%", "20%", "30%", "40%", "50%")
    ) +
    guides(fill = guide_colorbar(
      barheight = unit(4, units = "mm"),
      barwidth = unit(50, units = "mm"),
      direction = "horizontal",
      title.position = "top",
      label.position = "bottom",
      title.hjust = 0.5
    )) +
    labs(fill = "Percent of bee colony lost") +
    theme_void() +
    theme(
      legend.text = element_blank(),
      legend.title = element_blank(),
      legend.position = "none"
    )
  
  return(bee_map)
}

bee_plot_legend <- function(yrs, month) {
  
  sub <- colony %>% 
    filter(year == yrs) %>% 
    filter(months == month)
  
  bee_sub <- us_counties %>% 
    left_join(sub)
  
  bee_map <- ggplot(bee_sub) +
    geom_sf(aes(fill = colony_lost_pct), color = "white", size = 0.2) +
    scale_fill_gradientn(
      colors = rev(blue_pal), 
      na.value = na_pal,
      limits = c(0, max),
      breaks = c(10, 20, 30, 40, 50),
      labels = c("10%", "20%", "30%", "40%", "50%")
    ) +
    guides(fill = guide_colorbar(
      barheight = unit(4, units = "mm"),
      barwidth = unit(50, units = "mm"),
      direction = "horizontal",
      title.position = "top",
      label.position = "bottom",
      title.hjust = 0.5
    )) +
    labs(fill = "Percent of bee colony lost") +
    theme_void() +
    theme(
      legend.text = element_blank(),
      legend.title = element_blank(),
    )
  
  return(bee_map)
}


# Year 2015

Y12015 <- bee_plot(yrs = "2015", month = "January-March")
Y22015 <- bee_plot(yrs = "2015", month = "April-June")
Y32015 <- bee_plot(yrs = "2015", month = "July-September")
Y42015 <- bee_plot(yrs = "2015", month = "October-December")

# Year 2016

Y12016 <- bee_plot(yrs = "2016", month = "January-March")
Y22016 <- bee_plot(yrs = "2016", month = "April-June")
Y32016 <- bee_plot(yrs = "2016", month = "July-September")
Y42016 <- bee_plot(yrs = "2016", month = "October-December")


# Year 2017

Y12017 <- bee_plot(yrs = "2017", month = "January-March")
Y22017 <- bee_plot(yrs = "2017", month = "April-June")
Y32017 <- bee_plot(yrs = "2017", month = "July-September")
Y42017 <- bee_plot(yrs = "2017", month = "October-December")


# Year 2018

Y12018 <- bee_plot(yrs = "2018", month = "January-March")
Y22018 <- bee_plot(yrs = "2018", month = "April-June")
Y32018 <- bee_plot(yrs = "2018", month = "July-September")
Y42018 <- bee_plot(yrs = "2018", month = "October-December")


# Year 2019

Y12019 <- bee_plot(yrs = "2019", month = "January-March")
Y22019 <- bee_plot(yrs = "2019", month = "April-June")
Y32019 <- bee_plot(yrs = "2019", month = "July-September")
Y42019 <- bee_plot(yrs = "2019", month = "October-December")



# Year 2020

Y12020 <- bee_plot(yrs = "2020", month = "January-March")
Y22020 <- bee_plot(yrs = "2020", month = "April-June")
Y32020 <- bee_plot(yrs = "2020", month = "July-September")
Y42020 <- bee_plot(yrs = "2020", month = "October-December")


# Year 2021

Y12021 <- bee_plot(yrs = "2021", month = "January-March")
Y22021 <- bee_plot(yrs = "2021", month = "April-June")
Y32021 <- bee_plot(yrs = "2021", month = "July-September")
Y42021 <- bee_plot(yrs = "2021", month = "October-December")


# Plot position

x1<-0.12
x2<-0.33
x3<-0.54
x4<-0.76

y1 <- 0.55
y2 <- 0.40
y3 <- 0.26
y4 <- 0.12
y5 <- 0.9

h<-0.21
w<-0.21

legend <- cowplot::get_legend(bee_plot_legend(yrs = "2018", month = "January-March"))


rect <- rectGrob(
  x = 0,
  y = 0,
  width = 6,
  height = 4,
  #hjust = 0, vjust = 1,
  gp = gpar(fill = "white", alpha = 1)
)


ggdraw() +
  draw_grob(rect) + 
  # 2018
  draw_plot(Y12018, x = x1, y = y1, height = h, width = w) +
  draw_plot(Y22018, x = x2, y = y1, height = h, width = w) +
  draw_plot(Y32018, x = x3, y = y1, height = h, width = w) + 
  draw_plot(Y32018, x = x4, y = y1, height = h, width = w) +
  # 2019
  draw_plot(Y12019, x = x1, y = y2, height = h, width = w) +
  draw_plot(Y22019, x = x2, y = y2, height = h, width = w) +
  draw_plot(Y32019, x = x3, y = y2, height = h, width = w) + 
  draw_plot(Y32019, x = x4, y = y2, height = h, width = w) +
  #2020
  draw_plot(Y12020, x = x1, y = y3, height = h, width = w) +
  draw_plot(Y22020, x = x2, y = y3, height = h, width = w) +
  draw_plot(Y32020, x = x3, y = y3, height = h, width = w) + 
  draw_plot(Y32020, x = x4, y = y3, height = h, width = w) +
  #2021
  draw_plot(Y12021, x = x1, y = y4, height = h, width = w) +
  draw_plot(Y22021, x = x2, y = y4, height = h, width = w) +
  draw_plot(Y32021, x = x3, y = y4, height = h, width = w) + 
  draw_plot(Y32021, x = x4, y = y4, height = h, width = w) +
  #legend
  draw_plot(legend, x = 0.6, y = 0.755, height = 0.25, width = 0.4) +
  draw_label("Percent of bee colony lost\n across season per year",
             x = 0.711, y = 0.90, size = 11, vjust = 0, hjust = 0,
             fontface = "bold") +
  draw_label("0%", x = 0.695, y = 0.85,
             size = 10, hjust = 0, vjust = 0) +
  draw_label("10%", x = 0.725, y = 0.85,
             size = 10, hjust = 0, vjust = 0) +
  draw_label("20%", x = 0.765, y = 0.85,
            size = 10, hjust = 0, vjust = 0) +
  draw_label("30%", x = 0.805, y = 0.85,
             size = 10, hjust = 0, vjust = 0) +
  draw_label("40%", x = 0.845, y = 0.85,
             size = 10, hjust = 0, vjust = 0) +
  draw_label("50%", x = 0.885, y = 0.85,
             size = 10, hjust = 0, vjust = 0) +
  # Title
  draw_label("Percent of bee colony lost\nin the United States (2018-2021)",
             x = 0.07, y = 0.87, size = 27, vjust = 0, hjust = 0, fontface = "bold",
             color = "#013a63", fontfamily = "lobs") +
  # year
  draw_label("2018", x = x1-0.05, y = y1+0.12, size = 23, vjust = 0.5, hjust = 0.5, color = "#90caf9", fontface = "bold", fontfamily = "cav") +
  draw_label("2019", x = x1-0.05, y = y2+0.12, size = 23, vjust = 0.5, hjust = 0.5, color = "#90caf9", fontface = "bold", fontfamily = "cav") +
  draw_label("2020", x = x1-0.05, y = y3+0.12, size = 23, vjust = 0.5, hjust = 0.5, color = "#90caf9", fontface = "bold", fontfamily = "cav") +
  draw_label("2021", x = x1-0.05, y = y4+0.12, size = 23, vjust = 0.5, hjust = 0.5, color = "#90caf9", fontface = "bold", fontfamily = "cav") +
  # season
  draw_label("Jan-Mar", x = x1+0.08, y = y1+0.22, size = 19, vjust=0.5, hjust = 0.5, color = "#90caf9", fontface = "bold", fontfamily = "cav") +
  draw_label("Apr-Jun", x = x2+0.08, y = y1+0.22, size = 19, vjust=0.5, hjust = 0.5, color = "#90caf9", fontface = "bold", fontfamily = "cav") +
  draw_label("Jul-Sept", x = x3+0.08, y = y1+0.22, size = 19, vjust=0.5, hjust = 0.5, color = "#90caf9", fontface = "bold", fontfamily = "cav") +
  draw_label("Oct-Dec", x = x4+0.08, y = y1+0.22, size = 19, vjust=0.5, hjust = 0.5, color = "#90caf9", fontface = "bold", fontfamily = "cav") +
  draw_label(
    "Source: USDA (2018-2021)\n@Topenomics",
    x = 0.75,y = 0.08,size = 12,vjust = 1,hjust = 0, color = "#2a6f97"
  )
  
  
  
  
  
  
  
  
  
  
  
  
























