
# setting working directory
setwd("~/GitHub-repo/tidytuesday-entries/2023/w28_20230716_global_temperature")

# libraries
library(tidyverse)
library(janitor)
library(patchwork)

# data management
tuesdata <- tidytuesdayR::tt_load(2023, week = 28)
global_temps <- tuesdata$global_temps |> clean_names() |> select(year, j_d)
nh_temps <- tuesdata$nh_temps |> clean_names() |> select(year, j_d)
sh_temps <- tuesdata$sh_temps |> clean_names() |> select(year, j_d)

## global temp smooth average
for(nlag in 1:9){
  ncol_name <- paste0("lag", nlag)
  
  global_temps <- global_temps %>%
    mutate(!!sym(ncol_name) := lag(j_d, nlag))
}

global <- 
  global_temps |> 
  mutate(ma = rowMeans(across(j_d:lag9), na.rm = TRUE))

sd <- apply(global[, 2:11], 1, sd)

global_sm <- cbind(global, sd = sd) |> as_tibble()


## north hemisphere smooth average
for(nlag in 1:9){
  ncol_name <- paste0("lag", nlag)
  
  nh_temps <- nh_temps %>%
    mutate(!!sym(ncol_name) := lag(j_d, nlag))
}

nh <- 
  nh_temps |> 
  mutate(ma = rowMeans(across(j_d:lag9), na.rm = TRUE))

sd <- apply(nh[, 2:11], 1, sd)

nh_sm <- cbind(nh, sd = sd) |> as_tibble()


## south hemisphere smooth average
for(nlag in 1:9){
  ncol_name <- paste0("lag", nlag)
  
  sh_temps <- sh_temps %>%
    mutate(!!sym(ncol_name) := lag(j_d, nlag))
}

sh <- 
  sh_temps |> 
  mutate(ma = rowMeans(across(j_d:lag9), na.rm = TRUE))

sd <- apply(sh[, 2:11], 1, sd)

sh_sm <- cbind(sh, sd = sd) |> as_tibble()



# plotting

## creating plot function
plot_temp <- function(data = data, plot_title = "Global"){
  data |> 
    ggplot() +
    geom_ribbon(
      aes(x = year, ymin = ma - sd, ymax = ma + sd),
      fill = "gray60"
    ) +
    geom_line(
      aes(x = year, y = ma)
    ) +
    geom_path(
      aes(x = year, y = j_d),
      color = if_else(global$year > 1975, "red", "black"),
      linewidth = 1
    ) +
    geom_hline(yintercept = 0, lty = "dashed", color = "gray40") +
    scale_x_continuous(breaks = seq(1880, 2023, 10)) +
    scale_y_continuous(breaks = seq(-1.0, 1.5, 0.5), limits = c(-1.0, 1.5),
                       labels = scales::number_format(suffix = " °C")) +
    coord_cartesian(clip = "off", expand = TRUE) +
    theme_minimal() +
    theme(plot.margin = margin(rep(10, 4)),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          axis.title = element_blank()
    ) +
    labs(title = plot_title)
}

### global temperature
plt_global_temp <- plot_temp(data = global_sm, plot_title = "Global") + theme(axis.text.x = element_blank())

### northern hemisphere
plt_north_temp <- plot_temp(data = nh_sm, plot_title = "Northern hemisphere") + theme(axis.text.x = element_blank())

### southern hemisphere
plt_south_temp <- plot_temp(data = sh_sm, plot_title = "Southern hemisphere")


# combinig plots
plt_combined <- plt_global_temp / plt_north_temp / plt_south_temp & theme(plot.background = element_rect(fill = "transparent", color = NA))

ggsave(plot = plt_combined,
       filename = "w28_global_temperature.png", 
       bg = "transparent",
       dpi = 400, 
       width = 6.5, 
       height = 8
       )



