# TidyTuesday week 23: Pride Donation by Data for progress

# Libraries
library(tidyverse)
library(janitor)
library(lubridate)
library(tidytext)
library(patchwork)

# Reading and wrangling data
tuesdata <- tidytuesdayR::tt_load(2022, week = 23)

top_6_company_donation <- 
tuesdata$pride_aggregates %>% 
  clean_names() %>% 
  group_by(company) %>% 
  summarise(m_amount = mean(total_contributed, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(desc(m_amount)) %>% 
  filter(!company == "Grand Total") %>% 
  slice_head(n = 6) %>% 
  mutate(pct_donation = m_amount / sum(m_amount)) %>% 
  mutate(company = fct_reorder(company, pct_donation)) %>% 
  mutate(m_donation_lab = scales::dollar(m_amount, accuracy = 1)) %>% 
  mutate(pct_lab = str_c("  ", m_donation_lab,
                         " (", round(pct_donation*100, 2),
                         "%) ", company))


# Colors and texts

fill_colors <- c("#FF0000", "#FF9800", "#FFFF00",
                 "#009800", "#0000FF", "#980098")

lab_info <- str_wrap('"Each year, hundreds of corporations around the country participate in Pride, an annual celebration of the LGBTQ+ community’s history and progress. They present themselves as LGBTQ+ allies, but new research from Data for Progress finds that in between their yearly parade appearances, dozens of these corporations are giving to state politicians behind some of the most bigoted and harmful policies in over a decade." - Data for Progress', 50, )
sysfonts::font_add_google("Varela Round", "round")


#Plot
p1 <- 
  top_6_company_donation %>%
  ggplot(aes(pct_donation, company, fill = company)) +
  geom_col(width = 1.05) +
  geom_text(aes(label = pct_lab, x = 0, color = company), vjust = 0, hjust = 0) +
  scale_x_continuous(limits = c(0, 0.95)) +
  coord_polar(clip = "off", direction = -1) +
  scale_fill_manual(values = rev(fill_colors)) +
  scale_color_manual(values = rev(fill_colors)) +
  theme_minimal() +
  theme(plot.margin = margin(10, 0, 10, 10),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "none")
  
p2 <- 
ggplot() +
  geom_text(data = data.frame(x = -0.5, y = 0.7, label = lab_info), 
            mapping = aes(x = x, y = y, label = label), size = 4,
            hjust = 0, family = "round") +
  xlim(-0.6, 3) +
  ylim(0, 1.1) +
  theme_void() +
  theme(legend.position = "none")
  


p1 + p2 + plot_annotation(
  title = "Six largest company donors contributed\nto anti-LGBTQ+ campaigns.",
  subtitle = "The six largest company alone have contributed $1.2 million to anti-LGBTQ+ campaigns and politicians.",
  caption = "#TidyTuesday week 23\n Data source: Data for Progress | @Topenomics") &
  theme(plot.margin = margin(rep(10, 4)),
        plot.title = element_text(size = 26, hjust = 0.5, family = "round"),
        plot.subtitle = element_text(hjust = 0.5, family = "round"),
        plot.caption = element_text(size = 14, color = "gray40"),
        plot.background = element_rect(fill = "white", color = "white")
  )

# Saving plot
ggsave("2022/w23_2022067/pride_donations.jpeg")

