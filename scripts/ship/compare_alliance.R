# This script compares the three alliance ships

# set --------------------------------------------------------------------------
library(tidyverse)
library(scales)
library(ggpubr)

# fonts
library(showtext)
font_add(family = "Stonehenge", regular = "C:/Users/thoma/OneDrive/Documents/ed/fonts/stonehenge/stonehen.ttf")
font_add(family = "dune", regular = "C:/Users/thoma/OneDrive/Documents/fonts/dune_rise/Dune_Rise.ttf")
font_add(family = "nasa", regular = "C:/Users/thoma/OneDrive/Documents/fonts/nasalization/nasalization-rg.otf")
font_add(family = "starwars", regular = "C:/Users/thoma/OneDrive/Documents/fonts/star_jedi/starjedi/Starjedi.ttf")

showtext_auto()

# load -------------------------------------------------------------------------
data_all <-
  read_csv("./data/temp/ship_all.csv",
           show_col_types = F) |>
  filter(grepl("Alliance",model))

# wrangle ----------------------------------------------------------------------
# pick target attributes
attributes <-
  c("firepower",
    "utility",
    "power.plant",
    "speed",
    "boost",
    "agility",
    "shield",
    "armour",
    "optional",
    "military",
    "footprint",
    "jump")

# data for plotting
data_plot <-
  data_all %>% 
  select(-jump) %>%
  rename(jump = jump.max) %>% 
  pivot_longer(cols = all_of(attributes),
               names_to = "attribute") %>% 
  mutate(attribute = factor(attribute,
                            levels = attributes))

 # plot ------------------------------------------------------------------------
ggplot(data_plot) +
  
  geom_col(aes(x = model,
               y = value,
               fill = model)) +
  
  facet_wrap(~attribute,
             scales = "free_y",
             nrow = 1) +
  
  theme_minimal() +
  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


























