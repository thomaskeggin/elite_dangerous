# set --------------------------------------------------------------------------
library(readxl)
library(tidyverse)
library(ggrepel)
library(ggfortify)
library(viridis)
library(scales)
library(plotly)
library(ggtree)
library(tidytree)
library(ggtreeExtra)
library(ggstar)

attributes <-
  c("SPEED",
    "AGILITY",
    "SHIELD",
    "ARMOUR",
    "firepower",
    "optional",
    "military",
    "footprint",
    "jump_max",
    "Utility")

ships <-
  c("Krait Phantom",
    "Asp Scout",
    "Viper Mk III",
    "Dolphin",
    "Orca",
    "Keelback")

# load and wrangle -------------------------------------------------------------
data_all <-
  read_csv("./data/temp/ship_all.csv",
           show_col_types = F) %>% 
  filter(Model %in% ships) %>% 
  select(c(Model,any_of(attributes))) %>% 
  pivot_longer(cols = any_of(attributes),
               names_to = "attribute")

# plot -------------------------------------------------------------------------
ggplot(data_all) +
  geom_col(aes(x = Model,
               y = value,
               fill = Model)) +
  facet_wrap(~attribute,
             scales = "free_x") +
  coord_flip()
