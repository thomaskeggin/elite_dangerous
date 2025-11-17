# set --------------------------------------------------------------------------
library(tidyverse)

attributes <-
  c("speed",
    "agility",
    "shield",
    "armour",
    "firepower",
    "optional",
    "military",
    "footprint",
    "jump.max",
    "utility")

ships <-
  c("Viper Mk IV",
    "Viper Mk III",
    "Eagle Mk II")

# load and wrangle -------------------------------------------------------------
data_all <-
  read_csv("./data/temp/ship_all.csv",
           show_col_types = F) |> 
  filter(model %in% ships,
         pad == "Small") |> 
  select(c(model,any_of(attributes))) |> 
  pivot_longer(cols = any_of(attributes),
               names_to = "attribute")

# plot -------------------------------------------------------------------------
ggplot(data_all) +
  geom_col(aes(y = model,
               x = value,
               fill = model),
           position = "dodge",
           colour = "black") +
  facet_wrap(~attribute,
             scales = "free_x")
