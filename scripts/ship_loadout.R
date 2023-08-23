# set session ----
api_inara <- "93ed0iol73swsowgokocwogws8kc804s44ccggg"

library(tidyverse)
library(readxl)

dir_in <- "C:/Users/thoma/OneDrive/Documents/ed/data/meta/"

# load data ----
data_shield    <- read_excel(paste0(dir_in,"shields.xlsx"))
data_shield$id <- paste0(data_shield$Class,data_shield$Rating)

data_ship   <- read_excel(paste0(dir_in,"ships.xlsx"))

# ship ----
ship <- "sidewinder"

# shields ----
# this doesn't work
shielding(data_shield,
          hull_mass = 240.5,
          target_shield = "4E",
          base_shield = 110)
