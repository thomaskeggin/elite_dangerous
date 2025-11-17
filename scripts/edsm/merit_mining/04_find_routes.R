# set --------------------------------------------------------------------------
library(tidyverse)
library(readxl)

# load -------------------------------------------------------------------------
# routes
source_to_system <-
  read_csv("./outputs/mining/merit_mining/boom_acquisitions.csv", #input
           show_col_types = F)

# sell markets
sell_markets <-
  read_csv("./outputs/mining/merit_mining/03_sell_station_markets.csv", #input
           show_col_types = F)

# rings
rings <-
  read_csv("./outputs/mining/merit_mining/reinforced_rings.csv", #input
           show_col_types = F)

# hotspots
hotspots <-
  read_excel("./data/hotspots.xlsx") #input

# explore ----------------------------------------------------------------------
# add sources to market prices
all <-
  sell_markets |> 
  arrange(desc(sellPrice)) |> 
  left_join(source_to_system,
            relationship = "many-to-many") |> 
  
  select(system_source,
         system_source_update,
         system_sell,
         station_sell,
         system_sell_powerState,
         system_sell_update,
         name,
         demand,
         sellPrice,
         ly)

# export -----------------------------------------------------------------------
write_csv(all,
          paste0("./outputs/mining/merit_mining/04_kaine_boom_mining_", #output
                 Sys.Date(),".csv"))












