# for all the sell systems, identify which stations are in boom that we could
# sell to

# set --------------------------------------------------------------------------
library(tidyverse)

# load -------------------------------------------------------------------------
dir_processed <-
  "./data/processed/" #input

# find the latest populated systems download
latest_populated <-
  tibble(file_full = list.files(dir_processed, full.names = T),
         file = list.files(dir_processed)) |> 
  filter(grepl("pop",file)) |> 
  mutate(date = gsub("sys_pop_raw_|.rds","",file) |> 
           as.Date()) |> 
  filter(date == max(date)) |> 
  pull(file_full)

populated <-
  readRDS(latest_populated) |> 
  as_tibble()

# load in shortlist
mining_shortlist <-
  read_csv("./outputs/mining/merit_mining/boom_acquisitions.csv", #input
           show_col_types = F)

# find sell stations -----------------------------------------------------------
sell_system_names <-
  mining_shortlist |> 
  pull(system_sell) |> 
  unique()

sell_systems <-
  populated |> 
  filter(name %in% sell_system_names)

sell_stations_list <-
  list()

for(sys in sell_system_names){
  
  # extract boom factions
  factions <-
    sell_systems |> 
    filter(name == sys) |> 
    pull(factions)
  
  factions <-
    factions[[1]]
  
  boom_faction <-
    factions |> 
    filter(grepl("Boom",activeStates)) |> 
    pull(id)
  
  # extract boom stations
  stations_sys <-
    sell_systems |> 
    filter(name == sys) |>
    pull(stations)
  
  if(dim(stations_sys[[1]])[1] > 0){
    
    stations <-
      stations_sys[[1]] |> 
      select(name,
             marketId,
             controllingFaction) |> 
      rename(station_sell = name) |> 
      unnest(controllingFaction) |> 
      filter(!is.na(id))
    
    boom_stations <-
      stations |> 
      filter(id %in% boom_faction) |> 
      mutate(system_sell = sys) |> 
      select(system_sell,
             station_sell,
             marketId)
    
    sell_stations_list[[sys]] <-
      boom_stations
  }
}

sell_stations <-
  do.call(rbind.data.frame,
          sell_stations_list)

# export -----------------------------------------------------------------------
write_csv(sell_stations,
          "./outputs/mining/merit_mining/02_sell_stations.csv") #output












