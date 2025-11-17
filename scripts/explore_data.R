data <-
  readRDS(paste0("./data/processed/sys_pop_raw_",Sys.Date(),".rds"))

# current location
current_location <-
  "Khernidjal"

system_id <-
  grep(current_location,data$name)

# explore stations
system_stations <-
  as_tibble(data$stations[[system_id]])

system_stations_filtered <-
  system_stations |> 
  filter(type == "Odyssey Settlement") |> 
  filter(controllingFaction$name == "Alioth Independents") |> 
  filter(economy %in% c("Industrial","High Tech")) |> 
  select(name,economy,body) |> 
  rename(station_name = name) |> 
  unnest(body)

write_csv(system_stations_filtered,
          "./tmp.csv")

# bodies
system_bodies <-
  as_tibble(data$bodies[[system_id]])
  








# factions


factions <-
  do.call(rbind.data.frame,
          data$factions) |> 
  as_tibble()
  
target_faction <-
  filter(factions,name == "Alioth Independents")
