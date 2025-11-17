# set --------------------------------------------------------------------------
library(tidyverse)
library(jsonlite)
library(R.utils)
library(fields)
library(readxl)
library(plotly)

# tritium fuel function
jump.cost <-
  function(distance,usedcapacity,tritiumdepot){
    cost <-
      round(10+(distance/4)*(1+((usedcapacity+tritiumdepot)/25000)))
    # source https://cdn.discordapp.com/attachments/164411426939600896/720288828690137149/Tritium_Fuel.png
    
    return(cost)
  }

# load functions
for(file in list.files("./scripts/functions/")){
  source(paste0("./scripts/functions/",file))
}

# choose target
target_sys_name <-
  "Lalande 29437"

# jump_radius
jump_radius <-
  100

# load -------------------------------------------------------------------------
sys_pop_raw <- 
  readRDS(paste0("./data/processed/sys_pop_raw_",Sys.Date()-2,".rds")) |> 
  tibble() |> 
  unnest(cols = c(coords))

sys_power_raw <- 
  readRDS(paste0("./data/processed/sys_power_raw_",Sys.Date(),".rds")) |> 
  as_tibble() |> 
  unnest(cols = c(coords))

# wrangle ----------------------------------------------------------------------
# box filter to reduce data set
target_sys_coords <-
  sys_pop |> 
  filter(name == target_sys_name)

sys_power_box <-
  sys_power_raw |> 
  filter(x > target_sys_coords$x - jump_radius,
         x < target_sys_coords$x + jump_radius,
         
         y > target_sys_coords$y - jump_radius,
         y < target_sys_coords$y + jump_radius,
         
         z > target_sys_coords$z - jump_radius,
         z < target_sys_coords$z + jump_radius)

sys_pop_box <-
  sys_pop |> 
  filter(x > target_sys_coords$x - jump_radius,
         x < target_sys_coords$x + jump_radius,
         
         y > target_sys_coords$y - jump_radius,
         y < target_sys_coords$y + jump_radius,
         
         z > target_sys_coords$z - jump_radius,
         z < target_sys_coords$z + jump_radius)

# coordinates to matrix for dist()
coords <-
  sys_pop_box %>% 
  select(id,x,y,z) %>% 
  data.frame() |> 
  distinct()

row.names(coords) <-
  coords$id

coords_mat <-
  select(coords,-id) %>% 
  as.matrix()

# calculate distance matrix
sys_dist <-
  rdist(coords_mat)

row.names(sys_dist) <-
  row.names(coords_mat)
colnames(sys_dist) <-
  row.names(coords_mat)

# extract system id
sys_id <-
  sys_pop_box %>% 
  filter(name == target_sys_name) %>% 
  pull(id)

# find all populated systems within radius
system_in_radius <-
  tibble(sys_within_radius(sys_id,jump_radius,sys_dist))

# extract shortlisted system data
system_shortlist <-
  system_in_radius |> 
  left_join(sys_pop) |> 
  filter(name %in% sys_power_box$name)

# filter for those that are in powerplay
  power_info <-
   sys_power_box |>
   select(power,powerState,id)

 system_power_shortlist <-
   system_shortlist |>
   left_join(power_info)

# filter for anarchy 
anarchy <-
  system_power_shortlist |> 
  filter(security == "Anarchy",
         power == "Nakato Kaine")

# look at target system stations -----------------------------------------------
# extract the planet list column
stations_list <-
  anarchy$stations

for(i in 1:length(stations_list)){
  
  if(dim(stations_list[[i]])[1] > 0){
    stations_list[[i]]$system_name <-
      anarchy$name[i]
    
  }
  
}

# bind all systems together
stations_df <-
  bind_rows(stations_list) |> 
  as_tibble() |> 
  filter(government == "Anarchy",
         grepl("Od",type)) |> 
  select(name,system_name)

# refilter s
anarchy_systems <-
  anarchy |> 
  filter(name %in% stations_df$system_name)

stations_df |> 
  select(system_name) |> 
  distinct()





