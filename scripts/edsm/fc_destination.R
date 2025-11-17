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
  "Tiris"

# jump_radius
jump_radius <-
  100

# load -------------------------------------------------------------------------
sys_pop_raw <- 
  readRDS(paste0("./data/processed/sys_pop_raw_",Sys.Date()-1,".rds"))

# wrangle ----------------------------------------------------------------------
# unnest coordinates
sys_pop <-
  tibble(sys_pop_raw) %>% 
  unnest(cols = c(coords))

# coordinates to matrix for dist()
coords <-
  sys_pop %>% 
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

# sandbox ----------------------------------------------------------------------

# extract system id
sys_id <-
  sys_pop %>% 
  filter(name == target_sys_name) %>% 
  pull(id)

# find all populated systems within radius
system_in_radius <-
  tibble(sys_within_radius(sys_id,jump_radius,sys_dist))

# extract shortlisted system data
system_shortlist <-
  system_in_radius |> 
  left_join(sys_pop)

# look at planetary bodies -----------------------------------------------------
# extract the planet list column
bodies_list <-
  system_shortlist$bodies

# add the system id to the bodies list data frames
planets <- c()
for(star in 1:length(bodies_list)){
  
  if(dim(bodies_list[[star]])[2] > 0){
    
    # add the system name
    bodies_list[[star]]$system_name <-
      system_shortlist$name[[star]]
    
    # record that this system has planets
    planets <-
      c(planets,star)
    
  }
}

# remove systems with no planets
bodies_id <-
  bodies_list[planets]

# bind all systems together
bodies_df <-
  bind_rows(bodies_id) %>% 
  unnest(rings, names_sep = "_") %>% 
  filter(rings_type == "Icy") %>% 
  filter(grepl("gas",subType))

# look at station information --------------------------------------------------
# extract the planet list column
stations_list <-
  system_shortlist$stations

for(i in 1:length(stations_list)){
  
  if(dim(stations_list[[i]])[1] > 0){
    stations_list[[i]]$system_name <-
      system_shortlist$name[i]
    
  }
  
}

# bind all systems together
stations_df <-
  bind_rows(stations_list) %>%
  filter(grepl("Starport",type)) 

# systems with starports and icy rings
system_choice <-
  stations_df %>% 
  filter(system_name %in% bodies_df$system_name)

# summary ----------------------------------------------------------------------

# system map comparison
bodies_join <-
  bodies_df %>% 
  select(system_name,name,distanceToArrival,type)

stations_join <-
  stations_df %>% 
  select(system_name,name,distanceToArrival,type)

systems_objects <-
  bind_rows(bodies_join,stations_join) %>% 
  filter(system_name %in% system_choice$system_name) %>% 
  mutate(type = gsub(".* ","",type))

summary <-
  systems_objects %>% 
  group_by(system_name,type) %>% 
  summarise(n = n()) %>% 
  pivot_wider(values_from = "n",
              names_from = "type")

# plot -------------------------------------------------------------------------
# star map
plot_data <-
  system_shortlist %>% 
  select(id,name,x,y,z) %>% 
  mutate(designation = NA) %>% 
  left_join(system_in_radius)

plot_data$designation[which(plot_data$name %in% system_choice$system_name)] <- "candidate"
plot_data$designation[which(!plot_data$name %in% system_choice$system_name)] <- "dropped"
plot_data$designation[which(plot_data$name == target_sys_name)] <- "target"

fig <-
  plot_ly(plot_data,
          x = ~x,
          y = ~y,
          z = ~z,
          text = ~paste(name,"\n",round(distance),"LY"),
          color = ~designation,
          colors = c("#F26B6B","#03A6A6","#D9A86C"))
fig <-
  fig %>%
  add_markers()

fig

# system map
system_map <-
  ggplot(systems_objects %>% filter(distanceToArrival < 20000)) +
  geom_hline(aes(yintercept = system_name),
             colour = "lightgrey") +
  geom_point(aes(x = distanceToArrival,
                 y = system_name,
                 colour = type,
                 size = type),
             alpha = 0.5) +
  theme_classic()

ggplotly(system_map)


