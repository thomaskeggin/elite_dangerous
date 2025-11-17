# The idea is to generate a list of viable systems to mine in, and their respective
# sell systems.
# i.e., reinforced with rings -> boom acquisitions

# Data inputs:
# EDSM nightly dumps
#             populated system data
#             powerplay data
# INARA for Nakato reinforcement systems

# The sell price is dependent on the minor faction state, not the overall
# system state (state of the ruling minor faction only). So for each target
# system we need to check the states of all minor factions present.

# All information is taken from the EDSM nightly dumps, except for the powerplay.
# The EDSM powerplay data do not tell you who is in control of a reinforcement
# system, just which powers are present, which makes it a bit useless.
# Instead, I've gone the dirty route and scraped Nakato's INARA page.

# set --------------------------------------------------------------------------
# load libraries
library(tidyverse)
library(rvest)
library(httr2)
library(progress)

# manually add reinforced systems if the powerplay data are playing up
manual_reinforced <-
  list(stronghold = c(),
       fortified  = c())

# container for target systems
targets <-
  list()

# load powerplay ---------------------------------------------------------------
# load in pre-compiled powerplay data from EDSM
dir_processed <-
  "./data/processed/" #input

# find the latest local powerplay download
latest_powerplay <-
  tibble(file_full = list.files(dir_processed, full.names = T),
         file      = list.files(dir_processed)) |> 
  filter(grepl("power",file)) |> 
  mutate(date = gsub("sys_power_raw_|.rds","",file) |> 
           as.Date()) |> 
  filter(date == max(date)) |> 
  pull(file_full)

# load and unnnest coordinates
powerplay <-
  readRDS(latest_powerplay) |> 
  as_tibble() |> 
  unnest(coords)

# load populated ---------------------------------------------------------------
# find the latest populated systems download
latest_populated <-
  tibble(file_full = list.files(dir_processed, full.names = T),
         file = list.files(dir_processed)) |> 
  filter(grepl("pop",file)) |> 
  mutate(date = gsub("sys_pop_raw_|.rds","",file) |> 
           as.Date()) |> 
  filter(date == max(date)) |> 
  pull(file_full)

# load and unnnest coordinates
populated <-
  readRDS(latest_populated) |> 
  as_tibble() |> 
  unnest(coords)

# with coords only
populated_coords <-
  populated |> 
  select(name, x, y , z)

# subset Nakato reinforced systems ---------------------------------------------
# find reinforcement systems by webscraping INARA...
url <-
  "https://inara.cz/elite/power-controlled/13/"

resp <-
  request(url) |>
  req_user_agent("rvest scraper (research; contact: cmdr.rouailtagh@gmail.com)") |>
  req_headers("Accept-Language" = "en") |>
  req_perform()

page <-
  resp |>
  resp_body_html()

reinforced_all <-
  tibble(
    extraction =
      page |>
      html_elements(xpath = "//tr") |> 
      html_text2()) |> 
  
  separate(extraction,
           into = c("name",
                    "powerState",
                    "opposing_power",
                    "under",
                    "reinf",
                    "progress",
                    "dist",
                    "updated"),
           sep = "\\t") |> 
  select(name,
         powerState) |> 
  
  filter(powerState != "State") |> 
  
  # bullshit hidden unicode character removal
  mutate(name = stringi::stri_escape_unicode(name),
         name = gsub("\\\\ue81d\\\\ufe0e","",name),
         name = gsub("\\\\","",name)) |> 
  
  left_join(populated)

# keep only those with mineable rings
re_popped <-
  populated |> 
  filter(name %in% reinforced_all$name)

re_keepers    <- c()
re_rings_list <- list()

for(re in reinforced_all$name){
  
  # extract system bodies
  re_bodies <-
    re_popped |> 
    filter(name == re) |> 
    pull(bodies)
  
  # check to see if they have any rings
  if(length(grep("rings",colnames(re_bodies[[1]]))) > 0){
    
    # extract system rings
    re_rings_all <-
      re_bodies[[1]] |> 
      pull(rings)
    
    # extract rings (if any)
    re_rings_list[[re]] <-
      do.call(rbind.data.frame,
              re_rings_all[which(lengths(re_rings_all) > 0)]) |> 
      rename(ring_name = name) |> 
      mutate(name = re)
    
    re_keepers <-
      c(re_keepers,
        re)
  }
}

# compile all rings and their types
re_rings <-
  bind_rows(re_rings_list) |> 
  as_tibble() |> 
  select(name,ring_name,type) |> 
  rename(system_source = name)

# filter out reinforcement systems with no rings that can be mined
reinforced <-
  reinforced_all |> 
  filter(name %in% re_keepers) |> 
  mutate(date = as.POSIXct(date))

# record boom systems that can mine and sell in the same system
booming <-
  boom_update <-
  c()

for(sys in reinforced$name){
  
  # it's not about system state, but each minor faction, so we extract all the
  # minor factions present in a system
  factions <-
    populated |> 
    filter(name == sys) |> 
    pull(factions)
  
  # a bit sloppy, but active states can have multiple values, text searching the
  # unlisted column seems best
  booming <-
    c(booming,
      grepl("Boom",unlist(factions[[1]]$activeState)) |> sum())
  
  # keep the update time
  boom_update <-
    c(boom_update,
      unique(factions[[1]]$lastUpdate))
  
}

# compile all the self-selling reinforcement system information
targets$reinforcement <-
  reinforced |> 
  select(powerState,
         name,
         date) |> 
  mutate(boom_present = booming,
         boom_update  = as.POSIXct(boom_update)) |> 
  filter(boom_present == T) |> 
  
  rename(system_source        = name,
         system_source_update = date) |> 
  mutate(system_sell          = system_source,
         system_sell_update   = boom_update,
         ly = 0) |> 
  
  rename(system_sell_powerState = powerState) |> 
  select(-contains("boom"))

# find stronghold targets ------------------------------------------------------
# these are acquisition systems within the 30 ly radii of the stronghold systems
# with minable rings.
stronghold <-
  reinforced |> 
  filter(powerState == "Stronghold") |> 
  
  # create a 30 ly bounding box around each system
  mutate(xmax = x + 30,
         xmin = x - 30,
         ymax = y + 30,
         ymin = y - 30,
         zmax = z + 30,
         zmin = z - 30)

# loop through each system
targets$acquisitions <-
  list(stronghold = list(),
       fortified  = list())

strongholds <-
  stronghold$name

pb <-
  progress_bar$new(total = length(strongholds),
                   format = "strongholds: :current :total [:bar]")

for(strong in strongholds){
  
  pb$tick()
  
  # extract target bounding box
  bbox <-
    stronghold |> 
    filter(name == strong)
  
  # subset acquisition systems by bounding box to reduce computational load
  bbox_subset <-
    populated_coords |> 
    filter(x <= pull(bbox,xmax),
           x >= pull(bbox,xmin),
           y <= pull(bbox,ymax),
           y >= pull(bbox,ymin),
           z <= pull(bbox,zmax),
           z >= pull(bbox,zmin)) |> 
    filter(name != strong)
  
  # find euclidean distances between stronghold system and other systems
  coordinates <-
    rbind.data.frame(bbox |> select(name,x,y,z),
                     bbox_subset)
  
  coords_mat <-
    coordinates |> 
    select(-name) |> 
    as.matrix()
  
  distances <-
    tibble(name = bbox_subset$name,
           ly = fields::rdist(coords_mat)[-1,1]) |> # euclidean
    filter(ly <= 30) # filter to those actually within range
  
  # see if there are any boom factions in each system
  booming <-
    boom_update <-
    c()
  
  for(sys in distances$name){
    
    factions <-
      populated |> 
      filter(name == sys) |> 
      pull(factions)
    
    booming <-
      c(booming,
        grepl("Boom",unlist(factions[[1]]$activeState)) |> sum())
    
    boom_update <-
      c(boom_update,
        min(factions[[1]]$lastUpdate))
    
  }
  
  distances_booming <-
    distances |> 
    mutate(boom_present = booming,
           boom_update = as.POSIXct(boom_update)) |> 
    filter(boom_present == T)
  
  # potential systems
  potentials <-
    distances_booming |> 
    left_join(powerplay, by = "name") |> 
    select(name,ly,powerState,boom_update) |> 
    filter(powerState == "Unoccupied") |>
    distinct()
  
  # store systems
  if(dim(potentials)[1] > 0){
    targets$acquisitions$stronghold[[strong]] <-
      tibble(system_sell_powerState = potentials$powerState,
             system_sell        = potentials$name,
             system_sell_update = potentials$boom_update,
             system_source      = strong,
             system_source_update = bbox |> pull(date),
             ly                 = potentials$ly)
  }
}

# bind strongholds
targets$acquisitions$stronghold <-
  do.call(rbind.data.frame,
          targets$acquisitions$stronghold)

# find fortified systems -------------------------------------------------------
fortified <-
  reinforced |> 
  filter(powerState == "Fortified") |> 
  
  # create a 20 ly bounding box around each system
  mutate(xmax = x + 20,
         xmin = x - 20,
         ymax = y + 20,
         ymin = y - 20,
         zmax = z + 20,
         zmin = z - 20)

fortifieds <-
  fortified$name

pb <-
  progress_bar$new(total = length(fortifieds),
                   format = "fortifieds: :current :total [:bar]")

for(fort in fortifieds){
  
  pb$tick()
  
  # extract target bounding box
  bbox <-
    fortified |> 
    filter(name == fort)
  
  # subset acquisition systems by bbox
  bbox_subset <-
    populated_coords |> 
    filter(x <= pull(bbox,xmax),
           x >= pull(bbox,xmin),
           y <= pull(bbox,ymax),
           y >= pull(bbox,ymin),
           z <= pull(bbox,zmax),
           z >= pull(bbox,zmin)) |> 
    filter(name != strong)
  
  # find actual distances
  coordinates <-
    rbind.data.frame(bbox |> select(name,x,y,z),
                     bbox_subset)
  
  coords_mat <-
    coordinates |> 
    select(-name) |> 
    as.matrix()
  
  distances <-
    tibble(name = bbox_subset$name,
           ly = fields::rdist(coords_mat)[-1,1]) |> # euclidean distances
    filter(ly <= 20) # within range
  
  # see if there are any boom factions in each system
  booming <-
    boom_update <-
    c()
  
  for(sys in distances$name){
    
    factions <-
      populated |> 
      filter(name == sys) |> 
      pull(factions)
    
    booming <-
      c(booming,
        grepl("Boom",unlist(factions[[1]]$activeState)) |> sum())
    
    boom_update <-
      c(boom_update,
        min(factions[[1]]$lastUpdate))
    }
  
  distances_booming <-
    distances |> 
    mutate(boom_present = booming,
           boom_update = as.POSIXct(boom_update)) |> 
    filter(boom_present == T)
  
  # potential systems
  potentials <-
    distances_booming |> 
    left_join(powerplay, by = "name") |> 
    select(name,ly,powerState,boom_update) |> 
    filter(powerState == "Unoccupied") |> 
    distinct()
  
  # store systems
  if(dim(potentials)[1] > 0){
    targets$acquisitions$fortified[[fort]] <-
      tibble(system_sell_powerState         = potentials$powerState,
             system_sell        = potentials$name,
             system_sell_update = potentials$boom_update,
             system_source      = fort,
             system_source_update = bbox |> pull(date),
             ly                 = potentials$ly)
  }
}

# bind fortifieds
targets$acquisitions$fortified <-
  do.call(rbind.data.frame,
          targets$acquisitions$fortified)

# compile merit mining options -------------------------------------------------
targets$acquisitions <-
  do.call(rbind.data.frame,
          targets$acquisitions)

targets <-
  do.call(rbind.data.frame,
          targets)

# round distances up
targets_rounded <-
  targets |> 
  mutate(ly = round(ly,2))

# export -----------------------------------------------------------------------
# potential sell targets
write_csv(targets_rounded,
          "./outputs/mining/merit_mining/01_source_to_sell_systems.csv") #output

# potential source rings
write_csv(re_rings,
          "./outputs/mining/merit_mining/01_reinforced_rings.csv") #output

