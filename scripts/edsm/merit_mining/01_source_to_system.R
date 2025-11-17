# The idea is to generate a short list of systems to mine in their respective
# sell systems.
# i.e., reinforced with rings -> boom acquisitions
# the EDSM powerplay data do not tell you who is in control of a reinforcement
# system, just which powers are present, which makes it a bit useless.
# Would be better to source this info from INARA using an API query.

# set --------------------------------------------------------------------------
library(tidyverse)
library(progress)

# manually add reinforced
manual_reinforced <-
  list(stronghold = c(),
       fortified = c())

targets <-
  list()

# load powerplay ---------------------------------------------------------------
dir_processed <-
  "./data/processed/" #input

# find the latest powerplay download
latest_powerplay <-
  tibble(file_full = list.files(dir_processed, full.names = T),
         file = list.files(dir_processed)) |> 
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
# # find all reinforced Nakato systems using messy EDSM powerplay
# reinforced_all <-
#   powerplay |> 
#   filter(power == "Nakato Kaine" &
#            powerState %in% c("Stronghold",
#                              "Fortified"))
# 
# # remove manual if already there
# manual_reinforced$stronghold <-
#   manual_reinforced$stronghold[which(!manual_reinforced$stronghold %in% reinforced_all$name)]
# 
# reinforced_all <-
#   # add manual systems
#   rbind.data.frame(
#     reinforced_all,
#     tibble(populated_coords |> 
#              filter(name %in% manual_reinforced$stronghold),
#            
#            power = "Nakato Kaine",
#            powerState = "Stronghold",
#            id = NA,
#            id64 = NA,
#            allegiance = NA,
#            government = NA,
#            state = NA,
#            date = Sys.time() |> as.character()))

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

# keep only those with minable rings
re_popped <-
  populated |> 
  filter(name %in% reinforced_all$name)

re_keepers <- c()
re_rings_list   <- list()

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

re_rings <-
  do.call(rbind.data.frame,
          re_rings_list) |> 
  as_tibble() |> 
  select(name,ring_name,type) |> 
  rename(system_source = name)

reinforced <-
  reinforced_all |> 
  filter(name %in% re_keepers) |> 
  mutate(date = as.POSIXct(date))

# record boom systems that could self-sell
booming <-
  boom_update <-
  c()

for(sys in reinforced$name){
  
  factions <-
    populated |> 
    filter(name == sys) |> 
    pull(factions)
  
  booming <-
    c(booming,
      grepl("Boom",unlist(factions[[1]]$activeState)) |> sum())
  
  boom_update <-
    c(boom_update,
      unique(factions[[1]]$lastUpdate))
  
}

targets$reinforcement <-
  reinforced |> 
  select(powerState,
         name,
         date) |> 
  mutate(boom_present = booming,
         boom_update = as.POSIXct(boom_update)) |> 
  filter(boom_present == T) |> 
  
  rename(system_source = name,
         system_source_update = date) |> 
  mutate(system_sell = system_source,
         system_sell_update = boom_update,
         ly = 0) |> 
  
  rename(system_sell_powerState = powerState) |> 
  select(-contains("boom"))

# find stronghold targets ------------------------------------------------------
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
           ly = fields::rdist(coords_mat)[-1,1]) |> 
    filter(ly <= 30)
  
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
           ly = fields::rdist(coords_mat)[-1,1]) |> 
    filter(ly <= 30)
  
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
          "./outputs/mining/merit_mining/boom_acquisitions.csv") #output

# potential source rings
write_csv(re_rings,
          "./outputs/mining/merit_mining/reinforced_rings.csv") #output

