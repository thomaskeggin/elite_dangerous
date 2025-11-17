# set --------------------------------------------------------------------------
library(tidyverse)
library(plotly)

target_system_names <-
  c(# POI
    "CE Bootis",
    "Sol",
    "Nessa",
    
    # Vorpal
    "LTT 6377",
    "Col 285 Sector TW-O b21-1",
    "Scorpii Sector DL-Y d125",
    
    # Gladius
    "LHS 3277",
    "Alrai Sector FG-X b1-5",
    "LHS 470",
    "Wolf 718")

# load -------------------------------------------------------------------------
dir_systems <-
  "./data/processed/"

files_systems <-
  list.files(dir_systems,
             full.names = T)

target_file <-
  file.info(files_systems) |> 
  as_tibble(rownames = "file") |> 
  filter(grepl("sys_pop_raw_",file)) |> 
  arrange(mtime) |> 
  tail(1) |> 
  pull(file)

solar_systems_raw <-
  readRDS(target_file)

# extract and calculate --------------------------------------------------------
target_systems <-
  solar_systems_raw |> 
  select(name,coords) |> 
  filter(name %in% target_system_names) |> 
  unnest(cols = coords)

target_systems_mat <-
  target_systems |> 
  select(-name) |> 
  as.matrix()

rownames(target_systems_mat) <-
  target_systems$name

distances <-
  dist(target_systems_mat,
       diag = T)

as.matrix(distances) |> 
  as_tibble(rownames = "name") |> 
  pivot_longer(cols = -name,
               names_to = "target_system",
               values_to = "ly") |> 
  
  filter(name == "Nessa") |> 
  arrange(ly)


