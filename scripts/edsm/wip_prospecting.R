# map out potential mining spots in a target system
# set --------------------------------------------------------------------------
library(tidyverse)

target_system <-
  "HIP 63990"

# load -------------------------------------------------------------------------
sys_pop_raw <- 
  readRDS(paste0("./data/processed/sys_pop_raw_2025-09-12.rds")) |> 
  tibble() |> 
  filter(name == target_system)

# wrangle ----------------------------------------------------------------------
stations <-
  sys_pop_raw |> 
  select(stations) |> 
  unnest(cols = stations)

bodies <-
  sys_pop_raw |> 
  select(bodies) |> 
  unnest(cols = bodies)

ringed_bodies <-
  bodies |> 
  select(id,name, rings) |>
  rename(planet = name) |>
  filter(lengths(rings) > 0) |>
  unnest(rings)

# export -----------------------------------------------------------------------
write_csv(ringed_bodies,
          paste0("outputs/mining/raw/planets_",
                 target_system,
                 ".csv"))
