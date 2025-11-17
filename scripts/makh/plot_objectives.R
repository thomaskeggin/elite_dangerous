# set --------------------------------------------------------------------------
library(tidyverse)
library(plotly)

objective_systems <-
  tibble(name =
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
             "Wolf 718"),
         
         wing = 
           c("POI",
             "POI",
             "POI",
             rep("Vorpal",3),
             rep("Gladius",4)))

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

# wrangle ----------------------------------------------------------------------
# unnest coordinates
solar_systems_coords <-
  solar_systems_raw |>  
  as_tibble() |> 
  unnest(cols = coords)

solar_systems_target <-
  solar_systems_coords |> 
  as_tibble() |> 
  right_join(objective_systems)

# solar_systems_all <-
#   solar_systems_coords |> 
#   
#   filter(x < max(x + 10),
#          x > min(x - 10),
#          
#          y < max(y + 10),
#          y > min(y - 10),
#          
#          z < max(z + 10),
#          z > min(z - 10)) |> 
#   
#   right_join(objective_systems)

# plot -------------------------------------------------------------------------
fig <-
  plot_ly(solar_systems_target,
          x = ~x,
          y = ~y,
          z = ~z,
          text = ~name,
          color = ~wing,
          colors = c("#F26B6B","#03A6A6","#D9A86C"))
fig <-
  fig %>%
  add_markers() |>
  layout(
    scene = list(
      aspectratio = list(x = 1, y = 1, z = 1)
    )
  )

fig






























