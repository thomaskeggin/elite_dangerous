# set --------------------------------------------------------------------------
library(tidyverse)
library(plotly)
library(readxl)

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

# engineers
engineers <-
  read_excel("./data/engineering/engineer_locations.xlsx")

# wrangle ----------------------------------------------------------------------
# engineer systems
engineer_systems <-
  engineers |> 
  select(engineer,
         system)  |> 
  rename(name = system) |> 
  distinct() |> 
  
  filter(!engineer %in% c("Etienne Dorn",
                          "Marsha Hicks",
                          "Petra Olmanova",
                          "Mel Brandon"),
         grepl("Lori Jameson|Tah", engineer)) |> 
  
  mutate(type = "engineer") |> 
  
  rbind.data.frame(tibble(engineer = "CMDR Rouailtagh",
                          name = "Nessa",
                          type = "origin"))

# unnest coordinates
solar_systems_target <-
  solar_systems_raw |> 
  as_tibble() |> 
  unnest(coords) |> 
  right_join(engineer_systems)

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
          text = ~engineer,
          color = ~engineer)
fig <-
  fig %>%
  add_markers() |>
  layout(
    scene = list(
      aspectratio = list(x = 1, y = 1, z = 1)
    )
  )

fig






























