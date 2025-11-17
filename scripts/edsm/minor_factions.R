# set --------------------------------------------------------------------------
library(tidyverse)
library(plotly)

# load -------------------------------------------------------------------------
sys_pop_raw <- 
  readRDS("./data/processed/sys_pop_raw.rds")

# wrangle ----------------------------------------------------------------------
# systems and their ruling minor factions
systems_mf <-
  sys_pop_raw |>
  tibble::tibble() |>
  dplyr::select(id,name,coords,controllingFaction) |>
  tidyr::unnest(cols = c(coords,controllingFaction), names_sep = "_")

# how many systems per faction? ------------------------------------------------
systems_ruled <-
  systems_mf |>
  dplyr::group_by(controllingFaction_name) |>
  dplyr::summarise(n = dplyr::n()) |>
  dplyr::arrange(-n)

# plot distribution
data_plot <-
  systems_ruled |>
  filter(n > max(n)*0.4)

ggplot(data_plot) +
  geom_histogram(aes(x = n),
                 bins = 100,
                 fill = "grey") +
  theme_classic()

# plot 2d layout
data_map <-
  systems_mf |>
  filter(controllingFaction_name %in% data_plot$controllingFaction_name)

map <-
  ggplot(data_map) +
  geom_point(aes(x=coords_x,y=coords_y,
                 colour = controllingFaction_name)) +
  theme(legend.position = "none")

map <-
  
  # main plot
  plot_ly(data_map,
          x = ~coords_x,
          y = ~coords_y,
          z = ~coords_z,
          text = ~paste(name),
          color = ~controllingFaction_name) |>
  
  # marker settings
  add_markers(size = 1) |>
  
  # layout
  layout(scene = list(
    aspectratio = list(x=1,y=1,z=1),
    xaxis = list(range = list(-250,250)),
    yaxis = list(range = list(-250,250)),
    zaxis = list(range = list(-250,250)))
  )



map  
