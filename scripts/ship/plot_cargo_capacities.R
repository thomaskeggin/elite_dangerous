# set --------------------------------------------------------------------------
library(tidyverse)
library(readxl)

# load -------------------------------------------------------------------------
# cargo racks
cargo_cap <-
  tibble(class = c(1:8),
         cargo = c(2,4,8,16,32,64,128,256))

# ship optionals
ship_optionals <-
  read_excel("./data/meta/ships.xlsx", #input
             sheet = 4)

# ship data
ship_data <-
  read_csv("./data/temp/ship_all.csv", #input
           show_col_types = F)

# wrangle ----------------------------------------------------------------------
# cargo capacities
cargo_capacities <-
  ship_optionals |> 
  
  select(!contains("_m")) |> 
  
  pivot_longer(cols = contains("class"),
               names_to = "class",
               values_to = "n") |> 
  
  mutate(class = parse_number(class)) |> 
  
  left_join(cargo_cap) |> 
  
  mutate(cargo_sum = n * cargo) |> 
  
  group_by(Model) |> 
  
  reframe(cargo_capacity = sum(cargo_sum)) |> 
  
  rename(model = Model)

# combine
ship_all <-
  ship_data |> 
  left_join(cargo_capacities) |> 
  mutate(box_volume = width * height * length) |> 
  
  arrange(box_volume) |> 
  mutate(model = factor(model,
                        levels = model),
         
         # power distributor ratio
         pd_ratio = power.distributor /
           (sensors +
              thrusters +
              fsd +
              life.support))

# explore ----------------------------------------------------------------------
tmp <-
  ship_all |> 
  ##filter(pad == "Small") |> 
  arrange(power.plant) |> 
  mutate(model = factor(model,
                        levels = model))

# power dist ratio
ggplot(tmp) +
  
  geom_col(aes(y = model,
               x = pd_ratio,
               fill = cargo_capacity)) +
  scale_fill_viridis_c()

ggplot(tmp) +
  
  geom_label(aes(y = cargo_capacity,
               x = pd_ratio,
               label = model))


ggplot(ship_all) +
  
  geom_col(aes(y = model,
               x = box_volume,
               fill = pad))



# mass and thrusters
ggplot(ship_all) +
  
  geom_point(aes(x = ))



