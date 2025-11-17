# set --------------------------------------------------------------------------
library(tidyverse)
library(ggforce)
library(scales)
library(readxl)
library(patchwork)

# load -------------------------------------------------------------------------
edsy_data <-
  read_csv("./data/temp/ship_edsy.csv") #input

agility <-
  read_excel("./data/agility/agility_cleaned.xlsx") #input

# wrangle ----------------------------------------------------------------------
agility_wrangled <-
  agility |> 
  left_join(edsy_data) |> 
  
  # filter
   filter(pad == "S") |>
  
  group_by(model) |> 
  filter(enhanced_thrusters == max(enhanced_thrusters)) |> 
  ungroup() |> 
  
  na.omit() |> 
  
  # convert roll to radians for plotting
  mutate(roll = rescale(nRoll50,
                        to = c(pi*0.1,pi*0.2)),
         
         
         # scale values to get colours
         col_pitch = rescale(nPitch50, from = c(0, max(nPitch50))),
         col_yaw = rescale(nYaw50, from = c(0, max(nYaw50))),
         col_roll = rescale(nRoll50, from = c(0, max(nRoll50))),
         
         col_afor = rescale(aMaxF, from = c(0, max(aMaxF))),
         col_arev = rescale(aMaxR, from = c(0, max(aMaxR))),
         col_aside = rescale(aMaxS, from = c(0, max(aMaxS)))) |> 
  
  arrange(vMax) |> 
  
  mutate(model = factor(model,levels = model))


# maximum yaw and pitch
yaw_pitch <-
  c(agility_wrangled$nYaw50,
    agility_wrangled$nPitch50) |> 
  max()

# plot rotation ----------------------------------------------------------------
fill_width <- 0.5
outline_width <- 1
palette_direction <- 1
arrow_size <- unit(2, "mm")

plot_acceleration <-
  ggplot(agility_wrangled) +
  
  # yaw outline
  geom_segment(aes(x = -nYaw50,
                   xend = nYaw50,
                   y = 0),
               linewidth = outline_width,
               colour = "black",
               arrow = arrow(end = "both",
                             angle = 30,
                             length = arrow_size)) +
  
  # yaw
  geom_segment(aes(x = -nYaw50,
                   xend = nYaw50,
                   y = 0,
                   colour = col_yaw),
               linewidth = fill_width,
               arrow = arrow(end = "both",
                             angle = 30,
                             length = arrow_size)) +
  
  # roll outline
  geom_arc(aes(x0 = 0,
               y0 = 0,
               r = yaw_pitch / 2,
               start = -roll,
               end = roll),
           linewidth = outline_width,
           arrow = arrow(end = "both",
                         angle = 30,
                         length = arrow_size)) +
  
  # roll
  geom_arc(aes(x0 = 0,
               y0 = 0,
               r = yaw_pitch / 2,
               start = -roll,
               end = roll,
               colour = col_roll),
           linewidth = fill_width,
           arrow = arrow(end = "both",
                         angle = 30,
                         length = arrow_size)) +
  
  # pitch outline
  geom_segment(aes(x = 0,
                   y = -nPitch50,
                   yend = nPitch50),
               linewidth = outline_width,
               arrow = arrow(end = "both",
                             angle = 30,
                             length = arrow_size)) +
  
  # pitch
  geom_segment(aes(x = 0,
                   y = -nPitch50,
                   yend = nPitch50,
                   colour = col_pitch),
               linewidth = fill_width,
               arrow = arrow(end = "both",
                             angle = 30,
                             length = arrow_size)) +
  
  # colours
  scale_colour_viridis_c(option = "G",
                         direction = palette_direction) +
  
  facet_grid(cols = vars(model)) +
  
  coord_fixed() +
  
  # theme and labels
  labs(colour = "Scaled rotation\nvalue",
       y = "Pitch at 50% thrust",
       x = "Yaw at 50% thrust",
       title = "Rotation") +
  
  theme_bw()

# plot acceleration ------------------------------------------------------------
plot_accelaration <-
  ggplot(agility_wrangled) +
  
  # sideways outline
  geom_segment(aes(x = -aMaxS,
                   xend = aMaxS,
                   y = 0),
               linewidth = outline_width,
               colour = "black",
               arrow = arrow(end = "both",
                             angle = 30,
                             length = arrow_size)) +
  
  # sideways
  geom_segment(aes(x = -aMaxS,
                   xend = aMaxS,
                   y = 0,
                   colour = col_aside),
               linewidth = fill_width,
               arrow = arrow(end = "both",
                             angle = 30,
                             length = arrow_size)) +
  
  # forwards outline
  geom_segment(aes(x = 0,
                   y = 0,
                   yend = aMaxF),
               linewidth = outline_width,
               arrow = arrow(end = "last",
                             angle = 30,
                             length = arrow_size)) +
  
  # forwards
  geom_segment(aes(x = 0,
                   y = 0,
                   yend = aMaxF,
                   colour = col_afor),
               linewidth = fill_width,
               arrow = arrow(end = "last",
                             angle = 30,
                             length = arrow_size)) +
  
  # reverse outline
  geom_segment(aes(x = 0,
                   y = 0,
                   yend = -aMaxR),
               linewidth = outline_width,
               arrow = arrow(end = "last",
                             angle = 30,
                             length = arrow_size)) +
  
  # reverse
  geom_segment(aes(x = 0,
                   y = 0,
                   yend = -aMaxR,
                   colour = col_arev),
               linewidth = fill_width,
               arrow = arrow(end = "last",
                             angle = 30,
                             length = arrow_size))  +
  
  # colours
  scale_colour_viridis_c(option = "A",
                         direction = palette_direction) +
  
  facet_grid(cols = vars(model)) +
  
  coord_fixed() +
  
  # theme and labels
  labs(colour = "Scaled acceleration\nvalue",
       y = "Backwards <-> Forwards acceleration",
       x = "Sideways acceleration",
       title = "Acceleration") +
  
  theme_bw()

# plot speed -------------------------------------------------------------------
plot_speed <-
  
  ggplot(agility_wrangled) +
  
  geom_col(aes(x = model,
               y = vMax,
               fill = model),
           colour = "black") +
  
  # theme and labels
  labs(y = "Maximum velocity",
       x = "",
       title = "Velocity") +
  
  theme_bw() +
  theme(legend.position = "none")


# compile ----------------------------------------------------------------------
# rotation plot h x w ratio
rot_ratio <-
  max(agility_wrangled$nPitch50) / max(agility_wrangled$nYaw50)

# rotation plot height as function of width
rot_w <-
  max(agility_wrangled$nYaw50) * dim(agility_wrangled)[1]

rot_h <-
  rot_w * rot_ratio

rot_h_scaled <-
  rot_h / rot_w

# acceleration plot h x w ratio
acc_ratio <-
  max(agility_wrangled$aMaxF) / max(agility_wrangled$aMaxS)

# rotation plot height as function of width
acc_w <-
  max(agility_wrangled$aMaxS) * dim(agility_wrangled)[1]

acc_h <-
  acc_w * acc_ratio

acc_h_scaled <-
  acc_h / acc_w

# plot
plot_compiled <-
  plot_acceleration /
  plot_accelaration +
  
  plot_layout(heights = c(rot_h_scaled,
                          acc_h_scaled))

# export -----------------------------------------------------------------------
ggsave("./plots/model_rotation_acceleration.png",
       plot_compiled,
       height = 10,
       width = 3 + (2*dim(agility_wrangled)[1]),
       limitsize = FALSE)
