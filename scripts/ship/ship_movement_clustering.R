# set --------------------------------------------------------------------------
library(tidyverse)
library(readxl)
library(corrplot)
library(plotly)

# load -------------------------------------------------------------------------
agility <-
  read_excel("./data/agility/Elite_ Dangerous ship agility.xlsx",
             sheet = "summary") |> 
  rename(ship = `...1`,
         size = `...14`) |> 
  filter(!grepl("enh",ship))

agility_useful <-
  agility |> 
  as.data.frame() |> 
  select(-c(ship,size))

row.names(agility_useful) <-
  agility$ship

# look for correlations --------------------------------------------------------
correlations <-
  cor(agility_useful)

cor_filtered <-
  correlations

corrplot(correlations)

agility_uncorrelated <-
  agility_useful |> 
  mutate(pitch_roll = (nPitch50+nRoll50)/2) |> 
  select(pitch_roll,
         nYaw50)

ggplot(agility_uncorrelated) +
  geom_point(aes(x= pitch_roll,
                 y = nYaw50))

# wrangle ----------------------------------------------------------------------
agility_wrangled <-
  agility |> 
  
  # acceleration to speed ratio (correlated with acceleration, but not speed)
  # faster ships accelerate faster, so we want to know how well they accelerate
  # for their speed.
  mutate(acc_v_speed = aMaxF / vMax, # high values are good relative acceleration
         
         # and the same for pitching ability
         pitch_v_speed = nPitch50 / vMax,
         
         # numerise landing pad requirement
         pad = ifelse(size == "l",3,NA),
         pad = ifelse(size == "m",2,pad),
         pad = ifelse(size == "s",1,pad)) |> 
  
  # order ships by something
  arrange(vMax) |> 
  mutate(ship = factor(ship,levels = ship))

# compute distances ------------------------------------------------------------
# keep uncorrelated variables
agility_scaled <-
  agility_wrangled |>
  select(vMax,
         acc_v_speed,
         pitch_v_speed,
         nYaw50) |> 
  as.matrix()

rownames(agility_scaled) <- agility_wrangled$ship

agility_dist <-
  dist(agility_scaled) |> 
  hclust()


# good accelerators and pitchers -----------------------------------------------
acc_pitch <-
  ggplot(agility_wrangled) +
  
  geom_point(aes(x = acc_v_speed,
                 y = pitch_v_speed,
                 colour = vMax,
                 label = ship)) +
  scale_colour_viridis_c() +
  coord_fixed()

ggplotly(acc_pitch)

# ratios
acc_pitch_long <-
  agility_wrangled |> 
  pivot_longer(cols = c(acc_v_speed,pitch_v_speed),
               names_to = "ratio")

ggplot() +
  
  geom_segment(data = agility_wrangled,
               aes(y = ship,
                   x = acc_v_speed,
                   xend = pitch_v_speed)) +
  
  geom_point(data = acc_pitch_long,
             aes(y = ship,
               x = value,
               fill = ratio),
             shape = 21) +
  
  facet_grid(rows = vars(pad),
             scales = "free",
             space = "free") +
  
  scale_fill_manual(values = c("black","grey"))

# raw values
acc_pitch_long <-
  agility_wrangled |> 
  pivot_longer(cols = c(aMaxF,nPitch50),
               names_to = "ratio")

ggplot() +
  
  geom_segment(data = agility_wrangled,
               aes(y = ship,
                   x = aMaxF,
                   xend = nPitch50)) +
  
  geom_point(data = acc_pitch_long,
             aes(y = ship,
                 x = value,
                 fill = ratio),
             shape = 21) +
  
  facet_grid(rows = vars(pad),
             scales = "free",
             space = "free") +
  
  scale_fill_manual(values = c("black","grey"))

# pca the shit -------------------------------------------------------------
# no need, can reduce to only two dimensions
agility_pcable <-
  scale(agility_useful)


pca          <- prcomp(agility_pcable)
pca_df       <- as.data.frame(pca$x) |> 
  mutate(model =row.names(pca$x)) |> 
  as_tibble()

tmp <-
  ggplot(data = pca_df) +
  geom_point(aes(x= PC1,
                 y =PC2,
                 text = model))

plotly::ggplotly(tmp)




























