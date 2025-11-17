# set --------------------------------------------------------------------------
library(readxl)
library(tidyverse)
library(scales)
library(tidytree)
library(ggrepel)

attributes <-
  c("model",
    "pad",
    "volume",
    #"vMax",
    "aMaxF",
    #"nPitch50",
    "nYaw50")

enhanced_thruster_ships <-
  c("Adder",
    "Eagle",
    "Hauler",
    "Imperial Courier",
    "Imperial Eagle",
    "Sidewinder",
    "Viper Mk III")

# load -------------------------------------------------------------------------
# cargo racks 
cargo_racks <-
  read_excel("./data/temp/cargo_racks.xlsx") |> #input
  rename(optional = Class,
         cargo = Capacity) |> 
  select(optional,cargo)

# optionals
optionals <-
  read_csv("./data/temp/optionals_edsy.csv", #input
           show_col_types = F)

# all other data
data_all <-
  read_csv("./data/temp/ship_edsy.csv", #input
           show_col_types = F) |> 
  
  left_join(read_excel("./data/agility/agility_cleaned.xlsx")) |> 
  
  # remove non-enhanced
  filter(!(enhanced_thrusters == 0 & 
             model %in% enhanced_thruster_ships))

# wrangle ----------------------------------------------------------------------
# figure out how much cargo can be held after losing 4 slots to mining gear.
cargo_after_equipment <-
  optionals |> 
  group_by(model) |> 
  mutate(order = n():1) |> 
  
  filter(order > 4) |> 
  left_join(cargo_racks) |> 
  
  reframe(cargo = sum(cargo))

# compile model data
data_model <-
  data_all |> 
  
  # keep only more detailed movement info
  select(-c(speed,cargo)) |> 
  
  # remove large ships
  filter(pad != "L") |> 
  
  # keep only core mining stuff
  select(all_of(attributes)) |> 
  
  left_join(cargo_after_equipment)

# change pad size to numbers
data_model$pad <- as.numeric(as.factor(data_model$pad))

# shift ship model to rownames
all_quant <- as.data.frame(data_model |> select(-c(model,pad)))
rownames(all_quant) <- data_model$model

# scale variables
all_quant_stand <- scale(all_quant)

# phylo ------------------------------------------------------------------------
# distance matrix
ship_dist    <- dist(all_quant_stand)

# tree
ship_hclust  <- hclust(ship_dist)
ship_phylo   <- as.phylo(ship_hclust) # phylo object

# pca --------------------------------------------------------------------------
# calculate and format pca
pca          <- prcomp(all_quant_stand)
pca_df       <- as.data.frame(pca$x)

# add labels to model PCA table
pca_df_x <-
  as_tibble(pca$x,
            rownames = "model") |> 
  
  left_join(data_all,
            by = "model") |> 
  
  arrange(PC1) |> 
  
  mutate(model = factor(model,levels = model))

# add labels to rotation table
pca_df_rotation <-
  as_tibble(pca$rotation,
            rownames = "attribute")

# plot -------------------------------------------------------------------------
ggplot() +
  
  # loadings
  geom_segment(data = pca_df_rotation,
               aes(x = 0, y = 0,
                   xend = (PC1*4.9),
                   yend = (PC2*4.9)),
               color = "lightgrey") +
  geom_text(data = pca_df_rotation,
            aes(x = PC1*5,
                y = PC2*5,
                label = attribute)) +
  
  # points
  geom_point(data = pca_df_x,
             aes(x=PC1,
                 y=PC2)) +
  
  # labels
   geom_label_repel(data = pca_df_x,
                    aes(x=PC1,
                        y=PC2,
                        label = model)) +
  
  # layout
  coord_fixed() +
  
  # theme
  theme_classic() +
  theme(legend.position = "none")



# cargo vs acceleration
ggplot(data_model) +
  
  geom_point(aes(x = volume,
                 y = aMaxF,
                 size = cargo),
             shape = 15,
             alpha = 0.5) +
  
  geom_text_repel(aes(x = volume,
                      y = aMaxF,
                      label = model),
                  alpha = 0.5,
                  force = 5)


