# set --------------------------------------------------------------------------
library(readxl)
library(tidyverse)
library(ggrepel)
library(ggfortify)
library(viridis)
library(scales)
library(plotly)
library(ggtree)
library(tidytree)
#library(ggtreeExtra)
library(ggstar)
library(ggforce)
library(wesanderson)

attributes <-
  c("speed",
    "agility",
    "shield",
    "armour",
    "firepower",
    "optional",
    "military",
    #"footprint",
    "jump.max",
    "Utility")

k_groups <- 12

# fonts
#library(showtext)
#font_add(family = "Stonehenge", regular = "./fonts/stonehenge/stonehen.ttf")
#font_add(family = "Starjedi", regular = "./fonts/star_jedi/starjedi/Starjhol.ttf")

#showtext_auto()
font_family <- "sans"

# load -------------------------------------------------------------------------
data_all <-
  read_csv("./data/temp/ship_all.csv",
           show_col_types = F)

# wrangle ----------------------------------------------------------------------
# change pad size to numbers
data_all$pad <- as.numeric(as.factor(data_all$pad))

# shift ship model to rownames
all_quant <- as.data.frame(data_all[,-1])
rownames(all_quant) <- data_all$model

all_quant <-
  all_quant %>% 
  #filter(pad == 1) %>% 
  #filter(Thrusters < 4) %>% 
  select(any_of(attributes))

# scale variables
all_quant_stand <- scale(all_quant)

# phylo ------------------------------------------------------------------------
# distance matrix
ship_dist    <- dist(all_quant_stand)

# tree
ship_hclust  <- hclust(ship_dist)
ship_phylo   <- as.phylo(ship_hclust) # phylo object

# kmeans groups
ship_groups  <- cutree(ship_hclust, k = k_groups) # kmeans
ship_groups  <- tibble(model = names(ship_groups),
                       group = factor(ship_groups))

ship_group_list <- list()
for(i in unique(ship_groups$group)){
  
  ship_group <-
    ship_groups %>% 
    filter(group == i)
  
  ship_group_list[[i]] <-
    unique(ship_group$model)
}

# data frame for plotting
ship_tree_df <-
  tidytree::as_tibble(ship_phylo) %>%
  rename(model = label) %>% 
  left_join(ship_groups, by = "model") %>% 
  arrange(group)

ship_tree <-
  as.treedata(ship_tree_df)

tree_supp_data <-
  ship_groups %>% 
  rename(id = model,
         Function = group)

# assign groups to nodes
ship_group_nodes <- 
  data.frame(group = unique(ship_groups$group) , node=NA)

for(i in 1:length(ship_group_list)){
  
  #Get the MRCA for these taxa and put in into the dataframe
  ship_group_nodes[i, "node"] <-
    tidytree::MRCA(ship_phylo, .node1=ship_group_list[[i]])
  
}

# find niche -------------------------------------------------------------------
# ranked attributes
ship_niche <-
  all_quant %>% # unscaled attribute values
  data.frame() %>% # to data frame
  mutate(model = row.names(all_quant_stand)) %>% # ship names from row names
  pivot_longer(cols = any_of(attributes), # pivot for attributes
               names_to = "attribute") %>% 
  mutate(attribute = tolower(attribute)) %>% # sort out names
  group_by(attribute) %>%  # group by attrbute
  mutate(value = rescale(value)) %>%  # scale attribute values by attribute
  arrange(model,value) %>% # arrange by ship and their attribute value
  group_by(model) %>% 
  mutate(rank=row_number()) %>%  # rank attributes
  left_join(ship_groups, by = "model") %>% # add ship groups
  ungroup()

group_niche <-
  ship_niche %>% 
  group_by(group,attribute) %>% # regroup by functional group and attributes
  summarise(rating = sum(rank)/n(),
            group_value = mean(value),  
            .groups = "keep") %>% # rate mean attribute rank per group
  arrange(group,-rating)

group_niche$id <-
  1:dim(group_niche)[1]

# top 3
niche_top <-
  group_niche %>% 
  group_by(group) %>% 
  slice_max(order_by = rating,
            n = 3) %>% 
  mutate(top = 1)

# merge
niche_all <-
  niche_top %>% 
  ungroup() %>% 
  select(c(id,top)) %>% 
  right_join(group_niche, by = "id")

niche_all$top[is.na(niche_all$top)] <- 0.5

# pca --------------------------------------------------------------------------
# calculate and format pca
pca          <- prcomp(all_quant_stand)
pca_df       <- as.data.frame(pca$x)
pca_mat      <- as.matrix(pca_df)

# add labels
pca_df_x <-
  data.frame(pca$x) %>% 
  mutate(model = row.names(pca$x)) %>% 
  left_join(ship_groups,
            by = "model") %>% 
  left_join(data_all,
            by = "model")

pca_df_rotation <-
  data.frame(pca$rotation) %>% 
  mutate(attribute = row.names(pca$rotation))

# find hull for plot
hull <-
  pca_df_x %>%
  group_by(group) %>% 
  slice(chull(PC1,PC2))

# plot pca (plotly) ------------------------------------------------------------
fig <- plot_ly()

# ships
fig <- fig %>% 
  add_text(data = pca_df_x,
           x = ~PC1,
           y = ~PC2,
           z = ~PC3,
           color = ~group,
           mode = "text",
           #size = 1,
           text = ~model)

# loadings
fig2 <- fig %>% 
  add_text(fig,
           data = pca_df_rotation,
           x = ~PC1,
           y = ~PC2,
           z = ~PC3,
           text = ~attribute)

# plot pca (ggplot) ------------------------------------------------------------
pca_gg <-
  ggplot(pca_df_x,
         aes(x=PC1,
             y=PC2)) +  
  
  # loadings
  geom_segment(data = pca_df_rotation,
               aes(x = 0, y = 0,
                   xend = (PC1*4.9),
                   yend = (PC2*4.9)),
               color = "lightgrey") +
  geom_text(data = pca_df_rotation,
            aes(x = PC1*5,
                y = PC2*5,
                label = attribute),
            family = font_family) +
  
  # ellipses
  #stat_ellipse(aes(fill = group),
  #             geom = "polygon",
  #             alpha = 0.1) +
  
  # hull plot
  geom_polygon(data = hull,
               aes(fill = group),
               alpha = 0.2) +
  
  # points
  geom_point(aes(colour = group)) +
  
  # labels
  geom_label_repel(aes(label = model,
                       colour = group),
                   family = font_family) +
  
  # theme
  theme_classic() +
  theme(text = element_text(family = font_family),
        legend.position = "none")

# plot tree --------------------------------------------------------------------
p1 <-
  # base tree
  ggtree(ship_phylo,
         layout = "circular") +
  
  # tree aesthetics
  geom_tree() +
  
  # group boxes
  geom_fruit(data=ship_groups, 
             geom=geom_tile, 
             mapping=aes(y=model,
                         fill = group),
             colour = "transparent",
             linewidth = 1.25,
             width = 0.3,
             offset = 0) +
  
  # labels
  geom_tiplab(geom = "text",
              offset = 0.75) +
  
  # theme
  theme_tree2(plot.margin=margin(rep(100,4))) +
  theme_void() +
  theme(legend.position = "none") +
  xlim_tree(5.5)

p2 <- p1

for(j in 1:nrow(ship_group_nodes)){
  #Then add each clade label
  p2 <- p2 +
    geom_cladelabel(node=ship_group_nodes$node[j],
                    label=ship_group_nodes$group[j],
                    offset = 0.2,
                    angle="auto")
  
}
p2

# plot niche -------------------------------------------------------------------
# group
plot_niche_group <-
  ggplot(niche_all) +
  geom_col(aes(x = attribute,
               y = group_value,
               fill = group,
               alpha = top)) +
  scale_alpha_identity() +
  facet_wrap(~group) +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.line.x = element_blank(),
        axis.title.x = element_blank())
  

# by ship
plot_data <-
  ship_niche %>% 
  filter(group == 6)

plot_niche_ship <-
  ggplot(ship_niche) +
  geom_col(aes(x = model,
               y = value,
               fill = group),
           position = "dodge",
           colour = "black") +
  facet_grid(rows = c("group","attribute"),
             space = "free",
             scales = "free") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank())

# export -----------------------------------------------------------------------
sizing <- 15

# tree
ggsave("./plots/ship_tree.jpg",
       plot = p2,
       height = sizing,
       width = sizing)

# niche
ggsave("./plots/ship_niche.jpg",
       plot = plot_niche_group,
       height = sizing*0.66,
       width = sizing*0.66)

# pca
ggsave("./plots/ship_pca.jpg",
       plot = pca_gg,
       height = sizing,
       width = sizing)






