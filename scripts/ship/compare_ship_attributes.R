# This script produces a comparative ship 

# set --------------------------------------------------------------------------
library(tidyverse)
library(scales)
library(ggpubr)

# fonts
library(showtext)
font_add(family = "Stonehenge", regular = "C:/Users/thoma/OneDrive/Documents/ed/fonts/stonehenge/stonehen.ttf")
font_add(family = "dune", regular = "C:/Users/thoma/OneDrive/Documents/fonts/dune_rise/Dune_Rise.ttf")
font_add(family = "nasa", regular = "C:/Users/thoma/OneDrive/Documents/fonts/nasalization/nasalization-rg.otf")
font_add(family = "starwars", regular = "C:/Users/thoma/OneDrive/Documents/fonts/star_jedi/starjedi/Starjedi.ttf")

showtext_auto()

# load -------------------------------------------------------------------------
data_all <-
  read_csv("./data/temp/ship_all.csv",
           show_col_types = F)

# wrangle ----------------------------------------------------------------------
data_plot <- 
  data_all %>% 
  mutate(punch = firepower)

# pick target attributes
attributes <-
  c("firepower",
    "speed",
    "boost",
    "agility",
    "shield",
    "armour",
    "optional",
    "military",
    "footprint",
    "jump")

# scale target attributes
scale_me <- c(attributes,"jump.max")
data_plot[scale_me] <-
  lapply(data_plot[scale_me], rescale)

# pivot for plotting
data_plot <-
  data_plot %>% 
  select(-jump) %>%
  rename(jump = jump.max) %>% 
  pivot_longer(cols = all_of(attributes),
               names_to = "attribute") %>% 
  mutate(attribute = factor(attribute,
                            levels = attributes))

# lower case all model names
data_plot$model <- 
  tolower(data_plot$model)

# plot -------------------------------------------------------------------------
# settings
text_colour <- "black"

# make plot
plot_me <-
  
  # base plot
  ggplot(data_plot,
         aes(x = value,
             y = reorder(model,punch),
             fill = punch)) +
  
  geom_col(position = "dodge",
           colour = "black",
           linewidth = 0.2) +
  
  # colour scheme
  scale_fill_gradient(high = "#FFA136",
                      low = "white") +
  
  # split panels
  facet_grid(rows = c("multicrew","attribute"),
             space = "free_y",
             scales = "free_y") +
  
  # theme choices
  theme_minimal(base_size = 8) +
  theme(
    
    axis.text.y  = element_text(color = text_colour, hjust = 0.9),
    axis.title.x = element_text(color = text_colour),
    axis.title.y = element_text(color = text_colour),
    legend.text  = element_text(color = text_colour),
    legend.title = element_text(color = text_colour),
    strip.text   = element_text(color = text_colour),
    
    text = element_text(family = "nasa"),
    
    axis.ticks.x = element_blank(),
    axis.text.x  = element_blank(),
    
    #plot.background  = element_rect(fill='transparent', color=NA),
    
    panel.background = element_blank(),
    panel.border     = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) +
  
  xlab("") +
  ylab("") 

# export -----------------------------------------------------------------------
ggsave("./plots/compare_ship_attributes.svg",
       height = 10,
       width = 20,
       plot_me)
