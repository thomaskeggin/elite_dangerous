# set --------------------------------------------------------------------------
library(tidyverse)
library(scales)
library(ggpubr)

# fonts
library(showtext)
font_add(family = "Stonehenge", regular = "C:/Users/thoma/OneDrive/Documents/ed/fonts/stonehenge/stonehen.ttf")
font_add(family = "Starjedi", regular = "C:/Users/thoma/OneDrive/Documents/ed/fonts/star_jedi/starjedi/Starjhol.ttf")

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
    "SPEED",
    "BOOST",
    "AGILITY",
    "SHIELD",
    "ARMOUR",
    "optional",
    "military",
    "footprint",
    "jump")

# scale target attributes
scale_me <- c(attributes,"jump_max")
data_plot[scale_me] <-
  lapply(data_plot[scale_me], rescale)

# pivot for plotting
data_plot <-
  data_plot %>% 
  select(-jump) %>%
  rename(jump = jump_max) %>% 
  pivot_longer(cols = all_of(attributes),
               names_to = "attribute") %>% 
  mutate(attribute = factor(attribute,
                            levels = attributes))

# lower case all model names
data_plot$attribute <- 
  tolower(data_plot$attribute)
data_plot$Model <- 
  tolower(data_plot$Model)

# plot -------------------------------------------------------------------------
# settings
text_colour <- "black"


# make plot
plot_me <-
  
  # base plot
  ggplot(data_plot,
         aes(x = value,
             y = reorder(Model,punch),
             fill = punch)) +
  
  geom_col(position = "dodge",
           colour = "transparent") +
  
  # colour scheme
  scale_fill_gradient(high = "#FFA136",
                      low = "white") +
  
  # split panels
  facet_grid(rows = c("multicrew","attribute"),
             #nrow = 2,
             space = "free_y",
             scales = "free_y") +
  
  # theme choices
  theme_minimal() +
  theme(
    
    axis.text.y = element_text(color = text_colour),
    axis.title.x = element_text(color = text_colour),
    axis.title.y = element_text(color = text_colour),
    legend.text = element_text(color = text_colour),
    legend.title = element_text(color = text_colour),
    strip.text = element_text(color = text_colour),
    
    
    text = element_text(family = "Starjedi"),
    
    axis.ticks.x=element_blank(),
    axis.text.x=element_blank(),
    
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) +
  
  xlab("") +
  ylab("") 

# export -----------------------------------------------------------------------
ggsave("./plots/test.pdf",
       plot_me)
