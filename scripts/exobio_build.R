# set --------------------------------------------------------------------------
library(readxl)
library(tidyverse)
library(scales)
library(ggrepel)
library(plotly)

# load -------------------------------------------------------------------------
# excel details
excel_name <-
  "./data/meta/ships.xlsx"

sheets <-
  excel_sheets(excel_name)

# load all sheets into list
data_ed <- list()
for(sheet in sheets){
  data_ed[[sheet]] <-
    read_excel(excel_name,
               sheet = sheet)
}

# wrangle ----------------------------------------------------------------------
attributes <- c("agility",
                "speed",
                "boost",
                "jump",
                "jump_max",
                "footprint")

exo <-
  left_join(data_ed$base,data_ed$dimensions,by = "Model") %>% 
  left_join(data_ed$jump,by = "Model") %>% 
  left_join(data_ed$core,by = "Model") %>% 
  mutate(footprint = rescale(width*length, to = c(0,1)),
         footprint_unscaled = width*length,
         speed     = rescale(SPEED, to = c(0,1)),
         boost     = rescale(BOOST, to = c(0,1)),
         agility   = rescale(AGILITY, to = c(0,1)),
         jump      = rescale(jump, to = c(0,1)),
         jump_max  = rescale(jump_max, to = c(0,1))) %>% 
  mutate(handling = SPEED+BOOST+AGILITY) %>% 
  #filter(multicrew == 1) %>% 
  #filter(grepl("Krait",Model)) %>% 
  #select(all_of(c("Model",attributes,"multicrew","footprint_unscaled"))) %>% 
  pivot_longer(cols = all_of(attributes),
               names_to = "attribute") %>% 
  mutate(attribute = factor(attribute,
                            levels = attributes))

# plot -------------------------------------------------------------------------
plots <- list()

# enhanced thrusters
exo_fast <-
  exo %>% 
  filter(Thrusters < 4)

# overview
plots$overview <-
  ggplot(exo,
         aes(x = value,
             y = reorder(Model,-footprint_unscaled),
             fill = log(footprint_unscaled))) +
  geom_col(position = "dodge",
           colour = "black") +
  #scale_fill_manual(values = c("#CC6677","#88CCEE","#44AA99","black")) +
  scale_fill_viridis_c() +
  theme_classic() +
  facet_grid(rows = c("multicrew","attribute"),
             #nrow = 2,
             space = "free_y",
             scales = "free_y") +
  ylab("") +
  xlab("") +
  labs(fill="log(width * length)",
       caption = "I figured the best ship would be something with a tiny footprint (for landing in tight terrain), good jump range, and super manoeuvrable. Whaddya reckon?",
       title = "Best plant scanner?") +
  theme(axis.ticks.x=element_blank(),
        axis.text.x=element_blank())

ggsave("./plots/exo_ships.png",
       plots$overview,
       height = 8*0.8,
       width = 12*0.8)

# footprint
plots$foot <-
  ggplot(exo,
         aes(x = footprint_unscaled)) +
  geom_density(aes(fill = factor(multicrew)),
               alpha = 0.5)
  







