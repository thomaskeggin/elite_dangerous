# set ----
library(tidyverse)
library(readxl)
library(plotly)

phantom_mass <-
  270

# load ----
list_core_stats <-
  readRDS("./data/list_core_stats.rds")

# phantom slots ----
class_module <-
  data.frame(module = names(list_core_stats),
             class  = c(6,7,5,4,7,6,5))

phantom_modules <-
  list_core_stats

for(type in names(phantom_modules)){
  
  class_target <-
    class_module[which(class_module==type),2]
  
  phantom_modules[[type]] <-
    phantom_modules[[type]] %>% 
    filter(Class == class_target)
  
}

# generate variants ----
# idenitify modules
phantom_variants <- list()
for(type in names(phantom_modules)){
  
  phantom_variants[[type]] <-
    phantom_modules[[type]]%>% 
    dplyr::select(type_class_rating)
  
  colnames(phantom_variants[[type]]) <-
    type
}

# expand variants
variants  <- 
  bind_cols(phantom_variants) %>% 
  expand(Thrusters,
         Power_Plant,
         Frame_Shift_Drive,
         Life_Support,
         Power_Distributor,
         Sensors,
         Fuel_Tank)

variants$id <- 1:dim(variants)[1] 

variants <-
  variants %>% 
  pivot_longer(cols = Thrusters:Fuel_Tank,
               names_to = "type",
               values_to = "module")

# calculate variants ----
# merge all data
data_core_stats <-
  bind_rows(list_core_stats)
  

data_variant_stats <- data.frame()

for(i in unique(variants$id)){
  
  # filter each variant
  variant_modules <-
    variants %>% 
    filter(id == i)
  
  # all data for variant
  variant_stats_module <-
    data_core_stats %>% 
    filter(type_class_rating %in% variant_modules$module) %>% 
    select(-c(Class,Rating,type,type_class_rating))
  
  # clean
  # remove commas and make numeric
  variant_stats <-
    sapply(lapply(variant_stats_module, function(y) gsub(",", "", y)), as.numeric) %>% 
    as.data.frame()
  
  # sum stats
  variant_stats <-
    colSums(variant_stats,na.rm = TRUE)
  
  # add to 
  data_variant_stats <- 
    rbind(data_variant_stats,
          c(i,variant_stats))
  
  print(paste(i, "of", length(unique(variants$id))))
}

# change col names
colnames(data_variant_stats) <-
  c("id",names(variant_stats))

# add ship mass to total mass
data_variant_stats$`Mass(T)` <-
  data_variant_stats$`Mass(T)` + phantom_mass

# merge with variant data
variants_long <-
  variants %>% 
  pivot_wider(names_from = "type",
              values_from = "module")

# merge into whole thing
data_variants <-
  left_join(data_variant_stats,
            variants_long,
            by = "id")

# export ----
write_csv(data_variants,
          "./output/krait_phantom.csv")


# play ----
# plot
data_plot <-
  data_variants %>% 
  filter(`EngineOptimalMass(T)` == 1440) %>% 
  filter(Thrusters == "Thrusters6A",
         Frame_Shift_Drive == "Frame_Shift_Drive5A")

plot_me <-
ggplot(data_plot,
       aes(x = `Mass(T)`,
           y = PowerDraw,
           colour = `Engines/Systems_capacity`,
           label = id)) +
  geom_point()

ggplotly(plot_me)



# look at chosen variant
data_variants %>% 
  filter(id == 79) %>% 
  #dplyr::select(Thrusters:Fuel_Tank) %>% 
  t()






















































