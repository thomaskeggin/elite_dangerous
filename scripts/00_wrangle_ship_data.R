# set --------------------------------------------------------------------------
library(readxl)
library(tidyverse)
library(scales)
library(ggrepel)
library(plotly)

# load -------------------------------------------------------------------------
data_ed <-
  list()

for(sheet in excel_sheets("./data/meta/ships.xlsx")){
  data_ed[[sheet]] <-
    read_excel("./data/meta/ships.xlsx", sheet = sheet)
}

# hardpoints -------------------------------------------------------------------
data_wrangled <-
  data_ed

# hardpoints
hardpoint_key <-
  data.frame(hardpoint = c("Small","Medium","Large","Huge"),
             hardpoint_value = 1:4)

data_wrangled$hardpoints <-
  data_ed$hardpoints %>%
  select(-Utility) %>% 
  pivot_longer(cols = Small:Huge,
               names_to = "hardpoint") %>% 
  left_join(hardpoint_key, by = "hardpoint") %>% 
  group_by(Model) %>% 
  mutate(firepower = value*hardpoint_value) %>% 
  summarise(firepower = sum(firepower)) %>% 
  left_join(data_ed$fighters, by = "Model") %>% 
  mutate(fighters = replace_na(fighters,0)) %>% 
  mutate(firepower = firepower + (fighters*1)) %>% 
  select(-fighters)
  
# utility
data_wrangled$utility <-
  data_ed$hardpoints %>% 
  select(Model,Utility)

# optional
classes     <- colnames(data_ed$optional)[-1]
classes_reg <- data.frame(class = classes[-grep("m",classes)],
                          reg_value = 1:8)
classes_mil <- data.frame(class = classes[grep("m",classes)],
                          mil_value = 1:8)


data_wrangled$optional <-
  data_ed$optional %>% 
  pivot_longer(cols = -Model,
               names_to = "class",
               values_to = "quantity") %>% 
  left_join(classes_reg, by = "class") %>% 
  left_join(classes_mil, by = "class") %>% 
  mutate(reg_sum = quantity*reg_value,
         mil_sum = quantity*mil_value) %>% 
  group_by(Model) %>% 
  summarise(optional = sum(reg_sum,na.rm = T),
            military = sum(mil_sum,na.rm = T))

# mash together
data_all <-
  reduce(data_wrangled, left_join, by = "Model")

data_all$fighters[is.na(data_all$fighters)] <- 0

# uncaps colnames
colnames(data_all) <-
  make.names(tolower(colnames(data_all)))

# export -----------------------------------------------------------------------
write_csv(data_all,
          "./data/temp/ship_all.csv")
  
  
  
