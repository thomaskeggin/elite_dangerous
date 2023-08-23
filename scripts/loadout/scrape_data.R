# set ----
library(tidyverse)
library(rvest)
library(polite)

# scrape ----
module_types <-
  c("Thrusters",
    "Power_Plant",
    "Frame_Shift_Drive",
    "Life_Support",
    "Power_Distributor",
    "Sensors",
    "Fuel_Tank")

list_core_stats <- 
  list()

for(type in module_types){
  
  # set target url
  url_target <-
    paste0("https://elite-dangerous.fandom.com/wiki/",type)
  
  # scrape data
  list_core_stats[[type]] <-
    read_html(url_target) %>% 
    html_nodes("table.sortable") %>% 
    html_table(header = TRUE)
  
  # pick first table
  list_core_stats[[type]] <-
    list_core_stats[[type]][[1]]
  
  # fix value column
  colnames(list_core_stats[[type]]) <-
    gsub(" ","",colnames(list_core_stats[[type]]))
  
  list_core_stats[[type]]$`Value(CR)` <-
    gsub(",","",list_core_stats[[type]]$`Value(CR)`) %>% 
    as.numeric()
  
  # set type and class_rating columns
  list_core_stats[[type]]$type <- type
  list_core_stats[[type]]$type_class_rating <-
    paste0(list_core_stats[[type]]$type,
           list_core_stats[[type]]$Class,
           list_core_stats[[type]]$Rating)

}

# wrangle ----
# remove weird distributor header row and fix column types
# remove
list_core_stats$Power_Distributor <- 
  list_core_stats$Power_Distributor[-1,]

# change weapons and engine problem
colnames(list_core_stats$Power_Distributor)[which(colnames(list_core_stats$Power_Distributor) == "Weapons")] <-
  c("weapons_capacity","weapons_recharge")

colnames(list_core_stats$Power_Distributor)[which(colnames(list_core_stats$Power_Distributor) == "Engines/Systems")] <-
  c("Engines/Systems_capacity","Engines/Systems_recharge")

# set classes
list_core_stats$Power_Distributor$Class <-
  as.integer(list_core_stats$Power_Distributor$Class)

list_core_stats$Power_Distributor$`Mass(T)` <-
  as.numeric(list_core_stats$Power_Distributor$`Mass(T)`)

list_core_stats$Power_Distributor$Integrity <-
  as.integer(list_core_stats$Power_Distributor$Integrity)

list_core_stats$Power_Distributor$PowerDraw <-
  as.numeric(list_core_stats$Power_Distributor$PowerDraw)

# set power capacity to power draw in power plant
# and set values to negative
colnames(list_core_stats$Power_Plant)[5] <-
  "PowerDraw"
list_core_stats$Power_Plant$PowerDraw <-
  -list_core_stats$Power_Plant$PowerDraw

# export ----
saveRDS(list_core_stats,
        "./data/list_core_stats.rds")









