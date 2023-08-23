# set session ####

library(tidyverse)

# load data ####

setwd("C:/Users/thoma/OneDrive/Documents/ed/data/clean/01-01-2020")

files <- list.files()
data <- list()

for(i in 1:length(files)){
  data[[i]] <- read_csv(files[i])
}

names(data) <- gsub('.{4}$', '', files)

# filter systems for fleet carriers ####

fleet_carriers <- data$stations_main %>% filter(station_type == "Fleet Carrier")

# group fleet carriers into systems ####

count_carriers <- fleet_carriers %>% count(station_system_id)
colnames(count_carriers) <- c("system_id","carriers")

# join carrier counts to system information ####

target_systems <- inner_join(count_carriers, data$systems_main)

# filter stations to those in carrier systems ####

colnames(data$stations_main)[3] <- "system_id"
target_stations <- inner_join(target_systems, data$stations_main, by = "system_id") %>% 
  filter(station_type != "Fleet Carrier")

# join commodity meta information to listings and filter for minerals ####

commodities <- data$commodities
commodities <- commodities %>% filter(category_name == "Minerals")
commodities <- commodities[,1:2]

listings <- inner_join(data$listings, commodities)

# join listings with target station information ####

target_listings_all <- inner_join(target_stations, listings)

target_listings_all <- target_listings_all[,c(2,4,11,13,15,18,27,31,69,70,72,73,78)]

# identify tasty targets ####

targets <- aggregate(target_listings_all, by = target_listings_all$system_name, FUN = sum)

#target_listings_filtered <- target_listings_all %>% filter(carriers > 10 &
#                                                             commodity_name == "Painite")

#write.csv(target_listings_filtered, "C:/Users/thoma/Desktop/piracy.csv")

