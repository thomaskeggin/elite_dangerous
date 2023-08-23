# This script aims to take the eddb data structures and clean them up into relatable data frames
# that can be queried. These are output into /data/clean/[date] in .csv format.

# The resultant data frames should be:

#     factions_main:            faction metadata

#     systems_main:             system metadata
#     systems_states:           system overall states
#     systems_factions_states:  minor factions states

#     stations_main:            station metadata
#     stations_ships:           station ship availability
#     stations_modules:         station module availability
#     stations_states:          overall states for stations

#     commodities:              commodity data

#     listings

# set session ####

library(tidyverse)
library(jsonlite)

setwd("C:/Users/thoma/OneDrive/Documents/ed/data/")

date <- Sys.Date()

# load data ####
data <- 
systems       <- read_json("https://eddb.io/archive/v6/systems_populated.json", simplifyVector = TRUE)
stations      <- read_json("https://eddb.io/archive/v6/stations.json", simplifyVector = TRUE)
commodities   <- read_json("https://eddb.io/archive/v6/commodities.json", simplifyVector = TRUE)
factions_main <- read_json("https://eddb.io/archive/v6/factions.json", simplifyVector = TRUE)
listings      <- read_csv("https://eddb.io/archive/v6/listings.csv")

# factions ####
# factions_main
colnames(factions_main) <- paste("faction_",colnames(factions_main),sep="")

# systems ####
# systems_main
remove       <- which(sapply(systems,class) == "list")
systems_main <- systems[,-remove]

colnames(systems_main) <- paste("system_",colnames(systems_main),sep="")

# systems_states
systems_states_list <- systems$states

systems_states <- data.frame()
for(i in 1:length(systems_states_list)){
  if(dim(systems_states_list[[i]])[1]){
    states <- data.frame(systems_states_list[[i]])
    states$system_id <- systems_main$system_id[i]
    
    systems_states <- rbind(systems_states,states)
    print(paste("systems_states:",round(i/length(systems_states_list)*100,0),"%"))
  }
}
colnames(systems_states) <- c("system_state_id","system_state_name","system_id")

# systems_factions_states
systems_factions <- data.frame() # blank data frame for all minor factions in all systems

for(i in 1:length(systems$minor_faction_presences)){ # loop for all systems
  if(length(systems$minor_faction_presences[[i]]) > 0){ # skip empty systems
    system_factions <- data.frame(system_id = systems_main$system_id[i], # bind system id to faction data frame
                                  systems$minor_faction_presences[[i]])
    
    systems_factions <- rbind(systems_factions,system_factions) # add systems information to master data frame
  }
  print(paste("systems_factions:",round(i/length(systems$minor_faction_presences)*100,0),"%"))
}

systems_factions <- pivot_longer(systems_factions, cols = c(active_states,pending_states,recovering_states)) # pivot the three state scenario columns into one

systems_factions_states <- data.frame() # unpack the nested data frames
for(i in 1:dim(systems_factions)[1]){
  
  if(length(systems_factions$value[[i]]) > 0){
    systems_factions_value <- systems_factions$value[[i]]
    systems_factions_value <- data.frame(systems_factions[i,-6],systems_factions_value)
    
    systems_factions_states <- rbind(systems_factions_states,systems_factions_value)
  }
  print(paste("systems_factions_states:",round(i/dim(systems_factions)[1]*100,0),"%"))
}

colnames(systems_factions_states)[5:7] <- c("faction_state_category","faction_state_id","faction_state_name")

# stations ####
# stations_main
remove       <- which(sapply(stations,class) == "list")
stations_main <- stations[,-remove]

colnames(stations_main) <- paste("station_",colnames(stations_main), sep = "")

# stations_ship
stations_ships <- data.frame()

for(i in 1:dim(stations)[1]){
  if(length(stations$selling_ships[[i]]) > 0){
    station_id <- stations$id[i]
    ship <- stations$selling_ships[[i]]
    
    x <- data.frame(station_id,ship)
    
    stations_ships <- rbind(x,stations_ships)
  }
  print(paste(i,"of",dim(stations)[1]))
}

# stations_modules
stations_modules <- data.frame()

for(i in 1:dim(stations)[1]){
  if(length(stations$selling_modules[[i]]) > 0){
    station_id <- stations$id[i]
    module <- stations$selling_modules[[i]]
    
    x <- data.frame(station_id,module)
    
    stations_modules <- rbind(x,stations_modules)
  }
  print(paste(i,"of",dim(stations)[1]))
}

# stations_states

stations_states <- data.frame()

for(i in 1:dim(stations)[1]){
  if(length(stations$states[[i]]) > 0){
    station_id <- stations$id[i]
    state <- stations$states[[i]]
    
    x <- data.frame(station_id,state)
    
    stations_states <- rbind(x,stations_states)
  }
  print(paste(i,"of",dim(stations)[1]))
}
colnames(stations_states) <- c("station_id","state_id","state_name")


# commodities ####

commodities <- data.frame(commodities[,-14],commodities[,14])[-14]
colnames(commodities)[c(1,2,14)] <- c("commodity_id","commodity_name","category_name")

# listings ####

colnames(listings)[1] <- "listing_id"

# output ####

tables <- list(factions_main,
            systems_main,systems_states,systems_factions_states,
            stations_main,stations_ships,stations_modules,stations_states,
            commodities,
            listings)
table_names <- c("factions_main",
            "systems_main","systems_states","systems_factions_states",
            "stations_main","stations_ships","stations_modules","stations_states",
            "commodities",
            "listings")

dir.create(paste0("./clean/",date))
for(i in 1:length(table_names)){
  
  tables[[i]]$date <- date
  write_csv(tables[[i]],file=paste0("./clean/",date,"/",table_names[i],".csv"))
  
  
}




