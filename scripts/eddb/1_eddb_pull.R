# set session
library(httr)
library(readr)

dir_data <- "C:/Users/thoma/OneDrive/Documents/ed/data/raw/"

# get data
systems     <- GET("https://eddb.io/archive/v6/systems_populated.json")
stations    <- GET("https://eddb.io/archive/v6/stations.json")
commodities <- GET("https://eddb.io/archive/v6/commodities.json")
factions    <- GET("https://eddb.io/archive/v6/factions.json")
listings    <- read_csv("https://eddb.io/archive/v6/listings.csv")

# compile into a time stamped list
data <- list(date.time   = as.numeric(Sys.time()),
             systems     = systems,
             stations    = stations,
             commodities = commodities,
             factions    = factions,
             listings    = listings)

# write the data to disk
saveRDS(data, paste0(dir_data,Sys.Date(),".rds"))
