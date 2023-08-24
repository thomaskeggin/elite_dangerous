setwd("C:/Users/thoma/Documents/ED")

library(readxl)
library(tidyverse)
library("rjson")

# load data ####

comm <- "https://eddb.io/archive/v6/commodities.json"
commodities <- fromJSON(paste(readLines(comm), collapse=""))

stat <- "https://eddb.io/archive/v6/stations.json"
stations <- fromJSON(paste(readLines(stat), collapse=""))

listings <- read_delim("./data/listings.csv", delim = ",")
systems <- read_delim("./data/systems.csv", delim = ",")

# find largest profit margin ####

commRange <- range(1:length(commodities))
commSeq   <- seq(commRange[1],commRange[2])

profits <- c()
names   <- c()
commID      <- c()

for(commodity in commSeq){

  
  profit  <- commodities[[commodity]]$max_sell_price - commodities[[commodity]]$max_buy_price
  profits <- c(profits,profit)
  
  if(length(profit) > 0){
    name    <- commodities[[commodity]]$name
    names   <- c(names,name)
    
    id      <- commodities[[commodity]]$id
    commID  <- c(commID,id)
  } else{}

}

profitMargin <- data.frame(commID, names,profits)

# find station with target commodity

# to buy
data <- filter(listings, commodity_id == 22 & buy_price > 0)
data[order(data$buy_price),]

# to sell
data <- filter(listings, commodity_id == 22)
bestSell <- data[order(-data$sell_price),]

for(place in bestBuy){
  if(stations[[place]]$max_landing_pad_size == "L"){
  print(stations[[place]]$name)
  }
}

for(place in bestSell$station_id){
  if(stations[[place]]$max_landing_pad_size == "L"){
    
    statName  <- stations[[place]]$name
    commPrice <- bestSell[place == bestSell$station_id, ]
    commPrice <- commPrice$sell_price[1]
    
    print(paste(statName, " : ", commPrice)
    )
  }
}

for(place in bestSell){

    print(stations[[place]]$name)

}






















