# set --------------------------------------------------------------------------
library(tidyverse)
library(httr2)
library(jsonlite)
library(progress)

# load -------------------------------------------------------------------------
# sell stations
sell_stations <-
  read_csv("./outputs/mining/merit_mining/02_sell_stations.csv", #input
           show_col_types = F)

# build requests ---------------------------------------------------------------
# unlike system/faction information which is not too dynamic, we want up to date
# market data, so this time we use the EDSM API instead of the nightly dumps.
# Be kind to their servers!
sell_station_markets_list <-
  list()

sys_stations <-
  paste0("m_",sell_stations$marketId)

request_core <-
  request("https://www.edsm.net/api-system-v1/stations/market")

# yeet requests
pb <-
  progress_bar$new(total = dim(sell_stations)[1],
                   format = ":current of :total [:bar] :percent")

for(station in 1:dim(sell_stations)[1]){
  
  pb$tick()
  
  # send it
  market_json <-
    request_core |> 
    req_url_query(marketId = sell_stations$marketId[station]) |> 
    req_perform() |> 
    resp_body_json()
  
  # unpack it
  # skip no data markets
  if(length(market_json$commodities) > 0){
    
    # flatten json list
    market_list <- list()
    for(i in 1:length(market_json$commodities)){
      
      market_list[[i]] <-
        as.data.frame(market_json$commodities[[i]]) |> 
        as_tibble()
    }
    
    # add to markets list
    sell_station_markets_list[[sys_stations[i]]] <-
      do.call(rbind.data.frame,
              market_list) |> 
      mutate(.before = "id",
             system_sell  = sell_stations$system_sell[station],
             station_sell = sell_stations$station_sell[station],
             marketId     = sell_stations$marketId[station]) |> 
      select(-c(marketId,id))
  }
}

# compile
sell_station_markets <-
  do.call(rbind.data.frame,
          sell_station_markets_list)

# export -----------------------------------------------------------------------
write_csv(sell_station_markets,
          "./outputs/mining/merit_mining/03_sell_station_markets.csv") #output



