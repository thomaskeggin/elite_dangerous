
# script to find the shortest route between selected star systems

# set session ####

library(tidyverse)
library(reshape2)

# load system data
systems <- read_delim("C:/Users/thoma/OneDrive/Documents/ed/data/systems.csv", delim=",")


# specify target systems
way_systems <- c("Gliese 3258",
                 "Pleiades Sector IH-C2-I6")
# present system
start_system <- "LHS 28"
waypoints <- length(c(way_systems,start_system))

# filter for target system data
systems_target <- filter(systems, name %in% c(way_systems,start_system))
systems_target <- filter(systems, grepl(paste(c(way_systems,start_system),collapse="|"),name))

# create a distance matrix
sys_dist <- as.matrix(dist(systems_target[,c("x","y","z")], method = "euclidean"))
colnames(sys_dist) <- systems_target$name
rownames(sys_dist) <- systems_target$name

# melt distance matrix into routes
route_dist <- melt(sys_dist, varnames = c("row", "col"))

# filter out duplicated and non-viable routes
route_dist <- filter(route_dist) %>% filter(row !=col) %>% filter(duplicated(value))
colnames(route_dist) <- c("from","to","distance")

route_dist_recip <- route_dist[,c(2,1,3)]
colnames(route_dist_recip) <- c("from","to","distance")
route_dist <- rbind(route_dist,route_dist_recip)

# create list of iterations equal to number of target systems
route_iterate <- rep(list(way_systems),length(way_systems))

# generate all route combinations
possible_routes <- do.call(expand.grid, route_iterate)
possible_routes <- data.frame(start_system,possible_routes)

route_length <- c()
for(i in 1:dim(possible_routes)[1]){
  route_length <- c(route_length,length(unique(as.character(possible_routes[i,]))))
}

# filter out routes that do not reach all systems
possible_routes$route_length <- route_length
possible_routes <- filter(possible_routes, route_length == waypoints)[,-(waypoints+1)]

# calculate distance of each tour
possible_routes <- possible_routes %>% mutate_if(is.factor, as.character)

tour_dist <- c()
for(r in 1:dim(possible_routes)[1]){
  route_x <- as.character(possible_routes[r,])
  
  tour <- data.frame(route_x[1],route_x[2])
  for(i in 2:(length(route_x)-1)){
    tour <- rbind(tour,c(route_x[i],route_x[i+1]))
  }
  colnames(tour) <- c("from","to")
  
  tour <- merge(tour, route_dist)
  
  tour_dist <- c(tour_dist,sum(tour$distance))
}

possible_routes$distance <- tour_dist

# return shortest route
filter(possible_routes, distance == min(distance))
