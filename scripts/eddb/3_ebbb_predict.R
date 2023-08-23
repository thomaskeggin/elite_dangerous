
# set session ----

library(tidyverse)

setwd("C:/Users/thoma/OneDrive/Documents/ed/data/clean/")


# set predictors
predictors <- c("station_distance_to_star",
                "station_government",
                "station_allegiance",
                "station_type",
                #"station_settlement_size",
                #"station_settlement_security",
                "state_name")
predictors_lm <- paste(predictors,collapse = " + ")


# load data ####

files      <- list.files("./2021-10-04/")
file_names <- gsub('.{4}$', '', files)

data <- list()
for(i in 1:length(files)){
  
  data[[file_names[i]]] <- read_csv(paste0("./2021-10-04/",files[i]))
}

# exploration of listings ----
colnames(data$listings)
colnames(data$commodities)

# check columns in common
colnames(data$listings)[colnames(data$listings) %in% colnames(data$commodities)]

# merge tables ----

listings <- data$listings %>% filter(sell_price > 0)

# commodities
master <- left_join(data$listings, data$commodities, by = "commodity_id") %>% 
  select(listing_id:category_id,
         is_rare,
         is_non_marketable,
         ed_id,
         category_name)

# stations
master <- master %>%
  left_join(data$stations_main) %>% 
  select(listing_id,
         station_id,
         supply,
         supply_bracket,
         buy_price,
         sell_price,
         demand,
         demand_bracket,
         collected_at,
         commodity_name,
         category_id,
         is_non_marketable,
         ed_id,
         category_name,
         station_name,
         station_system_id,
         station_max_landing_pad_size,
         station_distance_to_star,
         station_government_id,
         station_government,
         station_allegiance_id,
         station_allegiance,
         station_type_id,
         station_type,
         station_settlement_size_id,
         station_settlement_size,
         station_settlement_security_id,
         station_settlement_security,
         station_controlling_minor_faction_id,
         station_ed_market_id)

# station states
master <- master %>% 
  left_join(data$stations_states)

# factions main
colnames(master)[colnames(master) %in% colnames(data$factions_main)]

# include systems information ####################################################



gc()

# partition data ----
# target data
data_model <- master %>% dplyr::select(all_of(c("commodity_name","sell_price",predictors)))

target_data <- data_model %>%
  filter(commodity_name == "Void Opals")

sample_size <- round(dim(target_data)[1]*0.1)


target_data$tk_id <- 1:dim(target_data)[1]

training_numbers <- round(runif(sample_size,
                                min = 1,
                                max = dim(target_data)[1]))
testing_numbers <- round(runif(sample_size,
                               min = 1,
                               max = dim(target_data)[1]))

# training data
training_data <- target_data %>%
  filter(tk_id %in% training_numbers)

# testing data
testing_data <- target_data %>%
  filter(tk_id %in% testing_numbers)


# make models ----
# check assumptions




model <- lm(sell_price ~ station_distance_to_star +
                            station_government +
                            station_allegiance +
                            station_type +
                            #station_settlement_size +
                            #station_settlement_security +
                            state_name,
            data = training_data)

model <- lm(sell_price ~ station_distance_to_star,
            data = training_data)

summary(model)

model_select <- step(model, direction = "both")

summary(model_select)

par(mfrow = c(2,2))
plot(model_select)

testing_data$predicted <- predict(model_select,
                                  newdata = testing_data)

# plot data ############################################################
plot_data <- rbind(testing_data,
                   training_data)

ggplot(plot_data, aes(x = sell_price, y = predicted)) +
  geom_point() +
  xlim(c(0,1000000)) +
  ylim(c(0,1000000)) 

ggplot() +
  geom_boxplot(data = training_data, aes(x = state_name, y = sell_price)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 7))

ggplot(plot_data, aes(x = station_government, y = sell_price)) +
  geom_boxplot() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 7))




























