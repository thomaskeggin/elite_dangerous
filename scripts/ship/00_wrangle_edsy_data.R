# set --------------------------------------------------------------------------
library(readxl)
library(tidyverse)

# load -------------------------------------------------------------------------
data_ed <-
  list()

for(sheet in excel_sheets("./data/meta/ships_edsy.xlsx")){
  data_ed[[sheet]] <-
    read_excel("./data/meta/ships_edsy.xlsx", sheet = sheet)
}

# wrangle ----------------------------------------------------------------------
data_wrangled <-
  data_ed$edsy |> 
  left_join(data_ed$fighters)

data_wrangled$fighters[is.na(data_wrangled$fighters)] <- 0

# hard points -------------------------------------------------------------------
hardpoint_key <-
  tibble(hardpoint = c("S","M","L","H"),
         hardpoint_value = 1:4)

for(i in 1:4){
  
  data_wrangled$hardpoints <-
    gsub(hardpoint_key$hardpoint[i],
         hardpoint_key$hardpoint_value[i],
         data_wrangled$hardpoints)
}

data_wrangled$hardpoints <-
  data_wrangled$hardpoints |> 
  str_split(" ") |> 
  lapply(as.numeric) |> 
  lapply(sum) |> 
  unlist()

data_wrangled <-
  data_wrangled  |> 
  mutate(firepower = hardpoints + fighters)

# military ---------------------------------------------------------------------
data_wrangled$military <-
  data_wrangled$military |> 
  str_split(" ") |> 
  lapply(as.numeric) |> 
  lapply(sum) |> 
  unlist()

data_wrangled$military[is.na(data_wrangled$military)] <- 0

# optional ---------------------------------------------------------------------
# unpack optionals
optional_numeric <-
  data_wrangled$optional |> 
  str_split(" ") |> 
  lapply(as.numeric)

# summed optional slots
data_wrangled$optional_sum <-
  optional_numeric |> 
  lapply(sum) |> 
  unlist()

# number of optional slots
data_wrangled$optional_n <-
  optional_numeric |> 
  lapply(sum) |> 
  unlist()

optionals <-
  optional_numeric

for(i in 1:length(optionals)){
  
  optionals[[i]] <-
    tibble(model = data_wrangled$model[i],
           optional = optionals[[i]])
}

optionals <-
  do.call(rbind.data.frame,
          optionals)

# dimensions -------------------------------------------------------------------
data_wrangled <-
  data_wrangled |> 
  left_join(data_ed$dimensions)

# export -----------------------------------------------------------------------
write_csv(data_wrangled,
          "./data/temp/ship_edsy.csv") #output

write_csv(optionals,
          "./data/temp/optionals_edsy.csv") #output



