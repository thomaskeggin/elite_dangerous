# set --------------------------------------------------------------------------
library(jsonlite)
library(R.utils)

# pull populated systems -------------------------------------------------------
# download
download.file("https://www.edsm.net/dump/systemsPopulated.json.gz",
              destfile = "./data/edsm/pop_systems.json.gz")

# unzip
gunzip("./data/edsm/pop_systems.json.gz",
       overwrite = TRUE)

# read in populated system data
pop_file_path <-
  "./data/edsm/pop_systems.json"

sys_pop_raw  <-
  read_json(pop_file_path,
            simplifyVector = TRUE)

# pull powerplay systems -------------------------------------------------------
# download
download.file("https://www.edsm.net/dump/powerPlay.json.gz",
              destfile = "./data/edsm/powerPlay.json.gz")

# unzip
gunzip("./data/edsm/powerPlay.json.gz",
       overwrite = TRUE)

# read in powerplay system data
power_file_path <-
  "./data/edsm/powerPlay.json"

sys_power_raw  <-
  read_json(power_file_path,
            simplifyVector = TRUE)

# export -----------------------------------------------------------------------
saveRDS(sys_pop_raw,
        paste0("./data/processed/sys_pop_raw_",Sys.Date(),".rds"))

saveRDS(sys_power_raw,
        paste0("./data/processed/sys_power_raw_",Sys.Date(),".rds"))
