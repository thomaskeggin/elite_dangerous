# set --------------------------------------------------------------------------
library(tidyverse)
library(readxl)
library(corrplot)
library(plotly)

# load -------------------------------------------------------------------------
agility <-
  read_excel("./data/agility/agility_cleaned.xlsx") |> #input
  
  # acceleration to speed ratio (correlated with acceleration, but not speed)
  # faster ships accelerate faster, so we want to know how well they accelerate
  # for their speed.
  mutate(acc_v_speed = aMaxF / vMax, # high values are good relative acceleration
         
         # and the same for normal flight pitching ability
         normal_pitch_v_speed = nPitch50 / vMax,
         
         # and for supercruise pitching ability
         cruis_pitch_v_speed = cPitch50 / vMax)

# export -----------------------------------------------------------------------
write_csv(agility,
          "./data/temp/agility.csv") #output
