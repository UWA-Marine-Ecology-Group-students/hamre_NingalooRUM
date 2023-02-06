# libraries 
library(tidyverse)
library(dplyr)
library(magrittr)
library(rgeos)
library(rgdal)
library(sf)

# Wrangling number of trips from boat ramp data

  ## This script is wrangling Trips_Ramp_out.csv to get only the BR we are interested in. 
  ##
  ## Trips_Ramp_out.csv was from Matt's national RUM. Made in Matt's "TripAllocation.R script". 
  ## Data shows the number of boat launches at each boat ramp per month (not sure of what year or where these number came from).
  ## The RAW data was annual "TRIPS" per "Name_reg". 
  ## 
  ## Matt used gravity function to calculate the number of trips for each boat ramp per month across the region given the proximity to populations centres and other boat ramps. 
  ## The gravity function assumes there will be a higher proportion of launches from ramps closer to larger population centres. 
  ## There are some issue with this, for example the gravity function would assume more launches from Exmouth Marina than Tantabiddi because its closer to a population centre. 
  ## The gravity function has been applied with different assumption represented by the numbers in the columns names. The number is a measure of interference. 
  ## The higher the number the more interference there will be between boat ramp of close proximity and larger the radius of interference. 
  ## For example a small interference might consider how Bundegi BR and Exmouth Marina influence each other, but not Tantabiddi which would only be captured withb a larger interference. 
  ## 
  ## 
  ## This was done by:
  ##    - Getting the annual number of boat launches for each Name_reg. eg. South Coast (data source?)
  ##    - Calculate the proportion of launches that would happen each month (proportion?)
  ##    - Applying a gravity function to the monthly proportions. 
  ##    
  ## natBR.shp is an associated shp file with geometry of all boat ramps

# data
BRtrips <- read.csv("./data/RAW/Trips_Ramp_out.csv") # launch numbers
BR <- st_read("./data/shp/natBR.shp") %>%  dplyr::select(RampID) # boat ramps gpkg

# find ramps
Ramps <- BRtrips %>% dplyr::select(State_reg, Name_reg, Ramp_name) %>% 
  distinct() %>% 
  filter(State_reg == "WA")

# Filter to ramps surveyed
BRtrips %<>% 
  filter(Ramp_name %in% c("Exmouth", "Coral Bay")) %>% 
  dplyr::select(Ramp_name, RampID, Month, TRIPS_ramp_prop_Grvt__5, upp_TRIPS_Grvt__5, low_TRIPS_Grvt__5)

# Aggregating to annual boat launchs at surveyed boat ramps
BRtrips <- BRtrips %>% 
  group_by(RampID, Ramp_name) %>%
  summarize(across(where(is.numeric), list(sum), .names = "{.col}")) %>% # summing across months to get annual estimates
  mutate(RampID = as.numeric(RampID)) %>% 
  ungroup()

# getting rid of double ramps
BRtrips %<>%
  mutate(Ramp_name = ifelse(RampID %in% 73, "Bundegi", 
                            ifelse(RampID %in% 87, "Tantabiddi",
                                   ifelse(RampID %in% 72, "Coral Bay", Ramp_name))))

t1 <- BRtrips %>% dplyr::select(RampID, Ramp_name)

t2 <- BRtrips %>%
  group_by(Ramp_name) %>%
  summarise_at(vars(contains("TRIPS")),
               list(sum))

BRtrips <- left_join(t2, t1)
BRtrips <- BRtrips[!duplicated(BRtrips$Ramp_name), ]

# join geometry
BRtrips <- left_join(BRtrips, BR, by = c("RampID")) %>% # join to geometry
  st_as_sf() 

# write
write_csv(BRtrips, "./data/02_data/2.1_BRtrips.csv") # csv
saveRDS(BRtrips, "./data/gpkg/2.1_BRtrips.gpkg") # gpkg



##### Usng Korinna Ryans boat ramp trips
# trips <- c(21401, 13303.13, 12085.75, 6042.875)
# BRtrips_KR <- BRtrips
# BRtrips_KR$TRIPS_ramp_prop_Grvt__5 <- trips
# 
# saveRDS(BRtrips_KR, "./data/02_data/2.1_BRtrips_KR.gpkg")
