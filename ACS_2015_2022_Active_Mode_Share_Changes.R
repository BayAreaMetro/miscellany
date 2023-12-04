# ACS 2015_2022 Active Mode Share Changes.R
# Create "2020" TAZ data from ACS 2017-2021 
# SI

# Load libraries, disable scientific notation

library(tidycensus)
library(tidyverse)

options(scipen = 999)

# Census geographies

baycounties <- c("Alameda","Contra Costa","Marin","Napa","San Francisco","San Mateo","Santa Clara","Solano","Sonoma")
state_code <- "California"

# Inspect ACS variables

ACS_2015 <- load_variables(year=2015, dataset="acs1", cache=TRUE)
ACS_2022 <- load_variables(year=2022, dataset="acs1", cache=TRUE)

# Output location


USERPROFILE          <- gsub("\\\\","/", Sys.getenv("USERPROFILE"))
output               <- file.path(USERPROFILE,"Box","Modeling and Surveys","Census","processed")

# Mode variables
                      
ACS_variables <-  c(      drove_alone_                = "B08006_003",	# drove alone
                          carpool_                    = "B08006_004",	# carpool
                          transit_                    = "B08006_008",	# transit
                          bike_                       = "B08006_014", # bike
                          walk_                       = "B08006_015", # walk
                          other_                      = "B08006_016", # other
                          work_athome_                = "B08006_017",	# work at home
                          total_                      = "B08006_001"  # total
                          )

# Function to make tract and block group data API calls by county for ACS 2017-2021

ACS_2015_raw <- get_acs(
          geography = "county", variables = ACS_variables,
          state = state_code, county=baycounties,
          year=2015,
          output="wide",
          survey = "acs1") %>% 
  select(!(ends_with("_M"))) %>% 
  rename_with(~ gsub("_E$", "_2015", .), ends_with("_E")) %>% 
  mutate(NAME = str_replace(NAME, " County, California", ""))

ACS_2022_raw <- get_acs(
  geography = "county", variables = ACS_variables,
  state = state_code, county=baycounties,
  year=2022,
  output="wide",
  survey = "acs1") %>% 
  select(!(ends_with("_M"))) %>% 
  rename_with(~ gsub("_E$", "_2022", .), ends_with("_E")) %>% 
  mutate(NAME = str_replace(NAME, " County, California", ""))

joined <- left_join(ACS_2015_raw,ACS_2022_raw,by=c("GEOID","NAME")) %>% 
  select("NAME", "bike_2015", "bike_2022", "walk_2015", "walk_2022", "work_athome_2015",
         "work_athome_2022", "total_2015", "total_2022") %>% 
  mutate(total_no_athome_2015=total_2015-work_athome_2015,total_no_athome_2022=total_2022-work_athome_2022) %>% 
  rename(County=NAME) %>% 
  arrange(.,County)


write.csv(joined,file.path(output,"ACS_2015_2022_Bike_Walk.csv"),row.names = F)
