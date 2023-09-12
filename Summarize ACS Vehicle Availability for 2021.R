# Summarize ACS Vehicle Availability for 2021.R

# Import Libraries

suppressMessages(library(tidyverse))
library(tidycensus)

# Load ACS tables for inspection, set up directories, install census key, set ACS year

ACS_table <- load_variables(year=2021, dataset="acs1", cache=TRUE)

output               <- "M:/Data/Requests/Alix Bockelman"


censuskey            <- readLines("M:/Data/Census/API/api-key.txt")
census_api_key(censuskey, install = TRUE, overwrite = TRUE)
baycounties          <- c("01","13","41","55","75","81","85","95","97")
ACS_table            <- c(Total_         ="B08201_001",
                          Zero_Vehicle_  ="B08201_002",
                          One_Vehicle_   ="B08201_003",
                          Two_Vehicle_   ="B08201_004",
                          Three_Vehicle_ ="B08201_005",
                          Four_p_Vehicle_="B08201_006")

ACS_year            <- 2021
state_code          <- "06"

# Make ACS call, remove margins of error, rename variables, sort by county

acs_cars <- get_acs(geography = "county", variables = ACS_table,
                      state = state_code, county=baycounties,
                      year=ACS_year,
                      output="wide",
                      survey = "acs1",
                      key = censuskey) %>% 
  mutate(County=str_replace(NAME," County, California","")) %>%
  select(County, Total_E, Zero_Vehicle_E, One_Vehicle_E, Two_Vehicle_E,Three_Vehicle_E,Four_p_Vehicle_E)  %>% 
  arrange(County)

# Output file

write.csv(acs_cars,file.path(output,"ACS 2021 Vehicle Availability.csv"),row.names = F)

