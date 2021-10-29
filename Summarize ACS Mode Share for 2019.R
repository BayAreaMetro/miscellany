# Summarize ACS Mode Share for 2019.R

# Import Libraries

suppressMessages(library(tidyverse))
library(tidycensus)

# Set up directories, install census key, set ACS year

output               <- "M:/Data/Requests/Commissioner Worth/ACS2019"

censuskey            <- readLines("M:/Data/Census/API/api-key.txt")
census_api_key(censuskey, install = TRUE, overwrite = TRUE)
baycounties          <- c("01","13","41","55","75","81","85","95","97")
ACS_table            <- c(# Total_        ="B08006_001",
                          Drove_Alone_  ="B08006_003",
                          Carpooled_    ="B08006_004",
                          Transit_      ="B08006_008",
                          Bike_         ="B08006_014",
                          Walk_         ="B08006_015",
                          Other_        ="B08006_016",
                          Work_at_Home_  ="B08006_017")

ACS_year            <- 2019
state_code          <- "06"

# Make ACS call, remove margins of error, rename variables, sort by county

acs_mode <- get_acs(geography = "county", variables = ACS_table,
                      state = state_code, county=baycounties,
                      year=ACS_year,
                      output="wide",
                      survey = "acs1",
                      key = censuskey) %>% 
  mutate(County=str_replace(NAME," County, California","")) %>%
  select(County, 
         Drove_Alone  =Drove_Alone_E,
         Carpooled    =Carpooled_E,
         Transit      =Transit_E, 
         Bike         =Bike_E,
         Walk         =Walk_E, 
         Other        =Other_E, 
         Work_at_Home =Work_at_Home_E) %>% 
  arrange(County)

# Convert to long form for Tableau, so columns are County, Commute Mode, Commuters
acs_mode_long <- pivot_longer(acs_mode, !County, names_to = "Commute Mode", 
                              values_to = "Commuters")

# Output file

write.csv(acs_mode_long,file.path(output,"ACS 2019 Means of Transportation by County.csv"),row.names = F)

