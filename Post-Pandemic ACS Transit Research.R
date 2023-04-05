# Post-Pandemic ACS Transit Research.R
# Research differences in transit ridership 2019 and 2021


library (tidyverse)
library (tidycensus)
library(spatstat)

baycounties          <- c("01","13","41","55","75","81","85","95","97")
censuskey            <- readLines("M:/Data/Census/API/api-key.txt")
place_eq_in          <- "M:/Data/Census/corrlib/Census2010/Census 2010_Places_65k_Over.csv"
place_eq             <- read.csv(place_eq_in,header = T)

USERPROFILE          <- gsub("\\\\","/", Sys.getenv("USERPROFILE"))
BOX_PPR              <- file.path(USERPROFILE, "Box", "Modeling and Surveys","Census","Post Pandemic Research")


acs_variables <- c(
# Transit variables  
                   total_pop_              ="B01003_001",          # Total area population
                   total_transit_          ="C08301_008",          # Total transit users
                   total_commuters_        ="C08301_001",          # Total commuters
                   total_wfh_              ="C08301_011",          # Total work from home (for calculating out of home modes)
                   transit_wnh_            ="B08105H_004",         # White, not Hispanic transit use
                   transit_female_         ="C08006_032",          # Female transit user
                   transit_native_         ="C08111_017",          # Native public transit user
                   transit_naturalized_    ="C08111_019",          # Naturalized public transit user
                   transit_notcitizen_     ="C08111_020",          # Not a citizen public transit user
                   #commute_poverty_tot_   ="B08122_001",          # Poverty commuter total is different from full universe
                   #transit_poverty_tot_   ="B08122_013",          # Poverty transit user total is different from full universe
                   #transit_poverty_lt_    ="B08122_014",          # Transit user below 100 percent poverty
                   commute_tot_earnings_   ="C08119_001",          # Total commuters for earnings is different from full universe
                   transit_tot_earnings_   ="C08119_028",          # Transit commuters for earnings is different from full universe
                   transit_earnings_50_65_ ="C08119_034",          # Transit commuters with earnings 50-65
                   transit_earnings_65_75_ ="C08119_035",          # Transit commuters with earnings 65-75
                   transit_earnings_75p_   ="C08119_036",          # Transit commuters with earnings 75+       
                   transit_wah_occ_        ="C08124_023",          # Transit work at home occs - mgmt.,bus.,science,arts
                   transit_hh_             ="B08137_010",          # Transit users in households differs from full universe
                   transit_rent_           ="B08137_012",          # Transit user rent home
                   transit_0_veh_          ="C08141_017",          # Transit user zero vehicle households
                   transit_med_age_        ="B08103_004",          # Median age for transit users
                   transit_LEP_            ="C08113_020",          # Transit users speak English less than very well
# Total variables

                   total_wnh_              ="C03002_003",          # White, not Hispanic persons
                   total_female_           ="B01001_026",          # Female persons
                   total_notcitizen_       ="B05001_006",          # Not a citizen persons
                   commuter_earnings_50_65_="C08119_007",          # Total commuters with earnings 50-65
                   commuter_earnings_65_75_="C08119_008",          # Total commuters with earnings 65-75
                   commuter_earnings_75p_  ="C08119_009",          # Total commuters with earnings 75+ 
                   commuter_wah_occ_       ="C08124_002",          # Total commuter work at home occs - mgmt.,bus.,science,arts
                   commuter_hh_            ="B08137_001",          # Commuters in households differs from full universe
                   commuter_rent_          ="B08137_003",          # Commuters rent home
                   total_hh_               ="B25008_001",          # Persons in households differs from full universe
                   total_rent_             ="B25008_003",          # Persons rent home
                   commuter_0_veh_         ="C08141_002",          # Commuter zero vehicle households
                   commuter_med_age_       ="B08103_001",          # Median age for commuters
                   commuter_LEP_           ="C08113_005",          # Commuters speak English less than very well
                   total_language_         ="C06007_001",          # Total population 5 years and over
                   total_LEP_              ="C06007_005"           # Total LEP population 5 years and over
)
  
# 2019 data

places_19 <- get_acs(geography = "place", variables = acs_variables,
                     state = "06", 
                     year=2019,
                     output="wide",
                     survey = "acs1",
                     key = censuskey) %>% 
  arrange(NAME) %>% 
  left_join(place_eq,.,by="NAME") %>% 
  slice_max(total_pop_E,n=10)

counties_19 <- get_acs(geography = "county", variables = acs_variables,
                       state = "06", county = baycounties,
                       year=2019,
                       output="wide",
                       survey = "acs1",
                       key = censuskey) %>% 
  arrange(NAME) 

bay_19 <- counties_19 %>% 
  select(3:ncol(.)) %>% 
  summarise_all (sum) %>% 
  mutate(GEOID="99999",NAME="Bay Area",transit_med_age_E=NA_real_)


# 2021 data                 

places_21 <- get_acs(geography = "place", variables = acs_variables,
          state = "06", 
          year=2021,
          output="wide",
          survey = "acs1",
          key = censuskey) %>% 
  arrange(NAME) %>% 
  left_join(place_eq,.,by="NAME") %>% 
  slice_max(total_pop_E,n=10)

counties_21 <- get_acs(geography = "county", variables = acs_variables,
                  state = "06", county = baycounties,
                  year=2021,
                  output="wide",
                  survey = "acs1",
                  key = censuskey) %>% 
  arrange(NAME) 

bay_21 <- counties_21 %>% 
  select(3:ncol(.)) %>% 
  summarise_all (sum) %>% 
  mutate(GEOID="99999",NAME="Bay Area",transit_med_age_E=NA_real_)


# Combine and remove margin of error field
combined_19 <- rbind(counties_19,places_19,bay_19) %>% 
  select(!(ends_with("_M"))) %>% 
  rename_with(~str_remove(., '_E')) %>% 
  rename_with(~paste0(.,"_2019"),3:ncol(.)) %>% 
  filter(!(is.na(GEOID)))
  
combined_21 <- rbind(counties_21,places_21,bay_21) %>% 
  select(!(ends_with("_M"))) %>% 
  rename_with(~str_remove(., '_E')) %>% 
  rename_with(~paste0(.,"_2021"),3:ncol(.)) 

final <- left_join(combined_19,combined_21,by=c("GEOID","NAME")) %>% 
  select(sort(names(.))) %>% 
  relocate(c("GEOID","NAME"),.before = commute_tot_earnings_2019) %>% 
  mutate(transit_earnings_50p_2019=transit_earnings_50_65_2019+transit_earnings_65_75_2019+transit_earnings_75p_2019,
         transit_earnings_50p_2021=transit_earnings_50_65_2021+transit_earnings_65_75_2021+transit_earnings_75p_2021,
         commuter_earnings_50p_2019=commuter_earnings_50_65_2019+commuter_earnings_65_75_2019+commuter_earnings_75p_2019,
         commuter_earnings_50p_2021=commuter_earnings_50_65_2021+commuter_earnings_65_75_2021+commuter_earnings_75p_2021
      )

write.csv(final,file.path(BOX_PPR,"ACS 2019 and 2021 Means of Transportation to Work by Variables.csv"),row.names = F)

# PUMS analysis for county-to-county commutes and regional median age for Bay Area

baypowpuma = c(100,1300,4100,5500,7500,8100,8500,9500,9700) # place-of-work PUMAs(POWPUMA) in the Bay Area

# Input person census files

PUMS_2019 = "M:/Data/Census/PUMS/PUMS 2019/pbayarea19.Rdata"
PUMS_2021 = "M:/Data/Census/PUMS/PUMS 2021/pbayarea21.Rdata"

load (PUMS_2019)
load (PUMS_2021)

# Subset transit commuters for 2019 and 2021 PUMS files

transit_2019 <- pbayarea19 %>%
  filter(JWTRNS %in% 2:6) 

transit_2021 <- pbayarea21 %>%
  filter(JWTRNS %in% 2:6) 

commuter_2019 <- pbayarea19 %>%
  filter(JWTRNS %in% 1:12) 

commuter_2021 <- pbayarea21 %>%
  filter(JWTRNS %in% 1:12) 

# Median age for Bay Area from PUMS

transit_age_2019 <- weighted.median(transit_2019$AGEP,transit_2019$PWGTP)
transit_age_2021 <- weighted.median(transit_2021$AGEP,transit_2021$PWGTP)
commuter_age_2019 <- weighted.median(commuter_2019$AGEP,commuter_2019$PWGTP)
commuter_age_2021 <- weighted.median(commuter_2021$AGEP,commuter_2021$PWGTP)
print (transit_age_2019)
print (transit_age_2021)
print (commuter_age_2019)
print (commuter_age_2021)

# Analyze 2019 and 2021 Bay Area PUMS data for intra-regional Bay Area workers
county_2019 <- transit_2019 %>% 
  filter(POWPUMA %in% baypowpuma & POWSP==6) %>%                                # Remove records with a destination outside Bay Area
  mutate(POWNAME=recode(POWPUMA,"100"="Alameda",
                        "1300"="Contra Costa",
                        "4100"="Marin",
                        "5500"="Napa",
                        "7500"="San Francisco",
                        "8100"="San Mateo",
                        "8500"="Santa Clara",
                        "9500"="Solano",
                        "9700"="Sonoma"
  )) %>% 
  select(PUMA,County_Name, PWGTP,JWTRNS,POWPUMA,POWSP,POWNAME)

county_2021 <- transit_2021 %>% 
  filter(POWPUMA %in% baypowpuma & POWSP==6) %>%                                # Remove records with a destination outside Bay Area
  mutate(POWNAME=recode(POWPUMA,"100"="Alameda",
                        "1300"="Contra Costa",
                        "4100"="Marin",
                        "5500"="Napa",
                        "7500"="San Francisco",
                        "8100"="San Mateo",
                        "8500"="Santa Clara",
                        "9500"="Solano",
                        "9700"="Sonoma"
  )) %>% 
  select(PUMA,County_Name, PWGTP,JWTRNS,POWPUMA,POWSP,POWNAME)

# Summarize county-to-county transit commuting, output data

summary_2019 <- county_2019 %>%
  group_by(County_Name,POWNAME) %>%
  summarize(transit_commuters=sum(PWGTP)) %>% 
  pivot_wider(.,names_from = POWNAME,values_from = transit_commuters,values_fill = 0) %>% 
  ungroup()
            
summary_2021 <- county_2021 %>%
  group_by(County_Name,POWNAME) %>%
  summarize(transit_commuters=sum(PWGTP)) %>% 
  pivot_wider(.,names_from = POWNAME,values_from = transit_commuters,values_fill = 0) %>% 
  ungroup()

write.csv(summary_2019, file.path(BOX_PPR,"PUMS2019 Transit Commuters by County of Home and Work.csv"), row.names = FALSE)
write.csv(summary_2021, file.path(BOX_PPR,"PUMS2021 Transit Commuters by County of Home and Work.csv"), row.names = FALSE)
