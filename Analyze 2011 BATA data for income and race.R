
# Analyze 2011 BATA data for income and race.r

# Import Libraries and set output directory

suppressMessages(library(tidyverse))
library(readxl)

output <- "M:/Data/Requests/Lisa Zorn/TPS Bay Bridge Income and Race/"

# Bring in data

bata_data <- "M:/Data/BATA/2011 Bay Bridge Survey/San Francisco-Oakland Bay Bridge Survey - 2011 (New Group) - Final-RawData-scrubbed no single quotes ethnic crosswalk.xlsx"

bata    <- read_excel(bata_data, sheet = "survey")
oth_eth <- read_excel(bata_data, sheet = "other_eth_crosswalk")

final <- left_join(bata,oth_eth,by=c("Question 27: Other, please specify"="Other_Ethnicity"))

# Summarize income
final_income <- final %>% 
  rename(household_income=`Question 25: What is your approximate annual household income before taxes?`) %>% 
  mutate(
    income_rc=case_when(
      household_income=="less than $25,000"    ~"1_less than 25k",
      household_income=="$25,000 to $49,999"   ~"2_25-50k",
      household_income=="$50,000 to $74,999"   ~"3_50-75k",
      household_income=="$75,000 to $99,999"   ~"4_75-100k",
      household_income=="$100,000 to $124,999" ~"5_100-150k",
      household_income=="$125,000 to $149,999" ~"5_100-150k",
      household_income=="$150,000 or more"     ~"6_150k+",
      TRUE                                     ~"7_missing")
  ) %>% 
  group_by(income_rc) %>% 
  summarize(total=n()) %>% 
  spread(income_rc,total)

# Recode race/ethnicity variables to zeros and ones and summarize data by race

final_race <- final %>% 
  mutate_at(vars(`Question 27: Caucasian/White`:`Question 27: Native Hawaiian/Other Pacific Islander`),
            ~ case_when(
              is.na(.) ~0,
              .=="X"   ~1)) %>% mutate(
  sum_ethnicities=`Question 27: African-American/Black`+`Question 27: American Indian/Alaska Native`+        # Hispanic not included, addressed separately
    `Question 27: Asian`+`Question 27: Caucasian/White`+`Question 27: Native Hawaiian/Other Pacific Islander`,
  race_general=case_when(
    `Question 27: Hispanic/Latino`==1 | Recode_Other=="hispanic"                       ~ "3_hispanic",  
    sum_ethnicities>1 | Recode_Other=="multiracial"                                    ~ "4_other",
    sum_ethnicities==0 & Recode_Other=="other"                                         ~ "4_other",
    sum_ethnicities==0 & Recode_Other=="native american"                               ~ "4_other",
    sum_ethnicities==1 & `Question 27: American Indian/Alaska Native`==1               ~ "4_other",
    sum_ethnicities==1 & `Question 27: Native Hawaiian/Other Pacific Islander`==1      ~ "4_other",
    sum_ethnicities==0 & Recode_Other=="white"                                         ~ "5_white",
    sum_ethnicities==1 & `Question 27: Caucasian/White`==1                             ~ "5_white",
    sum_ethnicities==0 & Recode_Other=="black"                                         ~ "2_black",
    sum_ethnicities==1 & `Question 27: African-American/Black`==1                      ~ "2_black",
    sum_ethnicities==1 & `Question 27: Asian`==1                                       ~ "1_asian",
    sum_ethnicities==0 & Recode_Other=="asian"                                         ~ "1_asian",
    TRUE                                                                               ~ "6_missing")) %>% 
  group_by(race_general) %>% 
  summarize(total=n()) %>% 
  spread(race_general,total)


write.csv(final_income, paste0(output, "2011 BATA Survey Bay Bridge Income.csv"), row.names = FALSE, quote = T)
write.csv(final_race, paste0(output, "2011 BATA Survey Bay Bridge Race.csv"), row.names = FALSE, quote = T)
                           