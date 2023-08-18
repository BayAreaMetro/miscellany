# ACS 2021 PUMS workers from all states.r
# Extract all "commuters" from all pums and assign bay residence and worker boolean fields
# "Commuters" includes civilian employed and armed forces at work and excludes: civilian and armed forces
# with a job but not at work; unemployed; out of the labor force

library(tidycensus)
library(tidyverse)
library(tigris)
options(tigris_use_cache = TRUE)

baypowpuma <-  c("00100","01300","04100","05500","07500","08100","08500","09500","09700") 

baypuma    <- c("00101", "00102", "00103", "00104", "00105", "00106", "00107", "00108", "00109", "00110", 
                "01301", "01302", "01303", "01304", "01305", "01306", "01307", "01308", "01309", "04101", 
                "04102", "05500", "07501", "07502", "07503", "07504", "07505", "07506", "07507", "08101", 
                "08102", "08103", "08104", "08105", "08106", "08501", "08502", "08503", "08504", "08505", 
                "08506", "08507", "08508", "08509", "08510", "08511", "08512", "08513", "08514", "09501", 
                "09502", "09503", "09701", "09702", "09703")

output <- "M:/Data/Census/PUMS/PUMS 2021/US PUMS"

allpums <- get_pums(
  variables = c("PUMA","ST", "POWPUMA","POWSP"),
  state = "all",
  year = 2021,
  survey = "acs1"
  )

# Exclude non-commuters, create Bay resident and worker variables

bayworkers <- allpums %>% 
  filter(!(POWPUMA=="0000N")) %>% 
  mutate(bay_puma=if_else((PUMA %in% baypuma & ST=="06"),"yes","no"),
         bay_puma_work=if_else((POWPUMA %in% baypowpuma & POWSP=="06"),"yes","no"))

write.csv(bayworkers,file.path(output,"ACS 2021 PUMS Workers.csv"),row.names = F)




