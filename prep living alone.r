##
## Proportion of people living alone in MSOAs - from 'household composition' tables in censuses 
##
## England and Wales: QS112EW (Household composition - People), from https://www.nomisweb.co.uk/census/2011/qs112ew
## Scotland: QS112SC (Household composition - People), from https://www.scotlandscensus.gov.uk/ods-web/download/getDownloadFile.html?downloadFileIds=SNS%20Data%20Zone%202011%20blk
## NI: QS110NI (Household Composition - Usual Residents), from https://www.nisra.gov.uk/statistics/2011-census/results/quick-statistics --> http://www.ninis2.nisra.gov.uk/Download/Census%202011/Quick%20Statistics%20Tables%20(statistical%20geographies).zip
##
library(tidyverse)
library(readxl)

source("init.r")

##
## load data
##
alone_ew  = read_csv(file.path(data.dir.in, "census", "QS112UK - Household composition - People - MSOA.csv"))
alone_sco = read_csv(file.path(data.dir.in, "census", "QS112SC.csv"))
alone_ni  = read_csv(file.path(data.dir.in, "census", "QS110NIDATA0.CSV"))  # from Quick Statistics Tables (statistical geographies)/SUPER OUTPUT AREAS

# tidy up Eng/Wal columns
alone_ew = alone_ew %>% 
  select(-date, -geography)

##
## NI's data has codes for column names - replace codes with column names
##
# from Quick Statistics Tables (statistical geographies)/SUPER OUTPUT AREAS
alone_ni_cols = read_csv(file.path(data.dir.in, "census", "QS110NIDESC0.CSV"))

names(alone_ni) = c("geography code", alone_ni_cols$ColumnVariableDescription)

##
## Scotland's data is at Data Zone level - aggregate into Intermediate Zones
##
# Data Zone and Intermediate Zone 2011 Lookups
# source: https://www2.gov.scot/Topics/Statistics/sns/SNSRef/DZ2011Lookups
sco_lookup = read_csv("https://www2.gov.scot/Resource/0046/00462937.csv") %>% 
  select(DZ = DataZone, `geography code` = InterZone) %>% 
  distinct()

alone_sco = alone_sco %>% 
  # merge in Intermediate Zone codes
  rename(DZ = X1) %>% 
  left_join(sco_lookup, by = "DZ") %>% 
  select(`geography code`, everything()) %>% 
  
  filter(!is.na(`geography code`)) %>%  # get rid of any rows that don't have an associated Intermediate Zone
  select(-DZ) %>%  # don't need this column anymore
  
  # sum all the people count columns
  group_by(`geography code`) %>% 
  summarise_all(sum)

##
## calculate proportions of people living alone in each census/country
##
alone_ew = alone_ew %>% 
  mutate(prop_alone = (`Household Composition: One person household: Total; measures: Value` + `Household Composition: One family only: Lone parent: Total; measures: Value`) / `Household Composition: All categories: Household composition; measures: Value`) %>% 
  select(`geography code`, prop_alone, 
         n_people_total = `Household Composition: All categories: Household composition; measures: Value`,
         n_one_person_hh = `Household Composition: One person household: Total; measures: Value`, 
         n_lone_parents = `Household Composition: One family only: Lone parent: Total; measures: Value`)

alone_sco = alone_sco %>% 
  mutate(prop_alone = (`One person household` + `One family household: Lone parent family`) / `All people in households`) %>% 
  select(`geography code`, prop_alone,
         n_people_total = `All people in households`,
         n_one_person_hh = `One person household`,
         n_lone_parents = `One family household: Lone parent family`)

alone_ni = alone_ni %>% 
  mutate(prop_alone = (`One person household: Total` + `One family and no other people: Lone parent: Total`) / `All usual residents in households`) %>% 
  select(`geography code`, prop_alone,
         n_people_total = `All usual residents in households`,
         n_one_person_hh = `One person household: Total`,
         n_lone_parents = `One family and no other people: Lone parent: Total`)

##
## combine into single dataset and save
##
alone_uk = bind_rows(alone_ew, alone_sco, alone_ni)

write_csv(alone_uk, file.path(data.dir.processed, "UK - Household composition - People - MSOA.csv"))
