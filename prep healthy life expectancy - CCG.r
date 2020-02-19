##
## Healthy life expectancy at birth - by health area (CCG, Health Board, etc.) - for England, Wales and Scotland
##
library(tidyverse)
library(readxl)
library(httr)
library(brclib)
library(sf)

source("init.r")

##
## load all data
##

##
## England - 2011 Census analysis: Healthy Life Expectancy at Birth and at Age 65: Clinical Commissioning Groups (CCGs) 2010-12
## - data: https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/2011censusanalysishealthylifeexpectancyatbirthandatage65clinicalcommissioninggroupsccgs201012
## - ONS report: https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/articles/healthylifeexpectancyatbirthandatage65clinicalcommissioninggroups/2014-03-21
##
GET("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/2011censusanalysishealthylifeexpectancyatbirthandatage65clinicalcommissioninggroupsccgs201012/referencetable_tcm77-356975.xls",
    write_disk(tf <- tempfile(fileext = ".xls")))

hle_eng_m = read_excel(tf, sheet = "HLE at birth_Males", skip = 7)
hle_eng_f = read_excel(tf, sheet = "HLE at birth_Females", skip = 7)

unlink(tf)


##
## Scotland - Healthy Life Expectancy in Scottish Areas 2015-2017
## - data: https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/life-expectancy/life-expectancy-in-scottish-areas/healthy-life-expectancy-in-scottish-areas-2015-2017
##
GET("https://www.nrscotland.gov.uk/files//statistics/healthy-life-expectancy/15-17/healthy-le-15-17-tabs.xlsx",
    write_disk(tf <- tempfile(fileext = ".xlsx")))

hle_sco = read_excel(tf, sheet = "Table 3", skip = 2)

unlink(tf)

##
## Wales - Life expectancy and Healthy life expectancy at birth by Local Health Board and Local Authority
## - data: https://statswales.gov.wales/Catalogue/Health-and-Social-Care/Life-Expectancy
##
hle_wales = download_wales("http://open.statswales.gov.wales/en-gb/dataset/hlth1598")

# Clinical Commissioning Groups (April 2018) Names and Codes in England
# source: https://geoportal.statistics.gov.uk/datasets/clinical-commissioning-groups-april-2018-names-and-codes-in-england
ccg_lookup = read_csv("https://opendata.arcgis.com/datasets/4010cd6fc6ce42c29581c4654618e294_0.csv")


###############################################################################
## Wrangle data
##

##
## England - get female and male data into single table
##
hle_eng_m = hle_eng_m %>% select(Name = `CCG Name`, HLE_birth_male = `HLE2,3 (years)`)
hle_eng_f = hle_eng_f %>% select(Name = `CCG Name`, HLE_birth_female = `HLE2,3 (years)`)

hle_eng = hle_eng_m %>% 
  left_join(hle_eng_f, by = "Name") %>% 
  
  # some tweaking before we can merge in CCG codes
  mutate(Name = paste0(Name, " CCG")) %>%   # need to append "CCG" to do the lookup
  mutate(Name = str_replace_all(Name, "&", "and")) %>% 
  
  # get codes for the CCG names
  left_join(ccg_lookup %>% select(Name = CCG18NM, Code = CCG18CD), 
            by = "Name") %>% 
  
  filter(str_detect(Name, "NHS")) %>%  # get rid of rows that aren't about CCGs
  
  mutate(HLE_birth = rowMeans(select(., HLE_birth_male, HLE_birth_female))) %>% 
  
  select(Code, Name, everything())

# rm(hle_eng_f, hle_eng_m)

##
## Wales
##
hle_wales = hle_wales %>% 
  filter(Measure_Code == "HLE" & Year_Code == "201014" & str_detect(Area_ItemName_ENG, "Health Board")) %>% 
  select(Code = Area_AltCode1, Name = Area_ItemName_ENG, Gender = Gender_ItemName_ENG, HLE_birth = Data) %>% 
  
  # calculate average HLE at birth across genders
  group_by(Code, Name) %>% 
  summarise(HLE_birth = mean(HLE_birth, na.rm = T))

##
## Scotland
##
hle_sco = hle_sco %>% 
  select(Code = `Area code`, Name = `Health board`, Gender = Sex, HLE_birth = `Healthy Life Expectancy (HLE) (years)`) %>% 
  
  # calculate average HLE at birth across genders
  group_by(Code, Name) %>% 
  summarise(HLE_birth = mean(HLE_birth, na.rm = T)) %>% 
  na.omit

##
## combine into one dataframe and save
##
hle = bind_rows(hle_eng, hle_wales, hle_sco)

hle %>% write_csv(file.path(data.dir.processed, "Healthy life expectancy - CCGs - England, Wales, Scotland.csv"))
