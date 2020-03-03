##
## Dwelling fires
##
## Mapping at different geographies
## - England and Wales: Fire and Rescue Authority areas
## - Scotland: Intermediate Zones (equivalent of MSOA) *or* Local Authority(?)
## - NI: ** not available yet **
##
library(tidyverse)
library(readxl)
library(httr)
library(brclib)  # run `devtools::install_github("matthewgthomas/brclib")` to install this

source("init.r")

# change these filters when you update the data
fin_year = "2018/19"      # can be found by running `unique(fires_eng_lsoa$FINANCIAL_YEAR)` after you load that data
fin_year_wal = "2018-19"  # can be found by running `unique(fires_wal_all$Year_Code)` after you load that data
fin_year_sco = 2018       # can be found by running `unique(fires_sco$CalendarYear)` after you load that data


#########################################################################################################
## Load geographies
##
# Fire and Rescue Authorities (December 2019) Names and Codes in the United Kingdom
# source: https://geoportal.statistics.gov.uk/datasets/fire-and-rescue-authorities-december-2019-names-and-codes-in-the-united-kingdom
fra = read_csv("https://opendata.arcgis.com/datasets/c51a426a0fd24420954dcf0a9f6d407d_0.csv")

# Local Authorities
lad = load_lads(2019)

# Middle Layer Super Output Areas (December 2011) Names and Codes in England and Wales
# source: https://geoportal.statistics.gov.uk/datasets/middle-layer-super-output-areas-december-2011-names-and-codes-in-england-and-wales
msoa_eng = read_csv("https://opendata.arcgis.com/datasets/6b82649791b3481692d63d39a653143c_0.csv")

# Intermediate Zones (December 2011) Names and Codes in Scotland
# source: https://geoportal.statistics.gov.uk/datasets/intermediate-zones-december-2011-names-and-codes-in-scotland
msoa_sco = read_csv("https://opendata.arcgis.com/datasets/02f78aaf05df4f48a84cc5b10751ba3c_0.csv")

# Data Zones (December 2011) Names and Codes in Scotland
# source: https://geoportal.statistics.gov.uk/datasets/data-zones-december-2011-names-and-codes-in-scotland
dz = read_csv("https://opendata.arcgis.com/datasets/d278dbbe3b664d409368c8a936cdcf8e_0.csv")


#########################################################################################################
## Load population estimates
##

##
## England/Wales Lower layer Super Output Area population estimates
## source: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates
##
# download and unzip population data
GET("https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2flowersuperoutputareamidyearpopulationestimates%2fmid2018sape21dt1a/sape21dt1amid2018on2019lalsoasyoaestimatesformatted.zip",
    write_disk(tf <- tempfile(fileext = ".zip")))
unzip(tf, exdir = file.path(data.dir.in, "population"))
unlink(tf); rm(tf)

# load the data - if updating the file above, you may need to change the filename and the worksheet name
pop_eng_lsoa = read_excel(file.path(data.dir.in, "population/SAPE21DT1a-mid-2018-on-2019-LA-lsoa-syoa-estimates-formatted.xlsx"), sheet = "Mid-2018 Persons", skip = 4)

# keep only LSOA-level population totals
pop_eng_lsoa = pop_eng_lsoa %>% 
  filter(!is.na(LSOA)) %>%  # rows where LSOA is null contain totals for the Local Authority - we need to exclude these
  select(lsoa11cd = `Area Codes`, n_people = `All Ages`)

##
## Scotland
##
# Household estimates by data zone (2011)
# source: https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/households/household-estimates/small-area-statistics-on-households-and-dwellings
GET("https://www.nrscotland.gov.uk/files//statistics/household-estimates/small-area/hh-est-by-2011-dz-small-area-14-18_nov19.xlsx",
    write_disk(tf <- tempfile()))

dwellings_sco = read_excel(tf, skip = 2, sheet = "2018") %>% 
  na.omit

unlink(tf); rm(tf)


###############################################################################
## Load lookup tables
##
# Output Area to Lower Layer Super Output Area to Middle Layer Super Output Area to Local Authority District (December 2011) Lookup in England and Wales
# source: https://geoportal.statistics.gov.uk/datasets/output-area-to-lower-layer-super-output-area-to-middle-layer-super-output-area-to-local-authority-district-december-2011-lookup-in-england-and-wales
lsoa_to_msoa = read_csv("https://opendata.arcgis.com/datasets/6ecda95a83304543bc8feedbd1a58303_0.csv") %>% 
  select(starts_with("LSOA"), starts_with("MSOA")) %>% 
  distinct()

# Local Authority District to Fire and Rescue Authority (December 2019) Lookup in England and Wales
# source: https://geoportal.statistics.gov.uk/datasets/local-authority-district-to-fire-and-rescue-authority-december-2019-lookup-in-england-and-wales
fra_to_lad = read_csv("https://opendata.arcgis.com/datasets/cdfde6c07a2145e6a275a41ed9d7a906_0.csv")

# Data Zone and Intermediate Zone 2011 Lookups
# source: https://www2.gov.scot/Topics/Statistics/sns/SNSRef/DZ2011Lookups
dz_to_lad = read_csv("https://www2.gov.scot/Resource/0046/00462937.csv") %>% 
  select(DZ = DataZone, IZcode = InterZone, LAcode = Council) %>% 
  distinct()


#########################################################################################################
## England - Incidents by LSOA (Last updated 30 January 2020)
## source: "Low level geography dataset" from https://www.gov.uk/government/statistical-data-sets/fire-statistics-incident-level-datasets
## data: https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/858048/low-level-geography-dataset.ods
##
## This file was manually downloaded and converted to .xlsx because readODS::read_ods() doesn't seem to work with this file
##
unzip(file.path(data.dir.in, "fires.zip"), exdir = data.dir.in)

fires_eng_path = file.path(data.dir.in, "low-level-geography-dataset.xlsx")

# the file contains one worksheet per county - load all into one dataframe
fires_eng_lsoa = excel_sheets(fires_eng_path) %>% 
  set_names() %>% 
  map_df(~ read_excel(path = fires_eng_path, sheet = .x), .id = "sheet")

# some of the LSOA_DESCRIPTION columns actually contain LSOA codes - sort this out so all codes are in the same column
fires_eng_lsoa = fires_eng_lsoa %>% 
  mutate(is_code = str_detect(LSOA_DESCRIPTION, "E[0-9]+")) %>%  # check whether LSOA_DESCRIPTION is actually the LSOA code
  mutate(lsoa11cd = ifelse(is_code, LSOA_DESCRIPTION, LSOA_CODE)) %>% 
  select(-is_code)

eng_dwellings = c("Primary fire - dwelling", "Primary fire - dwelling or other building", "Primary fire - other buildings - Student Hall of Residence",
                  "Primary fire - other buildings - Other Residential Home")

# count the number of incidents in each LSOA 
fires_eng_lsoa_sum = fires_eng_lsoa %>% 
  filter(FINANCIAL_YEAR == fin_year & INCIDENT_TYPE %in% eng_dwellings) %>% 
  count(lsoa11cd) %>% 
  rename(n_fires = n)

##
## no. people in each LSOA
##
# merge population count into fires data and calculate density of fires per person
fires_eng_lsoa_sum = fires_eng_lsoa_sum %>% 
  left_join(pop_eng_lsoa, by = "lsoa11cd") %>% 
  mutate(dens_fires = n_fires / n_people)

##
## summarise by MSOA
##
fires_eng_msoa = fires_eng_lsoa_sum %>% 
  left_join(lsoa_to_msoa %>% select(LSOA11CD, MSOA11CD), by = c("lsoa11cd" = "LSOA11CD")) %>% 
  
  # summarise by msoa
  group_by(MSOA11CD) %>% 
  summarise(n_fires = sum(n_fires),
            n_people = sum(n_people)) %>% 
  
  # recalculate density
  mutate(dens_fires = n_fires / n_people)

##
## save
##
fires_eng_lsoa_sum %>% 
  write_csv(file.path(data.dir.processed, "Fires - LSOA - England.csv"))

fires_eng_msoa %>% 
  write_csv(file.path(data.dir.processed, "Fires - MSOA - England.csv"))


#########################################################################################################
## Wales - Incidents by Fire and Rescue Authority
## source: "Fires by detailed location, financial year and area" from https://statswales.gov.wales/Catalogue/Community-Safety-and-Social-Inclusion/Community-Safety/Fire-Incidents/Fires-and-False-Alarms
##
# download fires and dwellings data from StatsWales
fires_wal_all = brclib::download_wales("http://open.statswales.gov.wales/en-gb/dataset/csaf0048")
dwellings_wal_all = brclib::download_wales("http://open.statswales.gov.wales/en-gb/dataset/hous0501")

# all the things different types of dwellings are called in this dataset
dwelling_names_wales = c("House - single occupancy", "Bungalow - single occupancy", "Purpose built flat/maisonette", 
                         "Converted flat/maisonette", "Licensed HMO", "Unlicensed HMO", "Unknown if licensed HMO", 
                         "Self contained sheltered housing", "Caravan/mobile home (permanent dwelling)", "Other dwelling")

# summarise Wales dwelling fires
fires_wal = fires_wal_all %>% 
  filter(Area_Code != "All") %>% 
  filter(Location_ItemName_ENG %in% "Dwellings") %>% 
  filter(Year_Code == fin_year_wal) %>% 
  filter(Count_ItemName_ENG == "Fires") %>% 
  filter(Motive_ItemName_ENG == "All motives") %>% 
  group_by(Area_ItemName_ENG) %>% 
  summarise(n_fires = sum(Data)) %>% 
  rename(FRA19NM = Area_ItemName_ENG)

# add in FRA codes
fra_codes = fra %>% select(FRA19CD, FRA19NM)

fires_wal = fires_wal %>% 
  left_join(fra_codes, by = "FRA19NM") %>% 
  select(-FRA19NM)

##
## number of dwelling fires per dwelling in each FRA
##
dwellings_wal = dwellings_wal_all %>% 
  filter(Year_Code == max(dwellings_wal_all$Year_Code) & Tenure_ItemName_ENG == "All tenures (Number)") %>% 
  select(LAD19NM = Area_ItemName_ENG, n = Data) %>% 
  
  left_join(fra_to_lad, by = "LAD19NM") %>% 
  na.omit %>% 
  
  # count by FRA
  group_by(FRA19CD) %>% 
  summarise(n_dwellings = sum(n)) %>% 
  na.omit

# calculate fire density
fires_wal = fires_wal %>% 
  left_join(dwellings_wal, by = "FRA19CD") %>% 
  mutate(dens_fires = n_fires / n_dwellings)

# save
fires_wal %>% 
  select(FRA19CD, everything()) %>% 
  write_csv(file.path(data.dir.processed, "Fires - FRA - Wales.csv"))


#########################################################################################################
## Scotland - Incidents by Data Zone
## source: https://www.firescotland.gov.uk/about-us/fire-and-rescue-statistics.aspx
## data: https://www.firescotland.gov.uk/media/1332754/firesbydatazone.csv
##
fires_sco = read_csv("https://www.firescotland.gov.uk/media/1332754/firesbydatazone.csv")

# count fires by Data Zone (LSOA)
fires_sco_sum_dz = fires_sco %>% 
  filter(CalendarYear == fin_year_sco) %>%                      # fires in 2018
  group_by(DataZone) %>% 
  summarise(n_fires = n(), n_accidental = sum(Accidental), n_fatalities = sum(FireFatalities), n_casualties = sum(FireCasualties_exc_precautionary_checks)) %>% 
  
  left_join(dwellings_sco, by = c("DataZone" = "2011 Data Zone code")) %>% 
  mutate(dens_fires = n_fires / `Occupied Dwellings`)

# count fires by Intermediate Zone (MSOA)
fires_sco_sum_iz = fires_sco_sum_dz %>% 
  left_join(dz_to_lad, by = c("DataZone" = "DZ")) %>%    # lookup the Intermediate Zones and LADs in which they occurred
  group_by(IZcode) %>% 
  summarise(n_fires = sum(n_fires), n_dwellings = sum(`Occupied Dwellings`)) %>% 
  mutate(dens_fires = n_fires / n_dwellings)

# count fires by Council Area (LAD)
fires_sco_sum_lad = fires_sco_sum_dz %>% 
  left_join(dz_to_lad, by = c("DataZone" = "DZ")) %>%    # lookup the Intermediate Zones and LADs in which they occurred
  group_by(LAcode) %>% 
  summarise(n_fires = sum(n_fires), n_dwellings = sum(`Occupied Dwellings`)) %>% 
  mutate(dens_dires = n_fires / n_dwellings)

# save
fires_sco_sum_dz %>% 
  select(lsoa17cd = DataZone, n_fires, n_dwellings = `Occupied Dwellings`, dens_fires) %>% 
  write_csv(file.path(data.dir.processed, "Fires - LSOA - Scotland.csv"))

fires_sco_sum_iz %>% 
  write_csv(file.path(data.dir.processed, "Fires - MSOA - Scotland.csv"))

fires_sco_sum_lad %>% 
  write_csv(file.path(data.dir.processed, "Fires - LAD - Scotland.csv"))
