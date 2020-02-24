##
## Prepare risk data by nation - for each country, give the:
## - risk indicator
## - domain (overall inequality, deprivation decile or rural/urban classification)
## - value
##
## Also calculate a summary table with a measure of inequality:
## - absolute difference between most and least deprived
## - or inequality range/gap for rural/urban classification
##
library(tidyverse)
library(janitor)
library(brclib)

source("init.r")
source("load lookup tables.r")


###############################################################################
## Load risk data
##
risk_eng_msoa = read_csv(file.path(data.dir.out, "England - MSOA - risks.csv"))
risk_wal_msoa = read_csv(file.path(data.dir.out, "Wales - MSOA - risks.csv"))
risk_sco_msoa = read_csv(file.path(data.dir.out, "Scotland - MSOA - risks.csv"))
risk_ni_lsoa  = read_csv(file.path(data.dir.out, "NI - LSOA - risks.csv"))

risk_eng_lad = read_csv(file.path(data.dir.out, "England - LAD - risks.csv"))
risk_wal_lad = read_csv(file.path(data.dir.out, "Wales - LAD - risks.csv"))
risk_sco_lad = read_csv(file.path(data.dir.out, "Scotland - LAD - risks.csv"))
risk_ni_lad  = read_csv(file.path(data.dir.out, "NI - LAD - risks.csv"))

risk_wal_fra = read_csv(file.path(data.dir.out, "Wales - FRA - risks.csv"))


###############################################################################
## Load lookups - LSOA, MSOA, LAD, FRA
##
lookup_dz_iz_lad = load_lookup_dz_iz_lad()
lookup_lad_fra = load_lookup_lad_fra()
lookup_lsoa_msoa_lad = load_lookup_lsoa_msoa_lad()
lookup_sa_lgd = load_lookup_sa_lgd()

# make lookup table with LSOA and MSOA codes (doesn't include NI because there's no equivalent to MSOAs)
lookup_uk_lsoa_msoa = bind_rows(
  # England and Wales
  lookup_lsoa_msoa_lad %>% 
    select(LSOA11CD, MSOA11CD) %>% 
    distinct(),
  
  # Scotland
  lookup_dz_iz_lad %>% 
    select(LSOA11CD, MSOA11CD) %>% 
    distinct()
)

# make lookup table with LSOA and LAD codes
lookup_uk_lsoa_lad = bind_rows(
  # England and Wales
  lookup_lsoa_msoa_lad %>% 
    select(LSOA11CD, LAD17CD) %>% 
    distinct(),
  
  # Scotland
  lookup_dz_iz_lad %>% 
    select(LSOA11CD, LAD17CD) %>% 
    distinct(),
  
  # NI
  lookup_sa_lgd %>% 
    select(LSOA11CD, LAD17CD = LAD18CD) %>% 
    distinct()
)

# make lookup table with LSOA and FRA codes
lookup_uk_lsoa_fra = lookup_uk_lsoa_lad %>% 
  left_join(lookup_lad_fra, by = "LAD17CD") %>% 
  select(LSOA11CD, FRA17CD) %>% 
  distinct()


###############################################################################
## Summarise deprivation by MSOA, LAD, and FRA
## = % of LSOAs in each geography in the top 10% most deprived, split into quintiles
##
imd_lsoa = load_IMD()

##
## MSOA
##
imd_msoa = imd_lsoa %>% 
  # lookup MSOAs for each LSOA
  select(LSOA, IMD_decile) %>% 
  left_join(lookup_uk_lsoa_msoa, by = c("LSOA" = "LSOA11CD")) %>% 
  
  # label deciles by whether they're in top 10 then summarise by this label
  mutate(IMD_top10 = ifelse(IMD_decile <= 2, "Top10", "Other")) %>% 
  janitor::tabyl(MSOA11CD, IMD_top10) %>% 
  
  # calculate proportion of most deprived LSOAs
  mutate(Prop_top10 = Top10 / (Top10 + Other)) %>% 
  
  mutate(Country = get_country(MSOA11CD)) %>% 
  
  # calculate deciles for higher-level geography within nations
  group_by(Country) %>% 
  add_risk_quantiles("Prop_top10", quants = 10) %>% 
  ungroup()


##
## LAD
##
imd_lad = imd_lsoa %>% 
  # lookup LADs for each LSOA
  select(LSOA, IMD_decile) %>% 
  left_join(lookup_uk_lsoa_lad, by = c("LSOA" = "LSOA11CD")) %>% 
  
  # label deciles by whether they're in top 10 then summarise by this label
  mutate(IMD_top10 = ifelse(IMD_decile <= 2, "Top10", "Other")) %>% 
  janitor::tabyl(LAD17CD, IMD_top10) %>% 
  
  # calculate proportion of most deprived LSOAs
  mutate(Prop_top10 = Top10 / (Top10 + Other)) %>% 
  
  mutate(Country = get_country(LAD17CD))
  
  # calculate deciles for higher-level geography within nations
  # group_by(Country) %>% 
  # add_risk_quantiles("Prop_top10", quants = 10) %>% 
  # ungroup()

# need to calculate deciles separately and merge into dataframe
imd_lad_e = imd_lad %>% 
  filter(Country == "England") %>% 
  select(LAD17CD, Prop_top10) %>% 
  add_risk_quantiles("Prop_top10", quants = 10)

imd_lad_w = imd_lad %>% 
  filter(Country == "Wales") %>% 
  select(LAD17CD, Prop_top10) %>% 
  add_risk_quantiles("Prop_top10", quants = 10)

imd_lad_s = imd_lad %>% 
  filter(Country == "Scotland") %>% 
  select(LAD17CD, Prop_top10) %>% 
  add_risk_quantiles("Prop_top10", quants = 10)

imd_lad_n = imd_lad %>% 
  filter(Country == "Northern Ireland") %>% 
  select(LAD17CD, Prop_top10) %>% 
  add_risk_quantiles("Prop_top10", quants = 10)

# merge quantiles into imd_lad
imd_lad = imd_lad %>% 
  select(-Prop_top10) %>% 
  left_join(
    bind_rows(imd_lad_e, imd_lad_w, imd_lad_s, imd_lad_n),
    by = "LAD17CD"
  )

##
## FRA
##
imd_fra = imd_lsoa %>% 
  # lookup FRAs for each LSOA
  select(LSOA, IMD_decile) %>% 
  left_join(lookup_uk_lsoa_fra, by = c("LSOA" = "LSOA11CD")) %>% 
  
  # label deciles by whether they're in top 10 then summarise by this label
  mutate(IMD_top10 = ifelse(IMD_decile <= 2, "Top10", "Other")) %>% 
  janitor::tabyl(FRA17CD, IMD_top10) %>% 
  
  # calculate proportion of most deprived LSOAs
  mutate(Prop_top10 = Top10 / (Top10 + Other)) %>% 
  
  mutate(Country = get_country(FRA17CD)) %>% 
  
  filter(Country == "Wales") %>%  # we only have FRA-level data for Wales
  
  # calculate deciles for higher-level geography within nations
  group_by(Country) %>% 
  add_risk_quantiles("Prop_top10", quants = 10) %>% 
  ungroup()


###############################################################################
## Load rural-urban classifications
## - these files were created by the code in https://github.com/mattmalcher/IndexOfNeed/tree/master/Datasets/Rural-Urban%20Classifications
## - for details of what the classification codes mean, see: https://github.com/mattmalcher/IndexOfNeed/blob/master/Datasets/Rural-Urban%20Classifications/rural-urban%20classification%20codes.md
##
ruc_ew  = read_csv(file.path(data.dir.in, "rural-urban", "RUC England Wales - LSOA.csv"))
ruc_sco = read_csv(file.path(data.dir.in, "rural-urban", "RUC Scotland - DZ.csv"))
ruc_ni  = read_csv(file.path(data.dir.in, "rural-urban", "RUC Northern Ireland - SOA.csv"))

# dichotomise detailed classifications into rural or urban
ruc_ew = ruc_ew %>% 
  select(LSOA11CD, RUC11CD) %>% 
  mutate(RUC = case_when(
    RUC11CD %in% c("A1", "B1", "C1", "C2") ~ "Urban",
    RUC11CD %in% c("D1", "D2", "E1", "E2", "F1", "F2") ~ "Rural"
  ))

ruc_sco = ruc_sco %>% 
  select(LSOA11CD = DZ_CODE, UR2FOLD) %>% 
  mutate(RUC = ifelse(UR2FOLD == 1, "Urban", "Rural"))

# need to do something slightly different for NI, since they have a 'mixed' category - but conveniently they count how many of each category are in each LSOA
ruc_ni = ruc_ni %>% 
  select(LSOA11CD = `SOA Code`, Urban, Mixed = `mixed urban/rural`, Rural) %>% 
  mutate(Urban = replace_na(as.integer(Urban), 0),
         Mixed = replace_na(as.integer(Mixed), 0),
         Rural = replace_na(as.integer(Rural), 0))

##
## summarise into Local Authorities, calculating proportions of urban versus rural areas in each
## - count the number of urban and rural (and mixed, in NI) LSOAs in each LAD
## - calculate the proportion of urban LSOAs
## - categorise into three quantiles
##
# England
ruc_eng_lad = ruc_ew %>% 
  filter(startsWith(LSOA11CD, "E")) %>%
  left_join(lookup_uk_lsoa_lad, by = "LSOA11CD") %>% 
  
  tabyl(LAD17CD, RUC) %>% 
  mutate(Prop_Urban = Urban / (Urban + Rural)) %>% 
  add_risk_quantiles("Prop_Urban", quants = 3) %>% 
  
  select(LAD17CD, Prop_Urban, Prop_Urban_q, Prop_Urban_q_name)

# Wales
ruc_wal_lad = ruc_ew %>% 
  filter(startsWith(LSOA11CD, "W")) %>%
  left_join(lookup_uk_lsoa_lad, by = "LSOA11CD") %>% 
  
  tabyl(LAD17CD, RUC) %>% 
  mutate(Prop_Urban = Urban / (Urban + Rural)) %>% 
  add_risk_quantiles("Prop_Urban", quants = 3) %>% 
  
  select(LAD17CD, Prop_Urban, Prop_Urban_q, Prop_Urban_q_name)

# Scotland
ruc_sco_lad = ruc_sco %>% 
  left_join(lookup_uk_lsoa_lad, by = "LSOA11CD") %>% 
  
  tabyl(LAD17CD, RUC) %>% 
  mutate(Prop_Urban = Urban / (Urban + Rural)) %>% 
  add_risk_quantiles("Prop_Urban", quants = 3) %>% 
  
  select(LAD17CD, Prop_Urban, Prop_Urban_q, Prop_Urban_q_name)

# NI
ruc_ni_lad = ruc_ni %>% 
  left_join(lookup_uk_lsoa_lad, by = "LSOA11CD") %>% 
  
  group_by(LAD17CD) %>% 
  summarise(Urban = sum(Urban),
            Mixed = sum(Mixed),
            Rural = sum(Rural)) %>% 
  
  mutate(Prop_Urban = (Urban + (Mixed / 2)) / (Urban + Mixed + Rural)) %>%   # ARBITRARY DECISION: counting half the mixed LSOAs as urban
  add_risk_quantiles("Prop_Urban", quants = 3) %>% 
  
  select(LAD17CD, Prop_Urban, Prop_Urban_q, Prop_Urban_q_name)

# stitch these into a UK-wide dataframe
ruc_uk_lad = bind_rows(ruc_eng_lad, ruc_wal_lad, ruc_sco_lad, ruc_ni_lad)

# get categories
unique(ruc_eng_lad$Prop_Urban_q_name)
unique(ruc_wal_lad$Prop_Urban_q_name)
unique(ruc_sco_lad$Prop_Urban_q_name)
unique(ruc_ni_lad$Prop_Urban_q_name)


###############################################################################
## Calculate inequalities for each indicator in each nation
##

##
## Local Authority-level = HLE, Asylum support, migrant destitution, digital exclusion
##
risk_uk_lad = bind_rows(
  risk_eng_lad %>% select(LAD17CD, Sec95, destitution_migrant, HLE_birth, digital_total_mult, destitution_all), # %>% mutate(Country = "England"), 
  risk_wal_lad %>% select(LAD17CD, Sec95, destitution_migrant, HLE_birth, digital_total_mult, destitution_all), # %>% mutate(Country = "Wales"), 
  risk_sco_lad %>% select(LAD17CD, Sec95, destitution_migrant, HLE_birth, digital_total_mult, destitution_all), # %>% mutate(Country = "Scotland"), 
  risk_ni_lad  %>% select(LAD17CD = LAD18CD, Sec95, HLE_birth, digital_total_mult) # %>% mutate(Country = "Northern Ireland")
) %>% 
  
  # make friendly indicator names
  rename(`Asylum seekers receiving support` = Sec95,
         `Migrant destitution` = destitution_migrant,
         `Healthy life expectancy` = HLE_birth,
         `Digital exclusion` = digital_total_mult,
         Destitution = destitution_all
         )

# risk_uk_lad %>% 
#   filter(Country == "England") %>% 
#   ggplot(aes(x = `Destitution`)) + geom_histogram()

# convert to long format and tidy up data
sum_uk_lad = risk_uk_lad %>% 
  left_join(imd_lad, by = "LAD17CD") %>%   # merge deprivation summary
  left_join(ruc_uk_lad, by = "LAD17CD") %>% 
  
  select(-Other, -Top10, -Prop_top10_q_name) %>% 
  
  pivot_longer(cols = `Asylum seekers receiving support`:Destitution, names_to = "Indicator", values_to = "Indicator Value") %>% 
  
  rename(Deprivation = Prop_top10_q, `Rural-urban classification` = Prop_Urban_q) %>% 
  pivot_longer(cols = c(Deprivation, `Rural-urban classification`), names_to = "Domain", values_to = "Domain Value") %>% 

  # mutate(Domain = "Deprivation") %>%  # track which domain this is

  select(Country, LAD17CD, Domain, `Domain Value`, Prop_top10, Prop_Urban, Indicator, `Indicator Value`) %>% 
  arrange(Country, Domain, `Domain Value`)

##
## summarise into country-level inequalities
##
# difference between worst versus best outcomes
sum_uk = sum_uk_lad %>% 
  group_by(Country, Domain, Indicator) %>% 
  summarise(max = max(`Indicator Value`, na.rm = T),
            min = min(`Indicator Value`, na.rm = T),
            diff = abs(max - min)) %>% 
  ungroup()

##
## calculate mean difference between outcome in most-deprived geography versus least-deprived geographies (and most-urban versus least-urban)
##
# deprivation
sum_uk_dep = sum_uk_lad %>% 
  filter(Domain == "Deprivation", `Domain Value` %in% c(1, 10)) %>% 
  
  # calculate mean values for indicators in most and least deprived areas
  group_by(Country, Domain, `Domain Value`, Indicator) %>% 
  summarise(mean = mean(`Indicator Value`, na.rm = T)) %>% 
  
  # make the mean deprivation scores columns and calculate the absolute difference
  pivot_wider(names_from = `Domain Value`, names_prefix = "Dep_", values_from = mean) %>% 
  mutate(mean_diff = abs(Dep_1 - Dep_10)) %>% 
  
  ungroup() %>% 
  select(-Dep_1, -Dep_10)
  
# rural-urban classification
sum_uk_urb = sum_uk_lad %>% 
  filter(Domain == "Rural-urban classification", `Domain Value` %in% c(1, 3)) %>% 
  
  # calculate mean values for indicators in most and least deprived areas
  group_by(Country, Domain, `Domain Value`, Indicator) %>% 
  summarise(mean = mean(`Indicator Value`, na.rm = T)) %>% 
  
  # make the mean deprivation scores columns and calculate the absolute difference
  pivot_wider(names_from = `Domain Value`, names_prefix = "Urb_", values_from = mean) %>% 
  mutate(mean_diff = abs(Urb_1 - Urb_3)) %>% 
  
  ungroup() %>% 
  select(-Urb_1, -Urb_3)

# merge mean differences into 
sum_uk = sum_uk %>% 
  left_join(bind_rows(sum_uk_dep, sum_uk_urb), 
            by = c("Country", "Domain", "Indicator"))

##
## save
##
write_csv(sum_uk, file.path(data.dir.out, "Risks - nations - summary.csv"))
write_csv(sum_uk_lad, file.path(data.dir.out, "Risks - nations.csv"))

# output a list of indicators in the current dataset
sum_uk_lad %>%
  select(Indicator) %>% 
  distinct() %>% 
  arrange(Indicator) %>% 
  write_csv(file.path(data.dir.out, "Risks - nations - indicators.csv"))
