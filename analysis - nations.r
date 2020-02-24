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
library(brclib)

source("init.r")
source("load lookup tables.r")

##
## load risk data
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
  
  left_join(imd_lad, by = "LAD17CD") %>%   # merge deprivation summary
  
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
  pivot_longer(cols = `Asylum seekers receiving support`:Destitution, names_to = "Indicator", values_to = "Indicator Value") %>% 

  mutate(Domain = "Deprivation") %>%  # track which domain this is

  select(Country, LAD17CD, Domain, `Domain Value 1` = Prop_top10_q, Prop_top10, Indicator, `Indicator Value`) %>% 
  arrange(Country, desc(Prop_top10))

##
## summarise into country-level inequalities
##
# difference between worst versus best outcomes
sum_uk = sum_uk_lad %>% 
  group_by(Country, Indicator) %>% 
  summarise(max = max(`Indicator Value`, na.rm = T),
            min = min(`Indicator Value`, na.rm = T),
            diff = abs(max - min),
            
            # need the max and min values for next calculation
            most_dep = max(Prop_top10),
            least_dep = min(Prop_top10))

# save
write_csv(sum_uk, file.path(data.dir.out, "Risks - nations - summary.csv"))
write_csv(sum_uk_lad, file.path(data.dir.out, "Risks - nations.csv"))

# difference between outcome in most-deprived geography versus least-deprived geography
sum_uk_lad %>% 
  left_join(sum_uk, by = c("Country", "Indicator"))
