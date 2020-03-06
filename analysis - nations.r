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

# for stats models
library(broom)
library(rstanarm)
library(loo)

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
  
  # add Northern Ireland's LSOAs as if they're MSOAs
  bind_rows(imd_lsoa %>% filter(startsWith(LSOA, "9")) %>% mutate(MSOA11CD = LSOA)) %>% 
  
  # label deciles by whether they're in top 10 then summarise by this label
  mutate(IMD_top10 = ifelse(IMD_decile <= 2, "Top10", "Other")) %>% 
  janitor::tabyl(MSOA11CD, IMD_top10) %>% 
  
  # calculate proportion of most deprived LSOAs
  mutate(Prop_top10 = Top10 / (Top10 + Other)) %>% 
  
  mutate(Country = get_country(MSOA11CD))

# need to calculate deciles separately and merge into dataframe
imd_msoa_e = imd_msoa %>% 
  filter(Country == "England") %>% 
  select(MSOA11CD, Prop_top10) %>% 
  add_risk_quantiles("Prop_top10", quants = 10)

imd_msoa_w = imd_msoa %>% 
  filter(Country == "Wales") %>% 
  select(MSOA11CD, Prop_top10) %>% 
  add_risk_quantiles("Prop_top10", quants = 10)

imd_msoa_s = imd_msoa %>% 
  filter(Country == "Scotland") %>% 
  select(MSOA11CD, Prop_top10) %>% 
  add_risk_quantiles("Prop_top10", quants = 10)

imd_msoa_n = imd_msoa %>% 
  filter(Country == "Northern Ireland") %>% 
  select(MSOA11CD, Prop_top10) %>% 
  add_risk_quantiles("Prop_top10", quants = 10)

# merge quantiles into imd_msoa
imd_msoa = imd_msoa %>% 
  select(-Prop_top10) %>% 
  left_join(
    bind_rows(imd_msoa_e, imd_msoa_w, imd_msoa_s, imd_msoa_n),
    by = "MSOA11CD"
  )

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
## summarise into MSOAs, calculating proportions of urban versus rural areas in each
## - count the number of urban and rural (and mixed, in NI) LSOAs in each MSOA
## - calculate the proportion of urban LSOAs
## - categorise into three quantiles
##
# England
ruc_eng_msoa = ruc_ew %>% 
  filter(startsWith(LSOA11CD, "E")) %>%
  left_join(lookup_uk_lsoa_msoa, by = "LSOA11CD") %>% 
  
  tabyl(MSOA11CD, RUC) %>% 
  mutate(Prop_Urban = Urban / (Urban + Rural)) %>% 
  add_risk_quantiles("Prop_Urban", quants = 3) %>% 
  
  select(MSOA11CD, Prop_Urban, Prop_Urban_q, Prop_Urban_q_name)

# Wales
ruc_wal_msoa = ruc_ew %>% 
  filter(startsWith(LSOA11CD, "W")) %>%
  left_join(lookup_uk_lsoa_msoa, by = "LSOA11CD") %>% 
  
  tabyl(MSOA11CD, RUC) %>% 
  mutate(Prop_Urban = Urban / (Urban + Rural)) %>% 
  add_risk_quantiles("Prop_Urban", quants = 3) %>% 
  
  select(MSOA11CD, Prop_Urban, Prop_Urban_q, Prop_Urban_q_name)

# Scotland
ruc_sco_msoa = ruc_sco %>% 
  left_join(lookup_uk_lsoa_msoa, by = "LSOA11CD") %>% 
  
  tabyl(MSOA11CD, RUC) %>% 
  mutate(Prop_Urban = Urban / (Urban + Rural)) %>% 
  add_risk_quantiles("Prop_Urban", quants = 3) %>% 
  
  select(MSOA11CD, Prop_Urban, Prop_Urban_q, Prop_Urban_q_name)

# NI
ruc_ni_lsoa = ruc_ni %>%
  mutate(Prop_Urban = (Urban + (Mixed / 2)) / (Urban + Mixed + Rural)) %>%   # ARBITRARY DECISION: counting half the mixed LSOAs as urban
  add_risk_quantiles("Prop_Urban", quants = 3) %>% 
  
  select(MSOA11CD = LSOA11CD, Prop_Urban, Prop_Urban_q, Prop_Urban_q_name)

# stitch these into a UK-wide dataframe
ruc_uk_msoa = bind_rows(ruc_eng_msoa, ruc_wal_msoa, ruc_sco_msoa, ruc_ni_lsoa)


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

# get categories (to write as a note on Power BI dashboard)
unique(ruc_eng_lad$Prop_Urban_q_name)
unique(ruc_wal_lad$Prop_Urban_q_name)
unique(ruc_sco_lad$Prop_Urban_q_name)
unique(ruc_ni_lad$Prop_Urban_q_name)


###############################################################################
## Calculate inequalities for each indicator in each nation - for Local Authorities
##
risk_uk_lad = bind_rows(
  risk_eng_lad %>% select(LAD17CD, Sec95, destitution_migrant, HLE_birth, digital_total_mult, destitution_all,  # LAD-level risks
                          worst_fires, worst_floods, worst_lonely, worst_alone),  # MSOA-level risks
  
  risk_wal_lad %>% select(LAD17CD, Sec95, destitution_migrant, HLE_birth, digital_total_mult, destitution_all,  # LAD-level risks
                          worst_floods, worst_lonely, worst_alone),  # MSOA-level risks
  
  risk_sco_lad %>% select(LAD17CD, Sec95, destitution_migrant, HLE_birth, digital_total_mult, destitution_all,  # LAD-level risks
                          worst_fires, worst_lonely, worst_alone),  # MSOA-level risks
  
  risk_ni_lad  %>% select(LAD17CD = LAD18CD, Sec95, HLE_birth, digital_total_mult,  # LAD-level risks
                          worst_floods, worst_lonely, worst_alone)  # MSOA-level risks
)

# convert to long format and tidy up data
uk_lad = risk_uk_lad %>% 
  left_join(imd_lad, by = "LAD17CD") %>%   # merge deprivation summary
  left_join(ruc_uk_lad, by = "LAD17CD") %>% 
  
  select(-Other, -Top10, -Prop_top10_q_name) %>% 
  
  rename(Deprivation = Prop_top10_q, `Rural-urban classification` = Prop_Urban_q) %>% 
  pivot_longer(cols = c(Deprivation, `Rural-urban classification`), names_to = "Domain", values_to = "Domain Value")

# summarise each indicator within countries and domains (most need to report the max. value, but...
# ... Health Life Expectancy should report min. (i.e. lowest HLE))
sum_uk_lad = uk_lad %>% 
  group_by(Country, Domain, `Domain Value`) %>% 
  summarise(
    # LAD-level indicators
    `Asylum seekers receiving support` = max(Sec95, na.rm = T),
    `Migrant destitution` = max(destitution_migrant, na.rm = T),
    `Healthy life expectancy` = min(HLE_birth, na.rm = T),
    `Digital exclusion` = max(digital_total_mult, na.rm = T),
    Destitution = max(destitution_all, na.rm = T),
    
    # already-summarised MSOA-level indicators
    `Dwelling fires` = max(worst_fires, na.rm = T),
    `Flooding` = max(worst_floods, na.rm = T),
    `Loneliness` = max(worst_lonely, na.rm = T),
    `Living alone` = max(worst_alone, na.rm = T)
  ) %>% 
  ungroup() %>% 
  
  # pivot indicator columns into long format
  pivot_longer(cols = `Asylum seekers receiving support`:`Living alone`, names_to = "Indicator", values_to = "Indicator Value") %>% 
  
  # select(Country, LAD17CD, Domain, `Domain Value`, Prop_top10, Prop_Urban, Indicator, `Indicator Value`) %>% 
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


###############################################################################
## Calculate inequalities for each indicator in each nation - for MSOAs
##
risk_uk_msoa = bind_rows(
  risk_eng_msoa %>% select(MSOA11CD, n_fires, n_people_flood, loneills_2018, prop_alone),
  risk_wal_msoa %>% select(MSOA11CD, n_people_flood, loneills_2018, prop_alone),
  risk_sco_msoa %>% select(MSOA11CD, n_fires, loneills_2018, prop_alone),
  risk_ni_lsoa  %>% select(MSOA11CD = LSOA11CD, n_people_flood, loneills_2018, prop_alone)
)

# convert to long format and tidy up data
uk_msoa = risk_uk_msoa %>% 
  left_join(imd_msoa, by = "MSOA11CD") %>%   # merge deprivation summary
  left_join(ruc_uk_msoa, by = "MSOA11CD") %>% 
  
  select(-Other, -Top10, -Prop_top10_q_name) %>% 
  
  rename(Deprivation = Prop_top10_q, `Rural-urban classification` = Prop_Urban_q) %>% 
  pivot_longer(cols = c(Deprivation, `Rural-urban classification`), names_to = "Domain", values_to = "Domain Value")


###############################################################################
## Analyse healthy life expectancy
##
# HLE covars
uk_lad_dep_hle = uk_lad %>% 
  filter(Domain == "Deprivation") %>% 
  select(Country, LAD17CD, Deprivation = `Domain Value`, Prop_top10, HLE_birth) %>% 
  na.omit()

# histograms of HLE by country
uk_lad_dep_hle %>% 
  ggplot(aes(x = HLE_birth, fill = Country)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~Country, scales = "free_y")

# summary stats by country
tapply(uk_lad_dep_hle$HLE_birth, uk_lad_dep_hle$Country, summary)
tapply(uk_lad_dep_hle$Prop_top10, uk_lad_dep_hle$Country, summary)

# range of HLE in least-deprived LADs
uk_lad_dep_hle %>% 
  filter(Prop_top10 == 0) %>% 
  summarise(min(HLE_birth), max(HLE_birth))
  

##
## how does HLE differ between deprivation deciles and nations (non-spatial)?
##
m_hle_dep = lm(HLE_birth ~ Prop_top10 * Country, data = uk_lad_dep_hle)

plot(m_hle_dep)  # check residuals etc.
glance(m_hle_dep)  # look at model fit etc.

tidy(m_hle_dep, conf.int = T)

# Bayesian version
m_hle_country_dep = stan_glm(HLE_birth ~ Prop_top10 * Country, 
                             data = uk_lad_dep_hle,
                             prior_intercept = normal(0, 5), prior = normal(0, 5),
                             adapt_delta = 0.99, chains = 4)

# plot coefficients
plot(m_hle_country_dep)

tidy(m_hle_country_dep, conf.int = T)

# plot model predictions for proportion of deprivation and country on HLE
new.data = expand_grid(
  Country = unique(uk_lad_dep_hle$Country),
  Prop_top10 = seq(0, 1, by = 0.01)
)

post = posterior_predict(m_hle_country_dep, newdata = new.data)
pred = posterior_linpred(m_hle_country_dep, newdata = new.data)

quants = apply(post, 2, quantile, probs = c(0.025, 0.5, 0.975))  # quantiles over mcmc samples
quants2 = apply(pred, 2, quantile, probs = c(0.025, 0.5, 0.975))  # quantiles over mcmc samples
row.names(quants) = c("sim.lwr", "sim.med", "sim.upr")
row.names(quants2) = c("lwr", "HLE_birth.pred", "upr")

new.data = cbind(new.data, t(quants), t(quants2))

ggplot(new.data, aes(x = Prop_top10, y = HLE_birth.pred)) +
  geom_point(data = uk_lad_dep_hle, aes(y = HLE_birth), alpha = 0.2) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = Country), alpha = 0.4, show.legend = F) +
  geom_line(aes(colour = Country), lwd = 1.5, show.legend = F) +
  
  facet_wrap(~Country) +
  
  scale_color_brewer(palette = "Accent") +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(breaks = seq(40, 70, by = 5)) +
  
  labs(y = "Healthy life expectancy at birth (years)\n", x = "\nProportion of highly deprived neighbourhoods") +
  theme_classic()

ggsave("plots/nations/hle.png", width = 175, height = 150, units = "mm")


###############################################################################
## Analyse flooding
##
uk_msoa_flood = uk_msoa %>% 
  filter(Domain == "Deprivation") %>% 
  select(Country, MSOA11CD, Deprivation = `Domain Value`, Prop_top10, n_people_flood) %>% 
  na.omit()

uk_msoa_flood %>% 
  ggplot(aes(x = factor(Deprivation), y = n_people_flood)) +
  geom_boxplot() +
  facet_wrap(~ Country)


# m_flood = lm(n_people_flood ~ Prop_top10 * Country, data = uk_msoa_flood)
# tidy(m_flood, conf.int = T)

# fit model
m_flood = stan_glm(n_people_flood ~ Prop_top10 * Country, 
                   data = uk_msoa_flood,
                   prior_intercept = normal(0, 5), prior = normal(0, 5),
                   adapt_delta = 0.99, chains = 4)

# plot coefficients
plot(m_flood)

# plot model predictions for proportion of deprivation and country on HLE
new.data = expand_grid(
  Country = c("England", "Wales"),
  Prop_top10 = seq(0, 1, by = 0.01)
)

post = posterior_predict(m_flood, newdata = new.data)
pred = posterior_linpred(m_flood, newdata = new.data)

quants = apply(post, 2, quantile, probs = c(0.025, 0.5, 0.975))  # quantiles over mcmc samples
quants2 = apply(pred, 2, quantile, probs = c(0.025, 0.5, 0.975))  # quantiles over mcmc samples
row.names(quants) = c("sim.lwr", "sim.med", "sim.upr")
row.names(quants2) = c("lwr", "flood.pred", "upr")

new.data = cbind(new.data, t(quants), t(quants2))

ggplot(new.data, aes(x = Prop_top10, y = flood.pred)) +
  geom_point(data = uk_msoa_flood, aes(y = n_people_flood), alpha = 0.2) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = Country), alpha = 0.4, show.legend = F) +
  geom_line(aes(colour = Country), lwd = 1.5, show.legend = F) +
  
  facet_wrap(~Country) +
  
  scale_color_brewer(palette = "Accent") +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::comma) +
  
  labs(y = "Number of people at risk of flooding\n", x = "\nProportion of highly deprived neighbourhoods") +
  theme_classic()

ggsave("plots/nations/flood.png", width = 175, height = 150, units = "mm")

