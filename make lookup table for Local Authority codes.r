##
## Generate lookup table from Local Authority (2017) codes and Local Authority (2019) codes - part of this needs to be done manually
## - because several haven't remained the same
## - and some LADs changed boundaries/merged etc.
##
library(tidyverse)

source("init.r")
source("load lookup tables.r")

lad_codes_17 = load_lads(2017)
lad_codes_19 = load_lads(2019)

# get as many LAD codes as we can
lad_codes = left_join(lad_codes_17, lad_codes_19, by = c("LAD17NM" = "LAD19NM")) %>% 
  select(LAD17CD, LAD19CD) %>% 
  na.omit()

# find which 2017 LADs are missing codes in the 2019 set
lad_codes_missing = anti_join(lad_codes_17, lad_codes_19, by = c("LAD17NM" = "LAD19NM"))

# save the missing ones - we'll have to look up the 2019 codes manually
lad_codes_missing %>% 
  select(LAD17CD, LAD17NM) %>% 
  mutate(LAD19CD = "") %>% 
  write_csv(file.path(data.dir.in, "LAD 2017 to LAD 2019 codes - missing.csv"))

#! MANUAL STEP TO LOOKUP NEW CODES HAPPENS HERE (some are obvious and Wikipedia can help with the rest)

# now import the missing codes and create full set of LAD17 to LAD19 lookups
lad_codes_missing = read_csv(file.path(data.dir.in, "LAD 2017 to LAD 2019 codes - missing.csv")) %>% 
  select(-LAD17NM)

# merge these extra codes into the lookup table
lad_codes = bind_rows(lad_codes, lad_codes_missing)

# save
write_csv(lad_codes, file.path(data.dir.in, "LAD 2017 to LAD 2019 codes.csv"))
