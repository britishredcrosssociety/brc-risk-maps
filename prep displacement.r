##
## Prep data on people seeking asylum who receive Section 95 support
##
## Using Home Office stats: https://www.gov.uk/government/statistical-data-sets/asylum-and-resettlement-datasets
## - December 2019
##
library(tidyverse)
library(readxl)
library(lubridate)
library(httr)

source("init.r")

lads = load_lads(2019)

##
## load and process displacement data
##
# download the latest stats on Section 95 support by local authority
# note: you'll need to manually update this URL whenever the Home Office releases new statistics
GET("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/868203/section-95-support-local-authority-datasets-dec-2019.xlsx",
    write_disk(tf <- tempfile(fileext = ".xlsx")))

dispersal_raw = read_excel(tf, sheet = "Data - Asy_D11")  # check the sheet name is still valid if you're updating the URL above
unlink(tf)

# convert date column
dispersal = dispersal_raw %>% 
  mutate(Date = as.Date(`Date (as at…)`, format = "%d %b %Y"))
  
# get rid of rows with totals and keep only LADs with refugees  
dispersal = dispersal %>%
  filter(Date == max(dispersal$Date)) %>% 
  select(lad19cd = `LAD Code`, Support = `Support sub-type`, People) %>% 
  pivot_wider(names_from = Support, values_from = People, values_fn = list(People = sum), values_fill = list(People = 0)) %>% 
  
  mutate(Sec95 = `Dispersed Accommodation` + `Subsistence Only`)

# merge refugee stats into the LAD boundaries
lads = lads %>% 
  left_join(dispersal, by = c("LAD19CD" = "lad19cd")) %>% 
  mutate(Sec95 = ifelse(is.na(Sec95), 0, Sec95))

lads %>% 
  select(LAD19CD, `Dispersed Accommodation`, `Subsistence Only`, Sec95) %>% 
  write_csv(file.path(data.dir.processed, "UK - LAD - displacement.csv"))


##
## DEBUG - sometimes the official ONS LAD codes don't match up with what the Home Office provides... check this
##
# any LAD codes in `dispersal` that aren't in `lads`?
# dplyr::setdiff(dispersal$lad19cd, lads$LAD19CD)  # using Dec 2019 asylum stats, it seems not.
