##
## Destitution in the UK 2018 by local authority
##
## - from the Joseph Rowntree Foundation's "Destitution in the UK 2018" report: https://www.jrf.org.uk/file/51422/download?token=W92ukE_4&filetype=full-report
## - Data scraped from Appendix G of the technical report: https://researchportal.hw.ac.uk/en/publications/destitution-in-the-uk-2018-technical-report
##
## 10 = highest destitution; 1 = lowest destitution
##
library(tidyverse)
library(readxl)

source("init.r")

destitution = read_excel(file.path(data.dir.in, "Destitution in the UK 2018 - Local authority destitution deciles.xlsx"), sheet = "Destitution")

lads = load_lads()

# merge in destitution indices
lads = lads %>% 
  left_join(destitution, by = c("LAD17NM" = "Local authority"))

# remove NI LADs - they're not in the destitution dataset
lads = lads[substr(lads$LAD17CD, 1, 1) != "N", ]

# save destitution data and LAD codes
lads %>% 
  select(lad17cd = LAD17CD, destitution_all = `All destitute`, destitution_migrant = Migrant, desitution_complex = `Complex needs`, destitution_uk_other = `UK - other`) %>% 
  write_csv(file.path(data.dir.processed, "Destitution - Local Authorities - whole UK.csv"))
