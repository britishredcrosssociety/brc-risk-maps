##
## Flood risks - how many people are at high risk (>1% per year) of flooding?
##
## Note: we currently don't have access to Scottish flood data
##
##
library(tidyverse)
library(readxl)
library(httr)
library(rgdal)
library(spdplyr)
library(rmapshaper)

source("init.r")

##
## download Output Area population estimates for England/Wales
## source: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datalist?sortBy=release_date&query=output%20area&filter=datasets&fromDateDay=&fromDateMonth=&fromDateYear=&toDateDay=&toDateMonth=&toDateYear=&size=10
##
# ONS publishes output area population estimates in separate files for each region
pop_urls = c(
  # East Midlands
  "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fcensusoutputareaestimatesintheeastmidlandsregionofengland%2fmid2018sape21dt10f/sape21dt10fmid2018eastmidlands.zip",
  
  # North West
  "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fcensusoutputareaestimatesinthenorthwestregionofengland%2fmid2018sape21dt10b/sape21dt10bmid2018northwest.zip",
  
  # South East
  "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fcensusoutputareaestimatesinthesoutheastregionofengland%2fmid2018sape21dt10i/sape21dt10imid2018southeast.zip",
  
  # East
  "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fcensusoutputareaestimatesintheeastregionofengland%2fmid2018sape21dt10h/sape21dt10hmid2018east.zip",
  
  # West Midlands
  "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fcensusoutputareaestimatesinthewestmidlandsregionofengland%2fmid2018sape21dt10e/sape21dt10emid2018westmidlands.zip",
  
  # Wales
  "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fcensusoutputareaestimatesinwales%2fmid2018sape21dt10j/sape21dt10jmid2018wales.zip",
  
  # North East
  "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fcensusoutputareaestimatesinthenortheastregionofengland%2fmid2018sape21dt10d/sape21dt10dmid2018northeast.zip",
  
  # South West
  "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fcensusoutputareaestimatesinthesouthwestregionofengland%2fmid2018sape21dt10g/sape21dt10gmid2018southwest.zip",
  
  # Yorkshire and The Humber
  "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fcensusoutputareaestimatesintheyorkshireandthehumberregionofengland%2fmid2018sape21dt10c/sape21dt10cmid2018yorkshireandthehumber.zip",
  
  # London
  "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fcensusoutputareaestimatesinthelondonregionofengland%2fmid2018sape21dt10a/sape21dt10amid2018london.zip"
)

# download and upzip population estimates for each region
for (url in pop_urls) {
  GET(url, write_disk(tf <- tempfile(fileext = ".zip")))
  unzip(tf, exdir = file.path(data.dir.in, "population/output areas"))
  unlink(tf)
}

##
## load population data for England/Wales
##
pop_files = list.files(file.path(data.dir.in, "population/output areas"), full.names = T)

for (file in pop_files) {
  
  # if the merged dataset doesn't exist, create it
  if (!exists("pop")){
    pop = read_excel(file, sheet = "Mid-2018 Persons", skip = 4) %>% 
      select(OA11CD, LSOA11CD, `All Ages`)
    
  } else {
    # if the merged dataset does exist, append to it
    temp_dataset = read_excel(file, sheet = "Mid-2018 Persons", skip = 4) %>% 
      select(OA11CD, LSOA11CD, `All Ages`)
    
    pop = rbind(pop, temp_dataset)
    rm(temp_dataset)
  }
}

##
## load population data for Northern Ireland's Small Areas
## source: https://www.nisra.gov.uk/publications/2018-mid-year-population-estimates-small-areas
##
GET("https://www.nisra.gov.uk/sites/nisra.gov.uk/files/publications/SAPE18_SA_Totals.xlsx",
    write_disk(tf <- tempfile(fileext = ".xlsx")))

pop_ni = read_excel(tf, sheet = "Flat")

pop_ni = pop_ni %>% 
  filter(Year == max(pop_ni$Year)) %>% 
  select(SA2011 = Area_Code, `All Ages` = MYE)

unlink(tf)

##
## load output area centroids for England/Wales
##
# Output Areas (December 2011) Population Weighted Centroids
# source: http://geoportal.statistics.gov.uk/datasets/output-areas-december-2011-population-weighted-centroids
oa = readOGR("https://opendata.arcgis.com/datasets/638c93fa316d448bb59669a139042165_0.geojson")


#########################################################################################################################
## Flood data
##
## There are two options for loading flood data: use pre-processed data files attached to this repo (first code block) or download directly from sources (second code block)
## For speed, the pre-processed data are used by default; for sanity checking, uncomment and run the second code block
##

##
## load flood maps from manually simplified/edited data
##
# unzip the pre-packaged data
unzip(file.path(data.dir.in, "floods", "england.zip"), exdir = file.path(data.dir.in, "floods"))
unzip(file.path(data.dir.in, "floods", "wales.zip"), exdir = file.path(data.dir.in, "floods"))
unzip(file.path(data.dir.in, "floods", "ni.zip"), exdir = file.path(data.dir.in, "floods"))  # this file was created in QGIS and can't be downloaded - so this code must be run

floods_eng = readOGR(dsn = file.path(data.dir.in, "floods"),
                     layer = "England_Flood_Map_Zone_3_smaller",
                     verbose = F)

floods_wal = readOGR(dsn = file.path(data.dir.in, "floods"),
                     layer = "NRW_FLOODMAP_FLOOD_ZONE_3_Simplified",
                     verbose = F)

# NI flood map file was created in QGIS and can't be downloaded - so this code must be run
#
# Steps to reproduce this file (with huge thanks to Nick McWilliam from MapACtion (@nickmcw)):
# The data are provided as ArcGIS MapServer layers, which means that features are generally not available for local processing, styling etc. If they were FeatureServer layers, we'd be fine. The method shown here is more of a work-around than a good solution. It might be possible to access the data directly (and dynamically) using these tools https://github.com/Bolton-and-Menk-GIS/restapi - but I didn't want to go down this route just now! So, in QGIS......
# 
# 1. Connect to service
# Browser panel > ArcGISMapServer > New Connection > enter a name and this URL: https://mapping.dardni.gov.uk/arcgisra/rest/services/FloodMapsNI/SFRA2/MapServer
# In this case, two layers are available, APSFR and TAPSFR
# 
# 2. Add layer
# Add the relevant layer to you map. You won't have any control over the styling. Zoom to the extent you need (in this case, full extent) and check that no other layers are visible
# 
# 3. Export as georeferenced raster
# Project > Export > Export Map to Image >
# Increase resolution to 300dpi
# Un-check annotations and decorations
# Ensure that world file is checked
# Save > Save as > PNG (don't use a 'lossy' format)
# 
# 4. Add georeferenced raster
# Add the new layer to your map. Choose 'Irish Grid' if asked. Check it overlays with the MapServer layer.
# Because of the symbology from the server (with a different coloured outline), the styling needs to be changed:
#   Layer Styling > Singleband Psuedocolour
# Try bands 1/2/3 to find the closest to what you want. In the case of APSFR, I used band 3.
# NOTE: The symbology of TAPSFR is different, requiring (irritatingly) a re-classification before raster-vector conversion.
# 
# 5. Raster to vector conversion
# Raster > Conversion > Polygonise
# Choose the band from the previous step, and save
# 
# 6. Remove extra polygons
# In edit mode, remove any enclosing polygons and any 'islands' that shouldn't be filled
# It may be necessary to perform a dissolve, if there are edge artefacts
#
oa_ni = readOGR(dsn = file.path(data.dir.in, "floods"),
                layer = "NI small areas in flood risk zones",
                verbose = F)

##
## load/process flood maps directly from sources
##
# England: Flood Map for Planning (Rivers and Sea) - Flood Zone 3
# source: https://data.gov.uk/dataset/bed63fc1-dd26-4685-b143-2941088923b3/flood-map-for-planning-rivers-and-sea-flood-zone-3
# GET("https://environment.data.gov.uk/UserDownloads/interactive/37871f0022f646af91bb1c3a0275280d242568/EA_FloodMapForPlanningRiversAndSeaFloodZone3_SHP_Full.zip",
#     write_disk(tf <- tempfile(fileext = ".zip")))
# unzip(tf, exdir = file.path(data.dir.in, "floods/england"))
# unlink(tf)
# 
# floods_eng = readOGR(dsn = file.path(data.dir.in, "floods/england/data"),
#                      layer = "Flood_Map_for_Planning_Rivers_and_Sea_Flood_Zone_3",
#                      verbose = F)
# 
# # Wales: Flood Map: Flood Zone 3
# # source: http://lle.gov.wales/catalogue/item/Flood3/?lang=en#downloads-content
# floods_wal = readOGR("http://lle.gov.wales/catalogue/item/FloodMapFloodZone3.json")
# 
# # simplify the maps
# floods_eng = ms_simplify(floods_eng, keep = 0.02, keep_shapes = TRUE)
# floods_wal = ms_simplify(floods_wal, keep = 0.02, keep_shapes = TRUE)

##
## transform to WGS84 - web Mercator
##
floods_eng = spTransform(floods_eng, map_proj)
floods_wal = spTransform(floods_wal, map_proj)
oa_ni = spTransform(oa_ni, map_proj)

##
## keep only OAs that intersect with the flood risk areas
## - England
##
flood_oa_eng = oa %over% floods_eng

# merge the flood data into OAs...
oa_eng = oa
oa_eng@data = bind_cols(oa_eng@data, flood_oa_eng)

oa_eng = oa_eng %>% 
  # keep only OAs that are in flood risk zones
  filter(layer == "Flood Zone 3") %>% 
  
  # merge population counts into Output Areas
  left_join(pop, by = "OA11CD")

##
## Wales
##
flood_oa_wal = oa %over% floods_wal

# merge the flood data into OAs...
oa_wal = oa
oa_wal = bind_cols(oa_wal@data, flood_oa_wal)

oa_wal = oa_wal %>% 
  
  # keep only OAs that are in flood risk zones
  filter(!is.na(TYPE)) %>% 
  
  # merge population counts into Output Areas
  left_join(pop, by = "OA11CD")

##
## NI
##
oa_ni = oa_ni %>% 
  left_join(pop_ni, by = "SA2011") %>% 
  rename(OA11CD = SA2011, LSOA11CD = SOA2011)

##
## combine output areas into single dataframe
##
oa_data = bind_rows(oa_eng@data, oa_wal, oa_ni@data) %>%
  select(OA11CD, LSOA11CD, n = `All Ages`)

# write_csv(oa_data, file.path(data.dir.processed, "floods/Output Areas in flood risk zones.csv"))  # (commented out because we won't need this data to produce combined risks)

##
## count population at risk by LSOA
##
floods_lsoa = oa_data %>% 
  group_by(LSOA11CD) %>% 
  summarise(n = sum(n))

# save
write_csv(floods_lsoa, file.path(data.dir.processed, "LSOAs in flood risk zones.csv"))

##
## count population at risk by MSOA and LAD
##
# Output Area to Lower Layer Super Output Area to Middle Layer Super Output Area to Local Authority District (December 2011) Lookup in England and Wales
# source: https://geoportal.statistics.gov.uk/datasets/output-area-to-lower-layer-super-output-area-to-middle-layer-super-output-area-to-local-authority-district-december-2011-lookup-in-england-and-wales
oa_lookup = read_csv("https://opendata.arcgis.com/datasets/6ecda95a83304543bc8feedbd1a58303_0.csv")
# oa_lookup = read_csv(file.path(data.dir, "Lookups, names and codes/OA to LSOA to MSOA to LAD.csv"))

oa_data = oa_data %>% 
  left_join(oa_lookup %>% select(OA11CD, MSOA11CD), by = "OA11CD")

floods_msoa = oa_data %>% 
  filter(!startsWith(OA11CD, "N")) %>%   # MSOAs don't exist in NI, so filter them out
  group_by(MSOA11CD) %>% 
  summarise(n = sum(n))

# save
write_csv(floods_msoa, file.path(data.dir.processed, "MSOAs in flood risk zones.csv"))
