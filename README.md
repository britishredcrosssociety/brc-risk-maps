# Produce British Red Cross strategic risk maps for the UK
For disasters/emergencies, health inequalities, and displacement/migration. The [guide to risk maps](guide-to-risk-maps.pptx) contains more information about data definitions and sources, and how risks are aggregated into different geographies.

After cloning/forking/downloading this repo, install the following packages:

```
install.packages(
  # for loading/wrangling data
  "tidyverse",
  "lubridate",
  "readxl",
  "httr",
  "xml2",
  "xlsx",

  # for spatial data
  "rgdal",
  "sp",
  "sf",
  "spdplyr",
  "rmapshaper",

  # misc.
  "devtools"
)
```

You'll also need to install my [BRC data science library](https://github.com/matthewgthomas/brclib):

```
devtools::install_github("matthewgthomas/brclib")
```

## Download and process data
run `prep all data.r` to download and process data for:
- destitution
- digital exclusion
- displacement (asylum seekers receiving Section 95 support)
- dwelling fires
- flood risks
- healthy life expectancy (for Lower Layer Super Output Areas and Clinical Commissioning Groups)
- people living alone

The outputs from these scripts will be stored in `./data/processed`

This repo also uses data on [loneliness risks](https://github.com/matthewgthomas/loneliness) and [Index of Multiple Deprivation](https://github.com/matthewgthomas/IMD), which will be access directly from those external repos.

## Produce risk maps
Next, run `prep combined risks.r` to generate files. These will be stored in `./output`. You'll end up with multiple .csv files for each country, with risks aggregated by one or more of:

- Lower Layer Super Output Area
- Middle Layer Super Output Area
- Local Authority District
- Fire & Rescue Authority

There will also be a spreadsheet - `Multiple risks in Local Authorities.xlsx` - containing summaries of the risks for each Local Authority in each UK nation.
