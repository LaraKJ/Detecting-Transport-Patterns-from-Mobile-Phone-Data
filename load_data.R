#install.packages('R.utils')

library(readr)
library(readxl)
library(data.table)
library(arrow)
library(tidyverse)
library(psych)

# Census Data
excel_sheets("/data/census2021firstresultsenglandwales.xlsx")
census <- read_excel("/data/census2021firstresultsenglandwales.xlsx", sheet = "Contents")

# National Travel Survey (NTS)

# display all files in a zip folder
unzip("/data/NTS_Special_2019.zip", list = TRUE)

nts_attitudes <- read_csv(unzip("/data/NTS_Special_2019.zip", "data/attitudes_special_2019.csv"))
nts_household <- read_csv(unzip("/data/NTS_Special_2019.zip", "data/household_special_2019.csv"))
nts_individual <- read_csv(unzip("/data/NTS_Special_2019.zip", "data/individual_special_2019.csv"))
nts_stage <- read_csv(unzip("/data/NTS_Special_2019.zip", "data/stage_special_2019.csv"))
nts_trip <- read_csv(unzip("/data/NTS_Special_2019.zip", "data/trip_special_2019.csv"))

glimpse(nts_attitudes)
glimpse(nts_household)
glimpse(nts_individual)
glimpse(nts_stage)
glimpse(nts_trip)


# National Travel Attitudes Survey (subset of the NTS)
unzip("/data/NTAS Wave 1 2019.zip", list = TRUE)
ntas <- read_delim(unzip("/data/NTAS Wave 1 2019.zip", "UKDA-8859-tab/tab/ntas_wave_1.tab"))

dim(ntas)
glimpse(ntas)


# Csv.gz file
df <- fread("/data/Telecoms people counts by age & gender/People_counts_20190306.csv.gz")
view(head(df))

#df <- fread("/data/Telecoms people counts by age & gender")
#dt = fread("data.csv.gz")

# Connectivity data
lsao_pairs <- read_parquet("/data/Connectivity_lsoa_od_pairs_pt_new.parquet")
connectivity_scores <- read_parquet("/data/connectivity_scores_EW.parquet")
view(head(connectivity_scores))

# Individual
unzip("/data/IndividualTripsApril2019.zip", list = TRUE)

individual_trips_april <- read_csv(unzip("/data/IndividualTripsApril2019.zip", "IndividualTripsApril2019.csv"))

head(individual_trips_april)
n_distinct(individual_trips_april$PostcodeArea)
unique(individual_trips_april$JourneyMode)
head(individual_trips_april$DFT_ID)
individual_trips_april %>% filter(DFT_ID == "{15973ADB-FBC6-4C18-B289-B33601D1117B}") %>%
  view()
