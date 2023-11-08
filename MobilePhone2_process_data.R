library(readr)
library(data.table)
library(tidyverse)
library(lubridate)
library(psych)

# Load BetterPoints app data for April and November 2019
individual_trips_april <- read_csv(unzip("/data/IndividualTripsApril2019.zip", "IndividualTripsApril2019.csv"))
individual_trips_nov <- read_csv(unzip("/data/IndividualTripsNov2019.zip", "IndividualTripsNov2019.csv"))

# Combine dataframes
df_raw <- bind_rows(individual_trips_april, individual_trips_nov)

# ----------------------- Define variables -----------------------------#

# max wait time (minutes) within single journey
max_wait <- 45
# min duration (minutes) of single trip
min_duration <- 1
# minimum duration (minutes) two trips of same JourneyMode have to be apart to not be merged
min_trip <- 5

# Define peak times
## Monday to Friday (not on public holidays) 06:30-09:30 and 16:00-19:00
## https://tfl.gov.uk/fares/find-fares/tube-and-rail-fares
peak_start_am = "06:30:00"
peak_end_am = "09:30:00"
peak_start_pm = "16:00:00"
peak_end_pm = "19:00:00"

# -------------------------------- Process data -------------------------#
df <- df_raw %>%
  arrange(DFT_ID, StartTime, EndTime) %>%
  mutate(date_start = as.Date(StartTime),
         date_end = as.Date(EndTime))

# group overlapping times
df <- df %>% 
  arrange(DFT_ID, StartTime, EndTime) %>% 
  group_by(DFT_ID, JourneyMode) %>% 
  mutate(idx = c(0, cumsum(as.numeric(lead(StartTime)) > cummax(as.numeric(EndTime + 1)))[-n()])) %>% 
  group_by(DFT_ID, JourneyMode, idx) %>% 
  summarise(StartTime = min(StartTime), 
            EndTime = max(EndTime),
            date_start = as.Date(StartTime),
            date_end = as.Date(EndTime)) %>%
  select(!idx)

# remove trips that span more than 1 day
df <- df %>%
  filter(date_start == date_end)

# generate wait times
df <- df %>%
  arrange(DFT_ID, StartTime, EndTime) %>% 
  group_by(DFT_ID, date_start) %>%
  mutate(next_start = lead(StartTime), 
         prior_end = lag(EndTime), 
         wait_till_next = as.numeric(next_start - EndTime, units = "mins"), 
         wait_since_prior = as.numeric(StartTime - prior_end, units = "mins")
  ) 

# merging trips less than 5 minutes apart that are of same journeymode
df <- df %>%
  arrange(DFT_ID, StartTime, EndTime) %>%
  group_by(DFT_ID, date_start) %>%
  mutate(new_trip = ifelse(JourneyMode != lag(JourneyMode) |
                             wait_since_prior > min_trip, TRUE, FALSE), 
         new_trip = ifelse(is.na(new_trip), TRUE, new_trip), 
         trip_counter = cumsum(new_trip)) %>%
  group_by(DFT_ID, date_start, trip_counter) %>%
  summarise(StartTime = min(StartTime), 
            EndTime = max(EndTime), 
            JourneyMode = first(JourneyMode),
            wait_since_prior = first(wait_since_prior),
            wait_till_next = last(wait_till_next),
            n_trips = n())

# specify a journey as taking a trip within max_wait of the last
df <- df %>%
  group_by(DFT_ID, date_start) %>% 
  mutate(new_journey = ifelse(wait_since_prior > (max_wait), TRUE, FALSE), 
         new_journey = ifelse(is.na(new_journey), TRUE, new_journey), 
         journey_counter = cumsum(new_journey) 
  )

# create variables # to track trip and journey ids
df <- df %>%
  mutate(
    duration = as.numeric(EndTime - StartTime, units = "mins"),
    wday = wday(date_start, label = T),
    hour_start = as.numeric(format(StartTime, "%H")),
    hour_end = as.numeric(format(EndTime, "%H"))) %>%
  group_by(DFT_ID, date_start, journey_counter) %>% 
  mutate(journey_id = cur_group_id()) %>%
  ungroup() %>%
  mutate(trip_id = row_number())

# remove trips that take less than 1 minute
df <- df %>%
  filter(duration > min_duration)

# Create flag for peak travel times
df %<>%
  mutate(date = date(StartTime),
         time_start = format(StartTime, format = "%H:%M:%S"),
         time_end = format(EndTime, format = "%H:%M:%S"),
         day_of_week = weekdays(StartTime),
         peak_days = case_when((!day_of_week %in% c("Saturday", "Sunday") ~ "Peak")),
         public_holidays = case_when(
           # Classify Easter public holidays as weekend
           (date == "2019-04-19" | date == "2019-04-22") ~ "PH"),
         peak_time = case_when(
           (time_start > peak_start_am & time_start < peak_end_am) | 
             (time_start > peak_start_pm & time_start < peak_end_pm) |
             (time_end > peak_start_am & time_end < peak_end_am) | 
             (time_end > peak_start_pm & time_end < peak_end_pm) ~ "Peak"),
         peak_flag = case_when((peak_days == "Peak" & is.na(public_holidays) & peak_time == "Peak") ~ "Peak")) %>%
  select(-time_start, -time_end, -date, -day_of_week, -peak_days, -public_holidays, -peak_time)

df %>% head(100) %>% view()













