## Part 2.0 - Processing Log of Cyclistic Data for Data Analysis
library(tidyverse)
library(geosphere)
library(stringr)
library(knitr)

# ------------------------------------------------------------------
#  a) Load the raw data  
# ------------------------------------------------------------------
working_data <- read_csv("C:/Users/vishv/Downloads/Cyclistic Case Study/Data for Cyclistic/Trial 2 - In Progress/Cyclistic Data - Combined Using R/Combined_Data.csv")

# ------------------------------------------------------------------
#  b) Initialise an empty log tibble
# ------------------------------------------------------------------

clean_log <- tibble(
  step          = character(),
  category      = character(),   # e.g. "transform", "filter", "impute"
  description   = character(),
  rows_before   = integer(),
  rows_after    = integer(),
  rows_removed  = integer()
)

# ------------------------------------------------------------------
#  c) Convenience function to append a row to the log
# ------------------------------------------------------------------

log_step <- function(log_tbl,
                     step, category, description,
                     before, after) {
  log_tbl %>%
    add_row(step        = step,
            category    = category,
            description = description,
            rows_before = before,
            rows_after  = after,
            rows_removed = before - after)
}

# ------------------------------------------------------------------
#  d) drop rows with any NA 
# ------------------------------------------------------------------

rows_before <- nrow(working_data) 
  #counts the number of rows

working_data <- working_data %>%
  filter( if_all(everything(), ~ !is.na(.)) )

rows_after <- nrow(working_data)

clean_log <- log_step(
  clean_log,
  step        = "Remove rows with any NA",
  category    = "filter",
  description = "Dropped rows where one or more columns were blank/NA",
  before      = rows_before,
  after       = rows_after
 )

# ------------------------------------------------------------------
#  e) drop duplicate ride_id 
# ------------------------------------------------------------------

rows_before <- nrow(working_data)
  #counts the number of rows

working_data <- working_data %>%
  distinct(ride_id, .keep_all = TRUE)

rows_after <- nrow(working_data)
  #counts the number of rows after the cleaning

clean_log <- log_step(
  clean_log,
  step        = "Remove duplicate ride_id rows",
  category    = "filter",
  description = "Kept first occurrence of each ride_id using distinct()",
  before      = rows_before,
  after       = rows_after
)

print(clean_log) # to check if the functions and clean log are working properly

anyNA(working_data) # to check if there are any blanks - FALSE = 100% complete

# ------------------------------------------------------------------
#  f) filter impossible ride durations
# ------------------------------------------------------------------

rows_before <- nrow(working_data)
  #counts the number of rows

working_data <- working_data %>%
  mutate(
    ride_length = as.numeric(difftime(ended_at, started_at, units = "mins"))
  ) %>%
  filter(ride_length > 0, ride_length <= 1440)

rows_after <- nrow(working_data)
  #counts the number of rows after the cleaning

clean_log <- log_step(
  clean_log,
  step        = "Filter invalid ride_length",
  category    = "filter",
  description = "Kept only rides between 0 and 1440 mins",
  before      = rows_before,
  after       = rows_after
)

# ------------------------------------------------------------------
#  g) Drop rides shorter than 1 minute
# ------------------------------------------------------------------

rows_before <- nrow(working_data)
  #counts the number of rows

working_data <- working_data %>%
  filter(ride_length >= 1)

rows_after <- nrow(working_data)
  #counts the number of rows after the cleaning

clean_log <- log_step(
  clean_log,
  step        = "Remove short rides (<1 min)",
  category    = "filter",
  description = "Dropped rides shorter than 1 minute",
  before      = rows_before,
  after       = rows_after
)

# ------------------------------------------------------------------
#  h) remove rides with avg speed > 60 km/h
# ------------------------------------------------------------------

rows_before <- nrow(working_data)
  #counts the number of rows

working_data <- working_data %>%
  mutate(
    distance_km = distHaversine(
      cbind(start_lng, start_lat),
      cbind(end_lng, end_lat)
    ) / 1000,
    avg_speed_kmh = distance_km / (ride_length / 60)
  ) %>%
  filter(avg_speed_kmh <= 60)

rows_after <- nrow(working_data)
  #counts the number of rows after the cleaning

clean_log <- log_step(
  clean_log,
  step        = "Filter unrealistic speeds",
  category    = "filter",
  description = "Removed rides with avg speed > 60 km/h",
  before      = rows_before,
  after       = rows_after
)

print(clean_log) # check the cleaning log

# ------------------------------------------------------------------
#  i) drop short loop rides
# ------------------------------------------------------------------

rows_before <- nrow(working_data)

working_data <- working_data %>%
  filter(!(start_station_name == end_station_name & ride_length < 2))

rows_after <- nrow(working_data)

clean_log <- log_step(
  clean_log,
  step        = "Remove short same-station rides",
  category    = "filter",
  description = "Dropped rides with same start/end station and ride_length < 2 mins",
  before      = rows_before,
  after       = rows_after
)

print(clean_log) # check the cleaning log

# ------------------------------------------------------------------
#  j) remove rows with missing/invalid rideable_type or member_casual
# ------------------------------------------------------------------

rows_before <- nrow(working_data)

valid_bikes   <- c("classic_bike", "electric_bike", "docked_bike")
valid_members <- c("member", "casual")

working_data <- working_data %>%
  filter(
    rideable_type %in% valid_bikes,
    member_casual %in% valid_members
  )

rows_after <- nrow(working_data)

clean_log <- log_step(
  clean_log,
  step        = "Filter invalid rider / bike types",
  category    = "filter",
  description = "Kept only rideable_type in classic/electric/docked & member_casual in member/casual",
  before      = rows_before,
  after       = rows_after
)

# ------------------------------------------------------------------
#  k) remove blank‑string station IDs
# ------------------------------------------------------------------

rows_before <- nrow(working_data)

working_data <- working_data %>% 
  filter(
    !is.na(start_station_id), !is.na(end_station_id),
    start_station_id != "",    start_station_id != " ",
    end_station_id   != "",    end_station_id   != " "
  )

rows_after <- nrow(working_data)

clean_log <- log_step(
  clean_log,
  step        = "Remove blank‑string station IDs",
  category    = "filter",
  description = "Dropped rows where start/end station_id was '' or single space",
  before      = rows_before,
  after       = rows_after
)

# ------------------------------------------------------------------
#  l) create calendar features
# ------------------------------------------------------------------

rows_before <- nrow(working_data) 

working_data <- working_data %>%
  mutate(
    ride_day      = as.Date(started_at),
    day_of_week   = lubridate::wday(started_at, label = TRUE, week_start = 1), # Mon = 1
    ride_month    = lubridate::month(started_at, label = TRUE, abbr = TRUE),
    ride_hour     = lubridate::hour(started_at)
  )

rows_after <- nrow(working_data)

clean_log <- log_step(
  clean_log,
  step        = "Create calendar features",
  category    = "transform",
  description = "Added ride_day, day_of_week, ride_month, ride_hour for Tableau slices",
  before      = rows_before,
  after       = rows_after
)

# ------------------------------------------------------------------
#  m) standardise station names (trim & Title‑case)
# ------------------------------------------------------------------

rows_before <- nrow(working_data)

working_data <- working_data %>%
  mutate(
    start_station_name = str_to_title(str_squish(start_station_name)),
    end_station_name   = str_to_title(str_squish(end_station_name))
  )

rows_after <- nrow(working_data)

clean_log <- log_step(
  clean_log,
  step        = "Standardise station names",
  category    = "transform",
  description = "Trimmed spaces + Title‑cased start/end station names",
  before      = rows_before,
  after       = rows_after
)

# ------------------------------------------------------------------
#  n) time‑of‑day slots
# ------------------------------------------------------------------

rows_before <- nrow(working_data)

working_data <- working_data %>% 
  mutate(
    time_bucket = case_when(
      lubridate::hour(started_at) < 6  ~ "Early AM",   # 00‑05
      lubridate::hour(started_at) < 12 ~ "AM",         # 06‑11
      lubridate::hour(started_at) < 18 ~ "PM",         # 12‑17
      TRUE                             ~ "Late PM"     # 18‑23
    )
  )

rows_after <- nrow(working_data)

clean_log <- log_step(
  clean_log,
  step        = "Add time‑of‑day buckets",
  category    = "transform",
  description = "Created time_bucket (Early AM / AM / PM / Late PM) from started_at",
  before      = rows_before,
  after       = rows_after
)

# ------------------------------------------------------------------
#  o) weekend flag
# ------------------------------------------------------------------

rows_before <- nrow(working_data)

working_data <- working_data %>% 
  mutate(
    is_weekend = lubridate::wday(started_at, week_start = 1) %in% c(6, 7)  
    # week_start = 1 → Mon=1 … Sun=7
  )

rows_after <- nrow(working_data)

clean_log <- log_step(
  clean_log,
  step        = "Add weekend flag",
  category    = "transform",
  description = "Created is_weekend (TRUE for Sat/Sun)",
  before      = rows_before,
  after       = rows_after
)

# ------------------------------------------------------------------
#  p) ride‑length categories
# ------------------------------------------------------------------

rows_before <- nrow(working_data)

working_data <- working_data %>% 
  mutate(
    ride_length_cat = case_when(
      ride_length < 10               ~ "Short (<10m)",
      ride_length < 30               ~ "Medium (10‑30m)",
      ride_length >= 30 & ride_length < 60  ~ "Long (30‑60m)",
      ride_length >= 60              ~ "Very Long (60m+)"
    )
  )

rows_after <- nrow(working_data)

clean_log <- log_step(
  clean_log,
  step        = "Bucketise ride_length",
  category    = "transform",
  description = "Added ride_length_cat with Short/Medium/Long/Very Long bins",
  before      = rows_before,
  after       = rows_after
)

# ------------------------------------------------------------------
#  q) rounded lat‑lon (3 decimal places)
# ------------------------------------------------------------------

rows_before <- nrow(working_data)

working_data <- working_data %>% 
  mutate(
    start_lat3 = round(start_lat, 3),
    start_lng3 = round(start_lng, 3),
    end_lat3   = round(end_lat,   3),
    end_lng3   = round(end_lng,   3)
  )

rows_after <- nrow(working_data)

clean_log <- log_step(
  clean_log,
  step        = "Round lat‑lon to 3 dp",
  category    = "transform",
  description = "Created start_*3 / end_*3 for spatial clustering",
  before      = rows_before,
  after       = rows_after
)

# ------------------------------------------------------------------
#  r) season field
# ------------------------------------------------------------------

rows_before <- nrow(working_data)

working_data <- working_data %>% 
  mutate(
    season = case_when(
      lubridate::month(started_at) %in% 3:5  ~ "Spring",
      lubridate::month(started_at) %in% 6:8  ~ "Summer",
      lubridate::month(started_at) %in% 9:11 ~ "Fall",
      TRUE                                   ~ "Winter"        # Dec‑Feb
    )
  )

rows_after <- nrow(working_data)

clean_log <- log_step(
  clean_log,
  step        = "Add season label",
  category    = "transform",
  description = "Tagged each ride as Spring/Summer/Fall/Winter by start month",
  before      = rows_before,
  after       = rows_after
)

print(clean_log)
glimpse(working_data)

# ------------------------------------------------------------------
#  s) saving the work
# ------------------------------------------------------------------


output_dir <- "C:/Users/vishv/Downloads/Cyclistic Case Study/Data for Cyclistic/Trial 2 - In Progress/Cyclistic Data - Combined Using R"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Save final data

readr::write_csv(working_data, file.path(output_dir, "Cyclistic_Cleaned_Data.csv"))

# Save cleaning log
readr::write_csv(clean_log, file.path(output_dir, "Cyclistic_Cleaning_Log.csv"))

# ------------------------------------------------------------------
#  t) testing for data viz.
# ------------------------------------------------------------------

stopifnot(nrow(working_data) > 0)
stopifnot(!anyNA(working_data))

# ------------------------------------------------------------------
#  u) Creating RMarkdown.
# ------------------------------------------------------------------

knitr::spin("Trial_Case_Study_Cyclistic - Part 3.R")

## With these, I am done cleaning and manipulating the data.