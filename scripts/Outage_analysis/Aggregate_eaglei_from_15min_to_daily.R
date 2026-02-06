# *****************************************************************************************
# Title: EAGLE-I Outage Data Aggregation Functions
# Author: Heng Wan
# Date: 07/01/2025
# Purpose: To provide a set of utility functions for aggregating raw 15-minute EAGLE-I
#          outage data to daily resolution.
# Description: This script contains functions designed to aggregate 15-min EAGLE-I outage
#              data, specifically Customer Interrupted (CI) and Customer Minutes Interrupted 
#              (CMI), to daily resolution. The script provides three different functions 
#              for the CI aggregation, including maximum method (calculates daily CI as the
#              maximum CI value recorded within a single day), sum method (calculates daily
#              CI as the sum of all positive differences in CI values between subsequent 
#              15-minute observations), and moving-average method (smooths the daily CI 
#              values obtained via the sum method by applyiong a 3-day moving average)
# Requirements: Ensure the `lubridate`, `dplyr`, and `zoo` packages are installed.
# ******************************************************************************************

# Load required libraries ----
library(lubridate)
library(dplyr)
library(zoo)

# Constants for EAGLE-I aggregation functions ----
EAGLEI_RAW_INTERVAL_STRING <- "15 min" # Interval of raw EAGLE-I data
EAGLEI_INTERVAL_DURATION_MINUTES <- 15 # Duration of a single EAGLE-I interval in minutes
MISSING_CI_FILL_VALUE <- 0            # Value to fill for missing Customer Interruption
LAG_OBSERVATIONS_COUNT <- 1           # Number of observations to lag by
POSITIVE_DIFFERENCE_THRESHOLD <- 0    # Threshold for considering a CI difference as "positive"
CI_MOVING_AVERAGE_WINDOW_DAYS <- 3    # Window size for daily CI moving average


# Utility functions for EAGLE-I outage data aggregation ----

# Function to aggregate CI by maximum method
aggregate_by_max_count <- function(df) {
  aggregated_df <- df %>%
    mutate(Date = as.Date(run_start_time)) %>%
    group_by(fips_code, Date) %>%
    summarise(max_customer = ifelse(all(is.na(sum)), NA, max(sum, na.rm = TRUE)), .groups = 'drop')
  return(aggregated_df)
}


# Function to aggregate CI by sum method and moving-average method
aggregate_by_sum_positive_diff <- function(df) {
  
  fips_lookup <- df %>%
    select(fips_code, county, state) %>%
    distinct() %>%
    group_by(fips_code) %>%
    summarise(county = first(county), state = first(state), .groups = 'drop') %>%
    filter(!is.na(fips_code))
  
  # Create a complete 15-min time sequence for each fips_code (filling 0 CI values)
  time_range <- seq(
    from = min(df$run_start_time, na.rm = TRUE),
    to = max(df$run_start_time, na.rm = TRUE),
    by = EAGLEI_RAW_INTERVAL_STRING
  )
  
  # Create a data frame with all fips_code and time combinations
  complete_grid <- expand.grid(
    fips_code = unique(df$fips_code),
    run_start_time = time_range
  ) 
  
  # Merge with original data, filling missing CI values with 0
  df_complete <- complete_grid %>%
    left_join(df %>% select(fips_code, run_start_time, sum), 
              by = c("fips_code", "run_start_time")) %>%
    mutate(sum = replace(sum, is.na(sum), MISSING_CI_FILL_VALUE)) %>%
    # Join with fips_lookup to fill county and state
    left_join(fips_lookup, by = "fips_code") %>%
    arrange(fips_code, run_start_time)
  
  # Calculate CI differences
  df_complete <- df_complete %>%
    group_by(fips_code) %>%
    mutate(prev_ci = lag(sum, n= LAG_OBSERVATIONS_COUNT, default = MISSING_CI_FILL_VALUE),
           ci_diff = sum - prev_ci) %>%
    ungroup()
  
  # Aggregate by summing positive CI differences within each day
  df_complete <- df_complete %>%
    mutate(Date = as.Date(run_start_time)) %>%
    group_by(fips_code, Date) %>%
    summarise(
      county = first(county),
      state = first(state),
      daily_ci = sum(pmax(POSITIVE_DIFFERENCE_THRESHOLD, ci_diff), na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Calculate 3-day moving window average for daily_ci
  df_complete <- df_complete %>%
    arrange(fips_code, Date) %>%
    group_by(fips_code) %>%
    mutate(daily_ci_3day_avg = rollmean(daily_ci, k = CI_MOVING_AVERAGE_WINDOW_DAYS, fill = NA, align = "center")) %>%
    ungroup()
  
  return(df_complete)
}

# Function to calculate daily CMI
calculate_customer_minutes <- function(outage_data) {
  
  # Convert Run_start_time to POSIXct and extract date
  outage_data <- outage_data %>%
    mutate(
      Date = as.Date(run_start_time))
  
  # Calculate customer minutes (Sum * 15 minutes per interval)
  # Group by date, Fips_code, County, State to get total customer minutes per day
  daily_customer_minutes <- outage_data %>%
    group_by(Date, fips_code) %>%
    summarise(
      total_customer_minutes = sum(sum * EAGLEI_INTERVAL_DURATION_MINUTES, na.rm = TRUE),
      .groups = "drop"
    ) 
  return(daily_customer_minutes)
}
