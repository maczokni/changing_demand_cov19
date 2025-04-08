# forecast proportion of calls attended in total and by crime type

# Load packages
library(fable)
library(lubridate)
library(tsibble)
library(tidyverse)

# Read and clean data

calls <- read_csv("PATH TO FILE") %>%  janitor::clean_names()


# create attended variable

calls$attended <- case_when(calls$attended_flag == 1 ~ "Yes", 
                            calls$attended_flag == 0 ~ "No", 
                            TRUE ~ NA_character_)


# ATTENDED MODELS

# count the proportion which were attened each week in total first

count_attended_calls <- calls %>% 
  mutate(incident_week = yearweek(incident_date_time)) %>% 
  group_by(incident_week) %>% 
  summarise(num_calls = n(), 
            num_attended = sum(attended_flag, na.rm=TRUE), 
            percent_attended = round(num_attended/num_calls*100,1))

#  convert to a tsibble object

count_attended_calls <- count_attended_calls %>% 
  slice(2:(n() - 1)) %>% # remove first and last row
  select(-num_calls, -num_attended) %>%  # remove unnecessary cols
  as_tsibble(index = incident_week) %>% 
  fill_gaps(call_count = 0) %>% 
  # Add dummy variables
  mutate(
    # Dummy for IMPORTANT DATE
    VAR_1 = incident_week > yearweek(ymd("DATE HERE")),
    # Dummy for IMPORTANT DATE
    VAR_2 = incident_week > yearweek(ymd("DATE HERE")), 
    # Dummy for bank holiday 
    bank_holiday = incident_week %in% yearweek(as_date(timeDate::holidayLONDON(year = 2015:2020))))


model_attended_calls <- attended_calls %>% 
  filter(incident_week < yearweek(ymd("2020-01-31"))) %>% 
  model(
    arima = ARIMA(percent_attended ~ trend() + season() + VAR_1 + VAR_2 + bank_holiday)
  )


# Create data for forecasting
# This step is necessary because the model contains dummy variables, so we have
# to have some way of specifying the value of each dummy for each time point.
# The dummy variables are all `TRUE` for all future periods, so this is easy to 
# do. The variable names must match those in the original data and therefore the
# names of the model terms, excluding `trend()` and `season()`, which are
# handled automatically.
fdata_attended_calls <- expand_grid(
  incident_week = yearweek(seq.Date(
    from = ymd("2020-01-31"), 
    to = ymd("2020-12-31"),
    by = "week"
  )),
  VAR_1 = TRUE,
  VAR_2 = TRUE
) %>% 
  as_tsibble(index = incident_week) 

fdata_attended_calls$bank_holiday <- fdata_attended_calls$incident_week %in% yearweek(as_date(timeDate::holidayLONDON(year = 2020)))


# Create forecasts and extract confidence intervals
forecast_attended_calls <- model_attended_calls %>% 
  forecast(new_data = fdata_attended_calls) %>% 
  hilo(level = 95) %>% 
  janitor::clean_names() %>% 
  unpack_hilo(cols = x95_percent) 


# Join actual counts to the forecast object and check if actual calls were 
# outside the forecast range
final_attended_calls <- count_attended_calls %>% 
  filter(
    incident_week > yearweek(ymd("2019-12-31"))
  ) %>% 
  select(incident_week, actual_attended = percent_attended) %>% 
  full_join(
    forecast_attended_calls, 
    by = c("incident_week")
  ) %>% 
  select(
    incident_week, 
    actual_attended, 
    forecast_mean = mean, 
    forecast_lower = x95_percent_lower, 
    forecast_upper = x95_percent_upper
  ) %>% 
  mutate(
    # When plotting, it is more convenient to store the week as a date rather 
    # than a `yearweek` column
    incident_week = as_date(incident_week),
    # Calls are significantly different from the forecast if the call count is
    # less than the lower 95% CI or higher than the upper 95% CI
    sig = actual_attended < forecast_lower | actual_attended > forecast_upper
  ) %>% 
  replace_na(list(sig = FALSE))

# Save result for use elsewhere (e.g. in an Rmarkdown document)
write_rds(final_attended_calls, here::here("output/all_attended_forecasts.Rds"))


# RESPONSE TIME

# filter attended only 

attended_calls <- calls %>% filter(attended == "Yes")

# create response time variable

attended_calls$response_time <- as.period(ymd_hms(attended_calls$earliest_arrived_date_time) -  
                                            ymd_hms(attended_calls$earliest_deployed_date_time))

attended_calls$response_time_mins <- time_length(attended_calls$response_time, unit = "minute")

# some response times are negative (i.e. suggested that they were attended before call was made)
# after discussion the best approach is to add 12 hours to the negative time ones as it;s likely miscoding of am/pm
attended_calls$response_time_mins <- ifelse(attended_calls$response_time_mins <0, 
                                            attended_calls$response_time_mins + 720, 
                                            attended_calls$response_time_mins)


# count the median response time for each week in total first

median_response_time <- attended_calls %>% 
  mutate(incident_week = yearweek(incident_date_time)) %>% 
  group_by(incident_week) %>% 
  summarise(median_resp_time = median(response_time_mins, na.rm = TRUE))



#  convert to a tsibble object
median_response_time <- median_response_time %>% 
  # slice(2:(n() - 1)) %>% # remove first and last row
  as_tsibble(index = incident_week) %>% 
  fill_gaps(median_resp_time = NA) %>% 
  # Add dummy variables
  mutate(
    # Dummy for important date
    var_1 = incident_week > yearweek(ymd("DATE HERE")),
    # Dummy for other important date
    hmic_changes = incident_week > yearweek(ymd("DATE HERE")), 
    # Dummy for bank holidays 
    bank_holiday = incident_week %in% yearweek(as_date(timeDate::holidayLONDON(year = 2015:2020))))


# The model is based only on calls from before the first UK COVID case on 31
# January 2021
model_all_resp_time <- median_response_time %>% 
  filter(incident_week < yearweek(ymd("2020-01-31"))) %>% 
  model(
    arima = ARIMA(median_resp_time ~ trend() + season() + var_1 + var_2 + bank_holiday)
  )


# Create data for forecasting
fdata_resp_time <- expand_grid(
  incident_week = yearweek(seq.Date(
    from = ymd("2020-01-31"), 
    to = ymd("2020-12-31"),
    by = "week"
  )),
  var_1 = TRUE,
  var_2 = TRUE
) %>% 
  as_tsibble(index = incident_week) 

fdata_resp_time$bank_holiday <- fdata_resp_time$incident_week %in% yearweek(as_date(timeDate::holidayLONDON(year = 2020)))


# Create forecasts and extract confidence intervals
forecast_resp_time <-model_all_resp_time %>% 
  forecast(new_data = fdata_resp_time) %>% 
  hilo(level = 95) %>% 
  janitor::clean_names() %>% 
  unpack_hilo(cols = x95_percent) 


# Join actual counts to the forecast object and check if actual calls were 
# outside the forecast range
final_all_resp_time <- median_response_time %>% 
  filter(
    incident_week > yearweek(ymd("2019-12-31"))
  ) %>% 
  select(incident_week, actual_rt = median_resp_time) %>% 
  full_join(
    forecast_resp_time, 
    by = c("incident_week")
  ) %>% 
  select(
    incident_week, 
    actual_rt, 
    forecast_mean = mean, 
    forecast_lower = x95_percent_lower, 
    forecast_upper = x95_percent_upper
  ) %>% 
  mutate(
    # When plotting, it is more convenient to store the week as a date rather 
    # than a `yearweek` column
    incident_week = as_date(incident_week),
    # Calls are significantly different from the forecast if the call count is
    # less than the lower 95% CI or higher than the upper 95% CI
    sig = actual_rt < forecast_lower | actual_rt > forecast_upper
  ) %>% 
  replace_na(list(sig = FALSE))

# Save result for use elsewhere (e.g. in an Rmarkdown document)
write_rds(final_all_resp_time, here::here("output/all_resp_time_forecast.Rds"))

# repeat for different call types
# repeat for attended/response time by grade of call
