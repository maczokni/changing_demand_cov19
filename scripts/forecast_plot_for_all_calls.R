# Load packages
library(fable)
library(lubridate)
library(tsibble)
library(tidyverse)

# Read and clean data — replace with correct file path
df_1 <- read_csv("INSERT PATH TO DATA HERE") %>%
  janitor::clean_names()

# Identify low-frequency categories to exclude
variable_1 <- df_1 %>%
  count(variable_2) %>%
  filter(n < 1000) %>%
  pull(variable_2)

# Recode incident types
df_1 <- mutate(
  df_1,
  variable_2 = case_when(
    variable_2 %in% c("Distraction Burglary") ~ "Burglary - Residential",
    variable_2 %in% c(
      "Assistance to Other Agencies", "CRB Use Only", "CSI To Be Informed",
      "External System", "Found Stolen Vehicle", "Message to Pass/OOF Enquiries",
      "PNC Markers", "Police Vehicle Recovery", "Pre-Planned Events",
      "Stop Search", "Training"
    ) ~ "other - admin",
    variable_2 %in% c(
      "Alarm - Activation", "Alarm - No Response", "Audible Only Alarm",
      "Insecure Premises", "Police Installed Alarm", "Unauthorised Encampment"
    ) ~ "alarms",
    variable_2 %in% c(
      "Bail Breaches/Wanted Person", "Bail/Curfew/Wanted",
      "Prison Licence Recall", "Warrant Crown Court"
    ) ~ "warrant/bail issue",
    variable_2 %in% c("Domestic Animal Concern", "Wildlife Matters") ~ "animals",
    variable_2 %in% c("Firearms - Non Notifiable Crime", "Firearms Involved Crime") ~ "firearms",
    variable_2 %in% c("RTC", "RTC - Damage Only") ~ "traffic collision",
    variable_2 %in% variable_1 ~ "other - minor",
    TRUE ~ variable_2
  )
)

# Create weekly counts and add dummy variables
df_2 <- df_1 %>%
  mutate(variable_3 = yearweek(incident_date_time)) %>%
  count(variable_3, name = "call_count") %>%
  slice(2:(n() - 1)) %>%
  as_tsibble(index = variable_3) %>%
  fill_gaps(call_count = 0) %>%
  mutate(
    variable_4 = variable_3 > yearweek(ymd("IMPORTANT DATE HERE")),
    variable_5 = variable_3 > yearweek(ymd("IMPORTANT DATE HERE")),
    variable_6 = variable_3 %in% yearweek(as_date(timeDate::holidayLONDON(year = 2015:2020)))
  )

# Fit ARIMA model using pre-pandemic data
variable_7 <- df_2 %>%
  filter(variable_3 < yearweek(ymd("2020-01-31"))) %>%
  model(
    arima = ARIMA(call_count ~ trend() + season() + variable_4 + variable_5 + variable_6)
  )

# Create future data with dummy values for forecasting
df_3 <- expand_grid(
  variable_3 = yearweek(seq.Date(
    from = ymd("2020-01-31"),
    to = ymd("2020-12-31"),
    by = "week"
  )),
  variable_4 = TRUE,
  variable_5 = TRUE
) %>%
  as_tsibble(index = variable_3)

df_3$variable_6 <- df_3$variable_3 %in% yearweek(as_date(timeDate::holidayLONDON(year = 2020)))

# Generate forecasts and extract intervals
df_4 <- variable_7 %>%
  forecast(new_data = df_3) %>%
  hilo(level = 95) %>%
  janitor::clean_names() %>%
  unpack_hilo(cols = x95_percent)

# Join forecast to observed values and flag significant deviations
df_5 <- df_2 %>%
  filter(variable_3 > yearweek(ymd("2019-12-31"))) %>%
  select(variable_3, actual_calls = call_count) %>%
  full_join(df_4, by = "variable_3") %>%
  select(
    variable_3,
    actual_calls,
    forecast_mean = mean,
    forecast_lower = x95_percent_lower,
    forecast_upper = x95_percent_upper
  ) %>%
  mutate(
    variable_3 = as_date(variable_3),
    variable_7 = actual_calls < forecast_lower | actual_calls > forecast_upper
  ) %>%
  replace_na(list(variable_7 = FALSE))

# Annotate key dates for reference
df_6 <- tribble(
  ~variable_8, ~variable_9,
  "2020-01-31", "first UK COVID case",
  "2020-03-23", "first lockdown begins",
  "2020-06-15", "first lockdown ends",
  "2020-11-05", "second lockdown begins",
  "2020-12-02", "second lockdown ends"
) %>%
  mutate(
    variable_8 = as_date(yearweek(ymd(variable_8))),
    variable_10 = row_number(),
    variable_11 = str_glue("{variable_10}. {variable_9}")
  )

# Save processed results
write_rds(df_5, here::here("output/all_calls_forecast.Rds"))

# Plot forecasts vs actuals
df_5 %>%
  mutate(forecast_lower = ifelse(forecast_lower < 0, 0, forecast_lower)) %>%
  ggplot() +
  geom_ribbon(
    aes(variable_3, ymin = forecast_lower, ymax = forecast_upper),
    na.rm = TRUE,
    alpha = 0.5,
    fill = "grey80"
  ) +
  geom_line(aes(variable_3, forecast_mean), na.rm = TRUE, linetype = "22") +
  geom_vline(aes(xintercept = variable_8), data = df_6, linetype = "12") +
  geom_label(aes(variable_8, 0, label = variable_10), data = df_6, colour = "grey20") +
  geom_line(aes(variable_3, actual_calls)) +
  geom_point(aes(variable_3, actual_calls, fill = variable_7), shape = 21) +
  scale_x_date(
    date_labels = "%e %b\n%Y",
    limits = as.Date(c("2020-01-06", "2020-12-21"))
  ) +
  scale_y_continuous(limits = c(0, NA), labels = scales::comma_format()) +
  scale_fill_manual(values = c(`TRUE` = "black", `FALSE` = "grey80")) +
  labs(
    title = "Change in All Calls During 2021 Compared to Pre-Pandemic Forecast",
    subtitle = str_wrap(
      str_glue("Events by week: ", str_c(pull(df_6, variable_11), collapse = "; ")),
      80
    ),
    caption = "Forecast calculated using data up to 31 January 2020",
    x = NULL,
    y = "Weekly number of calls",
    fill = "Significant deviation"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold")) +
  xlim(c(min(df_5$variable_3), ymd("2020-12-14")))
