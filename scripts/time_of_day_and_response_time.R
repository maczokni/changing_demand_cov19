library(lubridate)
library(tsibble)
library(tidyverse)

# Load data — replace with correct file path
df_1 <- read_csv("INSERT PATH TO DATA HERE") %>%
  janitor::clean_names()

# Extract time of day from incident timestamp
df_1$variable_1 <- hour(ymd_hms(df_1$incident_date_time))
df_1$variable_2 <- case_when(
  df_1$variable_1 >= 5 & df_1$variable_1 < 11 ~ "Early",
  df_1$variable_1 >= 11 & df_1$variable_1 < 17 ~ "Daytime",
  df_1$variable_1 >= 17 & df_1$variable_1 < 23 ~ "Evening",
  df_1$variable_1 >= 23 | df_1$variable_1 < 5 ~ "Late",
  TRUE ~ NA_character_
)
df_1$variable_2 <- factor(df_1$variable_2, levels = c("Early", "Daytime", "Evening", "Late"))

# Visualise proportion of incidents by time of day
df_1 %>%
  mutate(variable_3 = as_date(yearweek(incident_date_time))) %>%
  count(variable_2, variable_3) %>%
  ggplot(aes(x = variable_3, y = n, fill = variable_2)) +
  geom_area(position = "fill") +
  geom_vline(xintercept = as.Date("2020-01-23"), linetype = "33") +
  geom_vline(xintercept = as.Date("2020-03-23"), linetype = "12") +
  geom_vline(xintercept = as.Date("2020-06-01"), linetype = "42") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y", expand = c(0, 0)) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_brewer(type = "qual") +
  labs(
    title = "Proportion of All Incidents by Time of Day",
    x = NULL,
    y = "Proportion",
    fill = "Time of Day"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))

# Categorise attendance
df_1$variable_4 <- case_when(
  df_1$attended_flag == 1 ~ "Yes",
  df_1$attended_flag == 0 ~ "No",
  TRUE ~ NA_character_
)

# Visualise proportion of incidents attended or not
df_1 %>%
  mutate(variable_3 = as_date(yearweek(incident_date_time))) %>%
  count(variable_4, variable_3) %>%
  ggplot(aes(x = variable_3, y = n, fill = variable_4)) +
  geom_area(position = "fill") +
  geom_vline(xintercept = as.Date("2020-01-23"), linetype = "33") +
  geom_vline(xintercept = as.Date("2020-03-23"), linetype = "12") +
  geom_vline(xintercept = as.Date("2020-06-01"), linetype = "42") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y", expand = c(0, 0)) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_brewer(type = "qual") +
  labs(
    title = "Proportion of Incidents by Attendance Status",
    x = NULL,
    y = "Proportion",
    fill = "Attended"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))

# Weekly count of incidents by attendance
df_1 %>%
  mutate(variable_3 = as_date(yearweek(incident_date_time))) %>%
  count(variable_4, variable_3)

# Filter attended incidents only
df_2 <- df_1 %>% filter(variable_4 == "Yes")

# Calculate response time
df_2$variable_5 <- as.period(
  ymd_hms(df_2$earliest_arrived_date_time) - 
    ymd_hms(df_2$earliest_deployed_date_time)
)

# Summary statistics of response time
mean(time_length(df_2$variable_5, unit = "minute"), na.rm = TRUE)
min(time_length(df_2$variable_5, unit = "minute"), na.rm = TRUE)
max(time_length(df_2$variable_5, unit = "minute"), na.rm = TRUE)

# Visualise distribution of response time
ggplot(df_2, aes(time_length(variable_5, unit = "minute"))) +
  geom_density() +
  scale_x_log10()
