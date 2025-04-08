library(lubridate)
library(tsibble)
library(stringr)
library(readr)
library(dplyr)
library(ineq)
library(ggplot2)

# Note important dates
# Source: https://www.instituteforgovernment.org.uk/sites/default/files/timeline-lockdown-web.pdf
dates <- tribble(
  ~date, ~event,
  "2020-01-31", "first UK COVID case",
  "2020-03-23", "first lockdown begins",
  "2020-06-15", "first lockdown ends",
  "2020-11-05", "second lockdown begins",
  "2020-12-02", "second lockdown ends"
) %>% 
  mutate(
    date = as_date(yearweek(ymd(date))), 
    row = row_number(),
    label = str_glue("{row}. {event}")
  )

# Read in data which has weekly count of calls (all calls) for each LSOA
weekly_count_by_lsoa <- read_csv("PATH TO DATA")
weekly_asb_by_lsoa <- read_csv("PATH TO DATA")

weekly_gini <- weekly_count_by_lsoa %>% 
  group_by(inc_wk) %>% 
  summarise(gini = ineq(n), type = "Gini")

ggplot(weekly_gini, aes(x = inc_wk, y = gini)) + 
  geom_line() + 
  # Dates of interest
  geom_vline(aes(xintercept = date), data = dates, linetype = "12") +
  geom_label(aes(date, 0.35, label = row), data = dates, colour = "grey20") +
  scale_x_date(date_labels = "%e %b\n%Y", 
               limits = as.Date(c("2015-01-01", "2020-12-21")))+
  labs(
    title = "Gini coefficient* over time",
    subtitle = "*measure of inequality with 0 = perfect equality and 1 = maximal inequality",
    x = NULL,
    y = "Gini coefficient") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))


# FUNCTION TO MAKE GINI COEFFICIENT PLOT - SO CAN BE MADE FOR DIFFERENT CALL TYPES
make_gini_plot <- function(x){
  
  weekly_gini <- x %>% 
    group_by(inc_wk) %>% 
    summarise(gini = ineq(n), type = "Gini")
  
  ggplot(weekly_gini, aes(x = inc_wk, y = gini)) + 
    geom_line() + 
    # Dates of interest
    geom_vline(aes(xintercept = date), data = dates, linetype = "12") +
    geom_label(aes(date, 0.35, label = row), data = dates, colour = "grey20") +
    scale_x_date(date_labels = "%e %b\n%Y", 
                 limits = as.Date(c("2015-01-01", "2020-12-21")))+
    labs(
      title = "Gini coefficient* over time",
      subtitle = "*measure of inequality with 0 = perfect equality and 1 = maximal inequality",
      x = NULL,
      y = "Gini coefficient") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))
}

# MAKE PLOT FOR ASB
make_gini_plot(weekly_asb_by_lsoa)
