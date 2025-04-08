library(dplyr)
library(ggplot2)
library(lubridate)
library(RColorBrewer)

all_calls <- read.csv("weekly_count_by_lsoa.csv")
asb_calls <- read.csv("weekly_asb_by_lsoa.csv")
asb_calls <- asb_calls %>% 
  rename("asb_calls" = "n")
all_calls <- left_join(all_calls, asb_calls)


# Convert `inc_wk` to date format, if not already
all_calls$inc_wk <- as.Date(all_calls$inc_wk)
all_calls$month <- floor_date(all_calls$inc_wk, unit = "month")

# 1. Calculate total ASB calls and total calls for each week
overall_weekly <- all_calls %>%
  group_by(month) %>%
  summarise(
    total_asb_calls = sum(asb_calls, na.rm = TRUE),
    total_calls = sum(n, na.rm = TRUE)
  )

# 2. Calculate ASB LQ for each decile in each week
decile_weekly_lq <- all_calls %>%
  group_by(month, index_of_multiple_deprivation_imd_decile) %>%
  summarise(
    decile_asb_calls = sum(asb_calls, na.rm = TRUE),
    decile_total_calls = sum(n, na.rm = TRUE)
  ) %>%
  # Join with overall weekly totals
  left_join(overall_weekly, by = "month") %>%
  # Calculate the Location Quotient (LQ) for each decile per week
  mutate(
    LQ = (decile_asb_calls / decile_total_calls) / (total_asb_calls / total_calls), 
    LQ_color = ifelse(LQ > 1.10, 
                           as.character(index_of_multiple_deprivation_imd_decile), 
                           "below 1.10"), 
    LQ_color = factor(as.factor(LQ_color), levels = c(as.character(1:10), "below 1.10")))

color_list <- c("below 1.10" = "gray90")
brewer_colors <- brewer.pal(10, "RdYlBu")

# Append colors for each decile from 1 to num_deciles
for (i in 1:10) {
  color_list <- c(color_list, setNames(brewer_colors[i], as.character(i)))
}



# Create the plot
ggplot(decile_weekly_lq, aes(x = month, y = LQ, group = index_of_multiple_deprivation_imd_decile)) +
  
  # Draw lines for both below and above 1.10
  geom_line(aes(color = LQ_color)) +
  
  # Add dashed horizontal line at 1.10
  geom_hline(yintercept = 1.10, linetype = "dashed", color = "black", size = 0.8) +
  
  # Set titles and labels
  labs(
    title = "Change in ASB Location Quotient by Deprivation Decile Over Time",
    x = "Month",
    y = "Location Quotient (LQ)",
    color = "Deprivation Decile"
  ) + 
  
  scale_color_manual(values = color_list) +  # Use the generated color list here
  
  # Minimal theme
  theme_minimal() +
  theme(legend.position = "right") 

# Look at LQ before and after covid (23rd Jan)

cutoff_date <- as.Date("2020-01-23")

# Calculate total ASB calls and total calls for each month across all deciles
overall_monthly <- all_calls %>%
  group_by(month) %>%
  summarise(
    total_asb_calls = sum(asb_calls, na.rm = TRUE),
    total_calls = sum(n, na.rm = TRUE)
  )


# Calculate ASB LQ for each decile in each month
decile_monthly_lq <- all_calls %>%
  group_by(month, index_of_multiple_deprivation_imd_decile) %>%
  summarise(
    decile_asb_calls = sum(asb_calls, na.rm = TRUE),
    decile_total_calls = sum(n, na.rm = TRUE)
  ) %>%
  # Join with overall monthly totals
  left_join(overall_monthly, by = "month") %>%
  # Calculate the Location Quotient (LQ) for each decile per month
  mutate(
    LQ = (decile_asb_calls / decile_total_calls) / (total_asb_calls / total_calls)
  )

# Split data into before and after the cutoff
before_cutoff <- decile_monthly_lq %>% filter(month < cutoff_date)
after_cutoff  <- decile_monthly_lq %>% filter(month >= cutoff_date)

# Calculate the average LQ for each decile before and after the cutoff
avg_before_cutoff <- before_cutoff %>%
  group_by(index_of_multiple_deprivation_imd_decile) %>%
  summarise(
    avg_LQ_before = mean(LQ, na.rm = TRUE)
  )

avg_after_cutoff <- after_cutoff %>%
  group_by(index_of_multiple_deprivation_imd_decile) %>%
  summarise(
    avg_LQ_after = mean(LQ, na.rm = TRUE)
  )

# Join the before and after results and calculate the change in LQ
change_in_lq <- avg_before_cutoff %>%
  left_join(avg_after_cutoff, by = "index_of_multiple_deprivation_imd_decile") %>%
  mutate(LQ_change = avg_LQ_after - avg_LQ_before)

# Plot the change in LQ across deciles
ggplot(change_in_lq, aes(x = factor(index_of_multiple_deprivation_imd_decile), y = LQ_change)) +
  geom_bar(stat = "identity", aes(fill = factor(index_of_multiple_deprivation_imd_decile))) +
  labs(
    title = "Change in ASB Location Quotient pre/post COVID-19",
    x = "Deprivation Decile",
    y = "Change in Location Quotient (LQ)", 
    fill = "IMD decile"
  ) +
  scale_fill_brewer(type = "qual", palette = "RdYlBu", ) + 
  theme_minimal()





