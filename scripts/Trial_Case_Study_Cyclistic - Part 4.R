## Part 3.0 - Analyzing Cyclistic Data

library(tidyverse)   # dplyr, readr, ggplot2, etc.
library(lubridate)   # wday() helper
library(knitr)       # nice tables
library(scales)      # for comma()
library(viridis)   # nice colour scale (optional)
library(ggrepel)



## ---------------------------------------------------------------
## P1 – Weekday vs Weekend Ride Counts by Rider Type
## ---------------------------------------------------------------

# 1. Load the CLEANED data

data_path <- "C:/Users/vishv/Downloads/Cyclistic Case Study/Data for Cyclistic/Trial 2 - In Progress/Cyclistic Data - Combined Using R/Cyclistic_Cleaned_Data.csv"
working_data <- read_csv(data_path, show_col_types = FALSE)

# 2. Create Weekday / Weekend flag & summarise

weekday_summary <- working_data %>% 
  mutate(week_part = if_else(day_of_week %in% c("Sat", "Sun"), 
                             "Weekend", "Weekday")) %>% 
  count(member_casual, week_part) %>% 
  group_by(member_casual) %>% 
  mutate(pct = n / sum(n) * 100) %>% 
  ungroup()

# 3. Table of counts & percentages

weekday_summary %>% 
  arrange(member_casual, desc(week_part)) %>% 
  mutate(Share = scales::percent(pct / 100, accuracy = 0.1)) %>%
  select(Rider_Type = member_casual,
         `Week Part` = week_part,
         Rides = n,
         Share) %>% 
  knitr::kable(
    caption = "✅ Ride counts and share: Weekday vs Weekend by rider type"
  )

# 4. Side‑by‑side bar chart

ggplot(weekday_summary, aes(x = week_part, y = n, fill = member_casual)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = comma(n)),
            position = position_dodge(width = 0.9),
            vjust = -0.15, size = 3) +
  labs(title = "Cyclistic Rides: Weekday vs Weekend",
       x = NULL, y = "Number of Rides",
       fill = "Rider Type") +
  scale_y_continuous(labels = comma) +      # ← pretty y‑axis ticks
  scale_fill_manual(values = c("casual" = "#F8766D",
                               "member" = "#00BFC4")) +
  theme_minimal(base_size = 12)

## Insight: data proves that for casual(62.1%) and member(77.0%), weekdays had majority of rides. 
## Consequently, marketing campaign for weekdays would be more be effective.

# ------------------------------------------------------------------
#  P2. Monthly Ride Count Comparison: Casual vs Member
# ------------------------------------------------------------------

# 1. Summarise rides per Year-Month and Rider Type

monthly_summary <- working_data %>% 
  mutate(ride_ym = floor_date(started_at, unit = "month")) %>%  # e.g., 2023-01-01
  count(ride_ym, member_casual) %>%
  mutate(ym_label = format(ride_ym, "%Y-%m"))  # e.g., "2023-01"

# 2. Summary table (wider format)

monthly_summary %>%
  pivot_wider(names_from = member_casual,
              values_from = n,
              values_fill = 0) %>%
  arrange(ride_ym) %>%
  kable(caption = "Monthly Ride Count Comparison: Casual vs Member")

# 3. Bar chart: grouped by rider type

ggplot(monthly_summary, aes(x = ym_label, y = n, fill = member_casual)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = comma(n)),
            position = position_dodge(width = 0.9),
            vjust = -0.15, size = 3) +
  labs(title = "Monthly Ride Counts by Rider Type",
       x = "Year-Month", y = "Number of Rides",
       fill = "Rider Type") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c("casual" = "#F8766D",
                               "member"  = "#00BFC4")) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Slanted x-axis

## Insight: data proves that highest ridership months were May to Oct 2024 for both rider types. 
## Consequently, marketing campaign during these months would be best to target riders.
## Additionally, Dec, Jan, and Feb had lowest monthly ride counts, so marketing a rounds of discounted rides could raise the numbers.
## It is understood that the cold winter of chicago might be the reason for the drop.

# ------------------------------------------------------------------
#  P3. Hourly Ride Distribution: Casual vs Member
# ------------------------------------------------------------------

# 1. Summarise rides per hour of day

hourly_summary <- working_data %>% 
  count(ride_hour, member_casual) %>%           # ride_hour already created in step l
  arrange(ride_hour)

# 2. Table: pivot wider for readability

hourly_summary %>% 
  pivot_wider(names_from = member_casual,
              values_from = n,
              values_fill = 0) %>% 
  rename(Hour = ride_hour,
         Casual = casual,
         Member = member) %>% 
  kable(caption = "Hourly Ride Counts: Casual vs Member")

# 3. Grouped bar chart

ggplot(hourly_summary, aes(x = factor(ride_hour), y = n, fill = member_casual)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = comma(n)),
            position = position_dodge(width = 0.9),
            vjust = -0.15, size = 2.5) +
  labs(title = "Hourly Ride Distribution by Rider Type",
       x = "Hour of Day (0–23)", y = "Number of Rides",
       fill = "Rider Type") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c("casual" = "#F8766D",
                               "member"  = "#00BFC4")) +
  theme_minimal(base_size = 12)

## Insights: 
## Mid‑morning to mid‑afternoon (10AM – 2PM) - Casual share peaks (~40%).
## Riders are in leisure mode;Promote “Unlimited weekend rides when you sub.today
## Late afternoon build‑up (3PM – 5PM) - High absolute casual counts with growing commuter traffic. 
## Offer “Ride home free as a member” prompts on mobile or dock kiosks.
## Evening leisure (8PM – 10PM) - Casual share rises 38 to 46% of total counts.
## Perfect for upselling night‑owl plans or highlighting safety perks with membership

# ------------------------------------------------------------------
#  P4. Average Ride Length by Rider Type
# ------------------------------------------------------------------

# 1. Summary statistics

ride_length_summary <- working_data %>%
  group_by(member_casual) %>%
  summarise(
    count = n(),
    mean_min = round(mean(ride_length), 1),
    median_min = round(median(ride_length), 1),
    max_min = round(max(ride_length), 1),
    .groups = "drop"
  )

# Show in table format

ride_length_summary %>%
  rename(
    `Rider Type` = member_casual,
    `Total Rides` = count,
    `Average Duration (min)` = mean_min,
    `Median Duration (min)` = median_min,
    `Max Duration (min)` = max_min
  ) %>%
  kable(caption = "Summary: Ride Lengths by Rider Type")

# 2. Boxplot (log scale if needed due to skewness)

ggplot(working_data, aes(x = member_casual, y = ride_length, fill = member_casual)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +  # suppress extreme outliers
  coord_cartesian(ylim = c(0, quantile(working_data$ride_length, 0.99))) +
  labs(title = "Distribution of Ride Lengths by Rider Type",
       x = "Rider Type", y = "Ride Duration (minutes)") +
  scale_fill_manual(values = c("casual" = "#F8766D", "member" = "#00BFC4")) +
  theme_minimal(base_size = 12)

## Insights:Casual users ride longer.
## So, offering unlimited rides as a member could lead to significant cost savings.”

# ------------------------------------------------------------------
#  P5. Popular Start Stations: Casual vs Member
# ------------------------------------------------------------------

# 1. Count rides by start station and rider type

station_summary <- working_data %>%
  group_by(member_casual, start_station_name) %>%
  summarise(ride_count = n(), .groups = "drop")

# 2. Top 10 stations by rider type

top_stations <- station_summary %>%
  group_by(member_casual) %>%
  slice_max(order_by = ride_count, n = 10, with_ties = FALSE) %>%
  arrange(member_casual, desc(ride_count))

# 3. Bottom 10 stations (rarely used)

bottom_stations <- station_summary %>%
  filter(start_station_name != "") %>%   # ignore blanks if any
  group_by(member_casual) %>%
  slice_min(order_by = ride_count, n = 10, with_ties = FALSE) %>%
  arrange(member_casual, ride_count)

# 4. Tables

top_stations %>%
  rename(`Rider Type` = member_casual,
         `Station` = start_station_name,
         `Rides` = ride_count) %>%
  kable(caption = "Top 10 Start Stations by Rider Type")

bottom_stations %>%
  rename(`Rider Type` = member_casual,
         `Station` = start_station_name,
         `Rides` = ride_count) %>%
  kable(caption = "Bottom 10 Start Stations by Rider Type")

# 5. Visualise top 10 casual start stations

top_casual_plot <- top_stations %>%
  filter(member_casual == "casual") %>%
  mutate(station = fct_reorder(start_station_name, ride_count))

ggplot(top_casual_plot, aes(x = station, y = ride_count)) +
  geom_col(fill = "#F8766D") +
  coord_flip() +
  labs(title = "Top 10 Start Stations for Casual Riders",
       x = NULL, y = "Ride Count") +
  theme_minimal(base_size = 12)

## Insights: 1) Streeter Dr & Grand Ave; 2) Dusable Lake Shore Dr & Monroe St are top start stations.
##For casual-to-member conversion, target high‑traffic tourist stations with signage/promotions highlighting:
##Offer weekday rides with membership. Faster checkouts for daily commuters. Savings on repeat usage.

# ------------------------------------------------------------------
#  P6. Bike Type Distribution by Rider Type
# ------------------------------------------------------------------

# 1. Count of rideable types by member type
bike_type_summary <- working_data %>%
  count(member_casual, rideable_type) %>%
  group_by(member_casual) %>%
  mutate(share = round(n / sum(n) * 100, 1)) %>%
  ungroup()

# 2. Summary Table
bike_type_summary %>%
  rename(
    `Rider Type` = member_casual,
    `Bike Type` = rideable_type,
    `Rides` = n,
    `Share (%)` = share
  ) %>%
  kable(caption = "Table: Rideable Bike Types by Rider Type")

# 3. Visualize with bar chart
ggplot(bike_type_summary, aes(x = rideable_type, y = n, fill = member_casual)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = scales::comma(n)),
            position = position_dodge(width = 0.9), vjust = -0.4, size = 3) +
  labs(title = "Bike Type Usage: Casual vs Member Riders",
       x = "Bike Type", y = "Number of Rides", fill = "Rider Type") +
  scale_fill_manual(values = c("casual" = "#F8766D", "member" = "#00BFC4")) +
  theme_minimal(base_size = 12)

## Insights: Classic bikes are prefered by both members and casual riders.
##Consequently, keep classic bikes more in docks and active operations.
## 37% of casual riders used e-bikes, market membership benefits on e-bikes.

# ------------------------------------------------------------------
#  P7 (100% Stacked) — Seasonal Share of Rides by Rider Type
# ------------------------------------------------------------------

# 1. Summarise ride counts
season_summary <- working_data %>%
  count(season, member_casual) %>%
  mutate(season = factor(season,
                         levels = c("Winter", "Spring", "Summer", "Fall")))

# 2. Table with shares (nice for report)
season_table <- season_summary %>%
  group_by(season) %>%
  mutate(share = round(n / sum(n) * 100, 1)) %>%
  select(season, member_casual, share) %>%
  pivot_wider(
    names_from = member_casual,
    values_from = share,
    names_prefix = "Share_"
  ) %>%
  rename(
    Season = season,
    `Casual Share (%)` = Share_casual,
    `Member Share (%)` = Share_member
  )

kable(season_table,
      caption = "Share of Rides by Season (Casual vs Member)")

# 3. 100% stacked bar chart
ggplot(season_summary,
       aes(x = season, y = n, fill = member_casual)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("casual" = "#F8766D",
                               "member"  = "#00BFC4")) +
  labs(title = "Seasonal Ride Share by Rider Type (100%)",
       x = "Season", y = "Share of Rides",
       fill = "Rider Type") +
  theme_minimal(base_size = 12)

## Insights:Focus on converting casual riders during summer, when their engagement is highest.
## Spring is a strategic time to promote seasonal discounts or "try before subscribing" campaigns to casual riders.

# ------------------------------------------------------------------
#  P8. Weekend–Hourly Heatmap by Rider Type
# ------------------------------------------------------------------


# 1. Create week_part flag (Weekday / Weekend)
week_hour_summary <- working_data %>%
  mutate(
    week_part = if_else(day_of_week %in% c("Sat", "Sun"),
                        "Weekend", "Weekday")
  ) %>%
  count(member_casual, week_part, ride_hour)

# 2. Heatmap (faceted by rider type)
ggplot(week_hour_summary,
       aes(x = ride_hour, y = week_part, fill = n)) +
  geom_tile(color = "white") +
  facet_wrap(~ member_casual, ncol = 1) +          # one panel per rider type
  scale_y_discrete(limits = c("Weekend", "Weekday")) +
  scale_x_continuous(breaks = 0:23) +
  scale_fill_viridis(option = "C", labels = scales::comma) +
  labs(
    title = "Heatmap: Ride Counts by Hour & Day Type",
    subtitle = "Weekend vs Weekday, faceted by Rider Type",
    x = "Hour of Day (0–23)",
    y = NULL,
    fill = "Ride Count"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.spacing.y = unit(1, "lines"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

## Insights:Brightest cells in casual Weekday row are between 2:30PM to 7:30PM
## Schedule dock‑side promo staff, push in‑app membership banners during these hours.

# ------------------------------------------------------------------
#  P9. Identify Power Casual Riders
# ------------------------------------------------------------------

# Count number of rides per ride_id per day
power_casuals <- working_data %>%
  filter(member_casual == "casual") %>%
  group_by(ride_day, start_station_name, end_station_name) %>%
  summarise(rides = n(), .groups = "drop") %>%
  arrange(desc(rides))

# Show top records with 5+ repeat rides on same route and day
power_casuals_filtered <- power_casuals %>%
  filter(rides >= 5)

knitr::kable(head(power_casuals_filtered, 10),
             caption = "Top Casual Rider Behaviors (Repeat Same-Day Routes)")

## Insights:1) Streeter Dr & Grand Ave and 2) Dusable Lake Shore Dr & Monroe St have power casuals.
## Upsell to Day/Week Pass	When a casual rider hits >4 rides in a single day, 
## trigger an in‑app prompt: “Save money—upgrade to a 24‑hour pass for unlimited rides.”
## If the same account appears on multiple high‑repeat days, email a personalised message: “You rode 120 times on Sept 1. An annual pass would’ve saved you $X.”
## Post pricing comparisons at Streeter Dr & Grand and Lake Shore & Monroe docks, highlight unlimited time for members.

# ------------------------------------------------------------------
#  P10.Geographic Clusters: Start Location Patterns
# ------------------------------------------------------------------

# Step 1: Count rides by rounded coordinates and rider type
location_summary <- working_data %>%
  group_by(member_casual, start_lat3, start_lng3) %>%
  summarise(rides = n(), .groups = "drop") %>%
  mutate(label = paste0(round(rides / sum(rides) * 100, 1), "%"))

# Step 2: Plot start location clusters
ggplot(location_summary %>% filter(rides > 500),  # or 1000+
       aes(x = start_lng3, y = start_lat3,
           color = member_casual,
           size  = rides)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~ member_casual) +
  coord_fixed() +
  scale_color_manual(values = c("casual" = "#F8766D", "member" = "#00BFC4")) +
  labs(
    title = "Popular Start Locations by Rider Type",
    x = "Longitude", y = "Latitude",
    color = "Rider Type", size = "Rides"
  ) +
  theme_minimal()

## Insights: Clusters with highest casual riders will be ideal for selecting marketing areas.