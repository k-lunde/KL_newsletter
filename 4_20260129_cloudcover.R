####-- Cloudcover - January 12th, 2026

# Clear environment if needed
#rm(list=ls(all=TRUE))

#install.packages(c("worldmet", "suncalc", "lubridate", "dplyr", "ggplot2"))
library(worldmet)
library(suncalc)
library(lubridate)
library(dplyr)
library(ggplot2)
library(ggpubr)

############################ Cloudcover #######################################

# Find weather stations near Cincinnati
#import_ghcn_stations(lat = 39.0527, lng = -84.6670, return = "map")

# Get hourly weather station data
# CVG Airport (most complete data) --time in GMT
met_CVG <- import_ghcn_hourly("USW00093814", year = 2005:2024, abbr_names = FALSE, extra = TRUE)
# Save data in case package breaks
#write.csv(met_CVG, "data/4_20260129_cvg.csv")

# Find which years have data (originally tested 1950-2025)
# na_find <- met_CVG %>%
#     mutate(year = year(date)) %>%
#     group_by(year) %>%
#     summarise(
#         n_rows = n(),
#         total_missing = sum(is.na(sky_cover)),
#         percent_missing =
#             total_missing / n_rows * 100

# Keep only cloud cover variables
cloud_hr <- met_CVG %>% select(date, sky_cover) %>% rename(datetime_utc = date)

############################ Sun Hours #######################################
# Suncalc manual
# https://cran.r-universe.dev/suncalc/doc/manual.html

# CVG coordinates
lat <- 39.0527
lon <- -84.6670

# Create initial dataframe of times to calculate hourly sun position
hours_utc <- seq(
    ymd_hms("2005-01-01 00:00:00", tz = "UTC"),
    ymd_hms("2025-01-01 00:00:00", tz = "UTC"),
    by = "1 hour")

# Get EST from UTC
hours_est <- tibble(
    datetime_utc = hours_utc,
    datetime_est = with_tz(hours_utc, "America/New_York"))

# Get sun position for CVG for 2025-2024 (in UTC)
sunpos <- getSunlightPosition(
    date = hours_est$datetime_utc,
    lat = lat,
    lon = lon)

# If sun positions is above horizon, then daylight (if not, then night)
sun_hr <- hours_est %>%
    mutate(sun = if_else(sunpos$altitude > 0, 1L, 0L)) %>%
    select(datetime_est, datetime_utc, sun)


##################### Merging Cloud and Sun Dataframes ########################

# Left join dataframes in case of missing airport data
cloud_sun <- sun_hr %>% left_join(cloud_hr, by = "datetime_utc")
# Save data in case package breaks
#write.csv(cloud_sun, "data/1_20260108_cloudsun.csv")

# Filter out nighttime hours and NAs (minimal) 
clouds_onlysunlight <- cloud_sun %>% 
    filter(sun==1) %>% 
    filter(!is.na(sky_cover)) %>%
    filter(year(datetime_est) != 2024) %>%
    mutate(sky_cover_round = round(sky_cover),
           sky_cover_08 = if_else(sky_cover_round > 8, 8, sky_cover_round)) #turn 9's into 8's bc functionally same for humans

# Explore classes
table(clouds_onlysunlight$sky_cover_08, useNA = "always")

############################ Graphing #######################################

####-- Total count (not used)

# Group classes by human readable names (and make a factor)
cloud_graph1 <- clouds_onlysunlight %>%
    mutate(
        month = month(datetime_utc, label = TRUE, abbr = TRUE),
        sky_cover_factor = factor(
            sky_cover_08,
            levels = 0:8,
            labels = c("Clear", 
                       "Few", "Few", 
                       "Scattered", "Scattered", 
                       "Broken", "Broken", "Broken", 
                       "Overcast")))

ggplot(data = cloud_graph1) +
    geom_bar(mapping = aes(x = month, fill = sky_cover_factor), 
             position = position_stack(reverse = TRUE)) +
    scale_fill_brewer(palette = "Blues", direction = -1) +
    labs(title = "Cincinnati Monthly Daytime Cloud Cover",
         x = NULL,
         y = "Daylight Hours",
         fill = "Cloud Cover") +
    theme_minimal(base_size = 11) +
    theme(plot.title.position = "plot")


####-- Monthly Means

monthly_counts <- clouds_onlysunlight %>%
    mutate(
        month = month(datetime_utc, label = TRUE, abbr = TRUE),
        year = year(datetime_utc)) %>%
    count(year, month, sky_cover_08, name = "hours")

# Calculate average cloud cover per month
monthly_means <- monthly_counts %>%
    group_by(month, sky_cover_08) %>%
    summarise(
        mean_hours = mean(hours, na.rm = TRUE),
        .groups = "drop"
    )

# Group classes by human readable names (and make a factor)
cloud_graph2 <- monthly_means %>%
    mutate(
        sky_cover_factor = factor(
            sky_cover_08,
            levels = 0:8,
            labels = c("Clear", 
                       "Few", "Few", 
                       "Scattered", "Scattered", 
                       "Broken", "Broken", "Broken", 
                       "Overcast")))

# Plot
p1 <- ggplot(data = cloud_graph2) +
    geom_bar(mapping = aes(x = month, y = mean_hours, fill = sky_cover_factor), 
             position = position_stack(reverse = TRUE), 
             stat = 'identity') +
    scale_fill_brewer(palette = "Blues", direction = -1) +
    labs(title = "Average Cincinnati Monthly Daytime Cloud Cover (2005-2024)",
         x = NULL,
         y = "Daylight Hours",
         fill = "Cloud Cover") +
    theme_minimal() +
    theme(plot.title.position = "plot")

# Save plot
ggsave("plots/4_20260129_p1.jpg", plot = p1, width = 6, height = 4, units = "in")


# Old plot for comparison
# Group classes by human readable names (and make a factor)
cloud_graph_lastweek <- clouds_onlysunlight %>%
    mutate(
        month = month(datetime_est, label = TRUE, abbr = TRUE),
        year = year(datetime_est),
        sky_cover_factor = factor(
            sky_cover_08,
            levels = 0:8,
            labels = c("Clear", 
                       "Few", "Few", 
                       "Scattered", "Scattered", 
                       "Broken", "Broken", "Broken", 
                       "Overcast"))) %>%
    filter(year == 2024)

# Plot
p1_m <- ggplot(data = cloud_graph_lastweek) +
    geom_bar(mapping = aes(x = month, fill = sky_cover_factor), 
             position = position_stack(reverse = TRUE)) +
    scale_fill_brewer(palette = "Blues", direction = -1) +
    labs(title = "Cincinnati Monthly Daytime Cloud Cover 2024",
         x = NULL,
         y = "Daylight Hours",
         fill = "Cloud Cover") +
    theme_minimal() +
    theme(plot.title.position = "plot")

# Combine plots for comparison
p1_c <- ggarrange(p1_m, p1, common.legend = TRUE, legend = "bottom")

# Save plot
ggsave("plots/4_20260129_p1_c.jpg", plot = p1_c, width = 12, height = 6, units = "in")



####-- Changes over time

# Create data frame for plotting
monthly_all <- clouds_onlysunlight %>%
    mutate(
        year  = year(datetime_utc),
        month = month(datetime_utc),
        month_label = month(datetime_utc, label = TRUE, abbr = TRUE)) %>%
    group_by(year, month, sky_cover_08) %>%
    summarise(
        hours = n(),  # total hours per month/class
        .groups = "drop") %>%
    mutate(
        cloud_class = factor(
            sky_cover_08,
            levels = 0:8,
            labels = c(
                "Clear",
                "Few", "Few",
                "Scattered", "Scattered",
                "Broken", "Broken", "Broken",
                "Overcast")),
        cloud_class = factor(
            cloud_class,
            levels = c("Clear", "Few", "Scattered", "Broken", "Overcast")),
        # Create a continuous month index for plotting across years
        month_seq = year + (month - 1)/12)

# Plot
p2 <- ggplot(monthly_all, aes(x = month_seq, y = hours, color = cloud_class)) +
    geom_smooth(se = FALSE, method = "loess") +
    scale_x_continuous(breaks = 2005:2024, labels = 2005:2024) +
    scale_color_manual(values = c(
        "Clear" = "#08519C", 
        "Few" = "#3182BD",
        "Scattered" = "#6BAED6",
        "Broken" = "#BDD7E7",
        "Overcast" = "grey60")) +
    labs(
        x = NULL,
        y = "Daytime cloud hours (monthly averages)",
        color = "Cloud Class",
        title = "Smoothed Cloud Cover Hours (2005-2024)"
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom",
          plot.title.position = "plot")

# Save plot
ggsave("plots/4_20260129_p2.jpg", plot = p2, width = 12, height = 6, units = "in")

