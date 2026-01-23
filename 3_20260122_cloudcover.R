####-- Cloudcover - January 12th, 2026

# Clear environment if needed
#rm(list=ls(all=TRUE))

#install.packages(c("worldmet", "suncalc", "lubridate", "dplyr", "ggplot2"))
library(worldmet)
library(suncalc)
library(lubridate)
library(dplyr)
library(ggplot2)

############################ Cloudcover #######################################

# Find weather stations near Cincinnati
#import_ghcn_stations(lat = 39.0527, lng = -84.6670, return = "map")

# Get hourly weather station data 
# CVG Airport (most complete data) --time in GMT
met_CVG <- import_ghcn_hourly("USW00093814", year = 2024, abbr_names = FALSE, extra = TRUE)
# Save data in case package breaks
#write.csv(met_CVG, "data/1_20260108_cvg.csv")

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
    ymd_hms("2024-01-01 00:00:00", tz = "UTC"),
    ymd_hms("2025-01-01 00:00:00", tz = "UTC"),
    by = "1 hour")

# Get EST from UTC, filter for 2024
hours_est <- tibble(
    datetime_utc = hours_utc,
    datetime_est = with_tz(hours_utc, "America/New_York")) %>%
    filter(year(datetime_est) == 2024) 

# Get sun position for CVG for 2024 (in UTC)
sunpos <- getSunlightPosition(
    date = hours_est$datetime_utc,
    lat = lat,
    lon = lon)

# If sun positions is above horizon, then daylight (if not, then night)
sun_hr <- hours_est %>%
    mutate(sun = if_else(sunpos$altitude > 0, 1L, 0L)) %>%
    select(datetime_est, datetime_utc, sun)


### Checks (no 02:00 on March 9 & two 01:00s on Nov 2 bc daylight savings)
#test <- sun_hr %>%
#    filter(date(datetime_est) %in% c("2025-03-09", "2025-11-02")) %>%
#    select(datetime_est, sun)
#print(test, n=100)

##################### Merging Cloud and Sun Dataframes ########################

# Left join dataframes in case of missing airport data
cloud_sun <- sun_hr %>% left_join(cloud_hr, by = "datetime_utc")
# Save data in case package breaks
#write.csv(cloud_sun, "data/1_20260108_cloudsun.csv")

# Filter out nighttime hours and NAs (minimal), 
clouds_onlysunlight <- cloud_sun %>% 
    filter(sun==1) %>% 
    filter(!is.na(sky_cover)) %>%
    mutate(sky_cover_round = round(sky_cover),
           sky_cover_08 = if_else(sky_cover_round > 8, 8, sky_cover_round)) #turn 9's into 8's bc functionally same for humans

# Explore classes
table(clouds_onlysunlight$sky_cover_08, useNA = "always")

############################ Graphing #######################################

# Group classes by human readable names (and make a factor)
cloud_graph <- clouds_onlysunlight %>%
    mutate(
        month = month(datetime_est, label = TRUE, abbr = TRUE),
        sky_cover_factor = factor(
            sky_cover_08,
            levels = 0:8,
            labels = c("Clear", 
                       "Few", "Few", 
                       "Scattered", "Scattered", 
                       "Broken", "Broken", "Broken", 
                       "Overcast")))
# Plot
p1 <- ggplot(data = cloud_graph) +
    geom_bar(mapping = aes(x = month, fill = sky_cover_factor), 
             position = position_stack(reverse = TRUE)) +
    scale_fill_brewer(palette = "Blues", direction = -1) +
    labs(title = "Cincinnati Monthly Daytime Cloud Cover",
         x = NULL,
         y = "Daylight Hours",
         fill = "Cloud Cover") +
    theme_minimal(base_size = 11) +
    theme(plot.title.position = "plot")

# Save plots
ggsave("plots/3_20260122_p1.jpg", plot = p1, width = 6, height = 4, units = "in")




