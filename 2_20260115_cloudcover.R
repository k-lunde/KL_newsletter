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

# Find weather stations near Cincinnati, Seattle, Sacramento
#import_ghcn_stations(lat = 39.0527, lng = -84.6670, return = "map")
#import_ghcn_stations(lat = 47.4503, lng = -122.3087, return = "map")
#import_ghcn_stations(lat = 38.6946, lng = -121.5887, return = "map")

# Get hourly weather station data --time in GMT
# CVG Airport
met_CVG <- import_ghcn_hourly("USW00093814", year = 2024, abbr_names = FALSE, extra = TRUE)
# SEA Airport
met_SEA <- import_ghcn_hourly("USW00024233", year = 2024, abbr_names = FALSE, extra = TRUE)
# SMF Airport
met_SMF <- import_ghcn_hourly("USW00093225", year = 2024, abbr_names = FALSE, extra = TRUE)

# Combine data
met_combined <- rbind(met_CVG, met_SEA, met_SMF) %>%
    mutate(location = case_when(station_id == "USW00093814" ~ "CVG",
                                station_id == "USW00024233" ~ "SEA",
                                station_id == "USW00093225" ~ "SMF"))
# Save data in case package breaks
#write.csv(met_combined, "data/2_20260115_threecities.csv")

# Keep only cloud cover variables
cloud_hr3 <- met_combined %>% 
    select(date, location, sky_cover) %>% 
    rename(datetime_utc = date)


############################ Sun Hours #######################################
# Suncalc manual
# https://cran.r-universe.dev/suncalc/doc/manual.html

# CVG coordinates
lat_CVG <- 39.0527
lon_CVG <- -84.6670
# SEA coordinates
lat_SEA <- 47.4503
lon_SEA <- -122.3087
# SMF coordinates
lat_SMF <- 38.6946
lon_SMF <- -121.5887

# Create initial dataframe of times to calculate hourly sun position
hours_utc3 <- seq(
    ymd_hms("2024-01-01 00:00:00", tz = "UTC"),
    ymd_hms("2025-01-01 00:00:00", tz = "UTC"),
    by = "1 hour")

# Combine locations, coordinates, and timezone
locations <- tibble(
    location = c("CVG", "SEA", "SMF"),
    lat = c(lat_CVG, lat_SEA, lat_SMF),
    lon = c(lon_CVG, lon_SEA, lon_SMF),
    tz  = c("US/Eastern", "US/Pacific", "US/Pacific")
)

# Expand locations and hours_utc3
hours_expand <- locations %>%
    tidyr::crossing(datetime_utc = hours_utc3)

# Create local time, filter for 2024
hours_local <- hours_expand %>%
    mutate(datetime_local = with_tz(datetime_utc, tz),
           date = datetime_utc) %>% #create date colummn for sun position
    filter(year(datetime_local) == 2024)
    

# Get sun position for CVG/SEA/SMF for 2024 (in UTC)
sunpos3 <- getSunlightPosition(
    data = hours_local)

# If sun positions is above horizon, then daylight (if not, then night)
sun_hr3 <- hours_local %>%
    mutate(sun = if_else(sunpos3$altitude > 0, 1L, 0L)) %>%
    select(datetime_local, datetime_utc, location, sun)


##################### Merging Cloud and Sun Dataframes ########################

# Left join dataframes in case of missing airport data
cloud_sun3 <- sun_hr3 %>% left_join(cloud_hr3, by = c("datetime_utc", "location"))
# Save data in case package breaks
#write.csv(cloud_sun3, "data/2_20260115_cloudsun3.csv")

# Filter out nighttime hours and NAs (minimal), 
clouds_onlysunlight3 <- cloud_sun3 %>% 
    filter(sun==1) %>% 
    filter(!is.na(sky_cover)) %>%
    mutate(sky_cover_round = round(sky_cover),
           sky_cover_08 = if_else(sky_cover_round > 8, 8, sky_cover_round)) #turn 9's into 8's bc functionally same for humans

# Explore classes by city
table(clouds_onlysunlight3$sky_cover_08[clouds_onlysunlight3$location=="CVG"], useNA = "always")
table(clouds_onlysunlight3$sky_cover_08[clouds_onlysunlight3$location=="SEA"], useNA = "always")
table(clouds_onlysunlight3$sky_cover_08[clouds_onlysunlight3$location=="SMF"], useNA = "always")

############################ Graphing #######################################

# Group classes by human readable names (and make a factor)
cloud3_graph <- clouds_onlysunlight3 %>%
    mutate(
        city_factor = factor(
            location,
            levels = c("SMF", "CVG", "SEA"),
            labels = c("Sacramento", "Cincinnati", "Seattle")),
        sky_cover_factor = factor(
            sky_cover_08,
            levels = 0:8,
            labels = c("Clear", 
                       "Few", "Few", 
                       "Scattered", "Scattered", 
                       "Broken", "Broken", "Broken", 
                       "Overcast")))

# Figure 1, bar plot of cloud cover hours
p1 <- ggplot(cloud3_graph, aes(x = sky_cover_factor, fill = sky_cover_factor)) +
    geom_bar(color = "black") +       
    facet_wrap(~ city_factor, nrow = 1) +
    scale_fill_brewer(palette = "Blues", direction = -1) + 
    labs(x = "Cloud Cover", y = "Number of Hours", title = "Total Daylight Cloud Cover Hours by City") +
    theme_minimal(base_size = 11) +
    theme(
        legend.position = "none",
        plot.title.position = "plot",
        panel.border = element_rect(
            color = "grey85",
            fill = NA,
            linewidth = 0.5))   

# Create binary cloud cover variable
cloud3_graph2 <- clouds_onlysunlight3 %>%
    mutate(
        city_factor = factor(
            location,
            levels = c("SMF", "CVG", "SEA"),
            labels = c("Sacramento", "Cincinnati", "Seattle")),
        sky_group = case_when(
            sky_cover_08 <= 4 ~ "Clear-Scattered",
            sky_cover_08 >= 5 ~ "Broken-Overcast"),
        sky_group_factor = factor(sky_group, levels = c("Clear-Scattered", "Broken-Overcast")))

# Figure 2, grouped bar plot with facet
p2 <- ggplot(cloud3_graph2, aes(x = sky_group_factor, fill = sky_group_factor)) +
    geom_bar(color = "black") +
    facet_wrap(~ city_factor, nrow = 1) +
    scale_fill_manual(
        values = c("Clear-Scattered" = "#08519C",
                   "Broken-Overcast" = "#EFF3FF")) +
    labs(x = "Cloud Cover", y = "Number of Hours", title = "Sacramento leads in clear skies") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "none",
          plot.title.position = "plot",
          panel.border = element_rect(
              color = "grey85",
              fill = NA,
              linewidth = 0.5))

# Save plots
ggsave("plots/2_20260115_p1.jpg", plot = p1, width = 10, height = 6, units = "in")
ggsave("plots/2_20260115_p2.jpg", plot = p2, width = 10, height = 6, units = "in")

############################ Stats #######################################


# Sun hours by location
city_sun_hr <- cloud_sun3 %>%
    group_by(location) %>%
    summarise(
        sun_hours = sum(sun, na.rm = TRUE),
        .groups = "drop")
print(city_sun_hr)





# Calculate days that are majority sunny
majority_clear_days <- clouds_onlysunlight %>%
    mutate(date_est = as.Date(datetime_est)) %>%
    group_by(date_est) %>%
    summarise(
        total_hours = sum(!is.na(sky_cover)),        # only count valid hours
        clear_hours = sum(sky_cover < 4, na.rm = TRUE), 
        is_majority_clear = (clear_hours / total_hours) >= 0.5,
        .groups = "drop") %>%
    summarise(n_days = sum(is_majority_clear))

print(majority_clear_days)

# Calculate days with at least on clear hour
days_with_clear_hour <- clouds_onlysunlight %>%
    mutate(date_est = as.Date(datetime_est)) %>%
    group_by(date_est) %>%
    summarise(
        has_clear_hour = any(sky_cover < 4, na.rm = TRUE),
        .groups = "drop") %>%
    summarise(n_days = sum(has_clear_hour))

print(days_with_clear_hour)
