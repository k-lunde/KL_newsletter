####--- All about cars

library(tidyverse)
library(janitor)
library(ggridges)
library(scales)

# Load data and clean column names
car_man <- read_csv("data/8_20260305_epa_autotrends_bymanufacturer.csv", na = "-") %>% clean_names()


# Create dataframes for graphing car vs truck data
breaks <- seq(1975, 2025, by = 5) # for grouping every 5 years
carM_c <- car_man %>% 
    filter(regulatory_class == "Car") %>% 
    filter(vehicle_type != "All Car") %>%
    filter(manufacturer != "All") %>% 
    drop_na(weight_lbs) %>%
    mutate(
        year = case_when(model_year == "Prelim. 2024" ~ "2024",
                         TRUE ~ model_year),
        year_group = cut(
            as.numeric(year),
            breaks = breaks,
            labels = paste0(breaks[-length(breaks)], "-", breaks[-1] - 1),
            include.lowest = TRUE,
            right = FALSE))
    
carM_t <- car_man %>% 
    filter(regulatory_class == "Truck") %>% 
    filter(vehicle_type != "All Truck") %>%
    filter(manufacturer != "All") %>% 
    drop_na(weight_lbs) %>%
    mutate(
        year = case_when(model_year == "Prelim. 2024" ~ "2024",
                         TRUE ~ model_year),
        year_group = cut(
            as.numeric(year),
            breaks = breaks,
            labels = paste0(breaks[-length(breaks)], "-", breaks[-1] - 1),
            include.lowest = TRUE,
            right = FALSE))

mean_car <- carM_c %>%
    group_by(year_group) %>%
    summarize(mean_weight = mean(weight_lbs, na.rm = TRUE))
mean_truck <- carM_t %>%
    group_by(year_group) %>%
    summarize(mean_weight = mean(weight_lbs, na.rm = TRUE))

all_mean <- car_man %>%
    filter(manufacturer == "All") %>%
    filter(regulatory_class == "Car" | regulatory_class == "Truck") %>% 
    filter(vehicle_type == "All Car" | vehicle_type == "All Truck") %>% 
    group_by(model_year, regulatory_class) %>%
    summarise(mean_weight = mean(weight_lbs, na.rm=TRUE)) %>%
    mutate(year = case_when(model_year == "Prelim. 2024" ~ "2024", TRUE ~ model_year))

################################## Graphing ###################################

# Mean Weight
p1 <- ggplot(all_mean) +
    geom_line(aes(x=year, y=mean_weight, 
                  group=regulatory_class, color=regulatory_class)) +
    scale_x_discrete(breaks = breaks_pretty(n = 5)) +
    scale_color_manual(values = c(
        "Car" = "#57A0D2",
        "Truck" = "#FF6000")) +
    labs(title = "Average Weight of American Passenger Vehicles (1975-2024)",
         x = NULL,
         y = "Mean Weight (lbs)",
         color = NULL) +
    theme_minimal() +
    theme(plot.title.position = "plot",
          legend.position = "bottom",
          legend.box.background = element_rect(colour = "black"))


# Car Weight over time
p2 <- ggplot(carM_c) +
    geom_density_ridges(aes(x = weight_lbs, 
                            y = year_group),
                        stat = "binline",
                        alpha = 0.6, fill = "#57A0D2", scale = .8) + 
    geom_point(aes(x = weight_lbs, y = year_group, color = vehicle_type),
               shape = "|", alpha = 0.7, size = 3, 
               position = position_nudge(y = 0.05)) +
    scale_color_manual(values = c(
        "Car SUV" = "#E41A1C",
        "Sedan/Wagon" = "#EBBD05")) +
    labs(
        title = "American Car Weight (1975-2024)",
        x = "Weight (lbs)",
        y = NULL,
        color = "Vehicle Type") +
    theme_minimal(base_size = 11) +
    theme(plot.title.position = "plot",
          legend.position = "bottom",
          legend.box.background = element_rect(colour = "black"))


# Truck Weight over time
p3 <- ggplot(carM_t) +
    geom_density_ridges(aes(x = weight_lbs, 
                            y = year_group),
                        stat = "binline",
                        alpha = 0.6, fill = "#FF6000", scale = .8,) + 
    geom_point(aes(x = weight_lbs, y = year_group, color = vehicle_type),
               shape = "|", alpha = 0.7, size = 3, 
               position = position_nudge(y = 0.05)) +
    scale_color_manual(values = c(
        "Truck SUV" = "#E41A1C",
        "Minivan/Van" = "#377EB8",
        "Pickup" = "#4DAF4A")) +
    labs(
        title = "American Passenger Truck Weight (1975-2024)",
        x = "Weight (lbs)",
        y = NULL,
        color = "Vehicle Type") +
    theme_minimal(base_size = 11) +
    theme(plot.title.position = "plot",
          legend.position = "bottom",
          legend.box.background = element_rect(colour = "black"))

# Save plots
ggsave("plots/10_20260326_p1.jpg", plot = p1, width = 6, height = 5, units = "in")
ggsave("plots/10_20260326_p2.jpg", plot = p2, width = 7, height = 8, units = "in")
ggsave("plots/10_20260326_p3.jpg", plot = p3, width = 7, height = 8, units = "in")



