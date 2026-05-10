####--- All about cars
# Car weight over time

library(tidyverse)
library(janitor)
library(ggridges)
library(scales)

# Load data and clean column names
car_man <- read_csv("data/8_20260305_epa_autotrends_bymanufacturer.csv", na = "-") %>% clean_names()


# Create dataframes for graphing car vs truck data
breaks <- c(2008, 2010, 2015, 2020, 2025)
breaks_lab <- c("2008-2009", "2010-2014", "2015-2019", "2020-2024")
carM_c <- car_man %>% 
    filter(regulatory_class == "Car") %>% 
    filter(vehicle_type != "All Car") %>%
    filter(manufacturer != "All") %>% 
    drop_na(footprint_sq_ft) %>%
    mutate(
        year = case_when(model_year == "Prelim. 2024" ~ "2024",
                         TRUE ~ model_year),
        year_group = cut(
            as.numeric(year),
            breaks = breaks,
            labels = breaks_lab,
            include.lowest = TRUE,
            right = FALSE)) %>%
    drop_na(model_year)

carM_t <- car_man %>% 
    filter(regulatory_class == "Truck") %>% 
    filter(vehicle_type != "All Truck") %>%
    filter(manufacturer != "All") %>% 
    drop_na(footprint_sq_ft) %>%
    mutate(
        year = case_when(model_year == "Prelim. 2024" ~ "2024",
                         TRUE ~ model_year),
        year_group = cut(
            as.numeric(year),
            breaks = breaks,
            labels = breaks_lab,
            include.lowest = TRUE,
            right = FALSE)) %>%
    drop_na(model_year)


################################## Graphing ###################################

# Car Size distribution over time
p1 <- ggplot() +
    geom_density_ridges(data = carM_t, aes(x = footprint_sq_ft, 
                                           y = year_group, 
                                           fill = "Truck"),
                        stat = "binline",
                        alpha = 0.4, scale = .85) + 
    geom_density_ridges(data = carM_c, aes(x = footprint_sq_ft, 
                                           y = year_group,
                                           fill = "Car"),
                        stat = "binline",
                        alpha = 0.6, scale = .85) +
    scale_y_discrete(expand = c(0, .1)) +
    scale_fill_manual(labels = c("Car", "Truck"), values = c("#57A0D2", "#FF6000")) +
    labs(
        title = "U.S. vehicle footprints have increased in the last two decadees",
        subtitle = "Truck and car sq footage from 2008-2024",
        x = "Vehicle Footprint (sq ft)",
        y = NULL,
        fill = "Vehicle Type") +
    theme_minimal(base_size = 11) +
    theme(plot.title.position = "plot",
          legend.position = "bottom",
          legend.box.background = element_rect(colour = "black"))



####------ Car size comparisons 2008/2023

car_size <- tribble(
    ~category, ~width, ~height, ~label,
    "Smallest Vehicles", 9,    18,    "Standard US Parking Space",
    "Smallest Vehicles", 5.71, 15.03, "2008 Subaru Impreza",
    "Smallest Vehicles", 5.83, 15.23, "2023 Subaru Impreza",
    "Largest Vehicles",  9,    18,    "Standard US Parking Space",
    "Largest Vehicles",  6.07, 18.28, "2008 Nissan Frontier",
    "Largest Vehicles",  6.77, 20.11, "2023 Chevy Silverado") %>%
    mutate(
        category = factor(
            category,
            levels = c("Smallest Vehicles", "Largest Vehicles")),
        xmin = -width / 2,
        xmax =  width / 2,
        ymin = -height / 2,
        ymax =  height / 2,
        col = case_when(
            grepl("^2008", label) ~ "#008837",
            grepl("^2023", label) ~ "#c2a5cf",
            TRUE ~ "grey20"))


vehicle_labels <- car_size %>%
    filter(label != "Standard US Parking Space") %>%
    mutate(
        x = xmin + 0.05 * (xmax - xmin),
        y = case_when(
            grepl("^2008", label) ~ ymin + 0.08 * (ymax - ymin), 
            grepl("^2023", label) ~ ymin - 0.05 * (ymax - ymin)))


p2 <- ggplot() +
    geom_rect(
        data = car_size, 
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, color = col),
        fill = NA,
        linewidth = 1.25,
        alpha = 0.1,
        show.legend = FALSE) +
    geom_text(
        data = vehicle_labels,
        aes(x = x, y = y, label = label, color = col),
        hjust = 0,
        size = 3,
        show.legend = FALSE) +
    annotate("text", x = -5.2, y = 0,
        label = "Common US Parking Space (9x18ft)",
        angle = 90, color = "grey20", alpha = 0.6, size = 4) +
    labs(title = "Trucks no longer fit in standard parking spaces",
         subtitle = "Smallest and largest cars available on US market for 2008 and 2023") +
    scale_color_identity() +
    facet_wrap(~category, nrow = 1) +
    coord_fixed() +
    theme_minimal(base_size = 13) +
    theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_text(face = "bold")
    )


# Save plots
ggsave("plots/11_20260509_p1.jpg", plot = p1, width = 6, height = 6, units = "in")
ggsave("plots/11_20260509_p2.jpg", plot = p2, width = 8, height = 6, units = "in")






#################### Exploration ###########################

#Check car sizes

car_m_large <- carM_c %>% arrange(-footprint_sq_ft) %>% select(c(manufacturer, model_year, regulatory_class, vehicle_type, footprint_sq_ft))
truck_m_large <- carM_t %>% arrange(-footprint_sq_ft) %>% select(c(manufacturer, model_year, regulatory_class, vehicle_type, footprint_sq_ft))

print(car_m_large, n = 30)
print(truck_m_large, n = 30)
tail(car_m_large)
tail(truck_m_large)


# Find largest and smallest sqft
# Largest vehicles
largest <- car_man %>%
    select(c(manufacturer, model_year, regulatory_class, vehicle_type, footprint_sq_ft)) %>%
    filter(model_year %in% c("2008", "2023")) %>%
    group_by(model_year) %>%
    slice_max(footprint_sq_ft, n = 1, with_ties = TRUE)

# Smallest vehicles
smallest <- car_man %>%
    select(c(manufacturer, model_year, regulatory_class, vehicle_type, footprint_sq_ft)) %>%
    filter(model_year %in% c("2008", "2023")) %>%
    group_by(model_year) %>%
    slice_min(footprint_sq_ft, n = 1, with_ties = TRUE)

largest
smallest


