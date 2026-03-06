####--- All about cars

library(tidyverse)
library(janitor)
library(scales)
library(ggpubr)

# Load data and clean column names
car_man <- read_csv("data/8_20260305_epa_autotrends_bymanufacturer.csv", na = "-") %>% clean_names()
car_man$model_year[car_man$model_year == "Prelim. 2024"] <- 2024

# Create dataframe for plotting
car_all <- car_man %>%
    filter(manufacturer == "All") %>%
    filter(regulatory_class == "Car" | regulatory_class == "Truck") %>% 
    filter(vehicle_type == "All Car" | vehicle_type == "All Truck")


# Car/Truck MPG over time
p1 <- ggplot() +
    geom_line(car_all, mapping = aes(x = model_year, 
                                     y = real_world_mpg,
                                     group = regulatory_class,
                                     color = regulatory_class), 
              na.rm = TRUE, 
              stat = "identity") + 
    scale_x_discrete(breaks = breaks_pretty(n = 5)) +
    labs(
        title = "Average Light Duty Vehicle Fuel Economy (1975-2024)",
        x = NULL,
        y = "Fuel Economy (MPG)",
        color = NULL) +
    theme_classic(base_size = 11) +
    theme(plot.title.position = "plot",
          panel.grid.major = element_line(color = "grey85", linewidth = 0.4))

# Car/Truck CO2 over time
p2 <- ggplot() +
    geom_line(car_all, mapping = aes(x = model_year, 
                                     y = real_world_co2_g_mi,
                                     group = regulatory_class,
                                     color = regulatory_class),
              na.rm = TRUE,
              stat = "identity") +
    scale_x_discrete(breaks = breaks_pretty(n = 5)) +
    labs(
        title = "Average Light Duty Vehicle CO2 Emissions (1975-2024)",
        x = NULL,
        y = "CO2 Emissions (g/mi)",
        color = NULL) +
    theme_classic(base_size = 11) +
    theme(plot.title.position = "plot",
          panel.grid.major = element_line(color = "grey85", linewidth = 0.4))

# For saving in side by side format
# p1 <- ggarrange(
#     p1.1, p1.2,
#     ncol = 2, nrow = 1,
#     common.legend = TRUE,
#     legend = "right")

# Save plots
ggsave("plots/8_20260305_p1.jpg", plot = p1, width = 5, height = 4, units = "in")
ggsave("plots/8_20260305_p2.jpg", plot = p2, width = 5, height = 4, units = "in")






