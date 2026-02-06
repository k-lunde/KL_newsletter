####-- Air Quality

# Clear environment if needed
#rm(list=ls(all=TRUE))

#install.packages(c("tidyverse"))
library(tidyverse)

############################ Data Cleaning #######################################

# Read in data
airq_pre <- read.csv("data/5_20260205_airquality.csv")

# Clean data
airq <- airq_pre %>%
    pivot_longer(
        cols = starts_with("X"),
        names_to = "year",
        values_to = "value") %>%
    mutate(year = as.numeric(str_sub(year, 2)),
           pollutant_m = paste(pollutant, trend_statistic, sep = " "))

############################ Graphing #######################################

# Separate data for graphing
airq_c <- airq %>% filter(CBSA == 17140)
airq_o3 <- airq %>% filter(CBSA == 17140 & pollutant == "O3")
airq_pm2 <- airq %>% filter(CBSA == 17140 & grepl("PM2.5", pollutant))


# Ozone plot
p1 <- ggplot() +
    geom_line(data = airq_o3, mapping = aes(x = year, y = value, color = pollutant_m),
              linewidth = 1, color = "#993404") +
    geom_hline(yintercept = 0.070, color = "black", linetype = "dashed", size = .5) +
labs(title = "Cincinnati Ground Level Ozone Pollution (2000-2023)",
     x = NULL,
     y = "Ozone Concentration (ppm)",
     color = "Pollutant") +
    annotate("text", x=2002, y=0.0695, label= "National Air Quality Standard") +
    theme_minimal(base_size = 14) +
    theme(plot.title.position = "plot")

# PM 2.5 plot
p2 <- ggplot() +
    geom_line(data = airq_pm2, mapping = aes(x = year, y = value, color = pollutant_m),
              linewidth = 1) +
    scale_color_manual(values = c("PM2.5 98th Percentile" = "#253494",
                                  "PM2.5 Weighted Annual Mean" = "#1c9099")) +

    geom_hline(yintercept = 35, color = "#253494", linetype = "dashed", size = .5) +
    geom_hline(yintercept = 9, color = "#1c9099", linetype = "dashed", size = .5) +
    labs(title = "Cincinnati PM2.5 Pollution (2000-2023)",
         x = NULL,
         y = expression("PM 2.5 Concentrations" (µg/m^3)),
         color = NULL) +
    annotate("text", x=2021, y=34, label= "National Air Quality Standard", col = "#253494") +
    annotate("text", x=2002, y=10, label= "National Air Quality Standard", col = "#1c9099") +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom",
        plot.title.position = "plot")
    
# Save plots
ggsave("plots/5_20260205_p1.jpg", plot = p1, width = 10, height = 8, units = "in")
ggsave("plots/5_20260205_p2.jpg", plot = p2, width = 10, height = 8, units = "in")


