####-- Air Quality

# Clear environment if needed
#rm(list=ls(all=TRUE))

#install.packages(c("tidyverse"))
library(tidyverse)
library(ggalluvial)
library(forcats)

# Turn off scientific notation
options(scipen=999)

############################ Load Data #######################################

# Read in data
airq_road <- read.csv("data/onroad_5.csv")
airq_nonroad <- read.csv("data/nonroad_5.csv")
airq_facility <- read.csv("data/point_5.csv")
airq_nonpoint <- read.csv("data/esg_cty_scc_23952.csv")

# Read in SCC table
scc_table <- read.csv("data/SCCDownload-2026-0212-141720.csv") %>%
    rename(scc = SCC)

# Filter for state and county
airq_road_OH <- airq_road %>% filter(state == "OH" & county == "Hamilton")
airq_nonroad_OH <- airq_nonroad %>% filter(state == "OH"& county == "Hamilton")
airq_facility_OH <- airq_facility %>% filter(state == "OH"& county == "Hamilton")
airq_nonpoint_OH <- airq_nonpoint %>% filter(state == "OH"& county == "Hamilton")

# IF need to clear space
#rm(airq_road, airq_nonroad, airq_facility, airq_nonpoint)

############################ NOx Emissions ####################################
#################################################################################

# Filter for pollutants:
#   Nitrogen Oxides 
OH_road_NOX <- airq_road_OH %>% 
    filter(pollutant.code == "NOX")
OH_nonroad_NOX <- airq_nonroad_OH %>% 
    filter(pollutant.code == "NOX")
OH_facility_NOX <- airq_facility_OH %>% 
    filter(pollutant.code == "NOX")
OH_nonpoint_NOX <- airq_nonpoint_OH %>% 
    filter(pollutant.code == "NOX")

###########----- Function to sum  emissions by source
summarise_wsource <- function(df, label) {
    df %>%
        group_by(scc) %>%
        summarise(
            emissions = sum(total.emissions, na.rm = TRUE),
            .groups = "drop") %>%
        mutate(
            typeMS = label,
            pct = round(emissions / sum(emissions), 3),
            scc = as.character(scc)) %>%
        left_join(scc_table, by = "scc")
}

road_sourceNOX      <- summarise_wsource(OH_road_NOX, "Road")
nonroad_sourceNOX   <- summarise_wsource(OH_nonroad_NOX, "Nonroad")
facility_sourceNOX  <- summarise_wsource(OH_facility_NOX, "Facility")
nonpoint_sourceNOX  <- summarise_wsource(OH_nonpoint_NOX, "Nonpoint")

# Combine data and filter columns
srcNOX_comb <- rbind(road_sourceNOX, nonroad_sourceNOX, facility_sourceNOX, nonpoint_sourceNOX) %>%
    filter(pct > 0.01) %>%
    select(c(emissions, pct, scc, data.category, 
             scc.level.one, scc.level.two, scc.level.three, scc.level.four,
             sector, short.name, 
             tier.1.description, tier.2.description, tier.3.description))

############################ Graphing #######################################

p_srcNOX_comb <- srcNOX_comb %>%
    mutate(sector = fct_reorder(sector, emissions, .fun = sum, .desc = FALSE))

p1 <- ggplot(p_srcNOX_comb, aes(x = emissions, y = sector)) +
    geom_bar(stat = "identity", position = "stack", fill = "#993404") +
    labs(title = "Hamilton County NOx Pollutant Emissions by Source (2020)",
         x = "NOx emissions (Tons)", y = NULL) +
    theme_minimal(base_size = 12) +
    theme(plot.title.position = "plot",
          plot.margin = margin(8, 15, 8, 8))

ggsave("plots/6_20260212_p1.jpg", plot = p1, width = 8, height = 6, units = "in")


############################ VOC Emissions ####################################
#################################################################################

# Filter for pollutants:
#   Volatile Organic Compounds
OH_road_VOC <- airq_road_OH %>% 
    filter(pollutant.code == "VOC")
OH_nonroad_VOC <- airq_nonroad_OH %>% 
    filter(pollutant.code == "VOC")
OH_facility_VOC <- airq_facility_OH %>% 
    filter(pollutant.code == "VOC")
OH_nonpoint_VOC <- airq_nonpoint_OH %>% 
    filter(pollutant.code == "VOC")

# Sum by source
road_sourceVOC      <- summarise_wsource(OH_road_VOC, "Road")
nonroad_sourceVOC   <- summarise_wsource(OH_nonroad_VOC, "Nonroad")
facility_sourceVOC  <- summarise_wsource(OH_facility_VOC, "Facility")
nonpoint_sourceVOC  <- summarise_wsource(OH_nonpoint_VOC, "Nonpoint")

# Combine data and filter columns
srcVOC_comb <- rbind(road_sourceVOC, nonroad_sourceVOC, facility_sourceVOC, nonpoint_sourceVOC) %>%
    filter(pct > 0.01) %>%
    select(c(emissions, pct, scc, data.category, 
             scc.level.one, scc.level.two, scc.level.three, scc.level.four,
             sector, short.name, 
             tier.1.description, tier.2.description, tier.3.description))

############################ Graphing #######################################

p_srcVOC_comb <- srcVOC_comb %>%
    mutate(sector = fct_reorder(sector, emissions, .fun = sum, .desc = FALSE))

p2 <- ggplot(p_srcVOC_comb, aes(x = emissions, y = sector)) +
    geom_bar(stat = "identity", position = "stack", fill = "#fd8d3c") +
    labs(title = "Hamilton County VOC Pollutant Emissions by Source (2020)",
         x = "VOC emissions (Tons)", y = NULL) +
    theme_minimal(base_size = 12) +
    theme(plot.title.position = "plot")

ggsave("plots/6_20260212_p2.jpg", plot = p2, width = 8, height = 6, units = "in")



############################ PM2.5 Emissions ####################################
#################################################################################

############################ Data Cleaning #######################################

# Filter for pollutants:
#   PM25-Primary from certain diesel engines
#   PM2.5 Primary (Filt + Cond)
OH_road_25 <- airq_road_OH %>% 
    filter(pollutant.code == "PM25-PRI" | pollutant.code == "DIESEL-PM25")
OH_nonroad_25 <- airq_nonroad_OH %>% 
    filter(pollutant.code == "PM25-PRI" | pollutant.code == "DIESEL-PM25")
OH_facility_25 <- airq_facility_OH %>% 
    filter(pollutant.code == "PM25-PRI" | pollutant.code == "DIESEL-PM25")
OH_nonpoint_25 <- airq_nonpoint_OH %>% 
    filter(pollutant.code == "PM25-PRI" | pollutant.code == "DIESEL-PM25")

# Sum PM 2.5 emissions by source
road_source25      <- summarise_wsource(OH_road_25, "Road")
nonroad_source25   <- summarise_wsource(OH_nonroad_25, "Nonroad")
facility_source25  <- summarise_wsource(OH_facility_25, "Facility")
nonpoint_source25  <- summarise_wsource(OH_nonpoint_25, "Nonpoint")

# Combine data and filter columns
src25_comb <- rbind(road_source25, nonroad_source25, facility_source25, nonpoint_source25) %>%
    filter(pct > 0.01) %>%
    select(c(emissions, pct, scc, data.category, 
             scc.level.one, scc.level.two, scc.level.three, scc.level.four,
             sector, short.name, 
             tier.1.description, tier.2.description, tier.3.description))

############################ Graphing #######################################

p_src25_comb <- src25_comb %>%
    mutate(sector = fct_reorder(sector, emissions, .fun = sum, .desc = FALSE))


p3 <- ggplot(p_src25_comb, aes(x = emissions, y = sector)) +
    geom_bar(stat = "identity", position = "stack", fill = "#253494") +
    labs(title = "Hamilton County PM2.5 Pollutant Emissions by Source (2020)",
         x = "PM2.5 emissions (Tons)", y = NULL) +
    theme_minimal(base_size = 12) +
    theme(plot.title.position = "plot")

# Save plot
ggsave("plots/6_20260212_p3.jpg", plot = p3, width = 8, height = 6, units = "in")

