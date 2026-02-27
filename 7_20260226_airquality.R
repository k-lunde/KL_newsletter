####-- Air Quality

# Clear environment if needed
#rm(list=ls(all=TRUE))

#install.packages(c("tidyverse", "usmap"))
library(tidyverse)
library(usmap)

# Turn off scientific notation
options(scipen=999)

############################ Load Data #######################################

# Data from 2020 EPA National Emissions Inventory
# https://www.epa.gov/air-emissions-inventories/2020-nei-supporting-data-and-summaries

# Get file path
files <- list.files(path = "data/67_airq", pattern = "\\.csv$", full.names = TRUE)
# Remove scc data
files <- files[files != "data/67_airq/SCCDownload-2026-0212-141720.csv"]

# Create empty list for files
airq_list <- list()

# Read files in a loop
for (file in files) {
    data <- read.csv(file)
    airq_list[[file]] <- data
}

############################ Clean Data #######################################

# Examine column names if needed
#map(airq_list, names)

# List of columns to keep 
keep_col <- c("total.emissions", "emissions.uom", "pollutant.code", "pollutant.desc",
              "state", "fips.state.code", "fips.code", "county", "epa.region.code",
              "scc")
    
# Bind datasets
airq <- airq_list %>%
    map(~ select(.x, all_of(keep_col))) %>%
    bind_rows()

# Read in SCC table
scc_table <- read.csv("data/67_airq/SCCDownload-2026-0212-141720.csv") %>%
    rename(scc = SCC) %>%
    select("scc", "data.category",
           "scc.level.one", "scc.level.two", "scc.level.three", "scc.level.four",
           "sector", "short.name", 
           "tier.1.description", "tier.2.description", "tier.3.description")

# Create mask for valid states
valid_states <- c(state.abb, "DC")

############################ NOX Plots #######################################

# Calculate NOX emissions by state
NOX_state <- airq %>%
    filter(pollutant.code == "NOX") %>%
    group_by(state) %>%
    summarise(emissions = sum(total.emissions, na.rm = TRUE),
              .groups = "drop") %>%
    filter(state %in% valid_states)

# Calculate top NOX emissions by state
NOX_top1 <- airq %>%
    filter(pollutant.code == "NOX") %>%
    group_by(state, scc) %>%
    summarise(emissions = sum(total.emissions, na.rm = TRUE),
              .groups = "drop") %>%
    group_by(state) %>%
    mutate(pct = round(emissions / sum(emissions), 10),
           scc = as.character(scc)) %>%
    slice_max(order_by = pct, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    filter(state %in% valid_states) %>%
    left_join(scc_table, by = "scc")

# Plot NOX total emissions by state
NOX_total <- plot_usmap(data = NOX_state, values = "emissions") +
    scale_fill_gradient(low = "lightyellow", high = "red", name = "Emissions (Tons)") +
    labs(title = "2020 Total NOx Emissions by State") +
    theme(plot.title.position = "plot",
          legend.position = "right")

# Plot NOX emissions source by state
NOX_source <- plot_usmap(data = NOX_top1, values = "sector") +
    scale_fill_discrete(name = "Top Pollution Source") +
    labs(title = "2020 Top NOx Emissions Source by State") +
    theme(plot.title.position = "plot",
          legend.position = "right")

# Save plots
ggsave("plots/7_20260226_p1.jpg", plot = NOX_total, width = 6, height = 4, units = "in")
ggsave("plots/7_20260226_p2.jpg", plot = NOX_source, width = 7, height = 4, units = "in")

############################ PM2.5 Plots #######################################v

# Calculate PM25 emissions by state
PM25_state <- airq %>%
    filter(pollutant.code == "PM25-PRI" | pollutant.code == "DIESEL-PM25") %>%
    group_by(state) %>%
    summarise(emissions = sum(total.emissions, na.rm = TRUE),
              .groups = "drop") %>%
    filter(state %in% valid_states)

# Calculate top PM25 emissions by state
PM25_top1 <- airq %>%
    filter(pollutant.code == "PM25-PRI" | pollutant.code == "DIESEL-PM25") %>%
    group_by(state, scc) %>%
    summarise(emissions = sum(total.emissions, na.rm = TRUE),
              .groups = "drop") %>%
    group_by(state) %>%
    mutate(pct = round(emissions / sum(emissions), 10),
           scc = as.character(scc)) %>%
    slice_max(order_by = pct, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    filter(state %in% valid_states) %>%
    left_join(scc_table, by = "scc")


# Plot PM25 total emissions by state
PM25_total <- plot_usmap(data = PM25_state, values = "emissions") +
    scale_fill_gradient(low = "lightyellow", high = "red", name = "Emissions (Tons)") +
    scale_color_manual(values = colors, name = "Top Source") +
    labs(title = "2020 Total PM2.5 Emissions by State") +
    theme(plot.title.position = "plot",
          legend.position = "right")

# Plot PM25 emissions source by state
PM25_source <- plot_usmap(data = PM25_top1, values = "sector") +
    scale_fill_discrete(name = "Top Pollution Source") +
    labs(title = "2020 Top PM2.5 Emissions Source by State") +
    theme(plot.title.position = "plot",
          legend.position = "right")

# Save plot
ggsave("plots/7_20260226_p3.jpg", plot = PM25_total, width = 6, height = 4, units = "in")
ggsave("plots/7_20260226_p4.jpg", plot = PM25_source, width = 7, height = 4, units = "in")
