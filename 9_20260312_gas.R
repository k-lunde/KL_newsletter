####--- All about cars

library(tidyverse)
library(scales)



options(scipen = 999)

# Define function to clean data
clean_eia <- function(file, region_name) {
    df <- read.csv(file, skip = 1) %>% # load csv but skip first row
        rename(date = Date)
    
    names(df)[2] <- "Total" # rename second column to "total"
    
    names(df)[3:ncol(df)] <- names(df)[3:ncol(df)] %>%
        stringr::str_remove("^.*from\\.") %>%       # remove prefix
        stringr::str_remove("\\.of.*$") %>%             # remove suffix
        stringr::str_replace_all("\\.", " ") %>%    # convert dots back to spaces
        stringr::str_trim()
    
    df %>%
        pivot_longer(      # pivot longer
            cols = -date,
            names_to = "country",
            values_to = "oil") %>%
        mutate(region = region_name) # add region
}

# Clean data
padd1 <- clean_eia("data/9_caroil/padd1-oil.csv", "PADD1")
padd2 <- clean_eia("data/9_caroil/padd2-oil.csv", "PADD2")
padd3 <- clean_eia("data/9_caroil/padd3-oil.csv", "PADD3")
padd4 <- clean_eia("data/9_caroil/padd4-oil.csv", "PADD4")
padd5 <- clean_eia("data/9_caroil/padd5-oil.csv", "PADD5")

# Combine regions
oil <- rbind(padd1, padd2, padd3, padd4, padd5) %>%
    mutate(region_name = case_when(region == "PADD1" ~ "East Coast",
                                   region == "PADD2" ~ "Midwest",
                                   region == "PADD3" ~ "Gulf Coast",
                                   region == "PADD4" ~ "Rocky Mountain",
                                   region == "PADD5" ~ "West Coast"))

# Data checks
str(oil)
table(oil$country)


################################## Graphing ###################################

####-- Plot 1: Regional imports of crude oil by country of origin

# Data for graphing
p1_oil <- oil %>% 
    filter(date == 2025) %>%
    filter(!country %in% c("Total", 
                           "X", 
                           "OPEC Countries", 
                           "Non OPEC Countries", 
                           "Persian Gulf Countries")) %>%
    drop_na() %>%
    mutate(world_region = case_when(
        country %in% c("Algeria", "Libya", "Tunisia", "Angola", "Cameroon", "Congo  Kinshasa", "Gabon", "Ghana",
                       "Ivory Coast  Cote d Ivore", "Nigeria", "Senegal") ~ "Africa",
        country %in% c("Argentina", "Brazil", "Colombia", "Ecuador", "Guyana",
                       "Guatemala", "Peru", "Venezuela", "Trinidad and Tobago") ~ "Latin America",
        country %in% c("Iraq", "Kuwait", "Saudi Arabia", "United Arab Emirates") ~ "Middle East",
        country %in% c("Kazakhstan") ~ "Central Asia",
        country %in% c("United Kingdom", "Norway") ~ "Europe",
        country == "Mexico" ~ "Mexico",
        country == "Canada" ~ "Canada",
        TRUE ~ "Other")) %>%
    group_by(region, world_region) %>%
    summarise(oil_sum = sum(oil, na.rm = TRUE), .groups = "drop") %>%
    mutate(region_name = case_when(region == "PADD1" ~ "East Coast",
                                   region == "PADD2" ~ "Midwest",
                                   region == "PADD3" ~ "Gulf Coast",
                                   region == "PADD4" ~ "Rocky Mountain",
                                   region == "PADD5" ~ "West Coast"),
           world_region = factor(world_region, levels = sort(unique(world_region), decreasing = TRUE)))



p1 <- ggplot(p1_oil) +
    geom_bar(aes(x = oil_sum, y = world_region, fill = region_name), 
             stat = "identity") +
    facet_wrap(~region_name, nrow = 5) +
    scale_fill_manual(values = c("East Coast" = "#1f9ed1", 
                                  "Midwest" = "#a8c63a", 
                                  "Gulf Coast" = "#d19a1f", 
                                  "Rocky Mountain" = "#e25563",
                                  "West Coast" = "#8a6fd1")) +
    labs(title = "United States Crude Oil Imports by Region of Origin 2025",
         x = "Crude Oil Imports (Annual-Thousand Barrels)",
         y = NULL) + 
    theme_minimal(base_size = 11) +
    theme(plot.title.position = "plot",
          legend.position = "none")

# Save plot
ggsave("plots/9_20260312_p1.jpg", plot = p1, width = 5, height = 8, units = "in")


####-- Plot 2: Regional imports over time


# Data for line graph
p2_oil <- oil %>%
    filter(!country %in% c("X", 
                           "OPEC Countries", 
                           "Non OPEC Countries")) %>%
    filter((country == "Total" & date %in% 1981:1992) | 
            (country != "Total" & date %in% 1993:2025)) %>%
    group_by(region_name, date) %>%
    summarize(oil_sum = sum(oil, na.rm = TRUE), .groups = "drop") %>%
    mutate(region_name = fct_relevel(region_name,
                                     "East Coast",
                                     "Midwest",
                                     "Gulf Coast",
                                     "Rocky Mountain",
                                     "West Coast"))

# Oil imports over time
p2 <- ggplot(p2_oil, aes(x = date, y = oil_sum, color = region_name)) +
    geom_line(linewidth = 0.75, na.rm = TRUE) +
    scale_color_manual(values = c("East Coast" = "#1f9ed1", 
                                 "Midwest" = "#a8c63a", 
                                 "Gulf Coast" = "#d19a1f", 
                                 "Rocky Mountain" = "#e25563",
                                 "West Coast" = "#8a6fd1")) +
    labs(
        title = "United States Regional Crude Oil Imports (1981-2025)",
        x = NULL,
        y = "Crude Oil Imports (Annual-Thousand Barrels)",
        color = NULL) +
    theme_minimal(base_size = 11) +
    theme(plot.title.position = "plot")

# Save plot
ggsave("plots/9_20260312_p2.jpg", plot = p2, width = 7, height = 4, units = "in")



####-- Plot 3: 


p3_oil <- oil %>%
    filter(!country %in% c("Total", 
                           "X", 
                           "OPEC Countries", 
                           "Non OPEC Countries", 
                           "Persian Gulf Countries")) %>%
    filter(oil > 0) %>%
    mutate(world_region = case_when(
        country %in% c("Algeria","Angola","Benin","Cameroon","Chad","Congo  Kinshasa",
                       "Congo  Brazzaville","Egypt","Equatorial Guinea","Gabon","Ghana",
                       "Guinea","Ivory Coast  Cote d Ivore","Libya","Mauritania",
                       "Nigeria","Senegal","South Africa","South Sudan","Tunisia") ~ "Africa",
        country %in% c("Argentina","Bahama Islands","Barbados","Belize","Bolivia","Brazil",
                       "Chile","Colombia","Ecuador","Guatemala","Guyana",
                       "Panama","Peru","Trinidad and Tobago","Venezuela", "Virgin Islands") ~ "Latin America",
        country %in% c("Azerbaijan","Iran","Iraq","Kazakhstan","Kuwait","Kyrgyzstan",
                       "Oman","Qatar","Saudi Arabia","Syria",
                       "United Arab Emirates","Yemen") ~ "Middle East",
        country %in% c("Brunei","China","India","Indonesia","Malaysia",
                       "Singapore","Thailand","Vietnam", "Kazakhstan") ~ "Asia",
        country %in% c("Albania","Belarus","Denmark","Estonia","Georgia","Italy",
                       "Netherlands","Norway","Russia","Spain","Sweden",
                       "United Kingdom") ~ "Europe",
        country %in% c("Australia","New Zealand","Papua New Guinea") ~ "Oceania",
        country == "Mexico" ~ "Mexico",
        country == "Canada" ~ "Canada",
        TRUE ~ "Other")) %>%
    group_by(date, world_region) %>%
    summarise(oil_sum = sum(oil, na.rm = TRUE), .groups = "drop")



p3 <- ggplot(p3_oil) +
    geom_area(aes(x = date, y = oil_sum, fill = reorder(world_region, -oil_sum))) +
    scale_fill_brewer(palette = "Set3") +
    labs(title = "United States Crude Oil Imports by Region of Origin (1993-2025)",
         x = NULL,
         y = "Crude Oil Imports (Annual-Thousand Barrels)",
         fill = NULL) +
    theme_minimal(base_size = 11) +
    theme(plot.title.position = "plot")


# Save plot
ggsave("plots/9_20260312_p3.jpg", plot = p3, width = 6, height = 4, units = "in")











##### Unused --- individual plots for each PADD


# Imports
p1_padd1 <- p1_oil %>% 
    filter(region == "PADD1") %>%
    ggplot() +
    geom_bar(aes(x = oil_sum, y = reorder(world_region, oil_sum)), 
             stat = "identity", fill = "#3380a2") +
    xlim(0, 150000) +
    labs(title = "East Coast Crude Oil Imports by Region of Origin 2025",
         x = "Crude Oil Imports (Annual-Thousand Barrels)",
         y = NULL) + 
    theme_minimal(base_size = 11) +
    theme(plot.title.position = "plot")

p1_padd2 <- p1_oil %>% 
    filter(region == "PADD2") %>%
    ggplot() +
    geom_bar(aes(x = oil_sum, y = reorder(world_region, oil_sum)), 
             stat = "identity", fill = "#92a753") +
    xlim(0, 1005000) +
    labs(title = "Midwest Crude Oil Imports by Region of Origin 2025",
         x = "Crude Oil Imports (Annual-Thousand Barrels)",
         y = NULL) + 
    theme_minimal(base_size = 11) +
    theme(plot.title.position = "plot")

p1_padd3 <- p1_oil %>% 
    filter(region == "PADD3") %>%
    ggplot() +
    geom_bar(aes(x = oil_sum, y = reorder(world_region, oil_sum)), 
             stat = "identity", fill = "#a27d33") +
    xlim(0, 152000) +
    labs(title = "Gulf Coast Crude Oil Imports by Region of Origin 2025",
         x = "Crude Oil Imports (Annual-Thousand Barrels)",
         y = NULL) + 
    theme_minimal(base_size = 11) +
    theme(plot.title.position = "plot")

p1_padd4 <- p1_oil %>% 
    filter(region == "PADD4") %>%
    ggplot() +
    geom_bar(aes(x = oil_sum, y = reorder(world_region, oil_sum)), 
             stat = "identity", fill = "#b55c66") +
    labs(title = "Rocky Mountain Crude Oil Imports by Region of Origin 2025",
         x = "Crude Oil Imports (Annual-Thousand Barrels)",
         y = NULL) + 
    xlim(0, 150000) +
    theme_minimal(base_size = 11) +
    theme(plot.title.position = "plot")

p1_padd5 <- p1_oil %>% 
    filter(region == "PADD5") %>%
    ggplot() +
    geom_bar(aes(x = oil_sum, y = reorder(world_region, oil_sum)), 
             stat = "identity", fill = "#746395") +
    xlim(0, 150000) +
    labs(title = "West Coast Crude Oil Imports by Region of Origin 2025",
         x = "Crude Oil Imports (Annual-Thousand Barrels)",
         y = NULL) + 
    theme_minimal(base_size = 11) +
    theme(plot.title.position = "plot")

# Save plot
ggsave("plots/9_20260312_p1-padd1.jpg", plot = p1_padd1, width = 5, height = 4, units = "in")
ggsave("plots/9_20260312_p1-padd2.jpg", plot = p1_padd2, width = 5, height = 1.4, units = "in")
ggsave("plots/9_20260312_p1-padd3.jpg", plot = p1_padd3, width = 5, height = 4, units = "in")
ggsave("plots/9_20260312_p1-padd4.jpg", plot = p1_padd4, width = 5, height = 1.4, units = "in")
ggsave("plots/9_20260312_p1-padd5.jpg", plot = p1_padd5, width = 5, height = 4, units = "in")




