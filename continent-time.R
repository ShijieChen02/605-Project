library(readr)
library(ggplot2)
library(dplyr)
lake_data <- read_csv("glcp.csv") 

avg_area_by_continent_year <- lake_data %>%
  group_by(continent, year) %>%
  summarise(avg_total_km2 = mean(total_km2, na.rm = TRUE))

ggplot(avg_area_by_continent_year, aes(x = year, y = avg_total_km2, color = continent)) +
  geom_line() +
  labs(title = "Annual Average Lake Area by Continent", x = "Year", y = "Average Total Area (km²)")

min_area_year_by_continent <- avg_area_by_continent_year %>%
  group_by(continent) %>%
  filter(avg_total_km2 == min(avg_total_km2)) 

print(min_area_year_by_continent)
