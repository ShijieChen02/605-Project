library(readr)
library(ggplot2)
library(dplyr)
lake_data <- read_csv("glcp.csv") 

lake_data <- lake_data %>% filter(!is.na(total_km2) & !is.na(continent))

lake_by_continent <- lake_data %>% 
  group_by(continent) %>% 
  summarise(mean_area = mean(total_km2, na.rm = TRUE))

ggplot(lake_by_continent, aes(x = continent, y = mean_area, fill = continent)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Lake Area by Continent",
       x = "Continent",
       y = "Average Lake Area (km^2)")

anova_result <- aov(total_km2 ~ continent, data = lake_data)
summary(anova_result)
