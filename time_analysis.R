library(forecast)
library(readr)
library(dplyr)
library(prophet)
library(ggplot2)
args <- commandArgs(trailingOnly = TRUE)
lake_file <- args[1]
data <- read_csv(lake_file)


correlations <- c()


for(lake_id in unique(data$Hylak_id)) {

  lake_data <- subset(data, Hylak_id == lake_id)


  corr <- cor(lake_data$total_precip_mm, lake_data$seasonal_km2, use = "complete.obs")
  correlations <- c(correlations, corr)
}


correlations <- na.omit(correlations)


ggplot(data.frame(correlations), aes(x = correlations)) +
  geom_histogram(binwidth = 0.05, fill = "lightcoral", color = "black") +
  scale_y_continuous(labels = function(y) y * 10) +
  labs(title = "Correlation Coefficients between Total Precipitation and Water Area",
       x = "Correlation Coefficient",
       y = "Frequency") +
  theme_minimal()



correlations <- c()


for(lake_id in unique(data$Hylak_id)) {

  lake_data <- subset(data, Hylak_id == lake_id)


  lake_data <- lake_data[!is.na(lake_data$pop_sum), ]


  if(nrow(lake_data) > 1) {
    corr <- cor(lake_data$pop_sum, lake_data$seasonal_km2, use = "complete.obs")
    correlations <- c(correlations, corr)
  }
}


correlations <- na.omit(correlations)

ggplot(data.frame(correlations), aes(x = correlations)) +
  geom_histogram(binwidth = 0.05, fill = "lightblue", color = "black") +
  scale_y_continuous(labels = function(y) y * 10) +
  labs(title = "Correlation Coefficients between Population Sum and Water Area",
       x = "Correlation Coefficient",
       y = "Frequency") +
  theme_minimal()



high_corr_lakes <- data.frame(Hylak_id = integer(),
                              Correlation = numeric(),
                              centr_lat = numeric(),
                              centr_lon = numeric(),
                              country = character())

for(lake_id in unique(data$Hylak_id)) {

  lake_data <- subset(data, Hylak_id == lake_id)


  corr <- cor(lake_data$pop_sum, lake_data$seasonal_km2, use = "complete.obs")


  if(!is.na(corr) && corr > 0.8) {

    lake_info <- lake_data[1, c("centr_lat", "centr_lon", "country")]


    high_corr_lakes <- rbind(high_corr_lakes,
                             cbind(Hylak_id = lake_id,
                                   Correlation = corr,
                                   lake_info))
  }
}


print(high_corr_lakes)
country_frequency <- table(high_corr_lakes$country)
country_frequency_df <- as.data.frame(country_frequency)
names(country_frequency_df) <- c("Country", "Frequency")
sorted_country_frequency <- country_frequency_df[order(-country_frequency_df$Frequency),]


top_countries <- head(sorted_country_frequency, 5)


print(top_countries)

ggplot(high_corr_lakes, aes(x = centr_lat-10)) +
  geom_histogram(binwidth = 1, fill = "lightyellow", color = "black") +
  scale_x_continuous(breaks = pretty(high_corr_lakes$centr_lat, n = 10)) +
  scale_y_continuous(labels = function(y) y * 10) +
  labs(title = "Histogram of Latitude for High Correlation Lakes",
       x = "Latitude",
       y = "Frequency") +
  theme_minimal()


ggplot(high_corr_lakes, aes(x = centr_lon)) +

  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black") +
  scale_x_continuous(breaks = pretty(high_corr_lakes$centr_lon, n = 10)) +
  scale_y_continuous(labels = function(y) y * 10) +
  labs(title = "Histogram of Longitude for High Correlation Lakes",
       x = "Longitude",
       y = "Frequency") +
  theme_minimal()
