#this is about building models with 100,0000 volume of data
library(ggplot2)


file_path <- "glcp.csv" 


data <- read.csv(file_path,nrows=1000000)


data <- na.omit(data)
data <- subset(data, pop_sum > 0 & total_km2 > 0)


data$log_pop_sum <- log(data$pop_sum)
data$log_total_km2 <- log(data$total_km2)


iqr <- IQR(data$log_total_km2)
upper <- quantile(data$log_total_km2, 0.75) + 1.5 * iqr
lower <- quantile(data$log_total_km2, 0.25) - 1.5 * iqr


data_clean <- subset(data, log_total_km2 > lower & log_total_km2 < upper)


poly_model <- lm(log_total_km2 ~ poly(log_pop_sum, 2), data = data_clean)

loess_model <- loess(log_total_km2 ~ log_pop_sum, data = data_clean)


poly_model_rsq <- summary(poly_model)$r.squared
loess_model_rsq <- summary(loess_model)$r.squared
better_model <- ifelse(poly_model_rsq > loess_model_rsq, "Polynomial", "LOESS")


ggplot(data_clean, aes(x = log_pop_sum, y = log_total_km2)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), col = "red") +
  geom_smooth(method = "loess", col = "blue", se = FALSE) +
  labs(title = paste("Model Comparison: ", better_model, " fits better"),
       x = "log(pop_sum)",
       y = "log(total_km2)") +
  theme_minimal()


pdf("model_comparison.pdf", width = 12, height = 10)


print(ggplot(data_clean, aes(x = log_pop_sum, y = log_total_km2)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), col = "red") +
  geom_smooth(method = "loess", col = "blue", se = FALSE) +
  labs(title = paste("Model Comparison: ", better_model, " fits better"),
              x = "log(pop_sum)",
       y = "log(total_km2)") +
  theme_minimal())


dev.off()

cat("Polynomial Model R-squared: ", poly_model_rsq, "\n")
cat("LOESS Model R-squared: ", loess_model_rsq, "\n")
cat("Better model is: ", better_model, "\n")
