#This is about building polynomial and LOESS model with 100,0000 volume of data.

library(ggplot2)


file_path <- "glcp.csv"


data <- read.csv(file_path,nrows=15000)


data <- na.omit(data)
data <- subset(data, pop_sum > 0 & total_km2 > 0)


data$log_pop_sum <- log(data$pop_sum)
data$log_total_km2 <- log(data$total_km2)


iqr <- IQR(data$log_total_km2)
upper <- quantile(data$log_total_km2, 0.75) + 1.5 * iqr
lower <- quantile(data$log_total_km2, 0.25) - 1.5 * iqr
data_clean <- subset(data, log_total_km2 > lower & log_total_km2 < upper)


poly_model <- lm(log_total_km2 ~ poly(log_pop_sum, 2), data = data_clean)
poly_model_rsq <- summary(poly_model)$r.squared
poly_model_residuals <- resid(poly_model)


loess_model <- loess(log_total_km2 ~ log_pop_sum, data = data_clean)
loess_model_rsq <- 1 - sum(resid(loess_model)^2) / sum((data_clean$log_total_km2 - mean(data_clean$log_total_km2))^2)
loess_model_residuals <- resid(loess_model)


pdf("poly_model_rsq.pdf")
cat("Polynomial Model R-squared: ", poly_model_rsq, "\n")
dev.off()

pdf("loess_model_rsq.pdf")
cat("LOESS Model R-squared: ", loess_model_rsq, "\n")
dev.off()


pdf("residual_plots.pdf", width = 12, height = 10)
par(mfrow=c(2, 1)) # 设置图形输出为2行1列


plot(poly_model_residuals, main="Polynomial Regression Residuals", ylab="Residuals", xlab="Fitted Values")
abline(h=0, col="red")


plot(loess_model_residuals, main="LOESS Regression Residuals", ylab="Residuals", xlab="Fitted Values")
abline(h=0, col="blue")

dev.off()
