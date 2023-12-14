#This is about building polynomial and LOESS model with 50,0000 volume of data
library(ggplot2)


data <- read.csv("glcp.csv",nrows=10000) 


data <- na.omit(data)
data <- subset(data, pop_sum > 0 & total_km2 > 0)


data$log_pop_sum <- log(data$pop_sum)
data$log_total_km2 <- log(data$total_km2)


poly_model <- lm(log_total_km2 ~ poly(log_pop_sum, 2), data = data) 


loess_model <- loess(log_total_km2 ~ log_pop_sum, data = data)




p <- ggplot(data, aes(x = log_pop_sum, y = log_total_km2)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), col = "red") +
  geom_smooth(method = "loess", col = "blue", se = FALSE) +
  labs(title = "log(pop)vslog(total_km2)",
       x = "log(pop_sum)",
       y = "log(total_km2)") +
  theme_minimal()


ggsave("population_vs_lake_area_polynomial_loess.pdf",plot=p,device="pdf")


summary(poly_model)
summary(loess_model)
