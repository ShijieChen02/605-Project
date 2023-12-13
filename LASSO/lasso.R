df <- read.csv("glcp.csv",nrows=100000)

df['pop_sum'] = df['pop_sum']
df <- df[complete.cases(df[, "pop_sum"]), ]
df['total_km2'] = df['total_km2']
df['total_precip_mm'] = log(df['total_precip_mm'])
df$total_km2 <- ifelse(is.infinite(df$total_km2), NA, df$total_km2)
df <- df[complete.cases(df[, "total_km2"]), ]

y <- df$total_km2

#x <- df%>% 
  #select(pop_sum, mean_annual_temp_k, total_precip_mm, centr_lat)
#x <- makeX(x, na.impute = TRUE)

x <- model.matrix(~ pop_sum + mean_annual_temp_k + total_precip_mm+centr_lat, data = df)
interaction_terms <- x[, c("pop_sum", "mean_annual_temp_k", "total_precip_mm","centr_lat")]
temp_x_precip <- interaction_terms[,2] * interaction_terms[,3]
temp_x_lat <- interaction_terms[,2] * interaction_terms[,4]
precip_x_lat <- interaction_terms[,3] * interaction_terms[,4]
x <- cbind(x, temp_x_precip,temp_x_lat,precip_x_lat)

lasso_coordinate_descent <- function(X, y, lambda, max_iter = 1000, tol = 0.1) {
  n <- nrow(X)
  p <- ncol(X)
  beta <- rep(0, p)
  attr_names <- colnames(X)
  converged <- FALSE
  
  for (iter in 1:max_iter) {
    beta_old <- beta
    
    for (j in 1:p) {
      X_j <- X[, -j, drop = FALSE]
      beta_j <- beta[-j]
      r <- y - X_j %*% beta_j
      
      # Soft thresholding
      beta[j] <- ifelse(sum(X[, j] * r) < -lambda / 2, 
                        (sum(X[, j] * r) + lambda / 2) / sum(X[, j]^2), 
                        ifelse(sum(X[, j] * r) > lambda / 2, 
                               (sum(X[, j] * r) - lambda / 2) / sum(X[, j]^2), 
                               0))
    }
    
    # Check for convergence
    if (max(abs(beta - beta_old)) < tol) {
      converged <- TRUE
      break
    }
  }
  
  if (!converged) {
    warning("Coordinate descent did not converge within the specified number of iterations.")
  }
  
  result <- data.frame(Attribute = attr_names, Coefficient = beta)
  return(result)
}

# Run lasso regression using coordinate descent
lasso_result <- lasso_coordinate_descent(x, y, 0.1151675)
# Print the result
write.csv(lasso_result ,"lasso_result.csv")




