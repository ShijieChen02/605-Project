# load_data.R This is building linear modl for precipitation and lake area

# Load the necessary libraries
library(ggplot2)

# Read a specific number of rows if the file is too large
# Adjust the 'nrows' parameter as needed
data <- read.csv("glcp.csv", nrows = 5000)

# It's good practice to clean up data you don't need
rm(list = setdiff(ls(), "data"))

# Check for linearity between total_precip_mm and total_km2
cor.test(data$total_precip_mm, data$total_km2)

# Create the plot
p <- ggplot(data, aes(x = total_precip_mm, y = total_km2)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship between Total Precipitation and Area",
       x = "Total Precipitation (mm)",
       y = "Total Area (km²)") +
  theme_minimal()

# Save the plot as a PDF for space efficiency
ggsave("plot.pdf", plot = p, device = "pdf")
t's good practice to clean up data you don't need
rm(list = setdiff(ls(), "data"))

# Check for linearity between total_precip_mm and total_km2
cor.test(data$total_precip_mm, data$total_km2)

# Create the plot
p <- ggplot(data, aes(x = total_precip_mm, y = total_km2)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship between Total Precipitation and Area",
       x = "Total Precipitation (mm)",
       y = "Total Area (km²)") +
  theme_minimal()

# Save the plot as a PDF for space efficiency
ggsave("plot.pdf", plot = p, device = "pdf")

# Clean up the data after saving the plot
rm(data)
gc() # garbage collection to free up memory

# Print the plot to the console if needed
print(p)
# Clean up the data after saving the plot
rm(data)
gc() # garbage collection to free up memory
# Print the plot to the console if needed
print(p)
# Clean up the data after saving the plot
rm(data)
gc() # garbage collection to free up memory

# Print the plot to the console if needed
print(p)
