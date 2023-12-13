library(readr)


data <- read_csv("glcp.csv")


rows_per_file <- 10000


file_count <- ceiling(nrow(data) / rows_per_file)


for(i in 1:file_count) {
  start_row <- (i - 1) * rows_per_file + 1
  end_row <- min(i * rows_per_file, nrow(data))

  file_name <- sprintf("glcp_part%02d.csv", i)
  write_csv(data[start_row:end_row, ], file_name)
}
