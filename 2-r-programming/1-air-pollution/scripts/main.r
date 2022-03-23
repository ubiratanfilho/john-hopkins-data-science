path_data <- list.files("data", full.names = TRUE)
mean_pollutants <- pollutantmean()
csv_example <- data.frame(read.csv("data/001.csv"))
