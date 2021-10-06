library(dplyr)
library(readr)
library(stringr)
library(sbtools)
library(whisker)

source("./1_fetch/src/fetch_data.R")
source("./2_process/src/process_data.R")
source("./3_visualize/src/plot_rmse.R")

# Get the data from ScienceBase
fetch_data(save_path <- "./1_fetch/out",output_file_name <- "model_RMSEs.csv")

# Prepare data for plotting
data <- process_data(input_file_path = file.path("./1_fetch/out", 'model_RMSEs.csv'),
                     save_path <- "./2_process/out")

# Create a plot
plot_rmse(data)
