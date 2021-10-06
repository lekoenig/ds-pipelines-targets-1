library(dplyr)
library(readr)
library(stringr)
library(sbtools)
library(whisker)

source("./1_fetch/src/fetch_data.R")
source("./2_process/src/process_data.R")
source("./3_visualize/src/plot_rmse.R")

# Get the data from ScienceBase
fetch_data()

# Prepare data for plotting
data <- process_data()

# Create a plot
plot_rmse(data)
