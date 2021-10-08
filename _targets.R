library(targets)

source("./1_fetch/src/fetch_data.R")
source("./2_process/src/process_data.R")
source("./3_visualize/src/plot_rmse.R")

tar_option_set(packages = c("tidyverse", "sbtools", "whisker"))

list(
  # Get the data from ScienceBase
  tar_target(
    model_RMSEs_csv,
    fetch_data(save_path = "1_fetch/out/",output_file_name="model_RMSEs.csv"),
    format = "file"
  ), 
  # Prepare the data for plotting
  tar_target(
    eval_data,
    process_data(input_file_path = model_RMSEs_csv,
                 plot_color = c('#1b9e77','#d95f02','#7570b3'),
                 plot_shape = c(21,22,23)),
  ),
  # Create a plot
  tar_target(
    figure_1_png,
    plot_rmse(save_path = "3_visualize/out/",
              plot_name = "figure_compare_rmse.png",
              data = eval_data,
              plot_width=8,plot_height=10,
              x_min=2,x_max=1000,y_min=4.7,y_max=0.75),
    format = "file"
  ),
  # Save the processed data
  tar_target(
    model_summary_results_csv,
    save_processed_data(data = eval_data, 
                        save_path = "2_process/out/",
                        file_name = "model_summary_results.csv"), 
    format = "file"
  ),
  # Save the model diagnostics
  tar_target(
    model_diagnostic_text_txt,
    save_diagnostics(data = eval_data,
                     save_path = "2_process/out/", 
                     file_name = "model_diagnostic_text.txt"), 
    format = "file"
  )
)
