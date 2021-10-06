
fetch_data <- function(save_path,output_file_name){
  
  # Set up save path:
  mendota_file <- file.path(save_path, output_file_name)
  
  # Get the data from ScienceBase:
  item_file_download('5d925066e4b0c4f70d0d0599', names = 'me_RMSE.csv', destinations = mendota_file, overwrite_file = TRUE)

}

