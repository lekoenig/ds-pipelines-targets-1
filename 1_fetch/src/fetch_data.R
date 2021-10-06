
fetch_data <- function(){
  
  # Set up save path:
  save_path <- "./1_fetch/out"
  mendota_file <- file.path(save_path, 'model_RMSEs.csv')
  
  # Get the data from ScienceBase:
  item_file_download('5d925066e4b0c4f70d0d0599', names = 'me_RMSE.csv', destinations = mendota_file, overwrite_file = TRUE)

}
