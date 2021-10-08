
# Function to filter the data (called in process_data function below):
filter_data <- function(data,model,experiment){
  
  filtered_data <- filter(data,model_type == model, exper_id == experiment) %>% pull(rmse) %>% mean %>% round(2) 
  return(filtered_data)
  
  
}


# Function to prepare the data for plotting:
process_data <- function(input_file_path,plot_color,plot_shape){
  
  # Prepare the data for plotting:
  eval_data <- readr::read_csv(input_file_path, col_types = 'iccd') %>%
    filter(str_detect(exper_id, 'similar_[0-9]+')) %>%
    mutate(col = case_when(
      model_type == 'pb' ~ plot_color[1],
      model_type == 'dl' ~ plot_color[2],
      model_type == 'pgdl' ~ plot_color[3]
    ), pch = case_when(
      model_type == 'pb' ~ plot_shape[1],
      model_type == 'dl' ~ plot_shape[2],
      model_type == 'pgdl' ~ plot_shape[3]
    ), n_prof = as.numeric(str_extract(exper_id, '[0-9]+')))
  
  return(eval_data)
  
}


# Function to save the model diagnostics
save_diagnostics <- function(data,save_path,file_name){
  
  render_data <- list(pgdl_980mean = filter_data(data, model = "pgdl",experiment = "similar_980"),
                      dl_980mean =  filter_data(data, model = 'dl', experiment = "similar_980"),
                      pb_980mean = filter_data(data, model = 'pb', experiment = "similar_980"),
                      dl_500mean = filter_data(data, model = 'dl', experiment = "similar_500"),
                      pb_500mean = filter_data(data, model = 'pb', experiment = "similar_500"),
                      dl_100mean = filter_data(data, model = 'dl', experiment = "similar_100"),
                      pb_100mean = filter_data(data, model = 'pb', experiment = "similar_100"),
                      pgdl_2mean = filter_data(data, model = 'pgdl', experiment = "similar_2"),
                      pb_2mean = filter_data(data, model = 'pb', experiment = "similar_2"))
  
  template_1 <- 'resulted in mean RMSEs (means calculated as average of RMSEs from the five dataset iterations) of {{pgdl_980mean}}, {{dl_980mean}}, and {{pb_980mean}}°C for the PGDL, DL, and PB models, respectively.
  The relative performance of DL vs PB depended on the amount of training data. The accuracy of Lake Mendota temperature predictions from the DL was better than PB when trained on 500 profiles 
  ({{dl_500mean}} and {{pb_500mean}}°C, respectively) or more, but worse than PB when training was reduced to 100 profiles ({{dl_100mean}} and {{pb_100mean}}°C respectively) or fewer.
  The PGDL prediction accuracy was more robust compared to PB when only two profiles were provided for training ({{pgdl_2mean}} and {{pb_2mean}}°C, respectively). '
  
  whisker.render(template_1 %>% str_remove_all('\n') %>% str_replace_all('  ', ' '), render_data ) %>% cat(file = file.path(save_path, file_name))
  
  return(file.path(save_path,file_name))
  
  
}

# Save the processed data:
save_processed_data <- function(data,save_path,file_name){
  
  readr::write_csv(data, file = file.path(save_path, file_name),append=FALSE)
  return(file.path(save_path, file_name))
  
}
