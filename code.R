fetch_data <- function(save_path,output_file_name){
  
  # Set up save path:
  mendota_file <- file.path(save_path, output_file_name)
  
  # Get the data from ScienceBase:
  item_file_download('5d925066e4b0c4f70d0d0599', names = 'me_RMSE.csv', destinations = mendota_file, overwrite_file = TRUE)
  
}


# Function to filter the data (called in process_data function below):
filter_data <- function(data,model,experiment){
  
  filtered_data <- filter(data,model_type == model, exper_id == experiment) %>% pull(rmse) %>% mean %>% round(2) 
  return(filtered_data)
  
  
}


# Function to prepare the data for plotting:
process_data <- function(input_file_path,save_path,plot_color,plot_shape){
  
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


# Plot model diagnostics:
plot_rmse <- function(data,save_path,plot_name,plot_height,plot_width,x_min,x_max,y_min,y_max){
  
  # Create a plot
  png(file = file.path(save_path, plot_name), width = plot_width, height = plot_height, res = 200, units = 'in')
  par(omi = c(0,0,0.05,0.05), mai = c(1,1,0,0), las = 1, mgp = c(2,.5,0), cex = 1.5)
  
  plot(NA, NA, xlim = c(x_min, x_max), ylim = c(y_min, y_max),
       ylab = "Test RMSE (°C)", xlab = "Training temperature profiles (#)", log = 'x', axes = FALSE)
  
  n_profs <- c(2, 10, 50, 100, 500, 980)
  
  axis(1, at = c(-100, n_profs, 1e10), labels = c("", n_profs, ""), tck = -0.01)
  axis(2, at = seq(0,10), las = 1, tck = -0.01)
  
  # slight horizontal offsets so the markers don't overlap:
  offsets <- data.frame(pgdl = c(0.15, 0.5, 3, 7, 20, 30)) %>%
    mutate(dl = -pgdl, pb = 0, n_prof = n_profs)
  
  for (mod in c('pb','dl','pgdl')){
    mod_data <- filter(data, model_type == mod)
    mod_profiles <- unique(mod_data$n_prof)
    for (mod_profile in mod_profiles){
      d <- filter(mod_data, n_prof == mod_profile) %>% summarize(y0 = min(rmse), y1 = max(rmse), col = unique(col))
      x_pos <- offsets %>% filter(n_prof == mod_profile) %>% pull(!!mod) + mod_profile
      lines(c(x_pos, x_pos), c(d$y0, d$y1), col = d$col, lwd = 2.5)
    }
    d <- group_by(mod_data, n_prof) %>% summarize(y = mean(rmse), col = unique(col), pch = unique(pch)) %>%
      rename(x = n_prof) %>% arrange(x)
    
    lines(d$x + tail(offsets[[mod]], nrow(d)), d$y, col = d$col[1], lty = 'dashed')
    points(d$x + tail(offsets[[mod]], nrow(d)), d$y, pch = d$pch[1], col = d$col[1], bg = 'white', lwd = 2.5, cex = 1.5)
    
  }
  
  points(2.2, 0.79, col = '#7570b3', pch = 23, bg = 'white', lwd = 2.5, cex = 1.5)
  text(2.3, 0.80, 'Process-Guided Deep Learning', pos = 4, cex = 1.1)
  
  points(2.2, 0.94, col = '#d95f02', pch = 22, bg = 'white', lwd = 2.5, cex = 1.5)
  text(2.3, 0.95, 'Deep Learning', pos = 4, cex = 1.1)
  
  points(2.2, 1.09, col = '#1b9e77', pch = 21, bg = 'white', lwd = 2.5, cex = 1.5)
  text(2.3, 1.1, 'Process-Based', pos = 4, cex = 1.1)
  
  dev.off()
  
  return(file.path(save_path,plot_name))
  
}