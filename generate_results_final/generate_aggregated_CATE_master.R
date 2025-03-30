##################################################################################
#            
#                                                                         
# Created on:   02/07/2025
# Purpose:      Generate master file of aggregated CATE for all simulations performed
# Authors:      Polina Polskaia
# 
#
###################################################################################


# Load script 08
source(here::here("simulation_pipeline/08_run_simulation.R"))

# Specify all available algorithms
ALL_MODELS <- c("ATE", "OLS S", "RF INF","RF T","RF MOM IPW","RF MOM DR",
                "CF","CF LC","CDML","LASSO INF",
                "LASSO T","LASSO MOM IPW","LASSO MOM DR", "LASSO MCM","LASSO MCM EA",
                "LASSO R", "SL T", "SL S", "XGBOOST S", "XGBOOST R", "BART T", "BART S", "BART S NI")

#' process_log_files:: Process log files from simulation results
#'
#' This function processes log files from simulation results stored in subfolders under 
#' the `results/logs` directory. Each subfolder is expected to contain a `log_file.csv`. 
#' The function reads these files into data frames and returns them as a list.
#'
#' @return A named list of data frames, where each element corresponds to a subfolder 
#'   under `results/logs` and contains the data from the respective `log_file.csv`. 
#'   If a log file is missing, a message is displayed, and the subfolder is skipped.
#'
#' @details The function performs the following steps:
#'   1. Identifies all subfolders in the `results/logs` directory.
#'   2. Attempts to read `log_file.csv` from each subfolder.
#'   3. Stores the contents of each `log_file.csv` as a data frame in a named list.
#'   4. Skips subfolders where `log_file.csv` is not found, displaying a message for each missing file.
#'
#' @export
#' 
process_log_files <- function() {
  # Construct the path to the 'results/logs' folder
  logs_path <- here("results", "logs")
  
  # Get the names of subfolders (only directories, no files)
  subfolder_names <- list.dirs(logs_path, full.names = FALSE, recursive = FALSE)
  
  # Initialize a list to store dataframes
  log_dataframes <- list()
  
  # Loop through each subfolder
  for (subfolder in subfolder_names) {
    # Construct the full path to the subfolder
    subfolder_path <- file.path(logs_path, subfolder)
    
    # Construct the path to 'log_file.csv'
    log_file_path <- file.path(subfolder_path, "log_file.csv")
    
    # Check if the file exists before reading
    if (file.exists(log_file_path)) {
      # Read the log_file.csv
      log_data <- read_csv(log_file_path)
      
      # Store the dataframe in the list
      log_dataframes[[subfolder]] <- log_data
    } else {
      message(paste("log_file.csv not found in", subfolder))
    }
    
  }
  
  return(log_dataframes)
}


# Call the function
log_files_data <- process_log_files()


generate_aggregated_CATE_master <- function(log_files_data, standardise = TRUE, verbose = 1) {
  final_results <- list()  # List to store results from all datasets
  
  for (dataset_name in names(log_files_data)) {
    # Turn the current list into a dataframe named 'log'
    log <- log_files_data[[dataset_name]]
    
    # Print the dataset name to track progress
    print(paste("Processing dataset:", dataset_name))
    
    # Extract simulation_name column and prepend 'simulation_from_'
    simulation_folders <- paste0("simulation_from_", log[["simulation_name"]])
    
    # Construct the full path to the aggregated_IATEs subfolder
    aggregated_CATE_path <- here("results", "aggregated_IATEs")
    aggregated_CATE_path_full <- file.path(aggregated_CATE_path, dataset_name)
    
    # Get the subfolders in the aggregated_CATE_path_full
    subfolder_names <- list.dirs(aggregated_CATE_path_full, full.names = FALSE, recursive = FALSE)
    
    # Match simulation folders to subfolders
    matched_folders <- intersect(simulation_folders, subfolder_names)
    
    # Iterate over all matched folders
    for (folder in matched_folders) {
      folder_path <- file.path(aggregated_CATE_path_full, folder)
      files_in_folder <- list.files(folder_path, full.names = TRUE)
      
      # Extract relevant values from the log for this matched folder
      simulation_index <- which(log[["simulation_name"]] == gsub("simulation_from_", "", folder))
      iterations <- log[simulation_index, "S"]
      outcome <- log[simulation_index, "outcome"]
      size_train <- log[simulation_index, "size_train"]
      covariates <- log[simulation_index, "covariates"]
      large_covariate_set <- log[simulation_index, "large_covariate_set"]
      small_covariate_set <- log[simulation_index, "small_covariate_set"]
      
      # Determine the covariate set
      covariate_set <- if (covariates %in% large_covariate_set) {
        "large"
      } else if (covariates %in% small_covariate_set) {
        "small"
      } else {
        "medium"
      }
      
      # For standartization load baseline object
      baseline_file_name <- paste0(dataset_name, "_baseline_object.rds")
      baseline_path <- here("baseline")
      baseline_path_full <- file.path(baseline_path, baseline_file_name)
      baseline_file <- readRDS(baseline_path_full)
      baseline_file <- baseline_file[["Data"]]
      outcome_Y0 <- paste0(outcome, "_Y0")
      # Default scaling unless need to shift to standardize effect units
      sd_value <- if (max(baseline_file[[outcome_Y0]], na.rm = TRUE) > 1) {
        sd(baseline_file[[outcome_Y0]], na.rm = TRUE)
      } else {
        1
      }
      
      # Temporary list to store results from this folder
      folder_results <- list()
      # Process all files in the folder
      for (file in files_in_folder) {

        # Read the file
        simulation_results <- readRDS(file)
        
        if ( verbose > 0 ) {
          cat( glue::glue( "Reading results from {file}\n" ) )
          cat( "\n" )
        }
        
        if (standardise) {
          # Standardize results using sd_value
          simulation_results <- simulation_results %>% 
          mutate(across(!c(metric, id, queen), 
                        #if the metric is percent_cut or runtime, don't divide
                        ~if_else(metric %in% c("percent_cut", "runtime"), 
                                 ., 
                        #standardize everything else
                                 . / div_sd)))
        }
        
        # Drop columns containing any NA values for cases when a model could not produce results
        simulation_results <- simulation_results[, colSums(is.na(simulation_results)) == 0]
        
        # Aggregate performance (Bias, SE, RMSE) by queen
        agg_perf_by_queen = aggregated_performance_by_queen(simulation_results)
        
        # Reshape agg_perf_by_queen so metrics are the columns
        wide_Ev_agg_perf_by_queen = pivot_wider(
          data = agg_perf_by_queen,
          id_cols = c("model", "queen" ),
          names_from = metric,
          values_from = c( Ev, Q1, Q3 ),
          names_prefix = ""
        ) %>%
          rename( bias = Ev_bias, se = Ev_se, rmse = Ev_rmse )
        
        # Now percent cut and runtime
        crt <- aggregate_outliers_by_queen(simulation_results)
        rt <- aggregate_time_by_queen(simulation_results)
        rt <- left_join( rt, crt, by = c("model", "queen") )
        wide_Ev_agg_perf_by_queen <- wide_Ev_agg_perf_by_queen %>%
          left_join( rt, by = c("model", "queen") )
        
        # Order by model by queen
        wide_Ev_agg_perf_by_queen <- wide_Ev_agg_perf_by_queen  %>% 
          arrange(factor(model, levels = ALL_MODELS), factor(queen, levels = ALL_MODELS))
        
        wide_Ev_agg_perf_by_queen["cov_set_size"] <- covariate_set
        wide_Ev_agg_perf_by_queen["dataset"] <- dataset_name
        wide_Ev_agg_perf_by_queen["outcome"] <- outcome
        wide_Ev_agg_perf_by_queen["train_set_size"] <- size_train
        
        # Add type column
        wide_Ev_agg_perf_by_queen = wide_Ev_agg_perf_by_queen %>%
          mutate(type = case_when(
            model == "ATE" ~ "ATE",
            model == "OLS S" ~ "OLS S",
            model %in% c("RF INF", "LASSO INF") ~ "INF",
            model %in% c("RF T", "RF MOM IPW", "RF MOM DR", "CF", "CF LC") ~ "RF",
            model %in% c("LASSO T", "LASSO MOM IPW", "LASSO MOM DR", "LASSO MCM", "LASSO MCM EA", "LASSO R") ~ "LASSO",
            model == "CDML" ~ "CDML",
            model %in% c("BART T", "BART S") ~ "BART",
            model %in% c("XGBOOST S", "XGBOOST R") ~ "XGBOOST",
            model %in% c("SL T", "SL S") ~ "SL",
            TRUE ~ "Unknown"
          ))%>%
          mutate(modelshort = case_when(
            model %in% c("OLS S", "ATE", "CDML") ~ type,
            .default = trimws(str_remove(model, type))))
        

        # Append the result to the folder-specific list
        folder_results <- append(folder_results, list(wide_Ev_agg_perf_by_queen))
      }
      
      # Combine all folder-specific results and append to final results
      folder_combined <- do.call(rbind, folder_results)
      final_results <- append(final_results, list(folder_combined))
      
    }
  }
  
  # Combine all results across all datasets into a single dataframe
  final_result <- do.call(rbind, final_results)

  # Remove duplicate rows
  final_result <- distinct(final_result)
  
  return(final_result)
}

master_aggregated_CATE = generate_aggregated_CATE_master(log_files_data)

deduplicated_data <- master_aggregated_CATE %>%
  group_by(dataset, outcome, train_set_size, cov_set_size, model, queen) %>%
  slice(1) %>% # Keep the first row in each group
  ungroup() # Ungroup to return a regular data frame

# View the deduplicated data
print(deduplicated_data)

# Group by the specified columns and check for duplicates
duplicates <- deduplicated_data %>%
  select(dataset, outcome, train_set_size, cov_set_size, model, queen) %>% # Keep only relevant columns
  group_by(dataset, outcome, train_set_size, cov_set_size, model, queen) %>%
  filter(n() > 1) %>% # Filter groups with more than one row (duplicates)
  ungroup()


write.xlsx(deduplicated_data, file = "results_final/master_aggregated_CATE.xlsx")



