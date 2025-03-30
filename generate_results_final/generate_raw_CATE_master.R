###################################################################################
#                                                                                                        
#                                                                         
# Created on:   12/20/2024
# Purpose:      Create a function that loads all combined raw CATE results by queen
#               and stacks one iteration of the raw CATE
# Authors:      Polina Polskaia
# 
#
###################################################################################


# Load script 08
source(here::here("simulation_pipeline/08_run_simulation.R"))

###################################################################################
# CREATE AND CALL A FUNCTION TO LOAD AND STACK THE LOGS FOR ALL DATASETS ----
###################################################################################


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


###################################################################################
# CREATE A HELPER FUNCTION TO GROUP RAW CATE RESULTS BY MODEL ----
###################################################################################


#' process_tau_df:: Transform and group tau dataframe by model
#'
#' This function processes a tau dataframe by selecting a specific row, 
#' grouping columns by model names, and transforming the data into a structured format.
#'
#' @param tau_df A dataframe where rows represent iterations and columns are named with 
#'   a format that includes model identifiers (e.g., `OLS S`, `BART T`).
#' @param which_iter An integer specifying the row (iteration) to select for processing. 
#'   Default is 1.
#'
#' @return A transformed dataframe with the following structure:
#'   - Each row corresponds to a model.
#'   - Columns represent observations (`obs_1`, `obs_2`, ..., `obs_n`).
#'   - The first column contains model names.
#'
#' @details The function performs the following steps:
#'   1. Selects the specified row from the input dataframe.
#'   2. Extracts model names from the column headers.
#'   3. Groups data by model names.
#'   4. Combines grouped data into a new dataframe with models as rows and observations as columns.
#'   5. Renames observation columns dynamically to ensure consistency.
#'
#' @export
#' 
process_tau_df <- function(tau_df, which_iter = 1) {
  # Step 1: Select the specified row
  selected_row <- tau_df[which_iter, , drop = FALSE]  # Select the specified row dynamically
  
  # Step 2: Extract 'model' names from the column names
  models <- gsub(".*\\.", "", colnames(selected_row))  # Extract text after the dot
  
  # Step 3: Group columns by model names
  # Create a list of matrices grouped by model
  grouped_data <- split(as.numeric(selected_row), models)
  
  # Step 4: Combine grouped data into a dataframe
  # Each model becomes a row, and columns remain consistent 
  transformed_df <- do.call(rbind, grouped_data)
  
  # Step 5: Add model names as a column and reset row names
  transformed_df <- data.frame(model = rownames(transformed_df), transformed_df, row.names = NULL)
  
  # Rename columns dynamically (for example, 1 to 10000)
  colnames(transformed_df)[-1] <- paste0("obs_", seq_len(ncol(transformed_df) - 1))
  
  return(transformed_df)
}


###################################################################################
# CREATE A FUNCTION TO EXTRACT RAW PREDICTED CATE FOR ONE ITERATION ----
###################################################################################


#' process_raw_CATE_hat:: Extract and process raw predicted CATE values for a specific iteration
#'
#' This function extracts all raw Conditional Average Treatment Effect (CATE) values for a specified 
#' iteration across all simulation runs logged for multiple datasets. The extracted data is processed 
#' and aggregated into a single data frame.
#'
#' @param log_files_data A named list of data frames, where each data frame contains the log data for 
#'   a dataset. Each dataset should include columns such as `simulation_name`, `S` (iterations), 
#'   `outcome`, `size_train`, `covariates`, `large_covariate_set`, and `small_covariate_set`.
#' @param which_iteration An integer specifying the iteration to extract and process. Default is 1.
#'
#' @return A data frame containing the processed raw CATE values for the specified iteration, 
#'   with columns for model, queen, dataset, iterations, outcome, size_train, and covariate set.
#'
#' @details The function performs the following steps:
#'   1. Iterates over all datasets provided in `log_files_data`.
#'   2. For each dataset, identifies simulation folders that match the logged simulation names.
#'   3. Reads and processes all files containing predicted tau values (`predictedtau`) for the specified iteration.
#'   4. Adds metadata such as model, queen, dataset, iterations, outcome, size_train, and 
#'      covariate set to the processed data.
#'   5. Combines all processed data into a single data frame, ensuring no duplicate rows.
#'
#' @export
#' 
process_raw_CATE_hat <- function(log_files_data, which_iteration = 1) {
  final_results <- list()  # List to store results from all datasets
  
  for (dataset_name in names(log_files_data)) {
    # Turn the current list into a dataframe named 'log'
    log <- log_files_data[[dataset_name]]
    
    # Print the dataset name to track progress
    print(paste("Processing dataset:", dataset_name))
    
    # Extract simulation_name column and prepend 'simulation_from_'
    simulation_folders <- paste0("simulation_from_", log[["simulation_name"]])
    
    # Construct the full path to the combined_CATE subfolder
    combined_CATE_path <- here("results", "combined_IATEs_by_queen")
    combined_CATE_path_full <- file.path(combined_CATE_path, dataset_name)
    
    # Get the subfolders in the combined_CATE_path_full
    subfolder_names <- list.dirs(combined_CATE_path_full, full.names = FALSE, recursive = FALSE)
    
    # Match simulation folders to subfolders
    matched_folders <- intersect(simulation_folders, subfolder_names)
    
    # Iterate over all matched folders
    for (folder in matched_folders) {
      folder_path <- file.path(combined_CATE_path_full, folder)
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
      
      # Temporary list to store results from this folder
      folder_results <- list()
      
      # Process all files in the folder
      for (file in files_in_folder) {
        # Extract the first word (before '_') from the file name
        file_name <- basename(file)
        queen <- strsplit(file_name, "_")[[1]][1]
        print(paste("Processing file:", file_name))
        print(paste("Queen:", queen))
        
        # Read the file
        list_of_lists <- readRDS(file)
        
        # Extract the 'predictedtau' element
        predicted_tau <- list_of_lists[["predictedtau"]]
        
        # Convert to dataframe
        tau_df <- as.data.frame(predicted_tau)
        
        # Transform the dataframe, using the specified row
        transposed_tau_df <- process_tau_df(tau_df, which_iter = which_iteration)
        
        # Add the metadata columns
        transposed_tau_df["queen"] <- queen
        transposed_tau_df["dataset"] <- dataset_name
        transposed_tau_df["iterations"] <- iterations
        transposed_tau_df["outcome"] <- outcome
        transposed_tau_df["size_train"] <- size_train
        transposed_tau_df["covariate_set"] <- covariate_set
        
        # Reorder columns so metadata columns follow 'model'
        transposed_tau_df <- transposed_tau_df[, c("model", "queen", "dataset", "iterations", 
                                                   "outcome", "size_train", "covariate_set", 
                                                   setdiff(names(transposed_tau_df), 
                                                           c("model", "queen", "dataset", 
                                                             "iterations", "outcome", "size_train", "covariate_set")))]
        
        # Append the result to the folder-specific list
        folder_results <- append(folder_results, list(transposed_tau_df))
      }
      
      # Combine all folder-specific results and append to final results
      folder_combined <- do.call(rbind, folder_results)
      final_results <- append(final_results, list(folder_combined))
    }
  }
  
  # Combine all results across all datasets into a single dataframe
  final_result <- do.call(rbind, final_results)
  final_result["iteration_number"] = which_iteration
  
  # Remove duplicate rows
  final_result <- distinct(final_result)
  
  final_result <- final_result   %>%
    relocate(measure, iteration_number, .after = covariate_set)
  
  return(final_result)
}


###################################################################################
# CREATE A FUNCTION TO EXTRACT RAW TRUE CATE FOR ALL DATASETS ----
###################################################################################


#' generate_true_CATE_master:: Generate and process true CATE master data
#'
#' This function processes files in the `results/true_iates` folder to generate a 
#' master dataset of true Conditional Average Treatment Effects (CATEs). The function 
#' converts data from wide to long format, adds metadata, and combines results across all datasets.
#'
#' @return A data frame containing processed true CATE data for all datasets and outcomes, 
#'   with metadata columns for `outcome`, `dataset`, and observation indices (`obs_1`, `obs_2`, ...).
#'
#' @details The function performs the following steps:
#'   1. Reads all `.rds` files from the `results/true_iates` folder.
#'   2. Extracts metadata such as dataset name and outcome from file names.
#'   3. Converts the first 10,000 rows of the data into a long format, adding observation indices.
#'   4. Transforms the long format data back into a wide format with observations as columns.
#'   5. Combines results across all files into a single data frame.
#'
#' @export
#' 
gererate_true_CATE_master <- function() {
  
  final_results <- list()  # List to store results from all datasets
  
  # Construct the path to the 'true_iates' folder
  true_iates_path <- here("true_iates")
  
  # Get the names of files
  files_in_folder <- list.files(true_iates_path, full.names = TRUE)
  
  # Loop through each files
  for (file in files_in_folder) {
    
    # Extract the file name from the full path
    file_name <- basename(file)
    
    # Extract `dataset` (anything before the first '_')
    dataset <- sub("_.*", "", file_name)
    
    # Extract `outcome` (everything after the first '_')
    outcome <- sub("^[^_]*_(.*)\\.rds$", "\\1", file_name)
    
    # Print the results
    print(paste("Dataset name:", dataset))
    print(paste("Outcome name:", outcome))
    
    # Construct the full path to the file
    true_iates_path_full <- file.path(true_iates_path, file_name)
    
    # Check if the file exists before reading
    if (file.exists(true_iates_path_full)) {
      # Read the file
      true_iates_data <- read_rds(true_iates_path_full)
      
    } else {
      message(paste("Not found:", true_iates_path_full))
    }
    
    df_subset <- true_iates_data[1:10000, ]      
    
    long_df <- data.frame(
      index = rep(1:nrow(df_subset), ncol(df_subset)),          # Repeat row indices for each column
      value = unlist(df_subset),                                # Flatten values
      queen = rep(colnames(df_subset), each = nrow(df_subset))  # Repeat column names for each row
    )
    
    long_df["outcome"] = outcome
    long_df["dataset"] = dataset
    
    # Convert `index` to prefixed names (e.g., obs_1, obs_2, ...)
    long_df <- long_df %>%
      mutate(index = paste0("obs_", index))  # Add prefix to index
    
    
    wide_df <- long_df %>%
      pivot_wider(
        names_from = index,          # Make `index` values the column names
        values_from = value          # Populate with `value`
      )
    
    final_results <- append(final_results, list(wide_df))
  }
  final_result <- do.call(rbind, final_results)
  return(final_result)
}


###################################################################################
# GENERATE THE MASTER FILE WITH TRUE AND PREDICTED CATE FOR ONE ITERATION ----
###################################################################################

  
# Call the function to combine raw predicted CATE
cate_hat_master_1st_iter <- process_raw_CATE_hat(log_files_data, which_iteration = 1)  
  
# Print the final dataframe
print(dim(cate_hat_master_1st_iter))

# Save the predicted raw CATE master
saveRDS(cate_hat_master_1st_iter, "cate_hat_master_1st_iter.rds")
  
# Call the function to combine raw true CATE
true_CATE_master = gererate_true_CATE_master()
  
# Drop measure columns from `cate_hat_master_1st_iter`
cate_hat_master_1st_iter_cleaned <- cate_hat_master_1st_iter %>%
  select(-measure)
  
# Merge datasets with explicit suffixes
merged_data <- cate_hat_master_1st_iter_cleaned %>%
  inner_join(true_CATE_master, by = c("dataset", "outcome", "queen"), suffix = c("_hat", "_true"))

# Pivot merged data to long format 
final_data <- merged_data %>%
  pivot_longer(
    cols = starts_with("obs_"),                         # Select all obs_* columns
    names_to = c("observation", "type"),                # Split into observation and type
    names_pattern = "obs_(\\d+)_(hat|true)",            # Regex to capture numeric and suffix (_hat, _true)
    values_to = "value"                                 # Combine values into a single column
  )

# Drop duplicates that occured from running the same scenario multiple times
final_result_non_duplicated <- final_data %>%
  distinct(across(-last_col()), .keep_all = TRUE)

# Pivot back to wide
final_data_wide <- final_result_non_duplicated %>%
  pivot_wider(
    names_from = "type",
    values_from = "value",
    names_prefix = "CATE_"
  ) 

# Save the master
saveRDS(final_data_wide, "cate_master_1st_iter.rds")

