#' load_and_process_all_results:: Load and process results for all queens
#'
#' This function processes simulation results for all queens by loading,
#'   and aggregating Individualized Average Treatment Effects (IATEs).
#'
#' @param dataset_name A character string specifying the name of the dataset.
#' @param outcome A character string specifying the outcome variable of interest.
#' @param queen_list A character vector specifying the list of models (queens) to process.
#' @param simulation_name A character string specifying the name of the simulation run, 
#'   used to locate the corresponding results.
#' @param true_iates_directory_path A character string specifying the path to the 
#'   directory containing the true IATEs. Default is `here::here("true_iates")`.
#' @param verbose A numeric value controlling the verbosity of the output. Higher 
#'   values produce more detailed messages. Default is 1.
#'
#' @return A data frame containing the aggregated IATEs from all queens.
#'
#' @details The function performs the following steps:
#'   1. Iterates through the list of models (queens) and processes results for each using `load_and_process_queen_results`.
#'   2. Stacks the processed results from all queens into a single data frame.
#'   3. Saves the aggregated results to a specified directory.
#'   4. Ensures the output directory exists, creating it if necessary.
#'
#' @export
#' 
load_and_process_all_results <- function(dataset_name, 
                                         outcome, 
                                         queen_list, 
                                         simulation_name,
                                         true_iates_directory_path = here::here("true_iates"),
                                         verbose = 1 
                                        ) {
  
  # Create an empty list to store the result dataframes
  result_list <- list()
  
  for (i in 1:length(queen_list)) {
    
    # Select queen
    queen <- queen_list[i]
    
    # Store sim_results in result_list
    result_list[[queen]] <- load_and_process_queen_results(dataset_name = dataset_name, 
                                                           outcome = outcome, 
                                                           queen = queen, 
                                                           simulation_name = simulation_name,
                                                           true_iates_directory_path = true_iates_directory_path)
  }
  
  #Stack all dataframes vertically
  stacked_result <- do.call(rbind, result_list)
  
  # Create the directory path
  dir_path <- here("results", "aggregated_IATEs", dataset_name, paste0("simulation_from_", simulation_name))
  
  # Ensure directory exists, if not create it
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
    if (verbose > 0) {
      cat(glue::glue("Created directory {dir_path} for aggregated IATEs\n"))
      cat("\n")
    }
  }
  
  # Save aggregated data 
  file_name <- file.path(dir_path, "aggregated_IATEs_data.rds")
  saveRDS(stacked_result, file_name)
  
  return(stacked_result)
}
