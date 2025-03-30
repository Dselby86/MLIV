#' load_and_process_queen_results:: Load and process queen results
#'
#' This function loads the predicted Individualized Average Treatment Effects (IATEs) for a specified queen, 
#' processes the simulation results, and calculates evaluation metrics such as bias, standard error (SE), 
#' and root mean squared error (RMSE).
#'
#' @param dataset_name A character string specifying the name of the dataset.
#' @param queen A character string specifying the queen for which results are being loaded and processed.
#' @param outcome A character string specifying the outcome variable of interest.
#' @param simulation_name A character string specifying the name of the simulation run, used to locate the 
#'   corresponding results.
#' @param true_iates_directory_path A character string specifying the path to the directory containing the 
#'   true IATEs. Default is `here::here("true_iates")`.
#' @param verbose A numeric value controlling the verbosity of the output. Higher values produce more 
#'   detailed messages. Default is 1.
#'
#' @return A dataframe containing processed simulation results for the specified queen, including:
#'   - `bias`: The bias of the predicted IATEs compared to the true IATEs.
#'   - `se`: The standard error of the predictions.
#'   - `rmse`: The root mean squared error of the predictions.
#'   - Additional details from the simulation such as runtime and number of outliers.
#'
#' @details The function performs the following steps:
#'   1. Loads the predicted IATEs for the specified queen from the results directory.
#'   2. Loads the true IATEs for the dataset and outcome from the specified directory.
#'   3. Calculates evaluation metrics (bias, SE, RMSE) using the predicted and true IATEs.
#'   4. Returns the processed results as a dataframe.
#'   
#' @export
#' 
load_and_process_queen_results <- function(dataset_name, 
                                           queen, 
                                           outcome, 
                                           simulation_name, 
                                           true_iates_directory_path = here::here("true_iates"),
                                           verbose = 1 
                                          ) {
  
  
  # Create a directory path to the appended IATEs by queen
  dir_path <- here("results", "combined_IATEs_by_queen", dataset_name,
                   paste0("simulation_from_", simulation_name))
  
  #load appended IATEs for this queen
  file_name <- file.path(dir_path, paste0(queen, "_queen_predicted_IATEs.rds"))
  queen_results <- readRDS(file_name)
  sim_data_list <- list(predictedtau = queen_results$predictedtau, 
                        runtime = queen_results$runtime, 
                        failrate = queen_results$failrate)
  
  
  # Load true IATEs for this queen
  data_file_name <- paste0(dataset_name, "_", outcome, ".rds")
  iates_data_path <- file.path(true_iates_directory_path, data_file_name)
  true_iates <- readRDS(iates_data_path)
  true_iates <- true_iates[[queen]]
  
  # Calculate Bias, SE, RMSE
  size_test = dim(queen_results$predictedtau)[2]
  
  sim_results <- process_sim_results(sim_data_list, 
                                     true_tau = true_iates[1:size_test]) 
  sim_results$queen <- queen
  
  # Store sim_results in result_list
  return(sim_results)
  
}
