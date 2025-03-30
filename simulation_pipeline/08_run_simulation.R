###################################################################################
#                                                                                                        
#                                                                         
# Created on:   06/29/2024
# Purpose:      Create a function that saves the simulation parameters, runs the
#               simulation and creates function to aggregate the results
# Authors:      Luke Miratrix, Polina Polskaia, Nick Commins
# 
#
###################################################################################


# Load script 07
source(here::here("simulation_pipeline/07_simulation_launcher.R"))

###################################################################################
# SIMULATION RUN FUNCTION AND HELPER FUNCTIONS ----
###################################################################################


#' run_simulation:: Run simulation
#'
#' This function runs a simulation, logs the running parameters, and
#' aggregates the Individualized Average Treatment Effects (IATEs).
#'
#' @param real_data A data frame containing the real data. 
#' @param dataset_name A name for the dataset. Call you data any way
#'   you like.
#' @param covariates A character vector specifying the names of
#'   covariates.
#' @param outcome A character string specifying the outcome of
#'   interest.
#' @param treatment A character string specifying the treatment
#'   variable.
#' @param large_covariate_set An optional character vector specifying
#'   a large set of covariates. If NULL, `covariates` will be used.
#' @param small_covariate_set An optional character vector specifying
#'   a small set of covariates. If NULL, `covariates` will be used.
#' @param all_outcomes An optional character vector specifying all
#'   outcomes. If NULL, `outcome` will be used.
#' @param S An integer specifying the number of simulations to run.
#'   Default is 100.
#' @param queen_list An optional character vector specifying the list
#'   of models to include. If NULL, a default list will be generated
#'   based on model inclusion parameters.
#' @param p_tx An optional numeric value specifying the proportion of
#'   treatment. If NULL, it will be calculated from the real data.
#' @param size_train An integer specifying the size of the training
#'   set. Default is 2000.
#' @param size_test An integer specifying the size of the test set.
#'   Default is 10000.
#' @param PARALLEL A logical indicating whether to run simulations in
#'   parallel. Default is TRUE.
#' @param verbose A numeric value controlling the verbosity of the output. 
#'   Higher values produce more detailed messages. Default is 1000.
#' @param include_LASSO A logical indicating whether to include LASSO
#'   model. Default is FALSE.
#' @param include_RF A logical indicating whether to include Random
#'   Forest model. Default is FALSE.
#' @param include_SL_S A logical indicating whether to include Super
#'   Learner S model. Default is FALSE.
#' @param include_SL_T A logical indicating whether to include Super
#'   Learner T model. Default is FALSE.
#' @param include_CDML A logical indicating whether to include CDML
#'   model. Default is FALSE.
#' @param include_XGBOOST A logical indicating whether to include
#'   XGBOOST model. Default is FALSE.
#' @param include_BART A logical indicating whether to include BART
#'   model. Default is FALSE.
#' @param master_seed An optional integer specifying the master seed
#'   for reproducibility. If NULL, a default seed will be used.
#' @param baseline_seed An integer specifying the seed for baseline
#'   generation. Default is 68814.
#' @param baseline_N An integer specifying the size of the baseline
#'   dataset. Default is 100000.
#' @param baseline_directory_path A character string specifying the
#'   path to the baseline directory. Default is
#'   `here::here("baseline")`.
#' @param true_iates_directory_path A character string specifying the
#'   path to the directory for true IATEs. Default is
#'   `here::here("true_iates")`.
#' @param simulation_name Either the name of the simulation run, or a Date
#'   object specifying today's date. Default is `Sys.time()`.
#'
#' @return aggregated IATEs
#'
#' @export
#' 
run_simulation <- function(# Required arguments
                           real_data = NULL, 
                           dataset_name,    
                           covariates,
                           outcome,
                           treatment,
                            
                           # Optional arguments
                           large_covariate_set = NULL, 
                           small_covariate_set = NULL, 
                           all_outcomes = NULL,
                           S = 100,  
                           chunk_size = 10,
                           queen_list = NULL,
                           model_list = NULL,
                           p_tx = NULL,
                           size_train = 2000,
                           size_test = 10000,
                           PARALLEL = TRUE,
                           verbose = 1000,
                           include_LASSO = FALSE,
                           include_RF = FALSE,
                           include_SL_S = FALSE,
                           include_SL_T = FALSE,
                           include_CDML = FALSE,
                           include_XGBOOST = FALSE,
                           include_BART = FALSE,
                           master_seed = NULL,
                           baseline_seed = 68814, 
                           baseline_N = 100000, 
                           baseline_directory_path =  here::here("baseline"),
                           true_iates_directory_path = here::here("true_iates"),
                           simulation_name = Sys.time() 
                          ) {
  
  # Record start time
  start_time <- Sys.time()
  start_time_formatted <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  # Ensure simulation_name is formatted for consistency
  formatted_simulation_name <- if (is.character(simulation_name)) {
    simulation_name
  } else if (is.null(simulation_name)) {
    "no_name"
  } else if (inherits(simulation_name, "POSIXt")) {
    format(simulation_name, "%m%d%y_%H%M")
  } else {
    as.character(simulation_name)
  }
  
  if (verbose > 0) {
    message(glue::glue("Running simulation for {dataset_name}/{outcome}. Sim name is '{simulation_name}'."))
  }
  
  # Determine which queens we are testing
  if (is.null(queen_list)) {
    queen_list <- model_queen_lists(include_RF = include_RF, 
                                    include_LASSO = include_LASSO, 
                                    include_SL_S = include_SL_S, 
                                    include_SL_T = include_SL_T, 
                                    include_CDML = include_CDML, 
                                    include_XGBOOST = include_XGBOOST, 
                                    include_BART = include_BART,
                                    make_queen_list = TRUE)
    
    qlist <- paste(queen_list, collapse=', ')
    if (verbose > 0) {
      message(glue::glue("Making default queen list: {qlist}"))
    }
  }
  
  
  # Run the simulation
  simulation <- perform_simulation_launcher(# Required arguments
                                            real_data = real_data, 
                                            dataset_name =  dataset_name,    
                                            covariates = covariates,
                                            outcome = outcome,
                                            treatment = treatment,
                                            
                                            # Optional arguments
                                            large_covariate_set = large_covariate_set, 
                                            small_covariate_set = small_covariate_set, 
                                            all_outcomes = all_outcomes,
                                            S = S,  
                                            chunk_size = chunk_size,
                                            queen_list = queen_list,
                                            model_list = model_list,
                                            p_tx = p_tx,
                                            size_train = size_train,
                                            size_test = size_test,
                                            PARALLEL = PARALLEL,
                                            verbose = verbose - 1,
                                            include_LASSO = include_LASSO,
                                            include_RF = include_RF,
                                            include_SL_S = include_SL_S,
                                            include_SL_T = include_SL_T,
                                            include_CDML = include_CDML,
                                            include_XGBOOST = include_XGBOOST,
                                            include_BART = include_BART,
                                            master_seed = master_seed,
                                            baseline_seed = baseline_seed, 
                                            baseline_N = baseline_N, 
                                            baseline_directory_path = baseline_directory_path,
                                            true_iates_directory_path = true_iates_directory_path,
                                            simulation_name = formatted_simulation_name)
  
  
  # Bail if we didn't actually do any simulations.  Nothing to combine!
  if (S == 0) {
    message("Simulation setup run but no simulations conducted")
    return(invisible(NULL))
  }
  
  # Loop through the queens to combine predicted IATEs by queen
  for (queen in queen_list) {
    combine_fragments_and_save(queen = queen, 
                               outcome = outcome, 
                               dataset_name = dataset_name, 
                               simulation_name = formatted_simulation_name,
                               verbose = verbose - 1)
  }
  
  # Aggregate IATEs
  stacked_result <- load_and_process_all_results(queen_list, 
                                                 outcome = outcome, 
                                                 dataset_name = dataset_name, 
                                                 simulation_name = formatted_simulation_name,
                                                 true_iates_directory_path = true_iates_directory_path,
                                                 verbose = verbose - 1)
  
  # Record end time and time to run the simulation
  end_time <- Sys.time() 
  end_time_formatted <- format(end_time, "%Y-%m-%d %H:%M:%S")
  total_time <- end_time - start_time
  
  # Calculate the total hours and minutes from the total_time object
  total_hours <- as.numeric(total_time, units = "hours")
  total_days <- floor(total_hours / 24)
  remaining_hours <- total_hours %% 24
  
  # Format the total time as a string
  total_time_formatted <- sprintf("%d day(s) and %.2f hour(s)",
                                  total_days, remaining_hours)
  
  # Collect parameters using the helper function
  params <- collect_simulation_params(dataset_name =  dataset_name,    
                                      covariates = covariates,
                                      outcome = outcome,
                                      treatment = treatment,
                                      
                                      # Optional arguments
                                      large_covariate_set = large_covariate_set, 
                                      small_covariate_set = small_covariate_set, 
                                      all_outcomes = all_outcomes,
                                      S = S,  
                                      queen_list = queen_list,
                                      p_tx = p_tx,
                                      size_train = size_train,
                                      size_test = size_test,
                                      PARALLEL = PARALLEL,
                                      verbose = verbose - 1,
                                      include_LASSO = include_LASSO,
                                      include_RF = include_RF,
                                      include_SL_S = include_SL_S,
                                      include_SL_T = include_SL_T,
                                      include_CDML = include_CDML,
                                      include_XGBOOST = include_XGBOOST,
                                      include_BART = include_BART,
                                      master_seed = master_seed,
                                      baseline_seed = baseline_seed, 
                                      baseline_N = baseline_N, 
                                      baseline_directory_path = baseline_directory_path,
                                      true_iates_directory_path = true_iates_directory_path,
                                      simulation_name = formatted_simulation_name,
                                      start_time_formatted = start_time_formatted,
                                      end_time_formatted = end_time_formatted,
                                      total_time_formatted = total_time_formatted)
  
  # Log the simulation run
  log_simulation_run(params, verbose)
  
  return(stacked_result)
}


###################################################################################
# TEST BLOCK ----
###################################################################################


if (FALSE) {  
  
  mnd_data <- load_and_validate_real_data(here::here("datasets/mnd.csv"))
  
  test <- run_simulation(# Required arguments
                         real_data = mnd_data, 
                         dataset_name = "mnd",    
                         covariates = paste0("X", 1:5),
                         outcome = "Y_continuous",
                         treatment = "Z",
                          
                         # Optional arguments
                         large_covariate_set = paste0("X", 1:9), 
                         small_covariate_set = paste0( "X", 1:5 ), 
                         all_outcomes = c("Y_continuous", "Y_binary"),
                         S = 3,  
                         chunk_size = 7,
                         queen_list = NULL,
                         p_tx = NULL,
                         size_train = 2000,
                         size_test = 10000,
                         PARALLEL = FALSE,
                         verbose = 1000,
                         include_LASSO = FALSE,
                         include_RF = FALSE,
                         include_SL_S = FALSE,
                         include_SL_T = FALSE,
                         include_CDML = FALSE,
                         include_XGBOOST = FALSE,
                         include_BART = FALSE,
                         master_seed = NULL,
                         baseline_seed = 68814, 
                         baseline_N = 100000, 
                         baseline_directory_path = here::here("baseline"),
                         true_iates_directory_path = here::here("true_iates"),
                         simulation_name = "mnd_test") 
                        
  # View the simulation results
  print(test)
  
  # Combine and save fragments
  combined_results <- combine_fragments_and_save(
    dataset_name = "mnd",  
    queen = "ATE", 
    simulation_name = "mnd_test")
  print(combined_results)
  
  # Load and process results for a single queen
  queen_results <- load_and_process_queen_results(
    dataset_name = "mnd", 
    queen = "ATE",
    outcome = "Y_continuous", 
    simulation_name = "mnd_test")
  print(head(queen_results))
  print(table(queen_results$queen))  
  print(table(queen_results$metric))  
  
  # Load and process results for all queens
  aggregated_results <- load_and_process_all_results(
    dataset_name = "mnd", 
    outcome = "Y_continuous",
    queen_list = c("ATE", "OLS S"), 
    simulation_name = "mnd_test")
  print(aggregated_results)
  
  # Filter results for specific conditions
  filtered_results <- aggregated_results %>% filter(queen == "ATE", id == 1)
  print(filtered_results)
  
  # Summary statistics for aggregated results
  print(table(aggregated_results$queen))
}
