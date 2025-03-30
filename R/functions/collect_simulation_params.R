#' collect_simulation_params:: Collect simulation parameters
#'
#' This function collects and processes all the necessary parameters for the log.
#' It ensures that the parameters are properly formatted and converted to strings where needed.
#'
#' @param dataset_name A name for the dataset. 
#' @param covariates A character vector specifying the names of covariates used in the simulation.
#' @param outcome A character string specifying the outcome variable of interest.
#' @param treatment A character string specifying the treatment variable.
#' @param large_covariate_set An optional character vector specifying a large set of covariates. 
#' @param small_covariate_set An optional character vector specifying a small set of covariates. 
#' @param all_outcomes An optional character vector specifying all outcomes of interest. 
#' @param S An integer specifying the number of simulations to run.
#' @param queen_list An optional character vector specifying the models to include. 
#' @param p_tx A numeric value specifying the proportion of treatment.
#' @param size_train An integer specifying the size of the training dataset.
#' @param size_test An integer specifying the size of the test dataset.
#' @param PARALLEL A logical indicating whether to run simulations in parallel.
#' @param verbose An integer controlling the verbosity level of messages printed during execution.
#' @param include_LASSO A logical indicating whether to include the LASSO model in the simulation.
#' @param include_RF A logical indicating whether to include the Random Forest model in the simulation.
#' @param include_SL_S A logical indicating whether to include the Super Learner (S) model in the simulation.
#' @param include_SL_T A logical indicating whether to include the Super Learner (T) model in the simulation.
#' @param include_CDML A logical indicating whether to include the CDML model in the simulation.
#' @param include_XGBOOST A logical indicating whether to include the XGBOOST model in the simulation.
#' @param include_BART A logical indicating whether to include the BART model in the simulation.
#' @param master_seed An optional integer specifying the master seed for reproducibility.
#' @param baseline_seed An integer specifying the seed for baseline generation.
#' @param baseline_N An integer specifying the size of the baseline dataset.
#' @param baseline_directory_path A character string specifying the directory path for baseline data storage.
#' @param true_iates_directory_path A character string specifying the directory path for true IATEs storage.
#' @param simulation_name A character string or date specifying the simulation name. Defaults to the current timestamp if NULL.
#' @param start_time_formatted A formatted string representing the start time of the simulation.
#' @param end_time_formatted A formatted string representing the end time of the simulation.
#' @param total_time_formatted A formatted string representing the total time taken for the simulation.
#'
#' @return A named list of parameters, with all entries formatted as strings where necessary.
#'
#' @examples
#' \dontrun{
#'   params <- collect_simulation_params(
#'     dataset_name = "example_dataset",
#'     covariates = c("age", "income", "education"),
#'     outcome = "health_status",
#'     treatment = "treatment_group",
#'     large_covariate_set = NULL,
#'     small_covariate_set = NULL,
#'     all_outcomes = NULL,
#'     S = 100,
#'     chunk_size = 10,
#'     queen_list = NULL,
#'     p_tx = 0.5,
#'     size_train = 2000,
#'     size_test = 10000,
#'     PARALLEL = TRUE,
#'     verbose = 1000,
#'     include_LASSO = FALSE,
#'     include_RF = TRUE,
#'     include_SL_S = FALSE,
#'     include_SL_T = FALSE,
#'     include_CDML = FALSE,
#'     include_XGBOOST = TRUE,
#'     include_BART = FALSE,
#'     master_seed = 12345,
#'     baseline_seed = 68814,
#'     baseline_N = 100000,
#'     baseline_directory_path = "baseline_path",
#'     true_iates_directory_path = "true_iates_path",
#'     simulation_name = Sys.time(),
#'     start_time_formatted = "2024-12-18 10:00:00",
#'     end_time_formatted = "2024-12-18 10:30:00",
#'     total_time_formatted = "0 day(s) and 0.04 hour(s)"
#'   )
#' }
#'
#' @export
#' 
collect_simulation_params <- function(# Required arguments
                                      dataset_name,    
                                      covariates,
                                      outcome,
                                      treatment,
                                      
                                      # Optional arguments
                                      large_covariate_set, 
                                      small_covariate_set, 
                                      all_outcomes,
                                      S,  
                                      queen_list,
                                      p_tx,
                                      size_train,
                                      size_test,
                                      PARALLEL,
                                      verbose,
                                      include_LASSO,
                                      include_RF,
                                      include_SL_S,
                                      include_SL_T,
                                      include_CDML,
                                      include_XGBOOST,
                                      include_BART,
                                      master_seed,
                                      baseline_seed, 
                                      baseline_N, 
                                      baseline_directory_path,
                                      true_iates_directory_path,
                                      simulation_name,
                                      start_time_formatted,
                                      end_time_formatted,
                                      total_time_formatted) 
{
  
  # Collect parameters explicitly
  
  if (!is.null(large_covariate_set)) {
    large_covariate_set_str <- paste(large_covariate_set, collapse = ", ")
  } else {
    large_covariate_set_str <- param_to_char(large_covariate_set)
  }
  
  if (!is.null(small_covariate_set)) {
    small_covariate_set_str <- paste(small_covariate_set, collapse = ", ")
  } else {
    small_covariate_set_str <- param_to_char(small_covariate_set)
  }
  
  if (!is.null(all_outcomes)) {
    all_outcomes_str <- paste(all_outcomes, collapse = ", ")
  } else {
    all_outcomes_str <- param_to_char(all_outcomes)
  }
  
  if (!is.null(queen_list)) {
    queen_list_str <- paste(queen_list, collapse = ", ")
  } else {
    queen_list_str <- param_to_char(queen_list) # Convert queen_list to string
  }
  
  # Collect parameters explicitly
  params <- list(
    dataset_name = param_to_char(dataset_name),
    covariates = paste(covariates, collapse = ", "),
    outcome = param_to_char(outcome),
    treatment = param_to_char(treatment),
    large_covariate_set = large_covariate_set_str,
    small_covariate_set = small_covariate_set_str,
    all_outcomes = all_outcomes_str,
    S = param_to_char(S),
    queen_list = queen_list_str,
    p_tx = param_to_char(p_tx),
    size_train = param_to_char(size_train),
    size_test = param_to_char(size_test),
    PARALLEL = param_to_char(PARALLEL),
    verbose = param_to_char(verbose),
    include_LASSO = param_to_char(include_LASSO),
    include_RF = param_to_char(include_RF),
    include_SL_S = param_to_char(include_SL_S),
    include_SL_T = param_to_char(include_SL_T),
    include_CDML = param_to_char(include_CDML),
    include_XGBOOST = param_to_char(include_XGBOOST),
    include_BART = param_to_char(include_BART),
    master_seed = param_to_char(master_seed),
    baseline_seed = param_to_char(baseline_seed),
    baseline_N = param_to_char(baseline_N),
    baseline_directory_path = param_to_char(baseline_directory_path),
    true_iates_directory_path = param_to_char(true_iates_directory_path),
    simulation_name = param_to_char(simulation_name),
    start_time = start_time_formatted,
    end_time = end_time_formatted,
    total_time = total_time_formatted
  )

  return(params)
}