####################################################################################                 
#                                                                         
# Created on:   06/23/2024
# Purpose:      Create simulation launcher function
# Authors:      Luke Miratrix, Polina Polskaia, Nick Commins
# 
###################################################################################


# Load script 06
source(here::here("simulation_pipeline/06_simulation_driver.R"))

###################################################################################
# SIMULATION LANCHER FUNCTIONS ----
###################################################################################


#' setup_simulation_data:: Set up the datasets for a given simulation scenario
#'
#' This function prepares the data required for running a simulation by loading or 
#' generating baseline data, treatment assignments, and true IATEs.
#'
#' @param dataset_name A character string specifying the name of the dataset.
#' @param covariates A character vector specifying the names of covariates.
#' @param outcome A character string specifying the outcome variable of interest.
#' @param treatment A character string specifying the treatment variable.
#' @param real_data An optional data frame containing the real data. If `NULL`, 
#'   baseline data will be generated. Default is `NULL`.
#' @param large_covariate_set An optional character vector specifying a large set of 
#'   covariates. If `NULL`, `covariates` will be used. Default is `NULL`.
#' @param small_covariate_set An optional character vector specifying a small set of 
#'   covariates. If `NULL`, `covariates` will be used. Default is `NULL`.
#' @param all_outcomes An optional character vector specifying all outcome variables. 
#'   If `NULL`, `outcome` will be used. Default is `NULL`.
#' @param queen_list An optional character vector specifying the list of models (queens) 
#'   for generating IATEs. Default is `NULL`.
#' @param p_tx An optional numeric value specifying the proportion of treatment. 
#'   If `NULL`, it will be derived from the baseline data. Default is `NULL`.
#' @param size_train An integer specifying the size of the training dataset. Default is 2000.
#' @param size_test An integer specifying the size of the test dataset. Default is 10000.
#' @param baseline_seed An integer specifying the seed for baseline data generation. Default is 68814.
#' @param baseline_N An integer specifying the size of the baseline dataset. Default is 100000.
#' @param baseline_directory_path A character string specifying the path to the baseline directory. 
#'   Default is `here::here("baseline")`.
#' @param true_iates_directory_path A character string specifying the path to the directory 
#'   containing true IATEs. Default is `here::here("true_iates")`.
#' @param verbose A numeric value controlling the verbosity of the output. Higher values 
#'   produce more detailed messages. Default is 1000.
#'
#' @return A list containing:
#'   - `baselinedata`: A data frame with baseline data and treatment assignments.
#'   - `true_iates_by_queen`: A data frame containing true IATEs for each model (queen).
#'   - `p_tx`: The proportion of treatment.
#'   - `binary_outcome`: A logical indicating whether the outcome is binary.
#'
#' @details The function performs the following steps:
#'   1. Loads or generates baseline data and assigns treatment based on `p_tx`.
#'   2. Ensures necessary covariates and outcomes are included.
#'   3. Loads or generates true IATEs for the specified models (queens).
#'   4. Validates consistency between baseline data and IATEs.
#'
#' @export
#' 
setup_simulation_data <- function(# Required arguments
                                  dataset_name,   
                                  covariates,
                                  outcome,
                                  treatment,
                                  
                                  # Optional arguments
                                  real_data = NULL, 
                                  large_covariate_set = NULL, 
                                  small_covariate_set = NULL, 
                                  all_outcomes = NULL,
                                  queen_list = NULL,
                                  p_tx = NULL,
                                  size_train = 2000,
                                  size_test = 10000,
                                  baseline_seed = 68814, 
                                  baseline_N = 100000, 
                                  baseline_directory_path = here::here("baseline"),
                                  true_iates_directory_path = here::here("true_iates"),
                                  verbose = 1000 
                                 ) {
  
  # Check if large_covariate_set is NULL and assign covariates if true
  if (is.null(large_covariate_set)) {
    large_covariate_set <- covariates
  }
  
  # Check if small_covariate_set is NULL and assign covariates if true
  if (is.null(small_covariate_set)) {
    small_covariate_set <- covariates
  }
  
  # Check if all_outcomes is NULL and assign outcome if true
  if (is.null(all_outcomes)) {
    all_outcomes <- outcome
  }
  
  # Load (or generate) baseline object
  baseline_object <- load_or_generate_baseline_object(dataset_name = dataset_name,
                                                      baseline_directory_path = baseline_directory_path,
                                                      real_data = real_data,
                                                      all_outcomes = all_outcomes, 
                                                      covariates_names = large_covariate_set,
                                                      treatment_var = treatment,
                                                      baseline_seed = baseline_seed,
                                                      baseline_N = baseline_N
                                                     )
  
  # Take baseline data from the baseline object
  baselinedata <- baseline_object$Data
  
  # Check that needed columns are present in baseline data
  stopifnot(paste0(outcome, "_Y0") %in% names(baselinedata))
  
  # Generate or load true IATEs
  true_iates_by_queen <- load_or_generate_iates(dataset_name = dataset_name, 
                                                true_iates_directory_path = true_iates_directory_path, 
                                                real_data = real_data, 
                                                queen_list = queen_list, 
                                                baseline_seed = baseline_seed, 
                                                baseline_data = baselinedata, 
                                                small_covariate_set = small_covariate_set, 
                                                outcome = outcome,
                                                treatment_var = treatment, 
                                                sigma_tau = sigma_tau, 
                                                verbose = verbose - 1
                                               )
  
  # Check that numbers of rows in baseline data and IATEs data are the same
  stopifnot(nrow(baselinedata) == nrow(true_iates_by_queen))
  
  # Set proportion treatment if not set.
  stopifnot(length(treatment) == 1)
  if (is.null(p_tx)) {
    p_tx <- baseline_object$p_tx
    stopifnot(!is.na(p_tx))
  } else {
    stopifnot(p_tx > 0 && p_tx < 1)
  }
  
  # Generate treatment with p_tx proportion treated
  set.seed(baseline_seed)
  n_baseline <- nrow(baselinedata)
  actual_Tx <- as.numeric(sample(n_baseline) <= p_tx * n_baseline) 
  baselinedata[, treatment] = actual_Tx
  
  stopifnot(treatment %in% names(baselinedata))
  
  binary_outcome <- baseline_object$is_binary[[outcome]]
  stopifnot(!is.null(binary_outcome))
  
  list(baselinedata = baselinedata,
       true_iates_by_queen = true_iates_by_queen,
       p_tx = p_tx,
       binary_outcome = binary_outcome)
}


#' perform_simulation_launcher:: Launches Simulations
#'
#' This function facilitates the execution of parallel or sequential
#' simulations with various algorithms.
#'
#' @param dataset_name A name for the dataset. Call you data any way
#'   you like.
#' @param covariates A character vector specifying the names of
#'   covariates.
#' @param outcome A character string specifying the outcome of
#'   interest.
#' @param treatment A character string specifying the treatment
#'   variable.
#' @param true_iates_directory_path A character string specifying the
#'   path to the directory for (possibly pre-generated) true IATEs.
#'   Default is `here::here("true_iates")`.
#' @param real_data A data frame containing the real data or directory
#'   to where it is saved as a csv.
#' @param large_covariate_set An optional character vector specifying
#'   a large set of covariates. If NULL, `covariates` will be used.
#' @param small_covariate_set An optional character vector specifying
#'   a small set of covariates. If NULL, `covariates` will be used.
#' @param all_outcomes An optional character vector specifying all
#'   outcomes. If NULL, `outcome` will be used. NOTE: This is only
#'   used if we need to generate baseline data.  Should probably not
#'   be part of this method?
#' @param baseline_seed An integer specifying the seed for baseline
#'   generation. Default is 68814.
#' @param baseline_N An integer specifying the size of the baseline
#'   dataset. Default is 100000.
#' @param baseline_directory_path A character string specifying the
#'   path to the baseline directory. Default is
#'   `here::here("baseline")`.
#' @param master_seed An optional integer specifying the master seed
#'   for reproducibility. If NULL, a default seed will be used.
#' @param S An integer specifying the number of simulations to run.
#'   Default is 100.
#' @chunk_size An integer defining by how many iterations processed by one core.
#'   Default is 10.
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
#'   parallel. Default is FALSE.
#' @param simulation_name Either a Date object specifying when
#'   simulation is run (Default is `Sys.time()`), or a string for a
#'   directory name to hold simulation results.
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
#' @param verbose A logical indicating whether to print messages.
#'   Default is TRUE.
#'
#' @return NULL
#'
#' @export
#' 
perform_simulation_launcher <- function(# Required arguments
                                        dataset_name,   
                                        covariates,
                                        outcome,
                                        treatment,
                                        
                                        # Optional arguments to load pre-generated data
                                        true_iates_directory_path = here::here("true_iates"),
                                        
                                        # Optional arguments to generate data on fly if needed
                                        real_data = NULL, 
                                        large_covariate_set = NULL, 
                                        small_covariate_set = NULL, 
                                        all_outcomes = NULL,
                                        baseline_seed = 68814, 
                                        baseline_N = 100000, 
                                        baseline_directory_path = here::here("baseline"),
                                        master_seed = NULL,
                                        
                                        # Arguments about the simulation itself
                                        S = 100,  
                                        chunk_size = 10,
                                        queen_list = NULL,
                                        model_list = NULL,
                                        
                                        p_tx = NULL,
                                        size_train = 2000,
                                        size_test = 10000,
                                        PARALLEL = FALSE,
                                        simulation_name = Sys.time(),
                                        
                                        # What models to test in simulation
                                        include_LASSO = FALSE,
                                        include_RF = FALSE,
                                        include_SL_S = FALSE,
                                        include_SL_T = FALSE,
                                        include_CDML = FALSE,
                                        include_XGBOOST = FALSE,
                                        include_BART = FALSE,
                                        
                                        # Other controls
                                        verbose = 1000 ) {
                                        
  # Get list of which queens we are testing 
  if (is.null(queen_list)) {
    queen_list <- model_queen_lists(include_RF = include_RF, 
                                    include_LASSO = include_LASSO, 
                                    include_SL_S = include_SL_S, 
                                    include_SL_T = include_SL_T, 
                                    include_CDML = include_CDML, 
                                    include_XGBOOST = include_XGBOOST, 
                                    include_BART = include_BART,
                                    make_queen_list = TRUE)
    
    qlist = paste(queen_list, collapse=', ')
    message(glue::glue("Generating and using queen list: {qlist}"))
  }
  
  if (is.null(model_list)) {
    model_list <- model_queen_lists(include_RF = include_RF, 
                                    include_LASSO = include_LASSO, 
                                    include_SL_S = include_SL_S, 
                                    include_SL_T = include_SL_T, 
                                    include_CDML = include_CDML, 
                                    include_XGBOOST = include_XGBOOST, 
                                    include_BART = include_BART,
                                    make_queen_list = FALSE)
    
    qlist = paste(model_list, collapse=', ')
    message(glue::glue("Working on models: {qlist}"))
  }
  
  
  # Load all the data for the given scenario and get it ready 
  data <- setup_simulation_data(real_data = real_data, 
                                dataset_name = dataset_name,   
                                covariates = covariates,
                                outcome = outcome,
                                treatment = treatment,
                                large_covariate_set = large_covariate_set, 
                                small_covariate_set = small_covariate_set, 
                                all_outcomes = all_outcomes,
                                queen_list = queen_list,
                                p_tx = p_tx,
                                size_train = size_train,
                                size_test = size_test,
                                baseline_seed = baseline_seed, 
                                baseline_N = baseline_N, 
                                baseline_directory_path = baseline_directory_path,
                                true_iates_directory_path = true_iates_directory_path,
                                verbose = verbose)
                              
  # Set seed after we possibly generate data, which would depending on
  # if we had a saved file, use a different number of random draws.
  if (is.null(master_seed)) {
    master_seed = 40444
    warning(paste("Seed hardcoded in perform_simulation_launcher:", master_seed))
  }
  
  set.seed(master_seed)  
  
  # Stop if no simulation cycles to run
  if ( S == 0 ) {
    return( invisible( NULL ) )
  }
  
  
  # Divide to chunks for parallel processing 
  
  # Generate a unique seed for each iteration
  iteration_seeds <- sample(1:1e6, S)
  
  # Define chunk size and prepare tasks
  total_iterations <- S
  
  # Calculate the number of chunks
  num_chunks <- ceiling(S / chunk_size)  
  
  # Create task entries per chunk
  tasks <- expand.grid(
    queen = queen_list,
    chunk_id = seq(1, num_chunks)  
  )
  
  # Pre-allocate a list for seed chunks
  seed_chunks <- vector("list", num_chunks)
  
  # Fill each chunk with the appropriate seeds
  for (i in 1:num_chunks) {
    start_index <- (i - 1) * chunk_size + 1
    end_index <- min(i * chunk_size, S)
    seed_chunks[[i]] <- iteration_seeds[start_index:end_index]
  }
  
  # Pre-allocate a list for iteration numbers
  iter_chunks <- vector("list", num_chunks)
  
  # Fill each chunk with the appropriate iteration numbers
  for (i in 1:num_chunks) {
    start_index <- (i - 1) * chunk_size + 1
    end_index <- min(i * chunk_size, S)
    iter_chunks[[i]] <- start_index:end_index
  }
  
  dir_path <- generate_directory(folder_name = dataset_name, 
                                 outcome = outcome,
                                 simulation_name = simulation_name)
  
  
  # Run the task for each queen-chunk  ----
  process_tasks <- function(queen, chunk_id) {
    current_seeds <- seed_chunks[[chunk_id]]
    current_iterations <- iter_chunks[[chunk_id]]
    
    if (verbose > 0) {
      if (length(verbose) < 5 ) {
        sds <- paste(current_seeds, collapse = ", ")
      } else {
        sds <- paste0(current_seeds[1], ", ..., ", current_seeds[length(current_seeds)])
      } 
      cat(glue::glue("Iteration {min(current_iterations)} to {max(current_iterations)} for queen {queen} with seeds {sds}"))
      cat("\n")
    }
    
    IATE <- data$true_iates_by_queen[[queen]]
    if (is.null(IATE)) {
      message(glue::glue("No pre-generated IATEs for queen {queen} -- use generator to make and save to file"))
      return(NULL)
    } else {
      baseline_full <- add_treatment_effects(baseline = data$baselinedata,
                                             IATE = IATE,
                                             outcome = outcome,
                                             treatment = treatment, 
                                             binary_outcome = data$binary_outcome )
      
      # Create test set and separate it from the rest of the baseline data
      test_set <- baseline_full[1:size_test, ]
      baseline_remainder <- baseline_full[-(1:size_test), ]
    }
    
    # Run the simulation
    sim_results <- perform_simulation(start_iteration = min(current_iterations), 
                                      end_iteration = max(current_iterations),
                                      test_set = test_set, 
                                      baseline_remainder = baseline_remainder,
                                      size_train = size_train, 
                                      seeds = current_seeds,
                                      p_tx = NULL, 
                                      outcome = outcome,
                                      treatment = treatment,
                                      covariates = covariates,
                                      verbose = verbose - 1,
                                      model_list = model_list
    ) 
    
    # Save the results to a file
    file_name <- file.path(dir_path, paste0("fragment_", chunk_id, "_", queen, ".rds"))
    saveRDS(sim_results, file_name)
    
    return(sim_results)
  }
  
  num_chunks = nrow(tasks)
  
  # Run simulations in parallel or sequentially
  if (PARALLEL) {
    num_cores = parallel::detectCores()
    plan(multisession, workers = num_cores) 
    if (verbose > 0) {
      message(glue::glue("Running simulations in parallel with {num_chunks} chunks and {num_cores} cores."))
    }
    future_pwalk(list(queen = tasks$queen, chunk_id = tasks$chunk_id),
                 process_tasks, .options = furrr_options(seed = NULL))
  } else {
    if (verbose > 0) {
      message(glue::glue( "Running simulations sequentially with {num_chunks} chunks."))
    }
    pwalk(list(queen = tasks$queen, chunk_id = tasks$chunk_id),
          process_tasks, .progress = TRUE)
  }
  
  
  if (verbose > 0) {
    message(glue::glue( "Ran {nrow(tasks)} chunks across {length(unique(tasks$queen))} queens." ) )
  }
  
  return(invisible(0))
}

###################################################################################
# TEST BLOCK ----
###################################################################################


if (FALSE) {  
  
  dat <- setup_simulation_data( 
    dataset_name = "mnd",
    covariates = c("X3,X4"),
    outcome = "Y_continuous",
    treatment = "Z" )
  
  mnd_data <- load_and_validate_real_data( here::here("datasets/mnd.csv") )
  
  start_time <- Sys.time()
  
  test <- perform_simulation_launcher(# Required arguments
                                      real_data = mnd_data, 
                                      dataset_name =  "mnd",    
                                      covariates = paste0( "X", 1:5 ),
                                      outcome = "Y_continuous",
                                      treatment = "Z",
                                      
                                      # Optional arguments
                                      large_covariate_set = paste0( "X", 1:9 ), 
                                      small_covariate_set = paste0( "X", 1:5 ), 
                                      all_outcomes = c("Y_continuous", "Y_binary"),
                                      S = 17,  
                                      queen_list = c( "ATE", "OLS S" ),
                                      p_tx = NULL,
                                      size_train = 2000,
                                      size_test = 10000,
                                      PARALLEL = FALSE,
                                      include_LASSO = TRUE,
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
                                      simulation_name = Sys.time(),
                                      verbose = 100 )
                                    
  end_time <- Sys.time()
  total_time <- end_time - start_time
  print(total_time)      
  
}

