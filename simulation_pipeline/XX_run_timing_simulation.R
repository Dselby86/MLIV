

# This script times the models.
#
# NOTE:
# When running for real, the training and test set size should be
# increased to the normal levels
#


library( tidyverse )
library(furrr)
library(purrr)
library(tibble)


NUM_ITERATIONS = 2

source( here::here( "simulation_pipeline/06_simulation_driver.R") )
source( here::here( "simulation_pipeline/07_simulation_launcher.R") )

options(error = NULL)

# Get simulation scenario ready ----

mnd_data <- load_and_validate_real_data( real_data = here::here("datasets/mnd.csv"))

# Load the data and IATEs
data <- setup_simulation_data(
  real_data = mnd_data, 
  dataset_name =  "mnd",    
  covariates = paste0( "X", 1:5 ),
  outcome = "Y_continuous",
  treatment = "Z",
  
  # Optional arguments in case we need to generate the IATEs, etc.
  large_covariate_set = paste0( "X", 1:9 ), 
  small_covariate_set = paste0( "X", 1:5 ), 
  all_outcomes = c("Y_continuous", "Y_binary"),
  queen_list = NULL,
  p_tx = NULL,
  size_train = 2000,
  size_test = 10000,
  baseline_seed = 68814, 
  baseline_N = 100000, 
  baseline_directory_path = here::here("baseline"),
  true_iates_directory_path = here::here("true_iates"),
  verbose = 100 )


# Look at queen IATEs, to check
head( data$true_iates_by_queen )


# Make method to run simulation ----


run_one_simulation <- function(
    chunk_id,
    data,
    queen,
    covariates,
    outcome,
    treatment,
    seed,
    S = 3,
    p_tx = NULL,
    size_train = 2000,
    size_test = 10000,
    model_list,
    dir_name = here::here( "results/timing" ),
    verbose = 1000 ) {
  
  
  current_seeds = seed + 13 * 1:S
  
  
  if ( verbose > 0 ) {
    cli::cli_alert_info( "Starting '{model_list}' with seed {seed}" )
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
  
  # Run the simulation (using function in script 06)
  sim_results <- perform_simulation(start_iteration = 1, 
                                    end_iteration = S,
                                    test_set = test_set, 
                                    baseline_remainder = baseline_remainder,
                                    size_train = size_train, 
                                    seeds = current_seeds,
                                    p_tx = NULL, 
                                    outcome = outcome,
                                    treatment = treatment,
                                    covariates = covariates,
                                    verbose = verbose - 1,
                                    model_list = model_list,
                                    sd_y0_real = data$sd_y0_real, 
                                    ate_real = data$ate_real
  )
  
  file_name <- file.path(dir_name, paste0("fragment_", chunk_id, "_v_", queen, ".rds"))
  saveRDS(sim_results, file_name)
  
  if ( verbose > 0 ) {
    cli::cli_alert_success( "Saved results {file_name} for {chunk_id}" )
  }
  
  invisible( sim_results )
}


# Testing code ----
if ( FALSE ) {
  rs <- run_one_simulation( chunk_id = "testing",
                            data = data,
                            queen = "ATE",
                            covariates = paste0( "X", 1:5 ),
                            outcome = "Y_continuous",
                            treatment = "Z",
                            seed = 4343443,
                            S = 1,  
                            p_tx = NULL,
                            size_train = 2000,
                            size_test = 100,
                            model_list = "CDML",
                            verbose = 100,
                            dir_name = here::here( "results" )
  )

  rs  
  dim( rs$runtime )
}





# Run simulation in parallel ----



# Define the list of "include" flags
model_queen_lists()
models = model_queen_lists( include_LASSO = FALSE,
                            make_queen_list = FALSE )
models = c( models, "LASSO INF", "LASSO T", "LASSO MCM EA" )

args_df = expand_grid( model_list = models,
                       chunk_id = 1:NUM_ITERATIONS ) %>%
  mutate( chunk_id = paste0( model_list, "-", chunk_id ) )
args_df
args_df = slice_sample( args_df, n=nrow(args_df) )

dir_name = generate_directory(folder_name = "timing", simulation_name = NULL )
cli::cli_alert_info("Saving results to {dir_name}." )



if ( FALSE ) {
  # testing, run in sequence for debugging.
  
  args_df = args_df[ -c(1:8), ]
  results <- pmap(args_df, run_one_simulation,
                  data = data,
                  queen = "ATE",
                  covariates = paste0( "X", 1:5 ),
                  outcome = "Y_continuous",
                  treatment = "Z",
                  seed = 4343443,
                  S = 3,  
                  p_tx = NULL,
                  size_train = 750,
                  size_test = 100,
                  dir_name = dir_name,
                  verbose = 2 )
  
  run_one_simulation( data = data,
                      queen = "ATE",
                      covariates = paste0( "X", 1:5 ),
                      outcome = "Y_continuous",
                      treatment = "Z",
                      model_list = "CF_LC",
                      chunk_id = "CF_LC",
                      seed = 4343443,
                      S = 3,  
                      p_tx = NULL,
                      size_train = 750,
                      size_test = 100,
                      dir_name = dir_name,
                      verbose = 2 )
  
}


cli::cli_alert_info("Working on {nrow(args_df)} tasks" )

# Use future_pmap to call the function for each row in the data frame.
# Each row corresponds to a single estimator (or small group of
# estimators in some cases).
num_cores = parallel::detectCores()
num_cores
plan(multisession, workers = num_cores - 1) 

results <- future_pmap(args_df, run_one_simulation,
                       data = data,
                       queen = "ATE",
                       covariates = paste0( "X", 1:5 ),
                       outcome = "Y_continuous",
                       treatment = "Z",
                       seed = 4343443,
                       S = 1,  
                       p_tx = NULL,
                       size_train = 750,
                       size_test = 100,
                       dir_name = dir_name,
                       verbose = 1,
                       .options = furrr_options(seed = 334334) )


dimnames( results[[1]]$runtime )



# Process simulation results for runtime ----

cli::cli_alert_info("Processing simulation results" )

# Function to load and stack arrays from RDS files using tidyverse
stack_rds_arrays <- function(dir_name) {
  # Get a list of all RDS files in the directory
  rds_files <- list.files( path = dir_name, 
                           pattern = "\\.rds$", full.names = TRUE)
  
  # Load all the RDS files into a list of lists
  all_data <- map(rds_files, readRDS)
  
  all_data = transpose( all_data )
  
  # Stack arrays on the third dimension for each of the three elements
  result_list <- map(all_data, abind, along = 3 )
  
  return(result_list)
}


res <- stack_rds_arrays( dir_name )

timings = res[[2]]
names = dimnames(timings)[[3]]
timings = timings[ , 1,  ]

# og_dims <- dim( timings )
# dim(timings) = c( og_dims[[1]], og_dims[[3]] )

stopifnot( length(timings) == length(names) )

timings = tibble( model = names,
                  time = timings )

# THE FOLLOWING CODE is when each fragment has multiple iterations, making them 3D matrices
#dim( timings )
#dimnames( timings )
#head( timings )

#as_tibble(timings) %>%
#  pivot_longer( cols = everything(), names_to = "model", values_to = "time" ) %>%
 

# Table of how long everything took to run ----
timings %>%
  group_by( model ) %>%
  summarise( avg = mean(time),
             sd = sd( time ),
             n = n() ) %>%
  arrange( -avg ) %>%
  knitr::kable( digits = 2 )



filter( timings, model == "SL S" )
