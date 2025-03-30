###################################################################################
#                                                                                                        
#                                                                         
# Created on:   06/22/2024
# Purpose:      Create simulation driver function
# Authors:      Luke Miratrix, Polina Polskaia, Nick Commins
# 
#
###################################################################################


# Load script 05
source(here::here("simulation_pipeline/05_generate_predicted_IATE.R"))

###################################################################################
# THE SIMULATION DRIVER FUCTION ----
###################################################################################


#' apply_estimators:: Apply Estimators to a Test Set
#'
#' This function applies various causal algorithms to a given test set using training data.
#' It allows for the inclusion of several machine learning models such as LASSO, Random Forest,
#' Super Learner, CDML, XGBoost, and BART.
#'
#' @param test_set A data frame or matrix containing the test set data.
#' @param train_set A data frame containing the training set data.
#' @param outcome The name of the outcome variable.
#' @param treatment The name of the treatment variable.
#' @param covariates A vector of covariate names to be used in the models.
#' @param verbose An integer for the verbosity level (default is 1000).
#' @param include_LASSO A logical indicating whether to include LASSO in the models (default is FALSE).
#' @param include_RF A logical indicating whether to include Random Forest in the models (default is FALSE).
#' @param include_SL_S A logical indicating whether to include Super Learner S in the models (default is FALSE).
#' @param include_SL_S A logical indicating whether to include Super Learner T in the models (default is FALSE).
#' @param include_CDML A logical indicating whether to include CDML in the models (default is FALSE).
#' @param include_XGBOOST A logical indicating whether to include XGBoost in the models (default is FALSE).
#' @param include_BART A logical indicating whether to include BART in the models (default is FALSE).
#' @param sd_y0_real The standard deviation of the real outcome (default is 1).
#' @param ate_real The actual average treatment effect (default is 0).
#'
#' @return A matrix of predicted individual treatment effects.
#'
#' @export
apply_estimators <- function(test_set, 
                             train_set,
                             outcome,
                             treatment,
                             covariates,
                             verbose = 1000,
                             model_list = model_list,
                             sd_y0_real = 1, 
                             ate_real = 0,
                             current_seed) {
  
  stopifnot( !is.null( ate_real ) )
  stopifnot( !is.null( sd_y0_real ) )
  stopifnot( length( model_list ) >= 1 )
  
  # Create the name for the tau variable
  tau_name <- paste(outcome, "_tau", sep = "")
  
  # Check the train set type
  
  # TODO (Luke): This seems wrong.  Shouldn't the second make_x_matrix call
  # work to generate both train and test matrices?  And if test_set is
  # already a matrix, it will crash the second make_x_matrix call.
  # Add some asserts to fix all this.
  #if (is.data.frame(test_set)) {
  #  x_val <- make_x_matrix(test_set, covariates, data_train = NULL)
  #} else {
  #  x_val <- test_set
  #}
  
  # Prep the data by making covariates into matrices 
  Xs <- make_x_matrix(test_set, covariates, data_train = train_set)
  x_tr <- Xs$X_tr
  x_val <- Xs$X_val
  d_tr <- train_set[, treatment]
  stopifnot( !is.null( d_tr ) )
  
  y_tr <- train_set[, outcome]
  stopifnot( !is.null( y_tr ) )
  
  tau_tr <- train_set[[tau_name]]
  stopifnot( !is.null( tau_tr ) )
  
  if (verbose > 0) {
    cat("Current seed:", current_seed, "\n")
  }
  
  # Run simulation models and store results
  iates_matrix <- generate_predicted_IATE(y_tr = y_tr,
                                          d_tr = d_tr,
                                          x_tr = x_tr,
                                          tau_tr = tau_tr, 
                                          x_val = x_val,
                                          verbose = verbose - 1,
                                          model_list = model_list,
                                          sd_y0_real = sd_y0_real, 
                                          ate_real = ate_real,
                                          current_seed = current_seed
  )
  
  return(iates_matrix)
}


#' perform_simulation:: Perform Simulation
#'
#' This function runs a simulation over a specified range of
#' iterations to generate predicted Individual Average Treatment
#' Effects (IATEs) using various machine learning algorithms.
#'
#' @param start_iteration An integer representing the starting
#'   iteration.
#' @param end_iteration An integer representing the ending iteration.
#' @param test_set A data frame containing the test set data.
#' @param baseline_remainder A data frame containing the baseline
#'   remainder data for training.
#' @param size_train An integer representing the size of the training
#'   set to sample.
#' @param seeds A numeric vector of seed values for random number
#'   generation, one for each iteration.
#' @param p_tx A numeric vector representing the treatment assignment
#'   probabilities. Default is NULL.
#' @param outcome A string representing the name of the outcome
#'   variable.
#' @param treatment A string representing the name of the treatment
#'   variable.
#' @param covariates A vector of strings representing the names of the
#'   covariate variables.
#' @param verbose A numerical value indicating whether to print
#'   detailed output and how many lines. Default is 1000.
#' @param sd_y0_real A numeric value representing the standard
#'   deviation of Y0.
#' @param ate_real A numeric value representing the average treatment
#'   effect.
#'
#' @return A list containing:
#' \describe{
#'   \item{predictedtau}{An array of predicted IATEs for each iteration, test unit, and model.}
#'   \item{runtime}{An array of runtimes for each iteration, test unit, and model.}
#'   \item{failrate}{An array of failure rates for each iteration, test unit, and model.}
#' }
#'
#' @export
#' 
perform_simulation <- function(start_iteration, 
                               end_iteration,
                               test_set, 
                               baseline_remainder,
                               size_train, 
                               seeds,
                               p_tx = NULL, 
                               outcome,
                               treatment,
                               covariates,
                               verbose = 1000,
                               model_list,
                               sd_y0_real = NULL, 
                               ate_real = NULL 
) {
  
  # Create a matrix of covariates in test data
  x_val <- make_x_matrix(test_set, covariates, data_train = NULL)
  nval <- nrow(x_val)
  
  tau_name <- paste(outcome, "_tau", sep = "")
  
  stopifnot(!is.null(test_set[[outcome]]))
  stopifnot(!is.null(test_set[[tau_name]]))
  stopifnot(!is.null(x_val))
  
  # Set up matrices to store simulation results
  num_iterations <- end_iteration - start_iteration + 1
  
  sim_data <- array(0, c(num_iterations, nval, length(model_list)))
  runtime_data <- array(0, c(num_iterations, nval, length(model_list)))
  percent_data <- array(0, c(num_iterations, nval, length(model_list)))
  
  if ( is.null( ate_real ) ) {
    Y0_name = paste0( outcome, "_Y0" )
    ate_real = mean( c( test_set[[ tau_name ]], baseline_remainder[[ tau_name ]] ) )
    sd_y0_real = sd( c( test_set[[ Y0_name ]], baseline_remainder[[ Y0_name ]] ) )
    stopifnot( !is.null(sd_y0_real) && !is.na( sd_y0_real) )
    stopifnot( !is.null(ate_real) && !is.na( ate_real) )
  }
  
  
  # Our iteration number
  for (i in start_iteration:end_iteration) { # Our iteration number
    
    # Select seed from list of current seeds provided
    local_index <- i - start_iteration + 1
    current_seed <- seeds[local_index]
    
    # Check if current_seed is an integer
    stopifnot(is.numeric(current_seed))
    
    if (verbose > 0) {
      cat("Current seed:", current_seed, "\n")
    }
    
    # Set the seed for this iteration
    set.seed(current_seed)
    
    # Sampling training data 
    train_set <- baseline_remainder[sample(nrow(baseline_remainder), size_train), ]
    
    # Run all estimators on the simulated data and store results
    iates_matrix <- apply_estimators(test_set = test_set, 
                                     train_set = train_set,
                                     outcome = outcome,
                                     treatment = treatment,
                                     covariates = covariates,
                                     verbose = verbose - 1,
                                     model_list = model_list,
                                     sd_y0_real = sd_y0_real, 
                                     ate_real = ate_real,
                                     current_seed = current_seed)
    
    # Storing predicted tau per model per unit per iteration
    sim_data[local_index, , ] <- iates_matrix$iate
    runtime_data[local_index, , ] <- iates_matrix$runtime
    percent_data[local_index, , ] <- iates_matrix$fail_rate
    
  }

  # Name the dimensions of the matrix for ease of use
  dms <- dim(sim_data)
  dimnames(sim_data) <- list(NULL, NULL, model_list)
  dimnames(runtime_data) <- list(NULL, NULL, model_list)
  dimnames(percent_data) <- list(NULL, NULL, model_list)
  
  # NOTE: Don't need to do this as the script now returns the right kind 
  # of results (just columns of estimated models)
  # Drop ML methods that we skipped by looking for NAs in the result matrix.
  #na_models <- ALL_MODELS[apply(sim_data[1, , ], 2, function(x) anyNA(x))]
  #sim_data <- sim_data[, , !ALL_MODELS %in% na_models, drop=FALSE]
  #runtime_data <- runtime_data[, , !ALL_MODELS %in% na_models, drop=FALSE]
  #percent_data <- percent_data[, , !ALL_MODELS %in% na_models, drop=FALSE]
  
  return(list(predictedtau = sim_data, runtime = runtime_data, failrate = percent_data))
}


###################################################################################
# TEST BLOCK ----
###################################################################################


if (FALSE) {  
  
  # Load data
  mnd_data <- load_and_validate_real_data(real_data = here::here("datasets/mnd.csv"))
  mnd_baseline <- load_or_generate_baseline_object(dataset_name = "mnd", 
                                                   baseline_directory_path = here::here("baseline"), 
                                                   real_data = mnd_data, 
                                                   all_outcomes = c( "Y_continuous", "Y_binary"), 
                                                   covariates_names = paste0( "X", 1:9 ) , 
                                                   treatment_var = "Z", baseline_seed = 68814, 
                                                   baseline_N = 100000)
  mnd_baseline_data <- mnd_baseline$Data
  
  # Add treatment to baseline:
  nval <- nrow(mnd_baseline_data)
  treatment <- "Z"
  # Generate treatment with p_tx proportion treated
  actual_Tx <- as.numeric(sample(nval) <= 0.5 * nval) 
  mnd_baseline_data[, treatment] <- actual_Tx
  
  stopifnot(treatment %in% names(mnd_baseline_data))
  
  # Load IATES
  load_mnd_iates <- load_or_generate_iates(dataset_name = "mnd", 
                                           true_iates_directory_path = here::here("true_iates"), 
                                           real_data = mnd_data, 
                                           queen_list = c("ATE", "OLS S"), 
                                           baseline_seed = 68814, 
                                           baseline_data = mnd_baseline_data, 
                                           small_covariate_set = paste0("X", 1:9), 
                                           outcome = "Y_continuous",
                                           treatment_var = "Z", 
                                           sigma_tau = 0.2, 
                                           ate_real = NULL, 
                                           sd_y0_real = NULL)
  
  # Select one queen
  IATE <- load_mnd_iates[["ATE"]]
  
  # Add treatmnt effects
  baseline_full <- add_treatment_effects(baseline = mnd_baseline_data,
                                         IATE = IATE,
                                         outcome = "Y_continuous",
                                         treatment = "Z",
                                         binary_outcome = FALSE )
  
  
  stopifnot(nrow(baseline_full) == nrow(IATE))
  
  # Select observations for test set
  test_set <- baseline_full[1:100, ]
  baseline_remainder <- baseline_full[-(1:10000), ]
  
  # Set seeds and iteration numbers
  seeds <- c(505315, 784017, 338579, 973262, 943898, 443146, 798534, 221910, 401433, 956971)
  iters <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  
  iters <- c(1,2,3)
  seeds <- seeds[iters]
  
  # Generate SD of Y0 and ATE
  y0_values <- mnd_data[mnd_data[["Z"]] == 0, "Y_continuous"]
  sd_y0_real <- sd(y0_values)
  ate_real <- mean(mnd_data[mnd_data[["Z"]] == 1, "Y_continuous"]) - mean(y0_values)
  
  # Perform the test
  test <- perform_simulation(start_iteration = min(iters), 
                             end_iteration = max(iters),
                             test_set, 
                             baseline_remainder,
                             size_train = 100, 
                             seeds,
                             p_tx = NULL, 
                             outcome = "Y_continuous",
                             treatment = "Z",
                             covariates = paste0("X", 4:6),
                             verbose = 1000,
                             model_list = model_queen_lists(
                               include_LASSO = TRUE,
                               include_RF = FALSE,
                               include_SL_S = TRUE,
                               include_SL_T = FALSE,
                               include_CDML = FALSE,
                               include_XGBOOST = FALSE,
                               include_BART = FALSE,
                               make_queen_list = FALSE ),
                             sd_y0_real, 
                             ate_real) 
  
  map( test, dim )
  dim( test_set )
  head( test_set )
  dim( test$predictedtau )
  test$predictedtau[,1,]
  test$runtime[,1,]
}


