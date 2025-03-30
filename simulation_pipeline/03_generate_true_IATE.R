# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
#           
#                                                                         
# Created on:   06/16/2024
# Purpose:      Script that generates and saves treatment effects of baseline data
# Authors:      Luke Miratrix, Polina Polskaia, Nick Commins
# 
#
# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #


# Load script 02
source(here::here("simulation_pipeline/02_generate_baseline_data.R"))

# Define the default list of queens
DEFAULT_QUEEN_LIST = c(
  "ATE", "OLS S", "RF T", "RF MOM IPW", "CF",
  "CDML", "LASSO R", "SL T", "XGBOOST R", "BART T")




# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
# STEP 1: HELPER FUNCTION THAT PREDICTS TRUE IATES BY QUEEN ----
# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #


#' predict_true_IATEs:: Predict Individual Average Treatment Effects
#' (IATEs)
#'
#' This function predicts the Individual Average Treatment Effects
#' (IATEs) for a passed dataset using various machine learning methods
#' trained on a separate "real" dataset.
#'
#' It supports methods such as Average Treatment Effect (ATE), OLS,
#' Random Forest (RF) T-Learner, RF MOM with Inverse Probability
#' Weighting (IPW) or Doubly Robust (DR), Causal Forest (CF), Lasso
#' T-Learner, and several others. The method to be used is specified
#' by the `queen` parameter.
#'
#' @param x_tr Matrix of covariates for the training set (N x p
#'   matrix).
#' @param y_tr Vector of outcome values for the training set.
#' @param d_tr Vector of treatment indicators for the training set.
#' @param x_val Matrix of covariates for the validation set (N x p
#'   matrix).
#' @param queen Character string specifying the method to be used for
#'   prediction. Valid values are:
#' \itemize{
#'   \item "ATE"
#'   \item "OLS S"
#'   \item "RF T"
#'   \item "RF MOM IPW"
#'   \item "CF"
#'   \item "CDML"
#'   \item "Lasso MCM EA"
#'   \item "Lasso R"
#'   \item "SL T"
#'   \item "XGBOOST R"
#'   \item "BART T"
#' }
#'
#' @return Returns a vector of predicted IATEs for each entry in
#'   x_val.
#'
#' @export
#' 
predict_true_IATEs <- function(x_tr, 
                               y_tr, 
                               d_tr, 
                               x_val, 
                               queen = "ATE",
                               current_seed = NULL) {
  
  # Create a 2-fold set for estimating nuisance parameters
  index <- caret::createFolds(y_tr, k = 2)
  
  #TODO:: Make all queens potentially valid?
  
  # Define valid queen values
  valid_queens <- c(
    "ATE", "OLS S", "RF T", "RF MOM IPW", "CF",
    "CDML", "LASSO MCM EA", "LASSO R", "SL T", "XGBOOST R", "BART T"
  )
  
  # Average treatment effect
  if (queen == "ATE"){
    predictions <- ate_alg(x_tr, y_tr, d_tr, x_val)
  }
  
  # linear regression 
  else if (queen == "OLS S"){
    predictions <- ols_s_alg(y_tr, d_tr, x_tr, x_val) 
  }
  
  # RF T Learner
  else if (queen == "RF T"){
    predictions <- rf_t_alg(x_tr, y_tr, d_tr, x_val, current_seed)
  }
  
  # MOM with RF
  else if (queen %in% c("RF MOM IPW")){
    # Estimate RF nuisance parameters
    np <- nuisance_cf_rf(y_tr, d_tr, x_tr, index, current_seed)
    if (queen == "RF MOM IPW") {estimator_nm <- rf_mom_ipw_alg}
    predictions <- do.call(cf_dml1, list(estimator_nm, y_tr, d_tr, x_tr, np, x_val, index, current_seed))
  }
  
  # Causal Forest
  else if (queen == "CF"){
    predictions <- cf_alg(x_tr, y_tr, d_tr, x_val, ntrain, current_seed)
  }
  
  # CDML
  else if (queen == "CDML"){
    predictions <- cdml_alg(y_tr, d_tr, x_tr, x_val, current_seed)
  }
  
  # MOMs, MCMs and RL with LASSO
  else if (queen %in% c("LASSO MCM EA", "LASSO R")){
    # Estimate LASSO nuisance parameters with same index
    np <- nuisance_cf_lasso_dgp(y_tr, d_tr, x_tr, index)
    if (queen == "LASSO MCM EA")   {estimator_nm <- lasso_mcm_ea_dgp}
    if (queen == "LASSO R")        {estimator_nm <- lasso_r_dgp}
    predictions <- cf_dml1(est = estimator_nm, y_tr, d_tr, x_tr,
                           np, x_val, index) 
  }
  
  # Superlearner T-Learner
  else if (queen == "SL T"){
    predictions <- sl_t_alg(x_tr, y_tr, d_tr, x_val)
  }
  
  # XGBOOST R-learner
  else if (queen == "XGBOOST R"){
    predictions <- xgboost_r_alg(x_tr, y_tr, d_tr, x_val)
  }
  
  # BART T-learner
  else if (queen=="BART T"){
    predictions <- bart_t_alg(x_tr, y_tr, d_tr, x_val)
  }
  
  # Handle case where no valid queen value is found
  else {
    stop(paste("Invalid queen value provided. Add your queen to predict_true_IATEs or choose a valid method from:", 
               paste(valid_queens, collapse = ", ")))
  }
  
  return(predictions)
}




# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
# STEP 2: FUNCTION THAT GENERATES TRUE IATES BY QUEEN ----
# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #


#' generate_true_IATEs:: Generate True Individual Average Treatment
#' Effects (IATEs)
#'
#' This function generates true Individual Average Treatment Effects
#' based on the provided data and parameters. It supports various
#' estimation algorithms specified by the `queen` parameter and
#' adjusts (rescales) the predicted effects based on summary
#' statistics of the real data.
#'
#' @param baseline_seed Numeric seed for reproducibility.
#' @param baseline_data Data frame containing baseline data.
#' @param small_covariate_set Vector of covariate names to be used for
#'   predicting IATEs.
#' @param outcome Character string specifying the outcome variable.
#' @param treatment_var Character string specifying the treatment
#'   variable.
#' @param real_data Data frame containing real data (to learn
#'   treatment effect model from).
#' @param queen Character string specifying the algorithm to be used
#'   for prediction.
#' @param sigma_tau Numeric value for scaling tau (default is 0.2).
#'   Treatment effects will be scaled to have a standard deviation of
#'   sigma_tau&sd_y0_real, for continuous outcomes, and sigma_tau/2 
#'   for binary outcomes.
#' @param sd_y0_real Numeric value for the standard deviation of Y0 in
#'   real data.
#' @param ate_real Numeric value for the Average Treatment Effect in
#'   real data to help with scaling.
#'
#' @return Returns a vector of true IATEs.
#' @export
#' 
generate_true_IATEs <- function(baseline_seed, 
                                baseline_data, 
                                small_covariate_set, 
                                outcome, 
                                treatment_var, 
                                real_data, 
                                queen, 
                                sigma_tau, 
                                sd_y0_real, 
                                ate_real,
                                verbose = 1000) {
  
  # Load real data if we just have a directory name
  real_data = load_and_validate_real_data( real_data )
  
  # Stop if conditins not met
  stopifnot(paste0(outcome, "_Y0") %in% names(baseline_data))
  stopifnot(outcome %in% names(real_data))
  stopifnot(treatment_var %in% names(real_data))
  stopifnot(!is.null(ate_real))
  stopifnot(!is.null(sd_y0_real))
  stopifnot(!is.null(sigma_tau))
  
  # Set seed to the same seed used to produce Y0 in baseline data
  set.seed(baseline_seed + 13) 
  
  # Save seed for true IATEs DGP
  current_seed <- baseline_seed + 13
  if (verbose > 0) {
    cat("Current seed:", current_seed, "\n")
  }
  
  # Build a design matrix of baseline covariates out of the baseline data
  Xs <- make_x_matrix(data = baseline_data, 
                      covariates = small_covariate_set,
                      data_train = real_data)
  
  # Extract the baseline data.
  x_val <- Xs$X_val
  
  # Extract train data's covariates (train data is our real data in this case)
  x_tr <- Xs$X_tr
  
  # Train data, treatment assignment
  d_tr <- real_data[[ treatment_var ]]
  
  # Train data, observed outcome
  y_tr <- real_data[[ outcome ]]
  
  # Check parameters passed:
  if (is.numeric(y_tr)) {
    y_tr <- as.matrix(y_tr)
  }
  
  # Training set size
  ntrain <- length(y_tr)
  
  # Stop if conditions not met
  stopifnot(is.matrix(x_val))
  stopifnot(length(d_tr) == ntrain)
  stopifnot(is.matrix(x_tr))
  stopifnot(nrow(x_tr) == ntrain)
  stopifnot(ncol(x_val) == ncol(x_tr))
  
  # Test set size
  nval <- nrow(x_val) 
  
  # Predict true IATEs
  tau <- predict_true_IATEs(x_tr, y_tr, d_tr, x_val, queen, current_seed)
  
  # Get our Y0 variables
  Y0_name <- paste(outcome, "_Y0", sep = "")
  Y0 <- baseline_data[ , Y0_name]
  
  # Rescale tau: 
  mean_tau <- mean(tau)
  sd_tau <- sd(tau)
  
  if (verbose > 0) {
    cat("SD tau = ", sd_tau, " and mean tau = ", mean_tau, "\n")
  }
  
  if (is_binary(y_tr)) {
    sigma_tau <- sigma_tau / 2
  } else {
    sigma_tau <- sigma_tau * sd_y0_real
  }
  
  if (sd_tau > 0) {
    tau <- mean_tau + sigma_tau * (tau - mean_tau) / sd_tau
  } else {
    if ( queen != "ATE" ) {
      warning("Warning: treatment variation of predicted individual effects is 0")
    }
  }
  
  if (is_binary(y_tr)) {
    
    # Make sure tau keeps probabilities in the [0,1] range
    stopifnot(all(Y0 >= 0 & Y0 <= 1)) 
    Y1 <- pmin(pmax(Y0 + tau, 0), 1) 
    
    # Calculate new tau given two bounded probabilities
    tau <- Y1 - Y0
  } else {
    # NOTE: The following code has been updated post initial simulation runs
    # And this truncation should happen BEFORE rescaling, above, in any case.
    # tau[(tau-ate_real) > sd_y0_real ] <- ate_real + sd_y0_real
    #tau[(tau-ate_real) < -sd_y0_real ] <- ate_real - sd_y0_real
  }
  
  # NOTE: The following diagnostics p_broke should be calculated before truncation, possibly?
  
  # No treatment impact for *any* method should be larger than one effect size unit (1 SD) 
  p_broke <- ifelse(abs(tau - ate_real) >= sd_y0_real, 1, 0)
  if (verbose > 0) {
    cat( "P_BROKE (%)", mean(p_broke), "\n")
    cat( "Binary outcome: ", is_binary(y_tr), "\n")
    cat( "Summary of generated true IATE:\n" )
    print(summary(tau))
  }
  
  return(tau)
}


# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
# STEP 3: FUNCTION TO CREATE A DATASET OF IATES FOR PROVIDED QUEENS ----
# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #



#' Make a datafile with all the individual IATEs for the queens for a
#' given dataset and outcome.
#'
#' The IATEs are saved as an RDS file in a specified directory.
#'
#' @param dataset_name The name of the dataset.
#' @param real_data Real data.
#' @param queens A list of queens to process. Defaults to
#'   `DEFAULT_QUEEN_LIST`.
#' @param baseline_seed The seed for generating baseline data.
#' @param baseline_data The baseline data used for generating IATEs.
#' @param small_covariate_set A set of covariates to be used in the
#'   analysis.
#' @param outcome The name of the outcome variable in the data.
#' @param treatment_var The name of the treatment variable in the
#'   data.
#' @param sigma_tau The standard deviation of the treatment effect.
#'   Defaults to 0.2.
#' @param ate_real The actual average treatment effect, if known.
#'   Defaults to NULL.
#' @param sd_y0_real The standard deviation of the outcome variable,
#'   if known. Defaults to NULL.
#' @param true_iates_directory_path The directory path where the IATEs
#'   should be saved.
#'
#' @return The IATEs dataset.
#'
#' @export
#' 
generate_IATE_dataset <- function(dataset_name, 
                                  real_data,
                                  queens = DEFAULT_QUEEN_LIST,
                                  baseline_seed,
                                  baseline_data,
                                  small_covariate_set,
                                  outcome,
                                  treatment_var,
                                  sigma_tau = 0.2,
                                  ate_real = NULL,
                                  sd_y0_real = NULL,
                                  verbose = 1000) { 
  
  stopifnot(!is.null(real_data))
  
  # Remove any rows with missing data in outcome
  real_data <- real_data[!is.na(real_data[[outcome]]), ]
  
  # Determine the number of rows in baseline data
  n_baseline <- nrow(baseline_data)
  
  # Calculate SD and ATE in real data
  if (is.null(sd_y0_real) || is.null(ate_real)) {
    message("Calculating sd(Y0) and ATE on fly since one of them wasn't passed")
    Y0s <- real_data[[outcome]][ real_data[[treatment_var]] == 0]
    Y1s <- real_data[[outcome]][ real_data[[treatment_var]] == 1]
    sd_y0_real <- sd(Y0s)
    ate_real <- mean(Y1s) - mean(Y0s)
  }
  
  
  # Generate IATEs by queen
  process_queen <- function(queen) {
    if ( verbose > 0) {
      cat("\nGenerating IATE for queen ", queen, "\n")
    }
    # Generate treatment effects (tau) for the given queen
    generate_true_IATEs(baseline_seed, 
                        baseline_data, 
                        small_covariate_set, 
                        outcome = outcome, 
                        treatment_var, 
                        real_data, 
                        queen, 
                        sigma_tau = 0.2, 
                        sd_y0_real = sd_y0_real, 
                        ate_real = ate_real,
                        verbose = verbose - 1 ) 
  }
  
  # Generate IATEs for all queens
  result_list <- map(queens, process_queen)
  
  # Name queens in the results
  names(result_list) <- queens
  
  if (verbose > 0) {
    message("Creating new file of IATEs")
  }
  
  # Convert all IATEs to vectors
  result_list <- lapply(result_list, function(x) {
    if(is.matrix(x)) {
      return(as.vector(x))
    } else {
      return(x)
    }
  })
  
  # Bind all columns
  result_list <- bind_cols(result_list)
  
  return( result_list )
} 




# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
# STEP 4: FUNCTION THAT LOADS SAVED IATES IF THEY EXIST ----
# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #


#' load_or_generate_iates:: Load or Generate IATEs Dataset
#'
#' This function attempts to load an Individual Average Treatment
#' Effects (IATEs) dataset from a specified path. If the dataset does
#' not exist or does not have all queens, it generates the IATEs
#' dataset, saves it to the specified path, and then loads it.
#'
#' @param dataset_name The name of the dataset.
#' @param true_iates_directory_path The directory path where the IATEs
#'   data should be saved or loaded from.
#' @param real_data The real data.
#' @param queen_list A list of queens to process.
#' @param baseline_seed The seed for generating baseline data.
#' @param baseline_data The baseline data used for generating IATEs.
#' @param small_covariate_set A set of covariates to be used in the
#'   analysis.
#' @param outcome The name of the outcome variable in the data.
#' @param treatment_var The name of the treatment variable in the
#'   data.
#' @param sigma_tau The standard deviation of the treatment effect.
#' @param ate_real The actual average treatment effect, if known.
#'   Defaults to NULL.
#' @param sd_y0_real The standard deviation of the outcome variable,
#'   if known. Defaults to NULL.
#' @param force_regenerate TRUE means regenerate even if the cached
#'   file exists.
#'
#' @return The IATEs dataset.
#'
#' @export
#' 
load_or_generate_iates <- function( dataset_name, 
                                    real_data, 
                                    queen_list = NULL, 
                                    baseline_seed, 
                                    baseline_data, 
                                    small_covariate_set, 
                                    outcome,
                                    treatment_var, 
                                    sigma_tau = NULL, 
                                    ate_real = NULL, 
                                    sd_y0_real = NULL,
                                    true_iates_directory_path = here::here("true_iates"), 
                                    force_regenerate = FALSE,
                                    verbose = 1000,
                                    queen = NULL ) {
  
  stopifnot(length(outcome) == 1)
  
  # Dynamically create a baseline object name
  data_file_name = paste0(dataset_name, "_", outcome, ".rds")
  
  # Generate your personal baseline file path
  iates_data_path <- file.path(true_iates_directory_path, data_file_name)
  
  # Number of rows in baseline data
  n_baseline = nrow(baseline_data)
  
  iates_data = NULL
  
  # Check if the baseline object already exists
  if (!force_regenerate && file.exists(iates_data_path)) {
    iates_data <- readr::read_rds(iates_data_path)
    
    if (!is.null(iates_data) && 
        is.data.frame(iates_data) && 
        nrow(iates_data) == n_baseline && 
        (is.null(queen) || queen %in% colnames(iates_data)) &&
        (is.null(queen_list) || all(queen_list %in% colnames(iates_data)))) {
      
      if (verbose > 0) {
        message("IATEs data loaded from: ", iates_data_path)
      }
    } else {
      if (verbose > 0) {
        message("Existing IATEs data is incomplete. Generating and saving new IATEs data...")
      }
      iates_data = NULL
    }
  } else {
    if (verbose > 0) {
      message("IATEs data not found. Generating and saving...")
    }
  }
  
  if (is.null(iates_data)) {
    # Generate IATEs dataset
    tryCatch({
      iates_data <- generate_IATE_dataset(dataset_name, 
                                          real_data,
                                          queens = queen_list,
                                          baseline_seed,
                                          baseline_data,
                                          small_covariate_set,
                                          outcome,
                                          treatment_var,
                                          sigma_tau,
                                          ate_real,
                                          sd_y0_real,
                                          verbose = verbose - 1) 
      
      # Create the file path for saving the result
      data_file_path <- file.path(true_iates_directory_path, 
                                  paste0(dataset_name, "_", outcome, ".rds"))
      
      # Save the IATEs as an RDS file
      if (!is.null(data_file_path)) {
        tryCatch({
          readr::write_rds(iates_data, data_file_path)
        }, error = function(e) {
          stop(paste("Error saving true IATEs RDS file:", e$message, "\n"))
        })
      }
      
      if (verbose > 0) {
        message("IATEs data generated and saved to: ", iates_data_path)
      }
    }, error = function(e) {
      stop("Error generating or saving IATEs data: ", e$message)
    })
  }
  
  if (is.null(queen)) {
    return(iates_data)
  } else {
    stopifnot( queen %in% colnames(iates_data))
    return( iates_data[,queen][[1]] )
  }
}






###################################################################################
# TEST BLOCK ----
###################################################################################


if (FALSE) {  
  
  DEFAULT_QUEEN_LIST = c(
    "ATE", "OLS S", "RF T", "RF MOM IPW", "CF",
    "CDML", "LASSO MCM EA", "LASSO R")
  
  mnd_data <- load_and_validate_real_data( here::here("datasets/mnd.csv") )
  mnd_baseline <- load_or_generate_baseline_object(dataset_name = "mnd", 
                                                   baseline_directory_path = here::here("baseline"), 
                                                   real_data = mnd_data, 
                                                   all_outcomes = c( "Y_continuous", "Y_binary"), 
                                                   covariates_names = paste0( "X", 1:9 ) , 
                                                   treatment_var = "Z", baseline_seed = 68814,
                                                   baseline_N = 100000, verbose = 1000 )
  mnd_baseline_data <- mnd_baseline$Data
  
  Xs <- make_x_matrix(data = mnd_baseline_data, covariates = paste0("X", 1:9), data_train = mnd_data)
  
  # Extract the baseline data.
  x_val <- Xs$X_val
  
  # Check if baseline data exists
  if (is.null(x_val)) {
    stop("No baseline data exists for the selected dataset.")
  }
  
  # Extract train data's covariates (train data is our real data)
  x_tr <- Xs$X_tr
  
  # Train data, treat assignment
  treatment <- "Z"
  d_tr <- mnd_data[ , treatment]
  
  # Train data, observed outcome
  y_tr <- mnd_data[ , "Y_binary"]
  
  # Check parameters passed:
  if (is.numeric(y_tr)) {
    y_tr <- as.matrix(y_tr)
  }
  
  ntrain <- length(y_tr)
  
  stopifnot(is.matrix(x_val))
  stopifnot(length(d_tr) == ntrain)
  stopifnot(is.matrix(x_tr))
  stopifnot(nrow(x_tr) == ntrain )
  stopifnot(ncol(x_val) == ncol(x_tr))
  
  # The number of obs in our target data
  nval = nrow(x_val) 
  
  # Function 1
  olss <- predict_true_IATEs(x_tr, y_tr, d_tr, x_val, queen = "OLS S") 
  
  # Funtion 2
  rfts = generate_true_IATEs(baseline_seed = 68814, 
                             baseline_data = mnd_baseline_data, 
                             small_covariate_set = paste0("X", 1:9), 
                             outcome = "Y_binary", 
                             treatment_var = "Z", 
                             real_data = mnd_data, 
                             queen = "RF T", 
                             sigma_tau = 0.2, 
                             sd_y0_real = 20, 
                             ate_real = 100) 
  
  # Test function 3
  mdn_iates <- generate_IATE_dataset(dataset_name = "test", 
                                     real_data =mnd_data,
                                     queens = DEFAULT_QUEEN_LIST,
                                     baseline_seed = 68814,
                                     baseline_data = mnd_baseline_data,
                                     small_covariate_set = paste0("X", 1:9),
                                     outcome = "Y_binary",
                                     treatment_var = "Z",
                                     sigma_tau = 0.2,
                                     ate_real = 30,
                                     sd_y0_real = 134) 
  
  # Test function 4
  load_mnd_iates <- load_or_generate_iates(dataset_name = "test", 
                                           real_data = mnd_data, 
                                           queen_list = c("ATE", "OLS S"), 
                                           baseline_seed = 68814, 
                                           baseline_data = mnd_baseline_data, 
                                           small_covariate_set = paste0("X", 1:9), 
                                           outcome = "Y_binary",
                                           treatment_var = "Z", 
                                           sigma_tau = 0.2, 
                                           ate_real = 77, 
                                           sd_y0_real = 999)
  
}

