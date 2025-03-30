# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
#                                                                                                        
#                                                                         
# Created on:   06/21/2024
# Purpose:      Main IATE estimator function: this function produces IATE 
#               estimates for a given dataset for a range of estimators
# Authors:      Luke Miratrix, Polina Polskaia, Nick Commins
# 
#
# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #


# Load script 04
source(here::here("simulation_pipeline/04_add_IATE_to_baseline.R"))

# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
# STEP 1: CREATE A FUNCTON THAT PREDICTS IATE ----
# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #




#' This function wraps a given function in a safely wrapper and then
#' calls it.  The results are the results of the function with some
#' timing code and error trapping.
#'
#' @return List of three things: the result (ideally a list of
#'   predictions for the validation set), the time it took to run, and
#'   any error messages or warnings produced.
safe_run <- function( alg, ... ) {
  
  safe_alg = quietly( safely( alg ) )
  
  start_time <- Sys.time()
  res <- safe_alg( ... )
  end_time <- Sys.time()
  
  warn <- res$warning
  if ( length( warn ) >= 1 ) {
    warn = paste( warn, collapse = "; " )
  }
  
  if ( is.null( res$result$result ) ) {
    
    msg = as.character( res$result$error )
    if ( length( warn ) > 0 ) {
      msg = paste0( msg, "; ", warn )
    }
    
    # it failed, plug in NAs
    list( prediction = NA,
          runtime = end_time - start_time,
          messages = msg )
    
  } else {
    
    predictions <- res$result$result
    
    list( prediction = as.numeric( predictions ),
          runtime = end_time - start_time,
          messages = warn )
  }
}



#' generate_predicted_IATE:: Generate Predicted Individual Average
#' Treatment Effects (IATE)
#'
#' This function estimates the Individual Average Treatment Effects
#' (IATE) using various machine learning algorithms. It supports ATE,
#' OLS S-learner, Random Forest, Lasso, Superlearner, XGBoost, and
#' BART methods. The function also calculates the runtime and failure
#' rate for each method.
#'
#' @param y_tr A numeric vector or matrix of training response values.
#' @param d_tr A numeric vector of training treatment indicators.
#' @param x_tr A numeric matrix of training covariates.
#' @param tau_tr A numeric vector of true IATE values for training
#'   data.
#' @param x_val A numeric matrix of validation covariates.
#' @param verbose A logical flag indicating whether to print progress
#'   messages. Default is FALSE.
#' @param include_RF A logical flag indicating whether to include
#'   Random Forest based methods. Default is FALSE.
#' @param include_LASSO A logical flag indicating whether to include
#'   Lasso based methods. Default is FALSE.
#' @param include_SL_S A logical flag indicating whether to include
#'   Superlearner S method. Default is FALSE.
#' @param include_SL_T A logical flag indicating whether to include
#'   Superlearner T method. Default is FALSE.
#' @param include_CDML A logical flag indicating whether to include
#'   CDML methods. Default is FALSE.
#' @param include_XGBOOST A logical flag indicating whether to include
#'   XGBoost methods. Default is FALSE.
#' @param include_BART A logical flag indicating whether to include
#'   BART methods. Default is FALSE.
#' @param sd_y0_real A numeric value representing the standard
#'   deviation of the control group outcome.
#' @param ate_real A numeric value representing the true average
#'   treatment effect.
#'
#' @return A list containing the following elements: \item{iate}{A
#'   matrix of predicted IATE values for each method.}
#'   \item{runtime}{A matrix of runtime values for each method.}
#'   \item{fail_rate}{A matrix of failure rates for each method,
#'   indicating the proportion of predictions that deviate
#'   significantly from the true ATE.}
#'
#' @export
#' 
generate_predicted_IATE <- function(y_tr, 
                                    d_tr,
                                    x_tr, 
                                    tau_tr = NULL, # optional 
                                    x_val,
                                    model_list,
                                    verbose = 1000, 
                                    sd_y0_real = NULL, 
                                    ate_real = NULL,
                                    current_seed) {
  
  
  # Check parameters passed
  if (is.numeric(y_tr)) {
    y_tr <- as.matrix(y_tr)
  }
  
  # Get number of rows/lengths
  nval <- nrow(x_val)
  ntrain <- length(y_tr)
  
  # Validate dimensions and lengths
  stopifnot(is.matrix(x_val))
  stopifnot(length(d_tr) == ntrain)
  stopifnot(is.matrix(x_tr))
  stopifnot(nrow(x_tr) == ntrain)
  if ( !is.null(tau_tr) ) {
    stopifnot(length(tau_tr) == ntrain)
  } else {
    # remove infeasible models since we have no true impacts to regress with
    model_list = setdiff( model_list, c( "RF INF", "LASSO INF" ) )
  }
  
  stopifnot(ncol(x_val) == ncol(x_tr))
  
  # Create a 2-fold set for cross-fitting
  index = caret::createFolds(y_tr, k = 2)
  
  N <- length(model_list) # Assume 'models' is your character vector
  results = setNames( vector( "list", N ), model_list )
  results
  
  
  ##### Default models #####  
  
  if ( "ATE" %in% model_list ) {
    # ATE
    vcat( verbose, "ATE" )
    ate_predictions <- safe_run( ate_alg, x_tr, y_tr, d_tr, x_val )
    results[["ATE"]] <- ate_predictions
  }
  
  if ( "OLS S" %in% model_list ) {
    # linear regression S-learner
    ols_s_predictions <- safe_run( ols_s_alg, y_tr, d_tr, x_tr, x_val )
    results[["OLS S"]] <- ols_s_predictions
  }
  
  ##### RF based methods #####
  
  
  # Infeasible RF
  if ( "RF INF" %in% model_list ) {
    vcat(verbose, "RF Inf")
    rf_inf_predictions <- safe_run( rf_inf_alg, x_tr, tau_tr, x_val)
    results[["RF INF"]] <- rf_inf_predictions 
  }
  
  # Random Forest T-learner
  if ( "RF T" %in% model_list ) {
    vcat( verbose, "RF T" )
    rf_t_predictions <- safe_run( rf_t_alg, x_tr, y_tr, d_tr, x_val, current_seed)
    results[["RF T"]] <- rf_t_predictions 
  }
  
  
  # Estimate RF nuisance parameters, if needed
  if ( any( c( "RF MOM IPW", "RF MOM DR", "CF LC" ) %in% model_list ) ) {
    start_time <- Sys.time()
    np <- nuisance_cf_rf(y_tr, d_tr, x_tr, index, current_seed)
    end_time <- Sys.time()
    rf_time <- end_time - start_time 
  }
  
  
  # Random Forest MOM IPW
  if ( "RF MOM IPW" %in% model_list ) {
    rf_mom_ipw_predictions <- safe_run( cf_dml1, rf_mom_ipw_alg, y_tr, d_tr, x_tr, 
                                        np, x_val, index, current_seed)
    results[["RF MOM IPW"]] <-     rf_mom_ipw_predictions
    results[["RF MOM IPW"]]$runtime = results[["RF MOM IPW"]]$runtime + rf_time
  }
  
  # Random Forest MOM DR
  if ( "RF MOM DR" %in% model_list ) {
    rf_mom_dr_predictions <- safe_run( cf_dml1, rf_mom_dr_alg, y_tr, d_tr, x_tr, 
                                       np, x_val, index, current_seed)
    results[["RF MOM DR"]] <-     rf_mom_dr_predictions 
    results[["RF MOM DR"]]$runtime = results[["RF MOM DR"]]$runtime + rf_time
  }
  
  
  # Causal Forest
  if ("CF" %in% model_list) {
    cf_predictions <- safe_run(cf_alg, x_tr, y_tr, d_tr, x_val, ntrain, current_seed)
    results[["CF"]] <- cf_predictions 
  }
  
  # Causal Forest with local centering
  if ( "CF LC" %in% model_list ) {
    vcat(verbose, "CF LC method")
    cf_lc_predictions <- safe_run(cf_lc_alg, x_tr, y_tr, d_tr, x_val,
                                  np, current_seed)
    results[["CF LC"]] <- cf_lc_predictions 
    results[["CF LC"]]$runtime = results[["CF LC"]]$runtime + rf_time
  }
  
  
  ##### CDML method #####
  if ("CDML" %in% model_list) {
    vcat(verbose, "CDML method")
    cdml_predictions <- safe_run(cdml_alg, y_tr, d_tr, x_tr, x_val, current_seed)
    results[["CDML"]] <- cdml_predictions 
  } 
  
  ##### Lasso based methods #####
  
  # Infeasible Lasso
  if ( "LASSO INF" %in% model_list ) {
    
    # Check if tau_tr is constant
    if (sd(tau_tr) == 0) {
      
      if ( verbose > 1 ) {
        cat("True IATE is constant. Skipping Infeasible Lasso algorithm. Populated predicted tau results with true tau\n")
      }
      results[["LASSO INF"]] <- list( prediction = rep( tau_tr[[1]], nrow(x_val) ),
                                      runtime = Sys.time() - Sys.time(), # lightning fast!
                                      messages = "" )
    } else {
      lasso_inf_predictions <- safe_run( lasso_inf_alg, x_tr, tau_tr, x_val)
      results[["LASSO INF"]] <- lasso_inf_predictions 
    }
  }
  
  
  # Lasso T-learner
  if (  "LASSO T" %in% model_list ) {
    lasso_t_predictions <- safe_run( lasso_t_alg, x_tr, y_tr, d_tr, x_val )
    results[["LASSO T"]] <-     lasso_t_predictions 
  }
  
  if ( any( c( "LASSO MOM IPW", "LASSO MOM DR", "LASSO MCM", "LASSO MCM EA", "LASSO R") %in% model_list ) ) {
    vcat( verbose, "Several lasso methods (all bundled)" )
    
    # Estimate Lasso nuisance parameters 
    start_time <- Sys.time()
    np <- nuisance_cf_lasso(y_tr, d_tr, x_tr, index)
    end_time <- Sys.time()
    lasso_nuisance_time <- end_time - start_time
    
    # Lasso MOM IPW
    lasso_mom_ipw_predictions <- safe_run( cf_dml1, lasso_mom_ipw, y_tr, d_tr, x_tr, np, x_val, index)
    results[["LASSO MOM IPW"]] <-     lasso_mom_ipw_predictions 
    results[["LASSO MOM IPW"]]$runtime <- results[["LASSO MOM IPW"]]$runtime + lasso_nuisance_time
    
    # Lasso MOM DR
    lasso_mom_dr_predictions <- safe_run( cf_dml1, lasso_mom_dr, y_tr, d_tr, x_tr, np, x_val, index)
    results[["LASSO MOM DR"]] <-     lasso_mom_dr_predictions 
    results[["LASSO MOM DR"]]$runtime <- results[["LASSO MOM DR"]]$runtime + lasso_nuisance_time
    
    # Lasso MCM
    lasso_mcm_predictions <- safe_run( cf_dml1, lasso_mcm, y_tr, d_tr, x_tr, np, x_val, index)
    results[["LASSO MCM"]] <-     lasso_mcm_predictions 
    results[["LASSO MCM"]]$runtime <- results[["LASSO MCM"]]$runtime + lasso_nuisance_time
    
    # Lasso MCM EA
    lasso_mcm_ea_predictions <- safe_run( cf_dml1, lasso_mcm_ea, y_tr, d_tr, x_tr, np, x_val, index)
    results[["LASSO MCM EA"]] <-     lasso_mcm_ea_predictions 
    results[["LASSO MCM EA"]]$runtime <- results[["LASSO MCM EA"]]$runtime + lasso_nuisance_time
    
    # Lasso R-learner
    lasso_r_predictions <- safe_run( cf_dml1, lasso_r, y_tr, d_tr, x_tr, np, x_val, index)
    results[["LASSO R"]] <-     lasso_r_predictions 
    results[["LASSO R"]]$runtime <- results[["LASSO R"]]$runtime + lasso_nuisance_time    
    
  } 
  
  
  ##### Superlearners #####
  
  # Superlearner T-learner
  if ("SL T" %in% model_list) {
    vcat(verbose, "SL T")
    sl_t_predictions <- safe_run(sl_t_alg, x_tr, y_tr, d_tr, x_val)
    results[["SL T"]] <- sl_t_predictions 
  } 
  
  # Superlearner S-learner
  if ("SL S" %in% model_list) {
    vcat(verbose, "SL S")
    sl_s_predictions <- safe_run(sl_s_alg,  x_tr, y_tr, d_tr, x_val)
    results[["SL S"]] <- sl_s_predictions 
  }
  
  ##### XGBOOST #####
  # XGBOOST S-learner
  if ("XGBOOST S" %in% model_list) {
    vcat(verbose, "XGBOOST S")
    xgboost_s_predictions <- safe_run(xgboost_s_alg, x_tr, y_tr, d_tr, x_val)
    results[["XGBOOST S"]] <- xgboost_s_predictions 
  } 
  
  # XGBOOST R-learner
  if ("XGBOOST R" %in% model_list) {
    vcat(verbose, "XGBOOST R")
    xgboost_r_predictions <- safe_run( xgboost_r_alg, x_tr, y_tr, d_tr, x_val)
    results[["XGBOOST R"]] <- xgboost_r_predictions 
  } 
  
  ##### BART #####
  
  # BART T-learner
  if ("BART T" %in% model_list) {
    vcat(verbose, "BART T")
    bart_t_predictions <- safe_run(bart_t_alg, x_tr, y_tr, d_tr, x_val)
    results[["BART T"]] <- bart_t_predictions 
  }
  
  # BART S-learner
  if ("BART S" %in% model_list) {
    bart_s_predictions <- safe_run(bart_s_alg, x_tr, y_tr, d_tr, x_val)
    results[["BART S"]] <- bart_s_predictions 
  }
  
  # BART S-learner No Interactions
  if ("BART S NI" %in% model_list) {
    bart_s_predictions <- safe_run(bart_s_ni_alg, x_tr, y_tr, d_tr, x_val)
    results[["BART S NI"]] <- bart_s_predictions 
  }
  
  
  #### Get results ready for return ####
  
  nulls <- map_lgl( results, is.null )
  if ( any( nulls ) ) {
    bads = paste0( model_list[ nulls ], collapse = ", " )
    warning( glue::glue( "Some models failed to run or are undefined: {bads}. Watch for NAs." ) )
  }
  
 #results <- results[model_list]
  rr <- transpose(results)
  N_model = length(model_list)
  iates_mat = do.call( cbind, rr$prediction )
  iates_mat = iates_mat[ , model_list, drop = FALSE ]
  stopifnot( ncol( iates_mat ) == N_model )
  
  runtimes = map_dbl( rr$runtime, \(tt) { units(tt) <- "mins"; as.numeric(tt) } )[ model_list ]
  runtime_mat = matrix( runtimes, 
                        nrow = nrow( iates_mat ), ncol = N_model, 
                        byrow = TRUE )
  colnames(runtime_mat) <- model_list
  
  if ( is.null( sd_y0_real ) ) {
    sd_y0_real = sd( y_tr[ d_tr == 0 ] )
    ate_real = median( iates_mat, na.rm = TRUE )
    message( glue::glue( "Windsorizing using data-inspired values of sd(Y0)={round(sd_y0_real,digits=2)} from median IATE across models of {round(ate_real,digits=2)}") )
  }
  
  percent_mat <- (abs(iates_mat - ate_real) > sd_y0_real) + 0
  iates_mat <- windsorize_impacts(iates_mat, ate_real, sd_y0_real)
  
  stopifnot( dim( percent_mat ) == dim( iates_mat ) )
  stopifnot( dim( runtime_mat ) == dim( iates_mat ) )
  
  warns <- map( rr$messages, length ) > 0
  
  return( list( iate = iates_mat,
                runtime = runtime_mat, 
                fail_rate = percent_mat,
                warnings = warns ) )
}






# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
# TEST BLOCK ----
# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #


if (FALSE) {  
  
  # Load data 
  source( here::here( "tests/create_testing_data.R" ) )
  
  y_tr <- mnd_baseline_data[ 1:ntrain, "Y_continuous"]
  tau_tr <- mnd_baseline_data[ 1:ntrain, "Y_continuous_tau" ]
  
  sd_y0_real = sd( mnd_baseline_data$Y_continuous_Y0 )
  ate_real = mean( mnd_baseline_data$Y_continuous_tau )
  
  x_val = x_val[1:5,]
  
  # debug( generate_predicted_IATE )
  # Generate predicted IATEs 
  test <- generate_predicted_IATE(y_tr, 
                                  d_tr,
                                  x_tr, 
                                  tau_tr, 
                                  x_val,
                                  verbose = 1000,
                                  model_list = model_queen_lists(
                                    
                                    include_RF = FALSE,
                                    include_LASSO = TRUE,
                                    include_SL_S = FALSE,
                                    include_SL_T = FALSE,
                                    include_CDML = FALSE,
                                    include_XGBOOST = FALSE,
                                    include_BART = FALSE, 
                                    make_queen_list = FALSE ),
                                  sd_y0_real = sd_y0_real, 
                                  ate_real = ate_real) 
  
  names(test)
  dimnames(test[[1]])
  dim(test[[1]])
  dim(test[[2]])
  dimnames(test[[3]])
}


