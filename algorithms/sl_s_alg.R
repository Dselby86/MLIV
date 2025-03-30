# SL S-LEARNER

#' sl_s_alg: Implementation of BART T-Learner
#'  
#' Implemented using the \code{\link{SuperLearner}} package
#' 
#' @description T-learner learns the treated and control expected outcome 
#' respectively by fitting two separate models and taking the difference between outcomes.
#' The SuperLearner model is an ensemble learning technique that combines multiple base 
#' learners to predict outcomes, selecting the optimal combination through cross-validation. 
#' 
#' @param x_tr Matrix of training covariates (N x X matrix)
#' @param y_tr Vector of outcome values
#' @param d_tr Vector of treatment indicators in training data
#' @param x_val Matrix of validation covariates (N x X matrix)
#' @import SuperLearner
#'
#' @return Returns predicted IATEs of the validation sample
#'
#' @export

sl_s_alg = function( x_tr, y_tr, d_tr, x_val ) {
  
  # Convert to df as RF only excepts dfs
  x_tr_df = as.data.frame( x_tr )
  x_val_df = as.data.frame( x_val )
  
  # Fit the model
  centered_Z = ( d_tr - 0.5 )
  fit_tau = SuperLearner( Y = y_tr, X = cbind( x_tr_df, ( d_tr - 0.5 ) * x_tr_df, centered_Z ), 
                          SL.library = c( "SL.lm", "SL.glmnet", "SL.randomForest", "SL.cforest" ) )

  # Predict expected outcomes under and without treatment
  centered_Z = ( 0 - 0.5 )
  sl.hat0 = predict( object = fit_tau, newdata = cbind( x_val_df, ( 0 - 0.5 ) * x_val_df, centered_Z ) )
  centered_Z = ( 1 - 0.5 )
  sl.hat1 = predict( object = fit_tau, newdata = cbind( x_val_df, ( 1 - 0.5 ) * x_val_df, centered_Z ) )
  
  # Calculate IATE
  iate = sl.hat1$pred - sl.hat0$pred
  return( iate ) 
}



# Switch to TRUE to test the function
if ( FALSE ) {
  
  # Call testing MND data
  source( here::here( "tests/create_testing_data.R") )
  
  # Run the model
  start_time_sim = Sys.time() 
  test = sl_s_alg( x_tr, y_tr, d_tr, x_val )  
  end_time_sim = Sys.time()
  time_sim = end_time_sim - start_time_sim 

}
