# SL T-LEARNER

#' sl_t_alg: Implementation of BART T-Learner
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

sl_t_alg = function( x_tr, y_tr, d_tr, x_val ) {
  
  # Convert to df as RF only excepts dfs
  x_tr_df = as.data.frame(x_tr)
  x_val_df = as.data.frame(x_val)
  
  # Fit two models
  sl.fit0 = SuperLearner( Y = y_tr[d_tr == 0], X = x_tr_df[d_tr == 0,],
                          SL.library = c( "SL.lm", "SL.glmnet", "SL.randomForest", "SL.cforest"))
  sl.fit1 = SuperLearner( Y = y_tr[d_tr == 1], X = x_tr_df[d_tr == 1,],
                          SL.library = c( "SL.lm", "SL.glmnet", "SL.randomForest", "SL.cforest"))
  
  # Predict and calculate IATE
  iate = predict( sl.fit1, x_val_df )$pred - predict( sl.fit0, x_val_df )$pred
  return( iate ) 
}



# Switch to TRUE to test the function
if ( FALSE ) {
  
  # Call testing MND data
  source( here::here( "tests/create_testing_data.R") )
  
  # Run the model
  test = sl_t_alg( x_tr, y_tr, d_tr, x_val )
  
}



