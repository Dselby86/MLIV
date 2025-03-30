
###################################################################################
# STEP 2: SPECIFY S-LEARNERS 
####################################################################################


# XGBOOST S-LEARNER

#' xgboost_s_alg: Implementation of XGBOOST S-Learner
#'
#' Implemented using the \code{\link{xnie/rlearner}} package
#'
#' @description S-learner treats the treatment variable as if it was
#'   just another covariate like those in the vector x_tr. Instead of
#'   having two models for the response as a function of the
#'   covariates x_tr, the S-learner has a single model for the
#'   response as a function of x_tr and the treatment d_tr.
#'
#'   But d_tr has interactions with the covariates. This is achieved
#'   by augmenting the covariates matrix with interaction terms,
#'   specifically by adjusting the treatment variable to center it
#'   around 0 and then multiplying it element-wise with the original
#'   covariates. These interactions enable the model to capture how
#'   the effects of certain covariates may vary based on the treatment
#'   received. XGBoost, or eXtreme Gradient Boosting, is a model that
#'   uses a technique called gradient boosting to build an ensemble of
#'   decision trees for both regression and classification tasks.
#'
#' @param x_tr Matrix of training covariates (N x X matrix)
#' @param y_tr Vector of outcome values
#' @param d_tr Vector of treatment indicators in training data
#' @param x_val Matrix of validation covariates (N x X matrix)
#' @import xnie/rlearner
#'
#' @return Returns predicted IATEs of the validation sample
#'
#' @export

xgboost_s_alg = function( x_tr, y_tr, d_tr, x_val, verbose = FALSE) {
  
  fit_tau = sboost( x_tr, d_tr, y_tr, nthread = 1, verbose = verbose )
  iate = predict(fit_tau, newx = x_val)
  return( iate )
}



if ( FALSE ) {
  
  # Call testing MND data
  source( here::here( "tests/create_testing_data.R") )
  
  
  # Run the model on binary
  y_tr = mnd_data$Y_binary[ 1:ntrain ]
  head( x_tr )
  y_tr = (as.numeric( x_tr[, "X5" ] * d_tr + runif( nrow(x_tr), -0.1, 0.1 )  > 0 )) 
  table( y_tr )
  test = xgboost_s_alg( x_tr, y_tr, d_tr, x_val, verbose = TRUE )
  
  # We have large effects and it looks like the treatment impacts are
  # contained in -1,1 so that is good:
  summary( test )
  
  
  # Run the model on constant ATE 
  mnd_baseline_data <- mutate( mnd_baseline_data,
                               Y_ate = Y_continuous_Y0 + Z*mean(Y_continuous_tau) )
  ntrain = 2000
  x_tr = x_val[1:ntrain,]  
  y_tr = mnd_baseline_data$Y_ate[1:ntrain]
  d_tr = mnd_baseline_data$Z[1:ntrain]
  head( x_tr )
  summary( y_tr )
  mean( y_tr[ d_tr == 1] ) - mean( y_tr[ d_tr == 0 ] )
  mean( mnd_baseline_data$Y_continuous_tau )
  
  test = xgboost_s_alg( x_tr, y_tr, d_tr, x_val, verbose = TRUE )
  summary( test )
  mean( mnd_baseline_data$Y_continuous_tau )
  
  
  
  # Run on continuous
  y_tr = mnd_data$Y_continuous[ 1:ntrain ]
  test2 = bart_t_alg( x_tr, y_tr, d_tr, x_val, verbose = TRUE )
  
  
  
  # Look at constant treatment effect and see what happens
  
  
}








