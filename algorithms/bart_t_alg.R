
###################################################################################
# STEP 3: SPECIFY T-LEARNERS 
####################################################################################

# BART T-LEARNER


#' bart_t_alg: Implementation of BART T-Learner
#'  
#' Implemented using the \code{\link{dbarts}} package
#' 
#' @description T-learner learns the treated and control expected outcome 
#' respectively by fitting two separate models and taking the difference between outcomes.
#' Bayesian Additive Regression Trees (BART) is a Bayesian nonparametric regression model 
#' that combines the flexibility of decision trees with the uncertainty quantification 
#' provided by Bayesian inference, allowing for robust predictions and inference in complex datasets.
#' 
#' @param x_tr Matrix of training covariates (N x X matrix)
#' @param y_tr Vector of outcome values
#' @param d_tr Vector of treatment indicators in training data
#' @param x_val Matrix of validation covariates (N x X matrix)
#' @import dbarts
#'
#' @return Returns predicted IATEs of the validation sample
#'
#' @export

bart_t_alg = function( x_tr, y_tr, d_tr, x_val, verbose = FALSE ) {
  
  stopifnot( length( y_tr ) == nrow( x_tr ) )
  stopifnot( is.numeric( d_tr ) && length( d_tr ) == nrow( x_tr ) )
  
  bart.fit0 = dbarts::bart(x_tr[d_tr == 0,], y_tr[d_tr == 0], x.test = x_val,
                           keeptrees=TRUE, verbose = verbose, nthread = 1)
  bart.fit1 = dbarts::bart(x_tr[d_tr == 1,], y_tr[d_tr == 1], x.test = x_val,
                           keeptrees=TRUE, verbose = verbose, nthread = 1)
  
  if ( is.binary(y_tr) ) {
    pd1 <- predict( bart.fit1, x_val, type="response" )
    pd0 <- predict( bart.fit0, x_val, type="response" )
    
    bart_t_predict_result = colMeans(predict(bart.fit1, x_val)) - colMeans(predict(bart.fit0, x_val)) 
  } else {
    bart_t_predict_result = bart.fit1$yhat.test.mean - bart.fit0$yhat.test.mean
  }
  return( bart_t_predict_result )
}


# Switch to TRUE to test the function
if ( FALSE ) {
  # Call testing MND data
  source( here::here( "tests/create_testing_data.R") )
  
  # Run the model on binary
  y_tr = mnd_data$Y_binary[ 1:ntrain ]
  test = bart_t_alg( x_tr, y_tr, d_tr, x_val, verbose = TRUE )
  
  # Run on continuous
  y_tr = mnd_data$Y_continuous[ 1:ntrain ]
  test2 = bart_t_alg( x_tr, y_tr, d_tr, x_val, verbose = TRUE )
  
}
