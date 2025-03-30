# BART S-LEARNER

#' bart_s_alg: Implementation of BART S-Learner
#'  
#' Implemented using the \code{\link{dbarts}} package
#' 
#' @description S-learner treats the treatment variable as if it was just another covariate 
#' like those in the vector x_tr. Instead of having two models for the response as a function 
#' of the covariates x_tr, the S-learner has a single model for the response as a function of x_tr 
#' and the treatment d_tr. But d_tr has interactions with the covariates. This is achieved by
#' augmenting the covariates matrix with interaction terms, specifically by adjusting the 
#' treatment variable to center it around 0 and then multiplying it element-wise 
#' with the original covariates. These interactions enable the model to capture how the 
#' effects of certain covariates may vary based on the treatment received.
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

bart_s_alg = function( x_tr, y_tr, d_tr, x_val, verbose = FALSE ) {
  # Fit a model with interactions with treatment
  fit_tau = dbarts::bart( cbind( x_tr, ( d_tr - 0.5 ) * x_tr, ( d_tr - 0.5 ) ), y_tr, keeptrees = TRUE, verbose = verbose, nthread = 1 ) 
  
  # Predict expected outcomes under and without treatment
  bart.hat0 = predict( object = fit_tau, newdata = cbind( x_val, ( 0 - 0.5 ) * x_val, ( 0 - 0.5 ) ) )
  bart.hat1 = predict( object = fit_tau, newdata = cbind( x_val, ( 1 - 0.5 ) * x_val, ( 1 - 0.5 ) ) )
  
  # Take a mean of predictions
  bart.hat0 = apply( bart.hat0, 2, mean )
  bart.hat1 = apply( bart.hat1, 2, mean )

  # Calculate IATE
  iate = bart.hat1 - bart.hat0
  return( iate ) 
}


# Switch to TRUE to test the function
if ( FALSE ) {
  
  # Call testing MND data
  source( here::here( "tests/create_testing_data.R") )
  
  # Run the model
  test = bart_s_alg( x_tr, y_tr, d_tr, x_val )
  
}

