# BART S-LEARNER

#' bart_s_alg: Implementation of BART S-Learner
#'  
#' Implemented using the \code{\link{dbarts}} package
#' 
#' @description S-learner treats the treatment variable as if it was just another covariate 
#' like those in the vector x_tr. Instead of having two models for the response as a function 
#' of the covariates x_tr, the S-learner has a single model for the response as a function of x_tr 
#' and the treatment d_tr. 
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

bart_s_ni_alg <- function(x_tr, y_tr, d_tr, x_val, verbose = FALSE) {
  
  # Combine features and treatment in one dataframe
  dat <- data.frame( x_tr )
  dat['P'] <- d_tr

  # Train the model
  fit_tau <- dbarts::bart(dat, y_tr, keeptrees = TRUE, verbose = verbose, nthread = 1 ) 

  # Combine features and treatment in one dataframe for validation set and make them ether 1 or 0
  valdat <- data.frame(x_val)
  valdat['P'] <- 1
  d0 <- valdat
  d1 <- d0
  d0$P <- 0
  d1$P <- 1
  
  # Predict expected outcomes under and without treatment
  bart.hat0 <- predict(object = fit_tau, newdata = d0)
  bart.hat1 <- predict(object = fit_tau, newdata = d1)
  
  # Take a mean of predictions
  bart.hat0 <- apply(bart.hat0, 2, mean)
  bart.hat1 <- apply(bart.hat1, 2, mean)

  # Calculate IATE
  iate <- bart.hat1 - bart.hat0
  
  return(iate) 
}


# Switch to TRUE to test the function
if (FALSE) {
  
  # Call testing MND data
  source(here::here("tests/create_testing_data.R"))
  
  # Run the model
  test <- bart_s_ni_alg(x_tr, y_tr, d_tr, x_val)
  
}

