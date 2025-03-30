

#' rf_inf_alg: Implementation of RF Inf using the \code{\link{grf}} package
#' Iinfeasible models are statistical or machine learning models that have access 
#' to information or data that would not be available in a real-world or practical setting,
#' in this case tau_tr. 
#' These models are often used as benchmarks or reference points to assess the performance 
#' of more realistic models when the ideal conditions are met.
#' 
#' @param x_tr Matrix of training covariates (N x X matrix)
#' @param tau_tr Vector of training IATE
#' @param x_val Matrix of validation covariates (N x X matrix)
#' @import grf
#'
#' @return Returns predicted IATEs of the validation sample
#'
#' @export

rf_inf_alg = function( x_tr, tau_tr, x_val ) {
  rf_inf = regression_forest( x_tr, tau_tr )
  predictions = predict( rf_inf, x_val )$predictions
  return( predictions )
}
