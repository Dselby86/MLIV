#' lasso_inf_alg: Implementation of Lasso using the \code{\link{glmnet}} package
#' Iinfeasible models are statistical or machine learning models that have access 
#' to information or data that would not be available in a real-world or practical setting,
#' in this case tau_tr. 
#' These models are often used as benchmarks or reference points to assess the performance 
#' of more realistic models when the ideal conditions are met.
#' 
#' @param x_tr Matrix of training covariates (N x X matrix)
#' @param tau_tr Vector of training IATE
#' @param x_val Matrix of validation covariates (N x X matrix)
#' @import glmnet
#'
#' @return Returns predicted IATEs of the validation sample
#'
#' @export

lasso_inf_alg = function( x_tr, tau_tr, x_val ) {
  lasso_inf = cv.glmnet( x_tr, tau_tr )
  predictions = predict( lasso_inf, x_val )
  return( predictions )
}