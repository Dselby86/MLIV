#' lasso_t_alg: Implementation of CMR Lasso using the \code{\link{glmnet}} package
#' Conditional mean regression Lasso is just a traditional approach that takes the 
#' difference of conditional expectations that are estimated in the two samples 
#' of treated and non-treated separately.
#' 
#' @param x_tr Matrix of training covariates (N x X matrix)
#' @param y_tr Vector of outcome values
#' @param d_tr Vector of treament indicators in training data
#' @param x_val Matrix of validation covariates (N x X matrix)
#' @import glmnet
#'
#' @return Returns predicted IATEs of the validation sample
#'
#' @export

lasso_t_alg = function( x_tr, y_tr, d_tr, x_val ) {
  lasso0 = cv.glmnet( x_tr[d_tr==0,], y_tr[d_tr==0] )
  lasso1 = cv.glmnet( x_tr[d_tr==1,], y_tr[d_tr==1] )
  predictions = predict( lasso1, x_val ) - predict( lasso0, x_val )
  return( predictions )
}