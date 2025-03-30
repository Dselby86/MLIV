
#' lasso_r:: Implementation of R-learning using the \code{\link{glmnet}} package
#' R-learning does not involve efficiency augmentation and estimates IATEs directly.
#' R-learning typically estimates nuisance parameters like propensity scores and main 
#' effects as part of the optimization process.
#' 
#' @param y_tr Vector of outcome values in training data
#' @param d_tr Vector of treament indicators in training data
#' @param x_tr Matrix of training covariates (N x X matrix)
#' @param np Matrix of nuisance parameters obtained by \code{nuisance_cf_glmnet}
#' @param x_val Matrix of validation covariates (N x X matrix)
#' @param args_tau List of arguments passed to estimate IATEs
#' @import glmnet
#'
#' @return Returns vector containing the out-of-sample IATEs
#'
#' @export

lasso_r = function( y_tr, d_tr, x_tr, np, x_val, args_tau = list() ) {
  mo = ( y_tr - np[,"y_hat"] ) / ( d_tr - np[,"p_hat"] )
  w = ( d_tr - np[,"p_hat"] )^2
  fit_tau = do.call( cv.glmnet, c( list( x = x_tr, y = mo, weights = w ), args_tau ) )
  iate = predict( fit_tau, x_val, s = "lambda.min" )
  return( iate )
}
