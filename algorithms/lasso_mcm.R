
#' lasso_mcm:: Implementation of MCM using the \code{\link{glmnet}} package
#' The modified covariate method (MCM) is a statistical technique used for estimating 
#' IATEs in both experimental and observational studies. MCM is based on the idea of 
#' replacing the nonparametric function of the IATE with a linear working model, allowing 
#' for the estimation of treatment effects in a more structured way.
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

lasso_mcm = function( y_tr, d_tr, x_tr, np, x_val, args_tau = list() ) {
  mo = 2 * y_tr * ( 2 * d_tr - 1 )
  w = ( 2 * d_tr - 1) * ( d_tr - np[,"p_hat"] ) / ( 4 * np[,"p_hat"] * ( 1 - np[,"p_hat"] ) )
  fit_tau = do.call( cv.glmnet, c( list( x = x_tr, y = mo, weights = w ), args_tau ) )
  iate = predict( fit_tau, x_val, s = "lambda.min" )
  return( iate )
}
