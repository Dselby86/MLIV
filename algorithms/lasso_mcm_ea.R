
#' lasso_mcm_ea:: Implementation of MCM with efficiency augmentation using the \code{\link{glmnet}} package
#' MCM with EA incorporates efficiency augmentation by replacing the outcome variable with residuals (observed 
#' outcome minus estimated main effects). This is done to potentially improve estimation efficiency.
#' MCM with EA requires prior estimation of both propensity scores and main effects before estimating IATEs.
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

lasso_mcm_ea = function( y_tr, d_tr, x_tr, np, x_val, args_tau = list() ) {
  mo = 2 * (y_tr - np[,"y_hat"]) * (2*d_tr - 1)
  w = ( 2 * d_tr - 1 ) * ( d_tr - np[,"p_hat"] ) / ( 4 * np[,"p_hat"] * ( 1 - np[,"p_hat"] ) )
  fit_tau = do.call( cv.glmnet, c( list( x = x_tr, y = mo, weights = w ), args_tau ) )
  iate = predict( fit_tau, x_val, s = "lambda.min" )
  return( iate )
}