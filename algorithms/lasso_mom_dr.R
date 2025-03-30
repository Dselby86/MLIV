
#' lasso_mom_dr:: Implementation of MOM DR using the \code{\link{glmnet}} package
#' Modified Outcome Methods Doubly Robust is just a approach that combines two 
#' modeling approaches to estimate treatment effects. It's called "doubly robust" 
#' because it remains consistent if either the treatment assignment model or the 
#' outcome model is correctly specified. 
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

lasso_mom_dr = function( y_tr, d_tr, x_tr, np, x_val, args_tau = list() ) {
  mo = np[,"y1_hat"] - np[,"y0_hat"] + d_tr * ( y_tr - np[,"y1_hat"] ) / np[,"p_hat"] - ( 1 - d_tr ) * ( y_tr - np[,"y0_hat"] ) / ( 1 - np[,"p_hat"] )
  fit_tau = do.call( cv.glmnet, c( list( x = x_tr, y = mo ), args_tau ) )
  iate = predict( fit_tau, x_val, s = "lambda.min" )
  return( iate )
}

