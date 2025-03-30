
#' cf_lc_alg: Implementation of CF model with local centering using the \code{\link{grf}} package
#' The Causal Forest with local centering is a machine learning approach used to estimate conditional
#' average treatment effects (CATEs) or treatment effects in the presence of confounding variables. 
#' This method builds upon the concept of Causal Forests but incorporates local centering to improve 
#' performance in the presence of confounding.
#' 
#' @param x_tr Matrix of training covariates (N x X matrix)
#' @param y_tr Vector of outcome values in training data
#' @param d_tr Vector of treament indicators in training data
#' @param x_val Matrix of validation covariates (N x X matrix)
#' @param np Matrix of nuisance parameters obtained by \code{nuisance_cf_grf}
#' @import grf
#'
#' @return Returns predicted IATEs of the validation sample
#'
#' @export

cf_lc_alg = function( x_tr, y_tr, d_tr, x_val, np, current_seed) {
  cf_lc = causal_forest( x_tr, y_tr, d_tr, Y.hat = np[,2], W.hat = np[,1], num.threads = 1, seed = current_seed )
  predictions = predict( cf_lc,x_val )$predictions
  return( predictions )
}
