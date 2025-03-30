
#' rf_mom_dr_alg: Implementation of MOM DR using the \code{\link{grf}} package
#' Modified Outcome Methods Doubly Robust is just a approach that combines two 
#' modeling approaches to estimate treatment effects. It's called "doubly robust" 
#' because it remains consistent if either the treatment assignment model or the 
#' outcome model is correctly specified. 
#' 
#' @param y_tr Vector of outcome values
#' @param d_tr Vector of treament indicators in training data
#' @param x_tr Matrix of training covariates (N x X matrix)
#' @param np Matrix of nuisance parameters obtained by \code{nuisance_cf_grf}
#' @param x_val Matrix of validation covariates (N x X matrix)
#' @param args_tau List of arguments passed to estimate IATEs
#' @import grf
#'
#' @return Returns predicted IATEs of the validation sample
#'
#' @export

rf_mom_dr_alg = function(y_tr, d_tr, x_tr, np, x_val, current_seed) {
  
  mo = np[,"y1_hat"] - np[,"y0_hat"] + d_tr * (y_tr - np[,"y1_hat"]) / np[,"p_hat"] - (1 - d_tr) * (y_tr - np[,"y0_hat"]) / (1-np[,"p_hat"])
  
  # Fit the regression forest with specified settings
  fit_tau = regression_forest(X = x_tr, Y = mo, tune.parameters = 'all', num.threads = 1, seed = current_seed)
  
  iate = predict( fit_tau, x_val )$prediction
  return( iate )
}
