
#' rf_mom_ipw_alg: Implementation of MOM IPF using the \code{\link{grf}} package
#' Modified Outcome Methods Inverse Probability Weighting is just a approach to estimate 
#' the IATE while accounting for differences in treatment assignment probabilities. 
#' 
#' @param y_tr Vector of outcome values
#' @param d_tr Vector of treament indicators in training data
#' @param x_tr Matrix of training covariates (N x X matrix)
#' @param np Matrix of nuisance parameters obtained by \code{nuisance_cf_grf}
#' @param x_val Matrix of validation covariates (N x X matrix)
#' @param current_seed Seed for grf
#' @import grf
#'
#' @return Returns predicted IATEs of the validation sample
#'
#' @export

rf_mom_ipw_alg = function(y_tr, d_tr, x_tr, np, x_val, current_seed) { 
  
  # Calculate moment orthogonalized (IPW-adjusted) outcome
  mo = y_tr * (d_tr - np[,"p_hat"]) / (np[,"p_hat"] * (1 - np[,"p_hat"]))
  
  # Fit the regression forest with specified settings
  fit_tau = regression_forest(X = x_tr, Y = mo, tune.parameters = 'all', num.threads = 1, seed = current_seed)
  
  # Predict individual treatment effects on the validation set
  iate = predict(fit_tau, x_val)$prediction
  
  return(iate)
}
