
#' cf_alg: Implementation of CF model using the \code{\link{grf}} package
#' causal forest estimates the impact of a treatment or intervention by combining 
#' random forest techniques with causal inference principles. It builds an ensemble 
#' of decision trees, each trained on different subsets of the data, to estimate 
#' individualized treatment effects based on subjects' characteristics (covariates). 
#' These individual estimates are then aggregated to provide an overall assessment of 
#' the treatment's impact while accounting for potential confounding factors, making 
#' it a valuable tool for causal analysis in observational data.
#' 
#' @param x_tr Matrix of training covariates (N x X matrix)
#' @param y_tr Vector of outcome values in training data
#' @param d_tr Vector of treament indicators in training data
#' @param x_val Matrix of validation covariates (N x X matrix)
#' @param ntrain N of rows in training data
#' @param current_seed The seed for grf
#' @import grf
#'
#' @return Returns predicted IATEs of the validation sample
#'
#' @export

cf_alg = function(x_tr, y_tr, d_tr, x_val, ntrain, current_seed) {
  cf = causal_forest(x_tr, y_tr, d_tr, 
                     Y.hat = NULL, W.hat = NULL, num.threads = 1, seed = current_seed) 
  predictions = predict(cf,x_val)$predictions
  return(predictions)
}
