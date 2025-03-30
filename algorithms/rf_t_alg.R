
#' rf_t_alg: Implementation of RF T using the \code{\link{grf}} package
#' Conditional mean regression RF (or RF T-learner) is just a traditional approach 
#' that takes the difference of conditional expectations that are estimated in 
#' the two samples of treated and non-treated separately.
#' 
#' @param x_tr Matrix of training covariates (N x X matrix)
#' @param y_tr Vector of outcome values
#' @param d_tr Vector of treament indicators in training data
#' @param x_val Matrix of validation covariates (N x X matrix)
#' @import grf
#'
#' @return Returns predicted IATEs of the validation sample
#'
#' @export

rf_t_alg = function(x_tr, y_tr, d_tr, x_val, current_seed) {
  
  rf0 = regression_forest(x_tr[d_tr == 0,], y_tr[d_tr == 0], num.threads = 1, seed = current_seed)
  rf1 = regression_forest(x_tr[d_tr == 1,], y_tr[d_tr == 1], num.threads = 1, seed = current_seed)
  predictions = predict(rf1, x_val)$predictions - predict(rf0, x_val)$predictions
  return(predictions)
}
