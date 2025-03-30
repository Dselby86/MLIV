#' cdml_alg: Implementation of CDML using the \code{\link{causalDML}} package
#' ndr_oos function implement the Doubly Robust (DR) and Normalized Doubly Robust (NDR) 
#' learner methods. This function estimates nuisance parameters, computes DR scores, 
#' and executes DR- and NDR-learner steps for different treatment comparisons.
#' 
#' @param x_tr Matrix of training covariates (N x X matrix)
#' @param y_tr Vector of outcome values
#' @param d_tr Vector of treament indicators in training data
#' @param x_val Matrix of validation covariates (N x X matrix)
#' @param current_seed Seed to use with grf
#' @import causalDML
#'
#' @return Returns predicted IATEs of the validation sample
#'
#' @export

cdml_alg = function(y_tr, d_tr, x_tr, x_val, current_seed) {
  cdml = ndr_oos(y = y_tr, w = d_tr, x = x_tr,
                 xnew = x_val,
                 ml_w = list(create_method("ols")),
                 ml_y = list(create_method("forest_grf", args = list( tune.parameters = "all", num.threads = 1, seed = current_seed))),
                 ml_tau = list(create_method("forest_grf", args = list( tune.parameters = "all", num.threads = 1, seed = current_seed))),
                 quiet=TRUE)
  predictions = cdml$cates[,2]
  return(predictions)
}
