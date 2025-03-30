#' calculate_te: Implementation of treatment effects for the MLIV Project
#' For each of a data set's dependent variables calculate_te calculates the IATE treatment effects.
#' The ml_method argument is used to specify the machine learning function that will be used to calculate treatment effects.
#' These method functions can be found in the MLIV/methods function and MUST take the arguments x_tr, y_tr, d_tr, x_val, and ntrain
#'
#' @param x_tr Matrix of training covariates (N x X matrix)
#' @param y_tr Vector of outcome values in training data
#' @param d_tr Vector of treament indicators in training data
#' @param x_val Matrix of validation covariates (N x X matrix)
#' @param ntrain N of rows in training data
#' @param ml_method Name of the Machine learning method called.
#'
#' @return Returns predicted IATEs of the validation sample
#'
#' @export

calculate_te <- function(x_tr, y_tr, d_tr, x_val, ntrain, ml_method) {
  # Create empty outcomes matrix
  outcome_te <- matrix(NA,
    nrow = nrow(y_tr),
    ncol = ncol(y_tr)
  )

  # Convert to Data frame
  outcome_te <- outcome_te %>%
    data.frame()

  # Calculate treatment effect for each model
  for (i in seq_len(ncol(y_tr))){
    cate <- ml_method(
      x_tr = x_tr,
      y_tr = y_tr[, i],
      d_tr = d_tr,
      x_val = x_val,
      ntrain = ntrain
    )

    outcome_te[, i] <- cate
  }

  # Set Column names of outcome
  colnames(outcome_te) <- paste0("te_", colnames(y_tr))

  # Get name of Method
  method_name <- deparse(substitute(ml_method))
  outcome_te$method <- method_name

  # return expression
  return(outcome_te)
}
