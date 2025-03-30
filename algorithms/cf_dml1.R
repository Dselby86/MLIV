
#' cf_dml1: Implementation of MOM DR and MOM INF using DML1
#' (cross-fitting with nuisance parameters)
#'
#' Double Machine Learning 1 (DML1) is a statistical method that
#' splits data into two parts 50:50 to estimate treatment effects more
#' accurately by separately handling nuisance parameters and IATE,
#' reducing overfitting, and improving the reliability of causal
#' inference.
#'
#' @param est Estimator inside cross-fitting
#' @param y_tr Vector of outcome values in training data
#' @param d_tr Vector of treatment indicators in training data
#' @param x_tr Matrix of training covariates (N x X matrix)
#' @param np Matrix of nuisance parameters obtained
#' @param x_val Matrix of validation covariates (N x X matrix)
#' @param index List indicating indices for cross-fitting (e.g.
#'   obtained by \code{createFolds} of \code{\link{caret}} pkg)
#' @param args_tau List of arguments passed to estimate IATEs
#'
#' @return Returns predicted IATEs of the validation sample
#'
#' @export
cf_dml1 = function( est, y_tr, d_tr, x_tr, np, x_val, index, current_seed = NULL) {
  
  iate = matrix( 0, nrow( x_val ), 1 )
  # A loop is initiated to iterate over the cross-fitting folds
  for ( i in 1:length( index ) ) { 
    iate = iate + 1/length( index ) * # Calculate a weighted average of the IATE estimates across folds
      do.call( est, list( y_tr[index[[i]]], # Call the estimator
                         d_tr[index[[i]]],
                         x_tr[index[[i]],,drop=F],
                         np[index[[i]],], 
                         x_val,
                         current_seed
      ) )
  }
  return( iate )
}

