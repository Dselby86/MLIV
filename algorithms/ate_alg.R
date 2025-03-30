#' ate_alg: Implementation of ATE 
#' This function computes the Average Treatment Effect (ATE) using the difference 
#' in means between treated and control groups in the training data and applies it 
#' to a validation dataset.
#' 
#' @param y_tr Vector of outcome values
#' @param d_tr Vector of treatment assignment values
#' @param nval N or rows in validation data
#'
#' @return Returns predicted ATE of the validation sample
#'
#' @export

ate_alg = function( x_tr, y_tr, d_tr, x_val ) {
  stopifnot( nrow(x_tr) == length(y_tr) )
  stopifnot( is.numeric(d_tr) && length(d_tr) == nrow(x_tr) )
  
  diff_in_means = mean( y_tr[d_tr==1] ) - mean( y_tr[d_tr==0] ) # ATE is the same for all obs
  predictions = rep( diff_in_means, nrow(x_val) )
}
