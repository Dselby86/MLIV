#' convert_to_z:: Convert Y to Z-scores
#'
#' This function converts the given outcome variable `Y` to Z-scores.
#'
#' @param Y A numeric vector representing the outcome variable to be converted to Z-scores.
#' 
#' @return A numeric vector representing the generated Z-scores.
#' 
#' @examples
#' \dontrun{
#'   y <- c(1, 2, 3, 4, 5)
#'   z_scores <- convert_to_z(y)
#'   print(z_scores)
#' }
#'
#' @export
#' 
convert_to_z <- function(Y) {
  
  # Calculate the number of observations in Y
  K <- length(Y)
  
  # Calculate the percentile of each Y
  r_Y <- rank(Y, ties.method = "random")/K - 1/(2*K)
  
  # Calculate the Z-scores of Y, given percentile.
  z <- qnorm(r_Y)
  
  # Return the calculated Z-scores
  return(z)
}

