#' calculate_rho:: Calculate the correlation between Y and Y_hat
#'
#' This function calculates the correlation between the given outcome variable `Y`
#' and its predictions `Y_hat`. The correlation is based on the Z-scores of the variables.
#'
#' @param y A numeric vector representing the actual values of the outcome variable.
#' @param y_hat A numeric vector representing the predicted values of the outcome variable.
#'
#' @return A numeric value representing the calculated correlation between `y` and `y_hat`.
#' 
#' @examples
#' \dontrun{
#'   y <- c(1, 2, 3, 4, 5)
#'   y_hat <- c(1.1, 1.9, 3.2, 3.8, 5.1)
#'   rho <- calculate_rho(y, y_hat)
#'   print(rho)
#' }
#'
#' @export
#' 
calculate_rho <- function(y, y_hat) {

  # Calculate the Z-scores of Y
  z_Y <- convert_to_z(y)

  # Calculate the Z-scores of Y_hat
  z_Yhat <- convert_to_z(y_hat)

  # Calculate the correlation between Y and Y_hat
  rho <- cor(z_Y, z_Yhat)

  # Return the calculated correlation
  return(rho)
}
