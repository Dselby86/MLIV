#' windsorize_impacts:: Windsorize Impacts
#'
#' This function windsorizes the impacts to be within a specified number of 
#' standard deviations of the true Average Treatment Effect (ATE). 
#' This helps prevent the impacts from being too large or too small.
#'
#' @param tau A numeric vector of impacts to be windsorized.
#' @param ATE A numeric value representing the true Average Treatment Effect. Default is 0.
#' @param sd A numeric value representing the standard deviation. Default is 1.
#' @param num_sd A numeric value representing the number of standard deviations within which to windsorize the impacts. Default is 1.
#'
#' @return A numeric vector with windsorized impacts.
#'
#' @examples
#' \dontrun{
#'   tau <- c(-3, -1, 0, 1, 3)
#'   ATE <- 0
#'   sd <- 1
#'   num_sd <- 1
#'   windsorized_tau <- windsorize_impacts(tau, ATE, sd, num_sd)
#'   print(windsorized_tau)  # Returns c(-1, -1, 0, 1, 1)
#' }
#'
#' @export
#' 
windsorize_impacts <- function(tau, ATE = 0, sd = 1, num_sd = 1) {
  
  # Calculate the lower and upper bounds
  lower_bound <- ATE - num_sd * sd
  upper_bound <- ATE + num_sd * sd
  
  # Windsorize the impacts
  tau[tau < lower_bound] <- lower_bound
  tau[tau > upper_bound] <- upper_bound
  
  return(tau)
}