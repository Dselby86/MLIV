


#' process_sim_results:: Process Simulation Results
#'
#' This function processes the results of a simulation (predicted
#' IATEs) by calculating various performance metrics such as bias,
#' standard error, RMSE, runtime, and fail rate. It takes a list of
#' simulation data and compares the simulated treatment effects to
#' true treatment effects.
#'
#' @param sim_data_list A list containing three elements:
#' \itemize{
#'   \item \strong{predictedtau}: A 3D array of simulated treatment effect estimates where the dimensions are [number of simulations, number of observations, number of methods].
#'   \item \strong{runtime}: A 3D array of runtime data where the dimensions are [number of simulations, number of observations, number of methods].
#'   \item \strong{failrate}: A 3D array of fail rate data where the dimensions are [number of simulations, number of observations, number of methods].
#' }
#' @param true_tau A numeric vector of the true treatment effects. Its
#'   length should match the number of observations in the test set.
#'
#' @return A tibble data frame where each row corresponds to a
#'   performance metric:
#' \itemize{
#'   \item \strong{metric}: The performance metric being reported ("bias", "se", "rmse", "runtime", "percent_cut").
#'   \item \strong{id}: The index of the treatment effect.
#'   \item \strong{value}: The computed value of the metric for a method.
#'   \item \strong{queen}: The string name of a queen for DGP.
#' }
#'
#' @export
#' 
process_sim_results <- function(sim_data_list, true_tau) {
  
  # Convert true IATEs to numeric vector
  true_tau <- as.numeric(true_tau)
  
  # Separate the list of lists
  predictedtau_data <- sim_data_list[[1]]
  runtime_data <- sim_data_list[[2]]
  failrate_data <- sim_data_list[[3]]
  dims <- dim(predictedtau_data) 
  
  # Calculate expected estimates for each method:
  
  tau_bar_hat <- apply(predictedtau_data, 2:3, mean) # Average across simulation runs
  stopifnot(nrow(tau_bar_hat) == length(true_tau))
  
  # Calculate BIAS
  bias <- tau_bar_hat - true_tau
  
  # Calculate squared errors
  errs2 <- (predictedtau_data - rep(true_tau, each=dims[[1]]))^2  # When you perform an operation between an array and a vector, R automatically recycles (repeats) the vector to match the dimensions of the array
  
  # Calculate RMSE
  rmse <- sqrt(apply(errs2, 2:3, mean)) 
  
  # Calculate SE
  se <- apply(predictedtau_data, 2:3, sd) 
  
  # Calculate run time and fail rate
  runtime <- apply(runtime_data, 2:3, mean) 
  percent_cut <- apply(failrate_data, 2:3, mean) 
  
  # Combine together
  res <- bind_rows(bias = as_tibble(bias), 
                   se = as_tibble(se), 
                   rmse = as_tibble(rmse),
                   runtime = as_tibble(runtime),
                   percent_cut = as_tibble(percent_cut),
                   .id = "metric")
  
  # Replicate all by id
  res <- res %>%
    mutate(id = rep( 1:length(true_tau), 5)) %>%
    relocate(metric, id)
  
  return(res)  
}
