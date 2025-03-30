#' generate_mnd_data:: Generate a Multivariate Normal Dataset with Treatment and Control Groups
#'
#' This function generates a multivariate normal dataset with `N` observations, randomized to treatment and control groups.
#'
#' @param n_seed An optional numeric value for the random seed to replicate results. If not provided, a random seed will be generated.
#' @param N A numeric value specifying the number of observations to simulate. Default is 1000.
#' @param exclude_Y0_Y1 A logical value indicating whether to exclude the `Y0` and `Y1` variables from the dataset. Default is `FALSE`.
#' 
#' @return A list containing the generated data (`new_data`) and the seed number (`seed_number`) used to generate the data.
#' 
#' @examples
#' \dontrun{
#'   result <- generate_mnd_data(n_seed = 123, N = 500, exclude_Y0_Y1 = TRUE)
#'   new_data <- result$new_data
#'   seed_used <- result$seed_number
#' }
#' 
#' @export
#' 
generate_mnd_data <- function(n_seed = NULL,  
                              N = 1000, 
                              exclude_Y0_Y1 = FALSE) {   
  
  # Set the random seed if it's not provided:
  if(is.null(n_seed)){
    n_seed <- sample(1:100000, 1) 
  }
  
  set.seed(n_seed)   
  
  # Generate the covariance matrix Sigma_X
  Sigma_X <- diag(c(1, 2, 3)) + 1 # Sigma_X matrix determines how the variables in the X dataset are correlated with each other
  Sigma_X[1, 2] = Sigma_X[2, 1] = -0.8 # Introduce correlation of -0.8 between the first and second variables in the covariance matrix
  
  # Generate the Xs dataset
  X <- mvrnorm(N, mu = c(1, 2, 3), Sigma = Sigma_X) # Generate N observations from a multivariate normal distribution with the specified mean vector and covariance matrix
  colnames(X) <- paste0("X", 1:ncol(X)) # Assign column names to the Xs dataset
  
  # Create the dat dataset
  dat <- X %>% as_tibble() %>%
    mutate(X4 = X1 + rnorm(n()), # Create X4 as X1 + random noise
           Y0 = (1 * X1^2 + 2 * X1*X2 + exp(X3 / 2) + rnorm(N, sd = 4))^2, # Calculate Y0 based on X1, X2, and X3
           Y0 = pmin(Y0, quantile(Y0, 0.95)), # Replace values in Y0 exceeding the 95th quantile with that quantile value
           X1 = round(2*X1), # Round X1 to the nearest integer after scaling by 2
           X1 = ifelse(X1 < 0, 0, round(2 * X1)), # Ensure X1 is never negative and round it to the nearest integer after scaling by 2
           X2 = 0 + (X2 >= 0.5), # Convert X2 to 0 if it's less than 0.5, 1 otherwise
           Y1 = Y0 + sd(Y0) * (0.2 * X1 + 0.3 * X2), # Calculate Y1 based on Y0, X1, and X2
           Z = as.numeric(sample(n()) <= n() * 0.4), # Create Z as treatment assignment
           Y_continuous = ifelse(Z, pmax(Y1, 0), pmax(Y0, 0)), # Ensure Y_continuous is 0 or greater
           Y_continuous = pmin(Y_continuous, quantile(Y_continuous, 0.95)), # Replace values in Y_continuous exceeding the 95th quantile with that quantile value
           Y_continuous = ifelse(Y_continuous > 300 & Y_continuous < 800, 550, Y_continuous), #  Set values between 300 and 800 to 550      
           Y_binary = ifelse(Y_continuous > 200, 1, 0) # Create Y_binary based on Y_continuous
          )
  
  # Add some junk covariates
  more_X <- round(mvrnorm(n = N, mu = rep(0,5), Sigma=diag(5)), digits=2)
  colnames(more_X) <- paste0("X", 5:9)
  dat <- bind_cols(dat, more_X)
  
  # Recode categorical covariates as factors
  dat$X1 <- as.factor(paste0("f", dat$X1))
  
  # Exclude Y0 and Y1 columns if exclude_Y0_Y1 is TRUE
  if (exclude_Y0_Y1) {
    dat <- dat %>%
      dplyr::select(-Y0, -Y1)
  }
  
  dat <- relocate(dat, Z, starts_with("X"))
  
  # Create a list with the generated data and the seed number
  data_list <- list(new_data = dat, seed_number = n_seed)
  
  return(data_list)
}
