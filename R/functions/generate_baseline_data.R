#' generate_baseline_data:: Generate Baseline Data
#'
#' This function generates a synthetic baseline dataset using the Synthpop package. 
#' It creates synthetic covariates based on the provided real data and covariate set, 
#' and then predicts control-side outcomes using a Random Forest model with a Copula apprach.
#'
#' @param real_data A data frame containing the real data.
#' @param all_outcomes A vector of outcome variable names to be included in the synthetic data.
#' @param large_covariate_set A vector of covariate names to be used for generating synthetic data.
#' @param treatment The name of the treatment variable.
#' @param baseline_seed An optional seed for reproducibility. If NULL, a seed will be randomly generated.
#' @param baseline_N The number of rows to generate in the synthetic dataset.
#'
#' @return A list containing:
#' \describe{
#'   \item{Data}{A data frame with the synthetic baseline data.}
#'   \item{Y0_seed}{The seed used for generating control-side outcomes.}
#'   \item{seed_synthpop}{The seed used by the Synthpop package.}
#' }
#'
#' @examples
#' \dontrun{
#' real_data <- data.frame(matrix(rnorm(1000), ncol=10))
#' colnames(real_data) <- paste0("V", 1:10)
#' real_data$treatment <- sample(0:1, 100, replace = TRUE)
#' all_outcomes <- c("V1", "V2")
#' large_covariate_set <- paste0("V", 3:10)
#' result <- generate_baseline_data(real_data, all_outcomes, large_covariate_set, "treatment", baseline_N = 100)
#' }
#'
#' @export
#' 
generate_baseline_data = function(real_data, 
                                  all_outcomes, 
                                  large_covariate_set, 
                                  treatment,
                                  baseline_seed = NULL,  
                                  baseline_N = NULL,
                                  verbose = 1000 ) {   
  
  stopifnot( is.data.frame(real_data) )
  real_data = as.data.frame(real_data) # This is so if real_data is a tibble, some of the subsetting later on doesn't totally crash.
  
  # Set the random seed if it's not provided
  if(is.null(baseline_seed)) {
    baseline_seed = sample(1:baseline_N, 1) 
  }
  
  # Generate covariates using Synthpop package:
  
  # Note:: we only generate covariates based on covariates, treatment and outcomes do not play any role 
  covariates_subset <- real_data[, large_covariate_set]
  
  # Create synthetic data as synds object
  new_data_syn <- syn(covariates_subset,     
                      seed = baseline_seed, # Set to the random seed generated using the seed at the beginning of the program
                      k = baseline_N,       # Set N rows
                      minnumlevels = 6,
                      print.flag = verbose > 1)     # Convert numeric variables with 5 or fewer unique values to factors
  
  # Convert our synthetic data from a list inside a synds object to a dataframe 
  new_data <- as.data.frame(new_data_syn$syn)
  
  # Capture the seed inside synthpop object 
  seed_synthpop <- new_data_syn$seed
  
  # Generae control-side outcomes:
  
  # Reset seed to bundle all syn() pseudorandomness and separate it
  # from our random DGP code of Y0 assignment to help with reproducibility
  Y0_seed <- (baseline_seed + 13)   
  set.seed(Y0_seed)   
  
  n_outcomes = length(all_outcomes)
  is_binary_list = rep( NA, n_outcomes )
  names(is_binary_list) = all_outcomes
  
  # Iterate over each outcome var in a list
  for (i in 1:n_outcomes) { 
    
    # The outcome in this particular iteration
    y <- all_outcomes[i] 
    
    if (verbose > 0) {
      cat("Working on", y, "\n")
    }
    
    
    # Drop rows with missing values in our target outcome
    realdata_inside <- real_data[!is.na(real_data[[y]]), ]
    
    vcat( verbose, paste("Sample size after dropping missing values in", 
                         y, ":", nrow(realdata_inside), "\n"))
    
    # We first use the original data to make a model that will allow us
    # to predict outcomes given the data. We use the full dataset, and
    # include treatment as a covariate attempting to remove 
    # any impact of the treatment.
    
    is_binary  <- is_binary( realdata_inside[[y]] )
    is_binary_list[i] = is_binary
    
    # Specify RF model
    covariates_formula <- paste(large_covariate_set, collapse = " + ")
    covariates_treatment_formula <- paste0(covariates_formula, " + ", treatment)
    form <- as.formula(paste(y, "~", covariates_treatment_formula))
    
    # We use RF model to make predictions of Y0 given Xs, and Z.
    
    # Our predictive model for Y0 is a different predictive model than
    # for our causal impact models to predict Y1: we are just trying
    # to get some realistic Y0 that appropriately corresponds to X.
    if ( is_binary ) {
      realdata_inside[[y]] <- as.factor(realdata_inside[[y]])
    }
    Y0_model <- randomForest(form, data = realdata_inside)
    
    y_tr <- realdata_inside[ , y]
    if (!is_binary) {
      
      # We need to predict Y on the real data with observed Y in order
      # to calculate rho.
      Y_hat <- predict(Y0_model, newdata = realdata_inside)
      
      # Predict control-side outcome using the above functions:
      # Identify control units
      ctls <- realdata_inside[[treatment]] == 0  
      emp_dist_Y0 <- realdata_inside[ctls, y]
      rho <- calculate_rho(emp_dist_Y0, Y_hat[ctls])
      Y0 <- generate_y(dat = new_data, model = Y0_model, R2 = rho^2,
                       empDist = emp_dist_Y0,
                       treatment = treatment, y = y)
    } else {
      
      # Predict the control-side potential outcomes as a probability
      # of a 1 using our predictive model. Set treatment to 0 first
      # so we get control-side predictions for everyone.
      ctls<- new_data
      ctls[, treatment] <- 0
      Y0 <- predict(Y0_model, newdata = ctls, type="prob")[,"1"]
    }
    
    if (verbose > 0) {
      cat("\tWe now have our control-side potential outcome for each observation in our baseline dataset!\n")
    }
    
    # Add Y0 to the baseline data
    new_data[paste0(y, "_Y0")] <- Y0 
    
  }
  
  data_list <- list(new_data, Y0_seed, seed_synthpop)
  names(data_list) <- c("Data", "Y0_seed", "seed_synthpop")
  
  data_list$is_binary = is_binary_list
  data_list$p_tx = mean( real_data[[treatment]] == 1 )
  
  return(data_list)
}

###################################################################################
# TEST BLOCK
###################################################################################


if (FALSE) {
  real_data <- data.frame(matrix(rnorm(1000), ncol=10))
  colnames(real_data) <- paste0("V", 1:10)
  real_data$treatment <- sample(0:1, 100, replace = TRUE)
  all_outcomes <- c("V1", "V2")
  real_data$V2 = as.numeric( real_data$V2 > 0.3 )
  large_covariate_set <- paste0("V", 3:10)
  result <- generate_baseline_data(real_data, all_outcomes, large_covariate_set,
                                   "treatment", baseline_N = 100, baseline_seed = 30303 )
  
  result
}

