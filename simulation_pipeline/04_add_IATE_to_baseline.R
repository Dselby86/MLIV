# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
#   
#                                                                         
# Created on:   06/18/2024
# Purpose:      Script that adds treatment effects to baseline data
# Authors:      Luke Miratrix, Polina Polskaia, Nick Commins
# 
#
# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #


# Load script 03
source(here::here("simulation_pipeline/03_generate_true_IATE.R"))

# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
#  A HELPER FUNCTION THAT ADDS IATEs TO DATASET ----
# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #


#' add_treatment_effects:: Add Treatment Effects to Baseline Data
#'
#' This function adds treatment effects to a baseline dataset by
#' taking the passed IATEs and generating the relevant potential
#' outcomes.  It will also generate outcomes for binary given the
#' IATEs and initial Y0 as probabilities.
#'
#' @param baseline The baseline data containing the initial outcome
#'   variable (the Y0), named `outcome_Y0`, where "outcome" is the
#'   name of the outcome variable, below.
#' @param IATE The individual average treatment effects to be added to
#'   the baseline outcome.
#' @param outcome The name of the outcome variable.
#' @param treatment The name of the treatment variable (optional).
#' @param binary_outcome TRUE/FALSE.  Whether the outcome variable is
#'   binary. If \code{NULL}, it is determined automatically based on
#'   \code{realdata}.
#'
#' @return The baseline data with added treatment effects, including
#'   potential outcomes \eqn{Y0} and \eqn{Y1}, and the observed
#'   outcome based on the treatment group.
#'
#' @export
#' 
add_treatment_effects <- function(baseline, 
                                  IATE, 
                                  outcome, 
                                  treatment = NULL, 
                                  binary_outcome = FALSE, 
                                  verbose = 1000) {
  
  # Create a name for the Y0 var  
  Y0_name <- paste(outcome, "_Y0", sep = "")
  
  # Create Y1 and Y0
  Y0 <- baseline[ , Y0_name]
  Y1 <- Y0 + IATE
  
  # Convert probabilities to binary
  if (binary_outcome) {
    
    stopifnot(all(Y0 >= 0 & Y0 <= 1))
    stopifnot(all(Y1 >= 0 & Y1 <= 1))
    
    # Y0 and Y1 _were_ probabilities.  We are converting these
    # probabilities to observed 0s and 1s given a latent coin flip of u
    u <- runif(length(IATE)) 
    
    # Update the values in the vectors Y1 and YO by setting them to 1 if the
    # corresponding value in vector u is less than or equal to the current value in Y1
    Y1 <- 0 + (u <= Y1) 
    Y0 <- 0 + (u <= Y0)
  } 
  
  # Rename variables
  baseline[paste0(outcome, "_Y0")] <- Y0 
  baseline[paste0(outcome, "_Y1")] <- Y1
  baseline[paste0(outcome, "_tau")] <- IATE
  
  # Assign predicted outcome based on treatment group, if treatment
  # flag passed
  if (!is.null(treatment)) {
    stopifnot(treatment %in% colnames(baseline))
    baseline[outcome] <- ifelse(baseline[[treatment]], Y1, Y0)
  }
  
  return(baseline)
}



# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
# TEST BLOCK ----
# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #


if (FALSE) {  
  
  # Load data
  mnd_data <- load_and_validate_real_data(real_data = here::here("datasets/mnd.csv"))
  mnd_baseline <- load_or_generate_baseline_object(dataset_name = "mnd", baseline_directory_path = here::here("baseline"), real_data = mnd_data, all_outcomes = c("Y_continuous", "Y_binary"), covariates_names = paste0("X", 1:9) , treatment_var = "Z", baseline_seed = 68814, baseline_N = 100000)
  mnd_baseline_data <- mnd_baseline$Data
  
  # Add treatment to baseline:
  nval <- nrow(mnd_baseline_data)
  treatment <- "Z"
  
  # Generate treatment with p_tx proportion treated
  actual_Tx <- as.numeric(sample(nval) <= 0.5 * nval) 
  mnd_baseline_data[, treatment] <- actual_Tx
  stopifnot(treatment %in% names(mnd_baseline_data))
  
  # Load IATES
  load_mnd_iates <- load_or_generate_iates(dataset_name = "mnd", 
                                           real_data = mnd_data, 
                                           queen_list = c("ATE", "OLS S"), 
                                           baseline_seed = 68814, 
                                           baseline_data = mnd_baseline_data, 
                                           small_covariate_set = paste0("X", 1:9), 
                                           outcome = "Y_binary",
                                           treatment_var = "Z", 
                                           sigma_tau = 0.2, 
                                           ate_real = NULL, 
                                           sd_y0_real = NULL)
  
  # Select one queen
  IATE <- load_mnd_iates[["ATE"]]
  
  # Add treatment effects
  baseline_full <- add_treatment_effects(baseline = mnd_baseline_data,
                                         IATE = IATE,
                                         outcome = "Y_binary",
                                         treatment = "Z")
  
}


