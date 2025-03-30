###################################################################################
#                                                                                                        
#                                                                         
# Created on:   13/06/24
# Purpose:      Load baseline data if it exists, if does not generate, save, and load
# Authors:      Luke Miratrix, Polina Polskaia, Nick Commins
# 
#
###################################################################################


# Load script 01
source(here::here("simulation_pipeline/01_load_real_data.R"))


###################################################################################
# FUNCTION TO GENERATE/LOAD BASELINE DATA ----
###################################################################################


#' load_or_generate_baseline_object:: Load or Generate Baseline Object
#'
#' This function loads a pre-existing baseline object from a specified
#' directory or generates a new one if it does not exist. The baseline
#' object is generated using the provided real data, outcome variables
#' names, covariates names, treatment variable name, seed, and sample
#' size, and is saved to the specified directory for future use.
#'
#' @param dataset_name A string representing the name of the dataset.
#' @param baseline_directory_path A string representing the path to
#'   the directory where the baseline object is stored or should be
#'   saved.
#' @param real_data A data frame containing the real data OR a string
#'   containing the directory of a csv to load.
#' @param all_outcomes A vector of outcome variable names to be
#'   included in the baseline object.
#' @param covariates_names A vector of covariate names to be used in
#'   generating the baseline object.
#' @param treatment_var The name of the treatment variable.
#' @param baseline_seed An integer seed for reproducibility when
#'   generating the baseline object.
#' @param baseline_N The number of rows to generate in the synthetic
#'   dataset.
#'
#' @return A baseline object
#'
#' @examples
#' \dontrun{
#' dataset_name <- "my_dataset"
#' baseline_directory_path <- "path/to/baseline_directory"
#' real_data <- data.frame(matrix(rnorm(1000), ncol=10))
#' colnames(real_data) <- paste0("V", 1:10)
#' real_data$treatment <- sample(0:1, 100, replace = TRUE)
#' all_outcomes <- c("V1", "V2")
#' covariates_names <- paste0("V", 3:10)
#' treatment_var <- "treatment"
#' baseline_seed <- 123
#' baseline_N <- 100
#' baseline_object <- load_or_generate_baseline_object(dataset_name, baseline_directory_path, real_data, all_outcomes, covariates_names, treatment_var, baseline_seed, baseline_N)
#' }
#'
#' @export
#' 
load_or_generate_baseline_object <- function( dataset_name,
                                              baseline_directory_path,
                                              real_data = NULL, 
                                              all_outcomes,
                                              covariates_names,
                                              treatment_var, 
                                              baseline_seed, 
                                              baseline_N,
                                              verbose = 1000,
                                              force_generate = FALSE) {
  
  # Dynamically create a baseline object name
  data_file_name = paste0(dataset_name, "_baseline_object", ".rds")
  
  # Generate your personal baseline file path
  baseline_object_path <- file.path(baseline_directory_path, data_file_name)
  
  # Check if the baseline object already exists
  if (!force_generate && file.exists(baseline_object_path)) {
    if ( verbose > 0 ) {
      message("Baseline object already exists. Loading from file: ", baseline_object_path)
    }
    # Load baseline data 
    baseline_object <- readr::read_rds(baseline_object_path)
    
    if ( is.null(baseline_object$is_binary) ) {
      message( "Baseline object does not have stored binary status for outcomes.  Regenerating..." )
    } else {
      return( baseline_object )
    }
  } else {
    if ( verbose > 0 ) {
      message("Baseline object not found. Generating and saving...")
    }
  }
  
  
  tryCatch({
    # Load/validate real data 
    real_data <- load_and_validate_real_data(real_data)
    
    # Generate baseline data
    baseline_object <- generate_baseline_data(real_data = real_data, 
                                              all_outcomes = all_outcomes,
                                              large_covariate_set = covariates_names,
                                              treatment = treatment_var,
                                              baseline_seed = baseline_seed, 
                                              baseline_N = baseline_N,
                                              verbose = verbose - 1)
    
    # Dynamically create an object name and assign the data to it
    object_name <- paste(dataset_name, "baseline_object", sep = "_")
    
    # Create rds path
    data_file_path = paste0(baseline_directory_path, "/", object_name, ".rds")
    
    # Save as RDS file if path is provided
    if (!is.null(data_file_path)) {
      tryCatch({
        readr::write_rds(baseline_object, data_file_path)
      }, error = function(e) {
        stop(paste("Error saving baseline RDS file:", e$message, "\n"))
      })
    }
    
    if (verbose > 0) {
      message("Baseline object generated and saved to: ", baseline_object_path)
    }
  }, error = function(e) {
    stop("Error generating or saving baseline object: ", e$message)
  })
  
  
  return(baseline_object)
  
}

###################################################################################
# TEST BLOCK ----
###################################################################################


if (FALSE) {
  dataset_name <- "test"
  baseline_directory_path <- here::here("baseline")
  real_data <- data.frame(matrix(rnorm(1000), ncol=10))
  colnames(real_data) <- paste0("V", 1:10)
  real_data$treatment <- sample(0:1, 100, replace = TRUE)
  all_outcomes <- c("V1", "V2")
  covariates_names <- paste0("V", 3:10)
  treatment_var <- "treatment"
  baseline_seed <- 123
  baseline_N <- 100
  
  baseline_object <- load_or_generate_baseline_object(dataset_name = dataset_name, 
                                                      baseline_directory_path = baseline_directory_path,
                                                      real_data = real_data, 
                                                      all_outcomes = all_outcomes, 
                                                      covariates_names = covariates_names, 
                                                      treatment_var = treatment_var, 
                                                      baseline_seed = baseline_seed, 
                                                      baseline_N = baseline_N, 
                                                      force_generate = TRUE )
  
  # We don't need much if we know the baseline data already exists
  baseline_object <- load_or_generate_baseline_object(dataset_name = dataset_name, 
                                                      baseline_directory_path = baseline_directory_path )
  
}

