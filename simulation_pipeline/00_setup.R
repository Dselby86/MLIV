###################################################################################
#            
#                                                                         
# Created on:   06/29/2024
# Purpose:      Loading packages and functions, setting up directories
# Authors:      Luke Miratrix, Polina Polskaia, Nick Commins
# 
#
###################################################################################


###################################################################################
# STEP 0: LOAD PACKAGES ----
###################################################################################


#' Install and Load R Packages
#'
#' This function checks if a specified R package is installed. If the package is 
#' not installed, it installs the package. 
#'
#' @param package A character string specifying the name of the package to check, install if necessary, and load.
#' 
#' @return No return value, called for side effects.
#' 
#' @examples
#' \dontrun{
#'   install_if_missing("dplyr")
#'   install_if_missing("ggplot2")
#' }
#' @export
#' 
install_if_missing <- function(package) {
  if (!require(package, character.only = TRUE)) {
    message(paste("Installing", package, "..."))
    install.packages(package)
    message(paste(package, "installed successfully."))
  } 
}

# Ensure the 'here' package is installed
install_if_missing("here")

# Load the 'here' package
library(here)

# Load all required packages
source(here::here("R/packages.R"))

# options(error = browser)

###################################################################################
# STEP 1: LOAD ALGORITHMS ----
###################################################################################


# Specify all available algorithms
ALL_MODELS <- c("ATE", "OLS S", "RF INF","RF T","RF MOM IPW","RF MOM DR",
                "CF","CF LC","CDML","LASSO INF",
                "LASSO T","LASSO MOM IPW","LASSO MOM DR", "LASSO MCM","LASSO MCM EA",
                "LASSO R", "SL T", "SL S", "XGBOOST S", "XGBOOST R", "BART T", "BART S")


# Load all available algorithms by sourcing all files in algorithms folder
purrr::walk(
  list.files(here::here("algorithms"), full.names = TRUE, pattern = "R$"),
  source
)

###################################################################################
# STEP 2: LOAD HELPER FUNCTIONS ----
###################################################################################


# Load all other helper functions by sourcing all files in
# 'R/functions' folder
purrr::walk(
  list.files(here::here("R", "functions"), full.names = TRUE, pattern = "R$"),
  source
)

# Create functions that specify how many messages are displayed
if (!exists("VERBOSE")) {
  VERBOSE <- FALSE
}

scat <- function(...) {
  if (VERBOSE) {
    cat(...)
  }
}

###################################################################################
# STEP 3: CREATE NEEDED DIRECTORIES ----
###################################################################################


# Create the baseline directory path
baseline_directory_path <- here::here("baseline")

if (!file.exists(baseline_directory_path)) {
  # Create the directory if it does not exist
  dir.create(baseline_directory_path, recursive = TRUE)
  scat("Baseline directory created:", baseline_directory_path, "\n")
} else {
  scat("Baseline path:", baseline_directory_path, "\n")
}

# Construct the directory path to save true IATEs
true_iates_directory_path <- here::here("true_iates")

if (!file.exists(true_iates_directory_path)) {
  # Create the directory if it does not exist
  dir.create(true_iates_directory_path, recursive = TRUE)
  scat("True IATE Directory created:", true_iates_directory_path, "\n")
} else {
  scat("True IATE path:", true_iates_directory_path, "\n")
}


###################################################################################
# STEP 4: CREATE MND DATA ----
###################################################################################

# Create mnd data path
datasets_directory_path <- here::here("datasets")

mnd_file_name <- paste(datasets_directory_path, 
                    "mnd.csv", sep = "/")

# Generate and save the mnd data if not saved
if (!file.exists(mnd_file_name)) {

  # Generate mnd data
  mnd_subset_object <- generate_mnd_data(n_seed = 5147, N = 1000, exclude_Y0_Y1 = TRUE) 
  
  # Save mnd dataset as a dataframe
  mnd_data <- as.data.frame(mnd_subset_object$new_data) 
  
  write.csv(mnd_data, file = mnd_file_name, row.names = FALSE)
  scat("mnd data generated and saved in:", mnd_file_name, "\n")
} else {
  scat("Using mnd data at:", mnd_file_name, "\n")
}

