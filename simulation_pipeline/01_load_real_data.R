###################################################################################
#                                                                                                        
#                                                                         
# Created on:   11/06/24
# Purpose:      Load and validate any real csv dataset 
# Authors:      Luke Miratrix, Polina Polskaia, Nick Commins
# 
#
###################################################################################


###################################################################################
# DATA LOADING RECOMMENDATIONS ----
###################################################################################


# The load_and_validate_data function allows you to load any data that is in the .csv 
# format. This function is designed to help you easily import your datasets and ensure 
# that the data is properly loaded into your R environment.

# Loading Example Data:

# You can load example data provided in the /datasets folder. For instance, to load 
# a multivariate normally distributed (MND) dataset, you can use the following command:

# data <- load_and_validate_data(file_path = here::here("datasets/mnd.csv"))

# Loading Your Own Data

# If you are loading your own dataset, ensure that it is clean and properly formatted. 
# Clean data should be free of errors, missing values, and inconsistencies. For detailed 
# steps on cleaning your data, please refer to the pipeline_documentation.pdf file 
# available in the repository.

# Here is an example of how to load your own dataset:

# my_data <- load_and_validate_data(file_path = here::here("path/to/your/dataset.csv"))

###################################################################################
# STEP 0: SET UP ----
###################################################################################


# Load packages, functions, paths, and variable names
source(here::here("simulation_pipeline/00_setup.R"))

###################################################################################
# STEP 1: DEFINE THE FUNCTION TO LOAD AND VALIDATE REAL DATA ----
###################################################################################


#' load_and_validate_real_data:: Load and Validate Real Data
#'
#' This function loads data from a CSV file if a file path is provided
#' or validates that data is already loaded. Additionally, it assigns
#' all character columns to factors.
#'
#' @param real_data EITHER A dataframe containing the data OR A string
#'   representing the file path to a CSV file to load.
#'
#' @return A dataframe containing the loaded data.
#'
#' @examples
#' \dontrun{
#' # Load data from a CSV file
#' your_data <- load_and_validate_real_data(real_data = "path/to/your/file.csv")
#' mnd_data <- load_and_validate_real_data(real_data = here::here("datasets/mnd.csv") )
#'
#' # Validate an existing dataframe
#' existing_data <- data.frame(A = c(1, 2, 3), B = c(4, 5, 6))
#' validated_data <- load_and_validate_real_data(real_data = existing_data)
#' }
#'
#' @export
#' 
load_and_validate_real_data <- function(real_data) {
  
  if (is.character(real_data)) {
    real_data <- read.csv(real_data)  # Adjust this depending on your data file type
  }
  
  # Check if real data is loaded
  if (is.null(real_data)) {
    stop(stringr::string_wrap( "No real data provided. Please load real data to proceed. To load real data either provide your dataset name or specify the path in real_data argument of the function" ) )
  }
 
  # Recode character columns as factors
  real_data <- tryCatch({
    real_data %>% dplyr::mutate_if(is.character, as.factor)
  }, error = function(e) {
    stop(paste("Error mutating character columns in real data to factors:", e$message, "\n"))
  })
  
  return(real_data)
}

###################################################################################
# TEST BLOCK
###################################################################################


if (FALSE) {
  # Use case 1
  mnd_data <- load_and_validate_real_data(real_data = here::here("datasets/mnd.csv"))
  print(head(mnd_data))  
  
  # Use case 2
  mnd_data_copy <- load_and_validate_real_data(real_data = mnd_data)
  print(head(mnd_data_copy))  
  identical(mnd_data, mnd_data_copy)
}
