###################################################################################
#                                                                                                        
#                                                                         
# Created on:   01/23/2025
# Purpose:      Template for setting up the MVN (fake) dataset for future simulations
# Authors:      Luke Miratrix, Polina Polskaia, Nick Commins
# 
#
###################################################################################


###################################################################################
# STEP 0: SET UP 
###################################################################################


# Load script 08
source(here::here("simulation_pipeline/08_run_simulation.R"))

###################################################################################
# STEP 1: CLEAN DATA
###################################################################################


# Load data
mvn_subset_real <- read_csv( here::here( "datasets/mnd.csv" ) )

colnames(mvn_subset_real)

# Create covariate sets
mvn_small_set <-  c( "X2", "X3", "X4" )

mvn_medium_set <- c( mvn_small_set, "X5", "X6" )

mvn_large_set <- paste0( "X", 1:9 )


# Specify treatment variable
mvn_treatment <- "Z"

# Specify outcomes:
mvn_outcomes <- c( "Y_continuous",
                   "Y_binary" )


sum( is.na( mvn_subset_real ) )



###################################################################################
# STEP 5: CONVERT CATEGORICAL VARIABLES TO FACTORS
###################################################################################


# Convert categorical vars to factors
mvn_subset_real$X1 <- as.factor(mvn_subset_real$X1)



###################################################################################
# STEP 6: RUN THE SIMULATION
###################################################################################

my_queens <- c("ATE",
               "OLS S",
               "RF T",
               "RF MOM IPW",
               "CF" )

# Call run_simulation to automatically set up all the IATEs and whatnot.
mvn_simulation <- run_simulation(
  # Required arguments
  real_data = mvn_subset_real, 
  dataset_name = "mvn",    
  covariates = mvn_small_set, 
  outcome = "Y_continuous",
  treatment = mvn_treatment,
  
  # Optional arguments
  large_covariate_set = mvn_large_set, 
  small_covariate_set = mvn_small_set, 
  all_outcomes = mvn_outcomes,
  S = 0, # don't actually run any simulations
  queen_list = my_queens,
  p_tx = NULL,
  size_train = 1000,
  size_test = 100, 
  PARALLEL = TRUE,
  verbose = 1000,
  include_LASSO = FALSE,
  include_RF = FALSE,
  include_SL_S = FALSE,
  include_SL_T = FALSE,
  include_CDML = FALSE,
  include_XGBOOST = FALSE,
  include_BART = FALSE,
  master_seed = 53343,
  baseline_seed = 68814, 
  baseline_N = 100000, 
  baseline_directory_path =  here::here("baseline"),
  true_iates_directory_path = here::here("true_iates"),
  simulation_name = "setup"
)

