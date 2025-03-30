###################################################################################
#                                                                                                        
#                                                                         
# Created on:   07/02/2024
# Purpose:      Template for calling the simulation with mnd data
# Authors:      Polina Polskaia
# 
#
###################################################################################


###################################################################################
# STEP 0: SET UP 
###################################################################################


# Load script 08
source(here::here("simulation_pipeline/08_run_simulation.R"))

###################################################################################
# STEP 1: LOAD MND DATA ----
###################################################################################


# Load mnd data
mnd_data <- load_and_validate_real_data(realdata_file_path = here::here("datasets/mnd.csv"))

###################################################################################
# STEP 2: SPECIFY MND VARIABLES ----
###################################################################################


# Specify covariate sets
mnd_small_set <- c("X1", "X2", "X3")
mnd_medium_set <- c("X1", "X2", "X3", "X4", "X5")
mnd_large_set <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9")

# Specify treatment variable
mnd_treatment <- "Z"

# Specify outcomes
mnd_outcomes <- c("Y_continuous", "Y_binary")

###################################################################################
# STEP 3: RUN THE SIMULATION WITH MND DATA
###################################################################################


start_time <- Sys.time()

mnd_simulation <- run_simulation(
                                 # Required arguments
                                 real_data = mnd_data, 
                                 realdata_file_path = NULL,
                                 dataset_name = "mnd",    
                                 covariates = mnd_medium_set,
                                 outcome = "Y_binary",
                                 treatment = mnd_treatment,
                                  
                                 # Optional arguments
                                 large_covariate_set = mnd_large_set, 
                                 small_covariate_set = mnd_small_set, 
                                 all_outcomes = mnd_outcomes,
                                 S = 11,  
                                 queen_list = NULL,
                                 p_tx = NULL,
                                 size_train = 2000,
                                 size_test = 10000,
                                 PARALLEL = FALSE,
                                 verbose = TRUE,
                                 include_LASSO = FALSE,
                                 include_RF = FALSE,
                                 include_SL = FALSE,
                                 include_CDML = FALSE,
                                 include_XGBOOST = FALSE,
                                 include_BART = FALSE,
                                 master_seed = NULL,
                                 baseline_seed = 68814, 
                                 baseline_N = 100000, 
                                 baseline_directory_path =  here::here("baseline"),
                                 true_iates_directory_path = here::here("true_iates"),
                                 today = Sys.time() )


end_time <- Sys.time()
total_time <- end_time - start_time
print(total_time)  






