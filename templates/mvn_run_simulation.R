###################################################################################
#                                                                                                        
#                                                                         
# Created on:   01/23/2025
# Purpose:      Template for running MVN (fake) simulations
# Authors:      Luke Miratrix, Polina Polskaia, Nick Commins
# 
#
###################################################################################


source(here::here("simulation_pipeline/08_run_simulation.R"))

mvn_small_set <-  c( "X2", "X3", "X4" )

models = c( "OLS S", "LASSO INF", "RF INF", "LASSO T" )

mvn_simulation <- run_simulation(
  # Required arguments
  dataset_name = "mvn",    
  covariates = mvn_small_set, 
  outcome = "Y_continuous",
  treatment = "Z",
  
  # Optional arguments
  S = 3,  
  queen_list = c( "ATE", "OLS S" ),
  model_list = models,
  
  size_train = 1000,
  size_test = 1000, 
  
  PARALLEL = TRUE,
  verbose = 1000,
  
  master_seed = 343434,
  simulation_name = "simulation1" 
)

table( mvn_simulation$queen )

mvn_simulation


# Run a second simulation with an additional queen and model.
#
# We could pass real_data to the run_simulation function if we want to
# make a new IATE on the fly (if we added a queen that we didn't
# precompute)
#
# Note we can also change number of iterations if we want.
mvn_simulation_2 <- run_simulation(
  # Required arguments
  dataset_name = "mvn",    
  covariates = mvn_small_set, 
  outcome = "Y_continuous",
  treatment = "Z",
  
  # Optional arguments
  S = 5,  
  queen_list = c( "OLS S", "RF T" ),
  model_list = c( "LASSO T"),
  
  size_train = 1000,
  size_test = 1000, 
  
  PARALLEL = FALSE,
  verbose = 1000,
  
  master_seed = 343434,
  simulation_name = "simulation2" 
)





