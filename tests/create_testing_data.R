
# Load MND testing data, with a continuous and a binary outcome, into memory.

# NOTE: 
# This will load cached data if it exists. If you want to force
# regeneration, set force_generate = TRUE.

FORCE_REGENERATE <- FALSE
VERBOSE <- FALSE

source( here::here( "simulation_pipeline/04_add_IATE_to_baseline.R" ) )

# Load real data 

mnd_data <- load_and_validate_real_data( real_data = here::here("datasets/mnd.csv"))


# Make baseline dataset
mnd_baseline <- load_or_generate_baseline_object(dataset_name = "mnd", 
                                                 baseline_directory_path = here::here("baseline"),
                                                 real_data = mnd_data, 
                                                 all_outcomes = c( "Y_continuous", "Y_binary"), 
                                                 covariates_names = paste0( "X", 1:9 ) , 
                                                 treatment_var = "Z", 
                                                 baseline_seed = 68814,
                                                 baseline_N = 10000,
                                                 verbose = 0,
                                                 force_generate = FORCE_REGENERATE )
mnd_baseline_data <- mnd_baseline$Data
rm( mnd_baseline )

# Add treatment indicator
mnd_baseline_data$Z = 0 + (sample( nrow(mnd_baseline_data) ) <= nrow(mnd_baseline_data)*0.3)


# Load IATEs (binary)
iate_bin <- load_or_generate_iates( dataset_name = "mnd", 
                                    real_data = mnd_data, 
                                    queen_list = c("ATE", "OLS S"), 
                                    queen = "OLS S",
                                    baseline_seed = 68814, 
                                    baseline_data = mnd_baseline_data, 
                                    small_covariate_set = paste0("X", 1:9), 
                                    outcome = "Y_binary",
                                    treatment_var = "Z", 
                                    sigma_tau = 0.2, 
                                    ate_real = NULL, 
                                    sd_y0_real = NULL, 
                                    verbose = 0,
                                    force_regenerate = FORCE_REGENERATE )


mnd_baseline_data <- add_treatment_effects( mnd_baseline_data, 
                                            iate_bin, 
                                            "Y_binary", 
                                            treatment = "Z", 
                                            binary_outcome = TRUE,
                                            verbose = 0 )
rm( iate_bin )


# Now continuous
iate_cont <- load_or_generate_iates( dataset_name = "mnd", 
                                    real_data = mnd_data, 
                                    queen_list = c("ATE", "OLS S"), 
                                    queen = "OLS S",
                                    baseline_seed = 68814, 
                                    baseline_data = mnd_baseline_data, 
                                    small_covariate_set = paste0("X", 1:9), 
                                    outcome = "Y_continuous",
                                    treatment_var = "Z", 
                                    sigma_tau = 0.2, 
                                    ate_real = NULL, 
                                    sd_y0_real = NULL, 
                                    verbose = 0,
                                    force_regenerate = FORCE_REGENERATE )


mnd_baseline_data <- add_treatment_effects( mnd_baseline_data, 
                                            iate_cont, 
                                            "Y_continuous", 
                                            treatment = "Z", 
                                            binary_outcome = FALSE,
                                            verbose = 0 )


# Clean up extra variables
rm( iate_cont )
rm( mnd_file_name )
rm( FORCE_REGENERATE )
rm( true_iates_directory_path )
rm( datasets_directory_path, baseline_directory_path)



# Make some data ready for the machine learners so we can test those ----

if ( TRUE ) {
  ntrain = 600
  
  # Separate data into matrices
  Xs <- make_x_matrix( data = mnd_baseline_data, 
                       covariates = paste0("X", 2:7) )
  
  
  # Extract the baseline data.
  x_val <- Xs[ -c(1:ntrain), ]
  
  # Check if baseline data exists
  if (is.null(x_val)) {
    stop("No baseline data exists for the selected dataset.")
  }
  
  # Extract train data's covariates (train data is our real data)
  x_tr <- Xs[ 1:ntrain, ]
  # Train data, treat assignment
  treatment <- "Z"
  d_tr <- mnd_baseline_data[ 1:ntrain, treatment]
  
  # Train data, observed outcome
  y_tr <- mnd_baseline_data[ 1:ntrain, "Y_binary" ]
  tau_tr <- mnd_baseline_data[ 1:ntrain, "Y_binary_tau" ]
  
  nval = nrow(x_val) 
  if (is.numeric(y_tr)) {
    y_tr <- as.matrix(y_tr)
  }
  rm( Xs )  
}

