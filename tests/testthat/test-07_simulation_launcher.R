

VERBOSE <- FALSE
source( here::here( "simulation_pipeline/07_simulation_launcher.R" ) )

test_that("simulation launcher", {
  
  mnd_data <- load_and_validate_real_data(real_data = here::here("datasets/mnd.csv"))
  
  test <- perform_simulation_launcher(
    # Required arguments
    real_data = mnd_data, 
    dataset_name =  "mnd_test",    
    covariates = paste0( "X", 2:8 ),
    outcome = "Y_continuous",
    treatment = "Z",
    
    # Optional arguments
    large_covariate_set = paste0( "X", 1:9 ), 
    small_covariate_set = paste0( "X", 2:5 ), 
    all_outcomes = c("Y_continuous", "Y_binary"),
    S = 12,
    chunk_size = 3,
    queen_list = c( "ATE", "OLS S"),
    p_tx = NULL,
    size_train = 1000,
    size_test = 2000,
    PARALLEL = FALSE,
    include_LASSO = TRUE,
    include_RF = FALSE,
    include_SL_S = FALSE,
    include_SL_T = FALSE,
    include_CDML = FALSE,
    include_XGBOOST = FALSE,
    include_BART = FALSE,
    baseline_seed = 68814, 
    master_seed = 4233434,
    baseline_N = 10000, 
    baseline_directory_path = here::here("baseline"),
    true_iates_directory_path = here::here("true_iates"),
    simulation_name = "testthat",
    verbose = VERBOSE )

  expect_equal( test, 0 )

} )


