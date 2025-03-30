


VERBOSE <- FALSE

# Load some data to run tests on
source( here::here( "simulation_pipeline/04_add_IATE_to_baseline.R" ) )

# Load baseline data
mnd_baseline <- load_or_generate_baseline_object(dataset_name = "mnd",
                                                 baseline_directory_path = here::here("baseline"),
                                                 real_data = mnd_data, 
                                                 all_outcomes = c( "Y_continuous", "Y_binary"), 
                                                 covariates_names = paste0( "X", 1:9 ) , 
                                                 treatment_var = "Z",
                                                 baseline_seed = 68814, 
                                                 baseline_N = 100000)
mnd_baseline_data <- mnd_baseline$Data

# load real data
mnd_data <- load_and_validate_real_data( real_data = here::here("datasets/mnd.csv"))




test_that("add_IATE_to_baseline", {
  
  
  # Load IATES
  load_mnd_iates <- load_or_generate_iates(dataset_name = "mnd", 
                                           true_iates_directory_path = here::here("true_iates"), 
                                           real_data = mnd_data, 
                                           queen_list = c("LASSO R", "OLS S"), 
                                           baseline_seed = 68814, 
                                           baseline_data = mnd_baseline_data, 
                                           small_covariate_set = paste0("X", 1:9), 
                                           outcome = "Y_continuous",
                                           treatment_var = "Z", 
                                           sigma_tau = 0.2, 
                                           ate_real = NULL, 
                                           sd_y0_real = NULL, 
                                           force_regenerate=TRUE, verbose = FALSE )
  
  
  # Select one queen
  IATE <- load_mnd_iates[["LASSO R"]]
  expect_is( IATE, "numeric" )
  expect_equal( length(IATE), nrow(mnd_baseline_data) )
  
  
  # Add treatment indicator to baseline
  nval <- nrow(mnd_baseline_data)
  mnd_baseline_data$Z <- as.numeric(sample(nval) <= 0.5 * nval) 
  
  # Add treatment effects
  b_full <- add_treatment_effects( baseline = mnd_baseline_data,
                                   IATE = IATE,
                                   outcome = "Y_continuous",
                                   treatment = "Z",
                                   binary_outcome = FALSE )
  
  expect_is( b_full, "data.frame" )
  expect_true( all( c( "Y_continuous_tau", "Y_continuous_Y0", "Y_continuous_Y1" ) %in% names(b_full) ) )
  
  expect_equal( b_full$Y_continuous_Y1 - b_full$Y_continuous_Y0- b_full$Y_continuous_tau,
                rep( 0, nrow(b_full) ) )
  
} )




test_that( "binary treatment effects works", {
  
  
  # Force some negative treatment effects by setting tx outcomes to 0 (hack)
  mnd_data$Y_binary[ mnd_data$Z & (1:nrow(mnd_data)) < 400 ] = 0
  
  tt2 = generate_true_IATEs(
    baseline_seed = 68814,
    baseline_data = mnd_baseline_data,
    small_covariate_set = paste0("X", 1:9),
    outcome = "Y_binary",
    treatment_var = "Z",
    real_data = mnd_data,
    queen = "OLS S",
    sigma_tau = 0.2,
    sd_y0_real = 2,
    ate_real = 0, verbose = FALSE
  )
  summary( tt2 )
  expect_true( all( tt2 >= -1 ) && all( tt2 <= 1 ) )
  head( tt2 )
  
  # Are the treatment effects all bounded correctly?
  expect_true( all( mnd_baseline_data$Y_binary_Y0 >= 0 ) )
  expect_true( all( mnd_baseline_data$Y_binary_Y0 <= 1 ) )
  expect_true( all( mnd_baseline_data$Y_binary_Y0 + tt2 <= 1 ) )
  
  tt2 <- add_treatment_effects( mnd_baseline_data, tt2, 
                                outcome = "Y_binary",
                                binary_outcome = TRUE )
  head( tt2 )
  
  tb <- table( tt2$Y_binary_Y0, tt2$Y_binary_Y1 )
  tb
  expect_equal( dim( tb ), c(2,2) )
  
  # Range of treatment impacts
  summary( tt2$Y_binary_tau )
  tb = table( tt2$Y_binary_Y1 - tt2$Y_binary_Y0 )
  tb
  expect_true( length(tb) == 3 )
  mn = mean( tt2$Y_binary_Y1 - tt2$Y_binary_Y0 )
  mn2 = mean( tt2$Y_binary_tau )
  mn2 - mn
  
  expect_true( abs( mn - mn2 ) < 0.001 )
  
  
  tt <- generate_true_IATEs(  baseline_seed = 68814,
                                              baseline_data = mnd_baseline_data,
                                              small_covariate_set = paste0("X", 1:9),
                                              outcome = "Y_continuous",
                                              treatment_var = "Z",
                                              real_data = mnd_data,
                                              queen = "ATE",
                                              sigma_tau = 0.2,
                                              sd_y0_real = 2,
                                              ate_real = 0, verbose = FALSE )
  
  expect_equal( sd( tt ), 0 )
  
})



#### Testing RF and Lasso to make treatment effects ####


test_that( "ML generation of IATE works", { 
  
  # Try actual machine learner to make treatment effects (takes awhile)
  tt = generate_true_IATEs(  baseline_seed = 68814,
                             baseline_data = mnd_baseline_data,
                             small_covariate_set = paste0("X", 2:9),
                             outcome = "Y_continuous",
                             treatment_var = "Z",
                             real_data = mnd_data,
                             queen = "RF T",
                             sigma_tau = 0.2,
                             sd_y0_real = 2,
                             ate_real = 0, verbose = FALSE )
  
  expect_equal( sd( tt ) / 2, 0.2 )
  
  tt = generate_true_IATEs(  baseline_seed = 68814,
                             baseline_data = mnd_baseline_data,
                             small_covariate_set = paste0("X", 2:8),
                             outcome = "Y_binary",
                             treatment_var = "Z",
                             real_data = mnd_data,
                             queen = "LASSO MCM EA",
                             sigma_tau = 0.2,
                             sd_y0_real = 2,
                             ate_real = 0, verbose = FALSE )  
  expect_true( sd( tt ) > 0 )
  summary( tt )  
  expect_true( all( tt >= -1 & tt <= 1 ) )
  
} )


