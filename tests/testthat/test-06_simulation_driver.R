

# NOTE: This test file actually runs all the MLs (other than
# superlearner) and thus is kind of slow, even on a small dataset.



VERBOSE <- FALSE

source( here::here( "simulation_pipeline/06_simulation_driver.R" ) )



# Load and prep some testing data
source( here::here( "tests/create_testing_data.R" ) )

b_full = mnd_baseline_data[ 1:20000, ]

# How big our mighty test set?
nrow( x_tr )

test_that("simulation driver", {
  
  # Add treatment to baseline:
  nval <- nrow(b_full)
  
  # Load IATES
  load_mnd_iates <- load_or_generate_iates(dataset_name = "mnd_small", 
                                           true_iates_directory_path = here::here("true_iates"), 
                                           real_data = mnd_data, 
                                           queen_list = c("ATE", "OLS S"), 
                                           baseline_seed = 68814, 
                                           baseline_data = b_full, 
                                           small_covariate_set = paste0("X", 1:9), 
                                           outcome = "Y_continuous",
                                           treatment_var = "Z", 
                                           sigma_tau = 0.2, 
                                           ate_real = NULL, 
                                           sd_y0_real = NULL, verbose = 0)
  
  # Select one queen
  IATE <- load_mnd_iates[["OLS S"]]
  
  # Generate treatment variable
  b_full$Z = as.numeric(  sample( nrow(b_full) ) <= nrow(b_full) * 0.4 )
  
  # Add treatment effects
  b_full <- add_treatment_effects(baseline = b_full,
                                  IATE = IATE,
                                  outcome = "Y_continuous",
                                  treatment = "Z",
                                  binary_outcome = FALSE )
  
  
  stopifnot(nrow(b_full) == nrow(IATE))
  
  
  # Select observations for test set
  test_set <- b_full[1:1000, ]
  baseline_remainder <- b_full[-(1:1000), ]
  
  # Set seeds and iteration numbers
  seeds <- c(505315, 784017, 338579)
  iters <- c(1, 2, 3)
  
  # Generate SD of Y0 and ATE

  test <- perform_simulation(start_iteration = min(iters), 
                             end_iteration = max(iters),
                             test_set, 
                             baseline_remainder,
                             size_train = 1000, 
                             seeds,
                             p_tx = NULL, 
                             outcome = "Y_continuous",
                             treatment = "Z",
                             covariates = paste0("X", 2:9),
                             verbose = VERBOSE,
                             model_list = model_queen_lists( 
                               include_LASSO = TRUE,
                               include_RF = FALSE,
                               include_SL_S = FALSE,
                               include_SL_T = FALSE,
                               include_CDML = FALSE,
                               include_XGBOOST = FALSE,
                               include_BART = FALSE,
                               make_queen_list = FALSE )
  )
  
  expect_equal( dim( test$predictedtau ), c( 3, 1000, 9 ) )
  
  sing_run <- test$predictedtau[1,1,]
  expect_true( all( c( "ATE", "OLS S", "LASSO MCM" ) %in% names( sing_run ) ) )
  
} )







test_that("all machine learners work on continuous", {
  
  # Add treatment to baseline:
  nval <- nrow(b_full)
  
  IATE <- rnorm( nrow( b_full ) )
  
  # Generate treatment variable
  b_full$Z = as.numeric(  sample( nrow(b_full) ) <= nrow(b_full) * 0.4 )
  
  # Add treatment effects
  b_full <- add_treatment_effects(baseline = b_full,
                                  IATE = IATE,
                                  outcome = "Y_continuous",
                                  treatment = "Z",
                                  binary_outcome = FALSE )
  
  
  stopifnot(nrow(b_full) == nrow(IATE))
  
  
  # Select observations for test set
  test_set <- b_full[1:600, ]
  train_set <- b_full[1001:1590, ]
  nrow( train_set )
  
  tt = tictoc::tic()
  VERBOSE = 0
  
  ml <- model_queen_lists(
    include_LASSO = TRUE,
    include_RF = TRUE,
    include_SL_S = FALSE,
    include_SL_T = FALSE,
    include_CDML = TRUE,
    include_XGBOOST = TRUE,
    include_BART = TRUE,
    make_queen_list = FALSE )
  
  test <- apply_estimators(  test_set, 
                             train_set = train_set,
                             outcome = "Y_continuous",
                             treatment = "Z",
                             covariates = paste0("X", 2:3),
                             verbose = VERBOSE,
                             model_list = ml, 
                             current_seed = 422424 )
  tt2 = tictoc::toc()
  
  expect_is( test, "list" )
  dd = test[[1]]
  dim(dd)
  expect_equal( dim( dd ), c( 600, length(ml) ) )
  dd2 = dd[2,]
  dd2  
  
  
  # Only two superlearners are skipped.
  #expect_true( sum( is.na( dd ) ) == 2 * nrow(dd) )
  
  sds <- apply( dd, 2, sd )
  sds
  
  # ATE at least should have no variation
  expect_true( sum( sds == 0, na.rm=TRUE ) > 0 )
})





test_that( "all machine learners work on binary", {
  
  
  # Generate treatment variable
  b_full$Z = as.numeric(  sample( nrow(b_full) ) <= nrow(b_full) * 0.5 )
  
  head( b_full )
  expect_true( all( b_full$Y_binary_Y1 >= 0 ) )
  expect_true( all( b_full$Y_binary_Y0 >= 0 ) )
  expect_true( all( b_full$Y_binary_Y1 <= 1 ) )
  expect_true( all( b_full$Y_binary_Y0 <= 1 ) )
  expect_true( all( b_full$Y_binary_tau >= -1 ) )
  expect_true( all( b_full$Y_binary_tau <= 1 ) )
  
  
  # Select observations for test set
  test_set <- b_full[1:650, ]
  train_set <- b_full[1001:1550, ]
  
  tt = tictoc::tic()
  ml <- model_queen_lists(
    include_LASSO = TRUE,
    include_RF = TRUE,
    include_SL_S = TRUE,
    include_SL_T = TRUE,
    include_CDML = TRUE,
    include_XGBOOST = TRUE,
    include_BART = TRUE,
    make_queen_list = FALSE )
  
  test <- apply_estimators(  test_set, 
                             train_set = train_set,
                             outcome = "Y_binary",
                             treatment = "Z",
                             covariates = paste0("X", 8:9),
                             verbose = VERBOSE,
                             model_list = ml,
                             sd_y0_real = 1, 
                             ate_real = 0, 
                             current_seed = 43434 )
  tt2 = tictoc::toc()
  
  expect_is( test, "list" )
  dd = test[[1]]
  dim(dd)
  expect_equal( dim( dd ), c( nrow(test_set), length(ml) ) )
  dd2 = dd[2,]
  dd2  
})




