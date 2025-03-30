

VERBOSE <- FALSE
# cat( "Set verbose", VERBOSE, "\n" )

source( here::here( "simulation_pipeline/05_generate_predicted_IATE.R" ) )



test_that("generate predicted IATE", {
  
  
  # Load data 
  mnd_data <- load_and_validate_real_data( real_data = here::here("datasets/mnd.csv"))
  mnd_baseline <- load_or_generate_baseline_object(dataset_name = "mnd", 
                                                   baseline_directory_path = here::here("baseline"),
                                                   real_data = mnd_data, 
                                                   all_outcomes = c( "Y_continuous", "Y_binary"), 
                                                   covariates_names = paste0( "X", 1:9 ) , 
                                                   treatment_var = "Z",
                                                   baseline_seed = 68814,
                                                   baseline_N = 100000)
  mnd_baseline_data <- mnd_baseline$Data
  
  # Separate data into matrices
  Xs <- make_x_matrix(data = mnd_baseline_data, covariates = paste0("X", 1:9),
                      data_train = mnd_data)
  
  # Extract the baseline data.
  x_val <- Xs$X_val
  
  expect_true( !is.null( x_val ) )
  expect_true( is.matrix(x_val) )
  
  
  # Extract train data's covariates (train data is our real data)
  x_tr <- Xs$X_tr
  
  # Train data, treat assignment
  treatment <- "Z"
  d_tr <- mnd_data[ , treatment]
  
  # Train data, observed outcome
  y_tr <- mnd_data[ , "Y_continuous"]
  expect_is( y_tr, "numeric" )
  
  y_tr <- as.matrix(y_tr)
  
  # Determine what is the number of rows in the train set
  ntrain <- length(y_tr)
  
  # Load IATEs
  load_mnd_iates <- load_or_generate_iates(dataset_name = "mnd", 
                                           true_iates_directory_path = here::here("true_iates"), 
                                           real_data = mnd_data, 
                                           queen_list = c("ATE", "OLS S"), 
                                           baseline_seed = 68814, 
                                           baseline_data = mnd_baseline_data, 
                                           small_covariate_set = paste0("X", 1:9), 
                                           outcome = "Y_continuous",
                                           treatment_var = "Z", 
                                           sigma_tau = 0.2, 
                                           ate_real = NULL, 
                                           sd_y0_real = NULL, 
                                           verbose = VERBOSE)
  
  # Select one IATE
  tau_tr <- load_mnd_iates["OLS S"]
  tau_tr <- tau_tr$`OLS S`[1:1000]
  
  # Make sure that data types are correct
  expect_true(is.matrix(x_val))
  expect_true(length(d_tr) == ntrain)
  expect_true(is.matrix(x_tr))
  expect_true(nrow(x_tr) == ntrain )
  expect_true(ncol(x_val) == ncol(x_tr))
  
  # The number of obs in our target data
  nval = nrow(x_val) 
  
  # Generate predicted IATEs
  test <- generate_predicted_IATE(y_tr, 
                                  d_tr,
                                  x_tr, 
                                  tau_tr, 
                                  x_val,
                                  verbose = VERBOSE,
                                  model_list = model_queen_lists(
                                    include_RF = FALSE,
                                    include_LASSO = TRUE,
                                    include_SL_S = FALSE,
                                    include_SL_T = FALSE,
                                    include_CDML = FALSE,
                                    include_XGBOOST = FALSE,
                                    include_BART = FALSE, 
                                    make_queen_list = FALSE ),
                                  sd_y0_real = 40, 
                                  ate_real = 400) 
  
  
  expect_is( test, "list" )
  expect_true( all( c( "iate", "runtime", "fail_rate", "warnings" ) %in% names(test) ) )
  expect_true( length(test) == 4 )
  expect_true( is.matrix(test$iate) )
  expect_true( nrow(test$iate) == nval )
  
  expect_equal( dim( test$iate ), dim( test$runtime ) )
  expect_equal( dim( test$iate ), dim( test$fail_rate ) )
  
  expect_true( all( !is.na( test$iate[,"OLS S"] ) ) )
  expect_equal( sd( test$iate[,"ATE"]), 0 )
  
  iate = test$iate
  dim( iate )
  expect_true( all( !is.na( iate[,"ATE"] ) ) )
  #expect_true( all( is.na( iate[,"XBART"] ) ) )
  #expect_true( all( is.na( iate[,"RF INF"] ) ) )
  #expect_true( all( is.na( iate[,"CDML"] ) ) )
  #expect_true( all( is.na( iate[,"CF"] ) ) )
  
  
} )


test_that( "further tests of core estimation", {
  
  source( here::here( "tests/create_testing_data.R" ) )
  tau_tr = rep( mean( tau_tr ), length(tau_tr) )
  
  # LASSO INF and a nonvarying treatment effect
  test <- generate_predicted_IATE(y_tr, 
                                  d_tr,
                                  x_tr, 
                                  tau_tr, 
                                  x_val,
                                  verbose = VERBOSE,
                                  model_list = c( "LASSO INF" ),
                                  sd_y0_real = 40, 
                                  ate_real = 400) 

  expect_true( test$warnings )
  
  
} )



