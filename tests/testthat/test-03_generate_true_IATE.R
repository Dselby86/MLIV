


VERBOSE <- FALSE

# Load some data to work with
source( here::here( "simulation_pipeline/03_generate_true_IATE.R"))


mnd_data <- load_and_validate_real_data( real_data = here::here("datasets/mnd.csv"))

mnd_baseline <- load_or_generate_baseline_object(
  dataset_name = "mnd",
  baseline_directory_path = here::here("baseline"),
  real_data = mnd_data,
  all_outcomes = c("Y_continuous", "Y_binary"),
  covariates_names = paste0("X", 1:9) ,
  treatment_var = "Z",
  baseline_seed = 68814,
  baseline_N = 100000, verbose = VERBOSE
)
mnd_base <- mnd_baseline$Data



test_that("generate true IATE", {
  
  
  
  ### test make_x_matrix ----
  
  #str( mnd_data )
  
  # Make sure we don't get factors appearing in test and not train, or
  # vice-versa
  mnd_data$X1 = as.character(mnd_data$X1)
  mnd_data$X1[1:20] = "newfac"
  
  Xs <- make_x_matrix(
    data = mnd_base,
    covariates = paste0("X", 1:9),
    data_train = mnd_data
  )
  
  expect_is(Xs, "list")
  expect_true( length(Xs) == 2 )
  
  expect_equal( ncol( Xs$X_tr ), ncol( Xs$X_val ) )
  expect_equal( nrow( Xs$X_tr ), 1000 )
  expect_true( all( Xs$X_val[, "X1newfac"] == 0 ) )
  expect_true( sum( Xs$X_tr[, "X1newfac"] ) == 20 )
  
  # Extract the baseline data.
  x_val <- Xs$X_val
  expect_true( !is.null(x_val) )
  expect_equal( nrow( x_val ), 100000 )
  
  
  # Extract train data's covariates (train data is our real data)
  x_tr <- Xs$X_tr
  
  # Train data, treat assignment
  treatment <- "Z"
  d_tr <- mnd_data[, treatment]
  
  # Train data, observed outcome
  y_tr <- mnd_data[, "Y_continuous"]
  expect_is( y_tr, "numeric")
  
  y_tr <- as.matrix(y_tr)
  
  ntrain <- length(y_tr)
  
  expect_true(is.matrix(x_val))
  expect_true(length(d_tr) == ntrain)
  expect_true(is.matrix(x_tr))
  expect_true(nrow(x_tr) == ntrain)
  expect_true(ncol(x_val) == ncol(x_tr))
  
 
  
  
  ### Test predict_true_IATEs ----
  
  # The number of obs in our target data
  nval = nrow(x_val)
  
  #cdmls <- predict_true_IATEs(x_tr, y_tr, d_tr, x_val, queen = "CDML")
  olss <- predict_true_IATEs(x_tr, y_tr, d_tr, x_val, queen = "OLS S")
  expect_true( length(olss) == nval )
} )





test_that( "DGP IATE generation works", {
  
  
  ### Test generate_true_IATEs ----
  
  sdY0 = sd( mnd_data$Y_continuous[ mnd_data$Z == 0 ] )
  ateReal = mean( mnd_data$Y_continuous[ mnd_data$Z == 1 ] ) - mean( mnd_data$Y_continuous[ mnd_data$Z == 0 ] )
  
  rfts = generate_true_IATEs(
    baseline_seed = 68814,
    baseline_data = mnd_base,
    small_covariate_set = paste0("X", 1:9),
    outcome = "Y_continuous",
    treatment_var = "Z",
    real_data = mnd_data,
    queen = "LASSO MCM EA",
    sigma_tau = 0.2,
    sd_y0_real = sdY0,
    ate_real = ateReal, verbose = VERBOSE
  )
  nval = nrow(mnd_base)
  
  expect_equal( dim( rfts ), c( nval, 1 ) )
  expect_equal( sdY0 * 0.2, sd( rfts ) )
  
  
  ### Test generate_IATE_dataset ----
  
  default_queen_list = c("OLS S",
                         "LASSO MCM EA",
                         "LASSO R")
  
  # NOTE: Warning from 0 variance in generated treatment effects
  mdn_iates <- generate_IATE_dataset(
    dataset_name = "test",
    real_data = mnd_data,
    queens = default_queen_list,
    baseline_seed = 68814,
    baseline_data = mnd_base,
    small_covariate_set = paste0("X", 1:9),
    outcome = "Y_continuous",
    treatment_var = "Z",
    sigma_tau = 0.2,
    ate_real = NULL,
    sd_y0_real = NULL,
    verbose = VERBOSE
  )
  expect_equal( dim( mdn_iates ), c( nval, 3 ) )
  
  
  ### Test load_or_generate_iates ----
  
  # Test function 4
  # Warning due to no variation in ATE
   mdn_iates2 <- load_or_generate_iates(
    dataset_name = "test",
    true_iates_directory_path = here::here("true_iates"),
    real_data = mnd_data,
    queen_list = c("ATE", "OLS S"),
    baseline_seed = 68814,
    baseline_data = mnd_base,
    small_covariate_set = paste0("X", 1:9),
    outcome = "Y_continuous",
    treatment_var = "Z",
    sigma_tau = 0.2,
    ate_real = NULL,
    sd_y0_real = NULL, force_regenerate = TRUE,
    verbose = VERBOSE
  )
  
  mdn_iates3 <- load_or_generate_iates(
    dataset_name = "test",
    true_iates_directory_path = here::here("true_iates"),
    real_data = mnd_data,
    queen_list = c("ATE", "OLS S"),
    baseline_seed = 68814,
    baseline_data = mnd_base,
    small_covariate_set = paste0("X", 1:9),
    outcome = "Y_continuous",
    treatment_var = "Z",
    sigma_tau = 0.2,
    ate_real = NULL,
    sd_y0_real = NULL,
    verbose = VERBOSE
  )
  
  expect_equal( mdn_iates2, mdn_iates3 )
} )


test_that( "loaded single IATE on canonical test data works", {

  source( here::here( "tests/create_testing_data.R" ) )
  
  iate_olss <- load_or_generate_iates( dataset_name="mnd", 
                                       outcome = "Y_binary",
                                       queen = "OLS S",
                                       baseline_data = mnd_baseline_data, 
                                       verbose = 0 )
  expect_true( all( iate_olss <= 1 & iate_olss >= -1 ) )  
  expect_equal( length(iate_olss), nrow(mnd_baseline_data) )

})
  






