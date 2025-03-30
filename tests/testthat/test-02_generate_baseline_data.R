

VERBOSE <- FALSE
source( here::here( "simulation_pipeline/02_generate_baseline_data.R" ) )

test_that("generate baseline", {
  
  
  dataset_name <- "test"
  baseline_directory_path <- here::here("baseline")
  
  # make some fake data!
  real_data <- data.frame(matrix(rnorm(1000), ncol=10))
  colnames(real_data) <- paste0("V", 1:10)
  real_data$treatment <- sample(0:1, 100, replace = TRUE)
  real_data$Ybin = 0 + (real_data$V4 + real_data$V5 > 0 )
  real_data$Ycont = real_data$V6 + real_data$V5 
  all_outcomes <- c("Ybin", "Ycont")
  covariates_names <- paste0("V", 1:10)
  treatment_var <- "treatment"
  baseline_seed <- 123
  baseline_N <- 100
  
  bdat <- generate_baseline_data( real_data=real_data,
                                  all_outcomes = all_outcomes,
                                  treatment = treatment_var,
                                  large_covariate_set = covariates_names,
                                  baseline_seed = baseline_seed, 
                                  baseline_N = baseline_N, verbose = 0)
  
  bdat
  
  expect_is( bdat, "list" )
  expect_is( bdat$Data, "data.frame" )
  
  dt = bdat$Data
  head( dt )
  expect_true( all( c( "Ybin_Y0", "Ycont_Y0" ) %in% colnames(dt) ) )
  expect_equal( nrow(dt), baseline_N )
  #expect_equal( baseline_seed, bdat$Y0_seed )
  expect_equal( bdat$seed_synthpop, baseline_seed )
  
  head( dt$Ybin_Y0 )
  expect_true( length( dt$Ybin_Y0 ) == nrow(dt) )
  expect_true( is.numeric( dt$Ybin_Y0 ) )
  expect_true( all( dt$Ybin_Y0 >= 0  & dt$Ybin_Y0 <= 1 ) )
  head( dt$Ybin_Y0 )
  
  bdat2 <- load_or_generate_baseline_object( dataset_name = dataset_name,
                                             baseline_directory_path = baseline_directory_path, 
                                             all_outcomes = all_outcomes,
                                             real_data = real_data,
                                             treatment_var = treatment_var,
                                             covariates_names = covariates_names,
                                             baseline_seed = baseline_seed, baseline_N = baseline_N,
                                             verbose = 0,
                                             force_generate = TRUE)
  
  expect_true( all( bdat2$Data$Ybin_Y0 >= 0 ) )
  expect_true( all( bdat2$Data$Ybin_Y0 <= 1 ) )
  
  bdat3 <- load_or_generate_baseline_object( dataset_name = dataset_name,
                                             baseline_directory_path = baseline_directory_path, 
                                             all_outcomes = all_outcomes,
                                             treatment_var = treatment_var,
                                             covariates_names = covariates_names,
                                             baseline_seed = baseline_seed, baseline_N = baseline_N,
                                             verbose = 0)
  
  
  expect_equal( bdat3, bdat2 )
  
})


test_that( "mrn baseline data works", {

  mnd_data <- load_and_validate_real_data( real_data = here::here("datasets/mnd.csv"))
  
  mnd_baseline <- load_or_generate_baseline_object(
    dataset_name = "mnd",
    baseline_directory_path = here::here("baseline"),
    real_data = mnd_data,
    all_outcomes = c("Y_continuous", "Y_binary"),
    covariates_names = paste0("X", 1:9) ,
    treatment_var = "Z",
    baseline_seed = 68814,
    baseline_N = 100000, verbose = VERBOSE, force_generate = TRUE
  )
  mnd_dat <- mnd_baseline$Data  
  
  expect_true( all( c( "Y_continuous_Y0", "Y_binary_Y0" ) %in% colnames(mnd_dat) ) )
  expect_true( all( mnd_dat$Y_binary_Y0 >= 0 ) )
  expect_true( all( mnd_dat$Y_binary_Y0 <= 1 ) )

} )
