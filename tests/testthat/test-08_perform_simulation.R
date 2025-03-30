

VERBOSE <- FALSE
source( here::here( "simulation_pipeline/08_run_simulation.R" ) )

library( testthat )

# NOTE: Prior test scripts need to be run to generate files for this
# test script to pass



test_that( "components of simulation launcher works", {
  
  
  # Loading data generated from running test 07 script
  rs <- combine_fragments_and_save( dataset_name = "mnd_test",
                                    queen = "ATE",
                                    simulation_name = "testthat", verbose = 0 )
  
  expect_is( rs, "list" )
  names(rs)
  dim( rs$predictedtau )
  
  tb <- load_and_process_queen_results( dataset_name = "mnd_test", 
                                        queen = "ATE",
                                        outcome="Y_continuous",
                                        simulation_name = "testthat", 
                                        true_iates_directory_path = here::here("true_iates"),
                                        verbose = 0 )
  head( tb )
  expect_is( tb, "data.frame" )
  table( tb$queen )  
  tbl = table( tb$metric )  
  expect_true( length(tbl) == 5 )
  
  expect_true( nrow(tb) > 0 )
  expect_true( "ATE" %in% colnames(tb) )
  
  rs <- combine_fragments_and_save( dataset_name = "mnd_test",
                                    queen = "OLS S",
                                    simulation_name = "testthat", verbose = 0 )
  
  t2 <- load_and_process_all_results( dataset_name = "mnd_test", 
                                      outcome="Y_continuous",
                                      queen_list = c("ATE", "OLS S"),
                                      simulation_name = "testthat",
                                      true_iates_directory_path = here::here("true_iates"),
                                      verbose = 0 )
  
  t2
  
  expect_is( t2, "data.frame" )
  expect_true( nrow(t2) > 0 )
  expect_true( "ATE" %in% colnames(t2) )
  qns = table( t2$queen )
  expect_true( all( c( "ATE", "OLS S" ) %in% names(qns) ) )
  
  table( t2$queen )
  
  table( t2$metric )  
  expect_true( length( table( t2$metric ) ) == 5 )
})





test_that("simulation launcher works", {
  
  test <- run_simulation(
    # Required arguments
    real_data = here::here("datasets/mnd.csv"), 
    dataset_name = "mnd_test",    
    covariates = paste0( "X", 2:6 ),
    outcome = "Y_continuous",
    treatment = "Z",
    
    # Optional arguments
    large_covariate_set = paste0( "X", 1:9 ), 
    small_covariate_set = paste0( "X", 2:6 ), 
    all_outcomes = c("Y_continuous", "Y_binary"),
    S = 7,  
    chunk_size = 4,
    queen_list = c( "ATE", "OLS S" ),
    p_tx = NULL,
    size_train = 1000,
    size_test = 2000,
    PARALLEL = FALSE,
    verbose = 0,
    include_LASSO = TRUE,
    include_RF = FALSE,
    include_SL_S = FALSE,
    include_SL_T = FALSE,
    include_CDML = FALSE,
    include_XGBOOST = FALSE,
    include_BART = FALSE,
    master_seed = 509503,
    baseline_seed = 68814, 
    baseline_N = 10000, 
    baseline_directory_path = here::here("baseline"),
    true_iates_directory_path = here::here("true_iates"),
    simulation_name = "testthat2" ) 
  
  expect_is( test, "data.frame" )
  
  tb = table( test$metric )  
  tb
  expect_true( length(tb) == 5 )  
  
  tb2 = table( test$queen )
  tb2
  expect_true( length(tb2) == 2 )  
  
  # ID corresponds to test unit ID
  ln = length( unique( test$id ) )
  expect_equal( ln, 2000 )
  
  
} )



test_that( "logger works", {
  
  mnd_data = tibble( X = 1:10 )
  total_time_formatted <- sprintf("%d day(s) and %.2f hour(s)", 3, 2)
  
  params <- collect_simulation_params(    dataset_name = "mnd_test_fake",    
                                          covariates = paste0( "X", 2:6 ),
                                          outcome = "Y_continuous",
                                          treatment = "Z",
                                          
                                          small_covariate_set = paste0( "X", 2:6 ),
                                          large_covariate_set = paste0( "X", 1:9 ),
                                          all_outcomes = c("Y_continuous", "Y_binary"),
                                          # Optional arguments
                                          S = 7,  
                                          queen_list = c( "ATE", "OLS S" ),
                                          p_tx = NULL,
                                          size_train = 1000,
                                          size_test = 2000,
                                          PARALLEL = FALSE,
                                          verbose = 0,
                                          include_LASSO = TRUE,
                                          include_RF = FALSE,
                                          include_SL_S = FALSE,
                                          include_SL_T = FALSE,
                                          include_CDML = FALSE,
                                          include_XGBOOST = FALSE,
                                          include_BART = FALSE,
                                          master_seed = 509503,
                                          baseline_seed = 68814, 
                                          baseline_N = 10000, 
                                          baseline_directory_path = here::here("baseline"),
                                          true_iates_directory_path = here::here("true_iates"),
                                          simulation_name = "test_logger",
                                          start_time_formatted = "2021-09-01 12:00:00",
                                          end_time_formatted = total_time_formatted,
                                          total_time_formatted = total_time_formatted)
  test <- log_simulation_run( params,
                              verbose = 0 )
  
  test
  
  fl <- read_csv( test )
  fl
  expect_is( fl$covariates, "character" )
  expect_is( fl$large_covariate_set, "character" )
})


