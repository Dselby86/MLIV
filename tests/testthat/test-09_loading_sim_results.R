
# Testing the loading simulation results and processing them functions

source( here::here( "simulation_pipeline/08_run_simulation.R"))


test_that( "loading and getting aggregate performance metrics works", {
  
  if ( FALSE ) {
    debug( process_sim_results )
  }
  rs <- combine_fragments_and_save(dataset_name = "mnd_test",
                                   queen = "ATE", simulation_name = "testthat",
                                   verbose = 0 )
  expect_true( is.array( rs$predictedtau ) )
  expect_equal( length( rs ), 4 )
  
  tb <- load_and_process_queen_results(dataset_name = "mnd_test", queen = "ATE",
                                       outcome="Y_continuous", simulation_name = "testthat",
                                       verbose = 0 )
  tb
  expect_true( is.data.frame(tb) )
  expect_true( !is.null( tb$ATE ) )
  
  t2 <- load_and_process_all_results(dataset_name = "mnd_test", outcome="Y_continuous",
                                     queen_list = c("ATE", "OLS S"), 
                                     simulation_name = "testthat",
                                     verbose = 0)
  t2
  tt <- t2 %>% filter( queen == "ATE", id == 1 ) 
  expect_equal( nrow( tt ), 5 )
  
  rt <- filter( t2, metric == "percent_cut" )
  summary( rt[ ,-c(1:2)])
  
} )



test_that( "read_and_aggregate works", {
  
  # Demo call from script 9:
  # ca_1_1000_small_SL = read_and_aggregate(cov_set_size = "small", dataset = "ca", outcome_index = 1, train_set_size = "1000", relative_path = "results/aggregated_IATEs/ca/simulation_from_091024/aggregated_IATEs_data.rds")  %>% filter(model %in% c("SL S", "SL T"))
  
  if ( FALSE ) {
    debug( read_and_aggregate )
  }
  
  # Note: the various variables are just added as columns and not used
  rr <- read_and_aggregate( cov_set_size = "extra", dataset = "extra", 
                            outcome_index = 3003, train_set_size = "4240", 
                            relative_path = "aggregated_IATEs_data.rds",
                            result_dir = here::here( "results/aggregated_IATEs/mnd_test/simulation_from_testthat/" ),
                            verbose = 0)
  
  rr
  expect_true( is.data.frame(rr) )
  expect_true( all( rr$percent_cut >= 0 ) )
  expect_true( all( rr$percent_cut <= 1 ) )
  
})
