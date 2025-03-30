

test_that("load real data works", {

  source( here::here( "simulation_pipeline/01_load_real_data.R" ) )
  
  # Use case 1: load real data from disk
  mnd_data <- load_and_validate_real_data(real_data = here::here("datasets/mnd.csv"))
  expect_true( is.data.frame(mnd_data ) )
  
  # Use case 2: just return adjusted dataset
  mnd_data_copy <- load_and_validate_real_data(real_data = mnd_data)
  expect_true( identical(mnd_data, mnd_data_copy) )
  
  
})
