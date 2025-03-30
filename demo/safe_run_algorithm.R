



save_run <- function( alg, ... ) {
  
  safe_alg = quietly( safely( alg ) )
  
  start_time <- Sys.time()
  res <- safe_alg( ... )
  end_time <- Sys.time()
  
  warn <- res$warning
  if ( length( warn ) >= 1 ) {
    warn = paste( warn, collapse = "; " )
  }
  
  if ( is.null( res$result$result ) ) {
    
    msg = as.character( res$result$error )
    if ( length( warn ) > 0 ) {
      msg = paste0( msg, "; ", warn )
    }
    
    # it failed, plug in NAs
    list( prediction = NULL,
          runtime = end_time - start_time,
          messages = msg )
    
  } else {
    
    predictions <- res$result$result
    
    list( prediction = predictions,
          runtime = end_time - start_time,
          messages = warn )
  }
}


# Testing ----

# Crash every so often, for testing
wack_lasso <- function( y_tr, d_tr, x_tr, np, x_val, args_tau = list() ) {
  
  if ( sample(2,1) == 1 ) {
    warning( "Warning pre error" )
  }
  
  if ( sample(2,1) == 1 ) {
    stop( "Decided to die" )
  }
  
  if ( sample(2,1) == 1 ) {
    warning( "Warning post error", .Call = NULL )
  }
  
  lasso_mcm_ea( y_tr, d_tr, x_tr, np, x_val, args_tau )
}


if ( FALSE ) {
  
  
  source( here::here( "simulation_pipeline/05_generate_predicted_IATE.R" ) )
  source( here::here( "tests/create_testing_data.R" ) )
  
  start_time <- Sys.time()
  index = caret::createFolds(y_tr, k = 2)
  np <- nuisance_cf_lasso(y_tr, d_tr, x_tr, index)
  end_time <- Sys.time()
  lasso_nuisance_time <- end_time - start_time
  
  
  #debug( save_run )
  #debug( lasso_mcm_ea )
  rs1 <- wack_lasso( y_tr, d_tr, x_tr, np=np, x_val = x_val )
  
  rs2 <- save_run( wack_lasso, y_tr, d_tr, x_tr, np=np, x_val = x_val )
  rs2
  
  rs2$messages
  
  testthat::expect_equal( length(rs1), length(rs2$prediction) )
  
  
  
  
}



if ( FALSE ) {
  library(purrr)
  
  # Example of a safely function
  safe_fn <- quietly(function(x) { 
    if(x == 1) stop("Error 1")
    if(x == 2) warning("Warning 2")
    if(x >= 2) warning("Warning big")
    x
  })
  
  result <- safe_fn(1)  # This will store an error
  result_safe <- safe_fn(2)  # This will store a warning
  
  # Function to collapse errors and warnings into a single string
  collapse_errors_warnings <- function(safe_result) {
    paste(c(
      if(!is.null(safe_result$error)) safe_result$error$message else NULL,
      if(length(safe_result$warnings) > 0) safe_result$warnings else NULL
    ), collapse = " | ")
  }
  
  # Apply the function
  collapsed <- collapse_errors_warnings(result)
  collapsed_warning <- collapse_errors_warnings(result_safe)
  
  collapsed
  collapsed_warning
  
  
}