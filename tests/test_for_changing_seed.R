
library( tidyverse )

# In order for all program paths to load we need to use here::here
library( here )

# Source function to generate file paths
source( here::here( "R/configs.R" ) )

# Load packages
source( here::here( "R/packages.R" ) )

# devtools::install_github("forestry-labs/causalToolbox")
library( causalToolbox )

set.seed(101010 )

make_data <- function( N, K=5 ) {
  dat = MASS::mvrnorm( N, 
                       mu = rep( 1, K ),
                       Sigma = diag( rep( 1, K ) ) ) %>%
    as.data.frame() 
  colnames( dat ) = paste0( "X", 1:ncol(dat) )
  dat$Z = as.numeric( sample( N ) <= N/2 )
  dat$Y_tau = dat$Z * dat$X3 + rnorm( N )
  dat$Y = dat$X5 + dat$X2 * dat$X3 + dat$Y_tau + rnorm( N )
  
  return( dat )
}


master_test = make_data( 1000 )



perform_simulation_test = function( S = 3, # Number of runs
                                    test_set, 
                                    size_train = 500,
                                    do_bart = TRUE,
                                    do_xgboost = TRUE,
                                    do_dbarts = FALSE) { 
  
  # Validation data (Test data, only covariates)
  x_val = test_set %>%
    dplyr::select( starts_with( "X" ) ) %>%
    as.matrix( )
  nval = nrow( x_val )
  d_val = test_set$Z
  y_val = test_set$Y
  
  matrix_list <- list()
  for (i in 1:S){
    # Generate training data
    train_set = make_data( size_train )
    
    # Separate out into its pieces
    
    x_tr = train_set %>%
      dplyr::select( starts_with( "X" ) ) %>%
      as.matrix()
    
    
    # Train data, only covariates
    d_tr = train_set$Z
    
    # Train data, only outcomes
    y_tr = as.matrix( train_set$Y )
    
    
    index = caret::createFolds(y_tr, k = 2)
    
    if ( do_bart ) {
      cat( glue::glue( "Fitting bart now on {length(y_tr)} points, validating on {nval} points..." ) )
      x_bart = causalToolbox::X_BART(feat = x_tr, tr = d_tr, yobs = as.numeric(y_tr) )
      cate_esti_bart = causalToolbox::EstimateCate(x_bart, x_val, verbose=FALSE)
      cat( " ...finished.\n" )
      
    }
    
    if ( do_xgboost ) {
      cat( glue::glue( "Fitting bart now on {length(y_tr)} points, validating on {nval} points..." ) )
      fit_tau = tboost(x_tr, d_tr, y_tr, nthread = 1, verbose = FALSE)
      iate = predict(fit_tau, newx = x_val)
      cat( " ...finished.\n" )
      
    }
    
    if ( do_dbarts ) {
      cat( glue::glue( "Fitting bart now on {length(y_tr)} points, validating on {nval} points..." ) )
      bart.fit0 = dbarts::bart(x_tr[d_tr == 0,], y_tr[d_tr == 0], x.test = x_val)
      bart.fit1 = dbarts::bart(x_tr[d_tr == 1,], y_tr[d_tr == 1], x.test = x_val)
      predictions = bart.fit1$yhat.test.mean - bart.fit0$yhat.test.mean
      cat( " ...finished.\n" )
      
    }
    
    
    variable_name <- paste0("run_", i) 
    matrix_list[[variable_name]] <- train_set
    
    cat( "loop done\n" )
  }  
  
  
  # Return list of generated training sets.
  return(matrix_list)
} 





## Run the simulation -----------------------------------------

cat( "Running with Bart\n" )


tictoc::tic()
sim_w_XBART = perform_simulation_test(test_set = master_test, do_xgboost =FALSE)
tictoc::toc()


## Check Sim Results -------------------------------------------
train_set_1 = sim_w_XBART[['run_1']]
train_set_2 = sim_w_XBART[['run_2']]
train_set_3 = sim_w_XBART[['run_3']]




head( cbind( master= master_test$Y_tau,
             one = train_set_1$Y_tau, 
             two = train_set_2$Y_tau, 
             three = train_set_3$Y_tau )  )

# The second and third training sets are the same!
cat( "Checking 2nd and third sets.  Equal?: ", all( train_set_2 == train_set_3 ), "\n"  )



## Run without Bart -----------------


cat( "Running without Bart\n" )

tictoc::tic()
sim_w_XBART = perform_simulation_test(test_set = master_test,
                                      do_bart = FALSE , do_xgboost =FALSE)
tictoc::toc()


## Check Sim Results -------------------------------------------
train_set_1 = sim_w_XBART[['run_1']]
train_set_2 = sim_w_XBART[['run_2']]
train_set_3 = sim_w_XBART[['run_3']]


head( cbind( master= master_test$Y_tau,
             one = train_set_1$Y_tau, 
             two = train_set_2$Y_tau, 
             three = train_set_3$Y_tau )  )

# The second and third training sets are no longer the same!
cat( "Checking 2nd and third sets.  Equal?: ", all( train_set_2 == train_set_3 ), "\n"  )




## Run the simulation -----------------------------------------

cat( "Running with Bart\n" )


tictoc::tic()
sim_w_XGBOOST = perform_simulation_test(test_set = master_test, do_bart = FALSE,
                                        do_xgboost = TRUE)
tictoc::toc()


## Check Sim Results -------------------------------------------
train_set_1 = sim_w_XGBOOST[['run_1']]
train_set_2 = sim_w_XGBOOST[['run_2']]
train_set_3 = sim_w_XGBOOST[['run_3']]




head( cbind( master= master_test$Y_tau,
             one = train_set_1$Y_tau, 
             two = train_set_2$Y_tau, 
             three = train_set_3$Y_tau )  )

# The second and third training sets are not the same!
cat( "Checking 2nd and third sets.  Equal?: ", all( train_set_2 == train_set_3 ), "\n"  )



## Run the simulation -----------------------------------------

cat( "Running with Bart\n" )


tictoc::tic()
sim_w_dBART = perform_simulation_test(test_set = master_test, do_bart = FALSE,
                                        do_xgboost = FALSE, do_dbarts = TRUE)
tictoc::toc()


## Check Sim Results -------------------------------------------
train_set_1 = sim_w_dBART[['run_1']]
train_set_2 = sim_w_dBART[['run_2']]
train_set_3 = sim_w_dBART[['run_3']]




head( cbind( master= master_test$Y_tau,
             one = train_set_1$Y_tau, 
             two = train_set_2$Y_tau, 
             three = train_set_3$Y_tau )  )

# The second and third training sets are not the same!
cat( "Checking 2nd and third sets.  Equal?: ", all( train_set_2 == train_set_3 ), "\n"  )


