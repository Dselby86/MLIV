

# Same as previous just as a separate function to implement lambda.min * 0.75. 
# Note:: I tried implementing as argument - that did not work well so it is a 
# not ideal solution but it works
nuisance_cf_lasso_dgp <- function( y_tr, d_tr, x_tr, index,
                                    args_p = list(),
                                    args_y = list(),
                                    args_y0 = list(),
                                    args_y1 = list() ) {
  
  # Create a nuisance parameters matrix
  np = matrix( NA, length( d_tr ), 4 )
  
  # Name the specific nuisance parameters being estimated.
  colnames( np ) = c( "p_hat", # p_hat: The probability of receiving treatment (d=1) given the covariates x, denoted as P(D=1 | X). It is p(x).
                      "y_hat", # y_hat: The expected outcome variable y given the covariates x, denoted as E(Y | X). It is mu(x).
                      "y0_hat", # y0_hat: The expected outcome variable y under the control group (d=0) given the covariates x, denoted as E(Y | D=0, X)
                      "y1_hat" ) # y1_hat: The expected outcome variable y under the treatment group (d=1) given the covariates x, denoted as E(Y | D=1, X). It is mu_d(x).
  
  
  # A loop is initiated to iterate over the cross-fitting folds
  for( i in 1:length( index ) ) {
    
    # Fit a cross-validated glmnet model to estimate propensity scores based on the covariate data
    # in x_tr and the treatment indicators in d_tr for a particular fold specified by index[[i]]
    fit_p = do.call( cv.glmnet, c( list( x = x_tr[-index[[i]],,drop=F],
                                         y = d_tr[-index[[i]]],
                                         family="binomial" ),
                                   args_p ) )
    
    # Get lambda.min value from cv.glmnet
    lambda_min <- fit_p$lambda.min
    
    # Modify lambda.min as lambda.min * 0.75
    modified_lambda_min <- lambda_min * 0.75
    
    
    # Predict the propensity scores for the current fold's test data and
    # store them in the np matrix
    np[index[[i]],1] = predict( fit_p, x_tr[index[[i]],,drop=F], s = modified_lambda_min, type = "response" )
    
    # Fit a cross-validated glmnet model to estimate the expected outcome based on the covariate data
    # in x_tr and the outcome in y_tr for a particular fold specified by index[[i]]
    fit_y = do.call( cv.glmnet, c( list( x = x_tr[-index[[i]],,drop=F],
                                         y = y_tr[-index[[i]]] ),
                                   args_y))
    
    # Get lambda.min value from cv.glmnet
    lambda_min <- fit_y$lambda.min
    
    # Modify lambda.min as lambda.min * 0.75
    modified_lambda_min <- lambda_min * 0.75
    
    # Predict the expected outcome for the current fold's test data and
    # store them in the np matrix
    np[index[[i]],2] = predict( fit_y, x_tr[index[[i]],,drop=F], s = modified_lambda_min )
    
    # Fit a cross-validated glmnet model to estimate the expected outcome under the control group
    # based on the covariate data for non-treated individuals [d_tr[-index[[i]]] == 0]
    # in x_tr and the outcome in y_tr for non-treated individuals for a particular fold specified by index[[i]]
    fit_y0 = do.call( cv.glmnet, c( list( x = x_tr[-index[[i]],,drop=F][d_tr[-index[[i]]] == 0,,drop=F],
                                          y = y_tr[-index[[i]]][d_tr[-index[[i]]] == 0] ),
                                    args_y0))
    
    # Get lambda.min value from cv.glmnet
    lambda_min <- fit_y0$lambda.min
    
    # Modify lambda.min as lambda.min * 0.75
    modified_lambda_min <- lambda_min * 0.75
    
    # Predict the expected outcome for non-treated individuals
    # for the current fold's test data and store them in the np matrix
    np[index[[i]],3] = predict( fit_y0, x_tr[index[[i]],,drop=F], s = modified_lambda_min )
    
    # Fit a cross-validated glmnet model to estimate the expected outcome under the treatment  group
    # based on the covariate data for treated individuals [d_tr[-index[[i]]] == 1]
    # in x_tr and the outcome in y_tr for treated individuals for a particular fold specified by index[[i]]
    fit_y1 = do.call( cv.glmnet, c( list( x = x_tr[-index[[i]],,drop=F][d_tr[-index[[i]]] == 1,,drop=F],
                                          y = y_tr[-index[[i]]][d_tr[-index[[i]]] == 1] ),
                                    args_y1 ) )
    
    # Get lambda.min value from cv.glmnet
    lambda_min <- fit_y1$lambda.min
    
    # Modify lambda.min as lambda.min * 0.75
    modified_lambda_min <- lambda_min * 0.75
    
    # Predict the expected outcome for treated individuals
    # for the current fold's test data and store them in the np matrix
    np[index[[i]],4] = predict( fit_y1, x_tr[index[[i]],,drop=F], s = modified_lambda_min )
  }
  
  return(np)
  
}