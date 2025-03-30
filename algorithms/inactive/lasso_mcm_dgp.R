
# Same as previous just as a separate function to implement lambda.min * 0.75. 
# Note:: I tried implementing as argument - that did not work well so it is a 
# not ideal solution but it works
lasso_mcm_dgp = function( y_tr, d_tr, x_tr, np, x_val, args_tau = list() ) {
  mo = 2 * y_tr * ( 2 * d_tr - 1 )
  w = ( 2 * d_tr - 1) * ( d_tr - np[,"p_hat"] ) / ( 4 * np[,"p_hat"] * ( 1 - np[,"p_hat"] ) )
  fit_tau = do.call( cv.glmnet, c( list( x = x_tr, y = mo, weights = w ), args_tau ) )
  
  # Get lambda.min value from cv.glmnet
  lambda_min <- fit_tau$lambda.min
  
  # Modify lambda.min as lambda.min * 0.75
  modified_lambda_min <- lambda_min * 0.75
  
  # Call the predict function with the modified lambda.min value
  iate = predict(fit_tau, x_val, s = modified_lambda_min)
  
  return( iate )
}
