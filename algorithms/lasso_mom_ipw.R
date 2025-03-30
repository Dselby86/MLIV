#' lasso_mom_ipw:: Implementation of MOM IPW using the \code{\link{glmnet}} package
#' Modified Outcome Methods Inverse Probability Weighting is just a approach to estimate 
#' the IATE while accounting for differences in treatment assignment probabilities. 
#' 
#' @param y_tr Vector of outcome values
#' @param d_tr Vector of treament indicators in training data
#' @param x_tr Matrix of training covariates (N x X matrix)
#' @param np Matrix of nuisance parameters obtained by \code{nuisance_cf_glmnet}
#' @param x_val Matrix of validation covariates (N x X matrix)
#' @param args_tau List of arguments passed to estimate IATEs
#' @import glmnet
#'
#' @return Returns vector containing the out-of-sample IATEs
#'
#' @export

lasso_mom_ipw = function( y_tr, d_tr, x_tr, np, x_val, args_tau = list(),
                          lambda_scale = 1 ) {
  mo = y_tr * ( d_tr-np[,"p_hat"] ) / ( np[,"p_hat"] * (1-np[,"p_hat"] ) )
  fit_tau = do.call(cv.glmnet, c( list( x = x_tr, y = mo ), args_tau ) )
  
  if ( lambda_scale != 1 ) {
    # Get lambda.min value from cv.glmnet
    lambda_min <- fit_tau$lambda.min
    
    # Modify lambda.min as lambda.min * 0.75
    lambda_min <- lambda_min * 0.75
    
    iate = predict(fit_tau, x_val, s = modified_lambda_min)
    
  } else { 
    iate = predict( fit_tau, x_val, s = "lambda.min" )
  }
  return( iate )
}



# Same as previous just as a separate function to implement lambda.min * 0.75. 
# Note:: I tried implementing as argument - that did not work well so it is a 
# not ideal solution but it works
lasso_mom_ipw_dgp = function( y_tr, d_tr, x_tr, np, x_val, args_tau = list() ) {
  lasso_mom_ipw( y_tr, d_tr, x_tr, np, x_val, args_tau, lambda_scale = 0.75 )
}


