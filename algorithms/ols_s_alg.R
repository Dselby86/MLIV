#' ols_s_alg:: Estimate IATEs using simple linear regression
#' This is a simple model as a (possibly stable) baseline for
#' comparison.
#' 
#' @param x_tr Matrix of training covariates (N x X matrix)
#' @param y_tr Vector of outcome values
#' @param d_tr Vector of treament indicators in training data
#' @param x_val Matrix of validation covariates (N x X matrix)
#'
#' @return A vector of individual IATEs, one for each unit.  I.e.,
#'   this returns, given covariate profiles given in new_data, a
#'   vector of E[ Y(1) - Y(0) | X = x ] for each observed covariate
#'   vector x in new_data.
#'   
#'   @export

ols_s_alg = function( y_tr, d_tr, x_tr, x_val ) {
  
  # Create a data frame for the training data.
  dat = data.frame( x_tr )
  
  dat['P'] = d_tr
  dat['outcome'] = y_tr
  
  # Extract column names for covariates.
  xvars = colnames(x_tr)
  
  # Fit a linear regression model.
  fm <- generate_regform('outcome', xvars ) # f is a function that creates a formula with treatment interactions
  mod <- lm(fm, data = dat)
  
  # Create a data frame for the validation data.
  valdat = data.frame(x_val)
  valdat['P'] = 1
  
  # Use the trained model to predict IATE on the validation set.
  d0 = valdat
  d1 = d0
  d0$P = 0
  d1$P = 1
  Yhat1 = predict(mod, newdata=d1)
  Yhat0 = predict(mod, newdata=d0)
  predictions = as.vector(Yhat1 - Yhat0)
  
  # Return the estimated IATE for the validation set.
  return(predictions)
}

