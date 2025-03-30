
#' nuisance_cf_lasso: This function creates the nuisance parameters p(x), mu(x), and
#' mu_d(x) using cross-validated glmnet via cross-fitting using the \code{\link{glmnet}} package
#'
#' @param y_tr Vector of outcome values in training data
#' @param d_tr Vector of treament indicators
#' @param x_tr Matrix of covariates (N x p matrix) in training data
#' @param index List indicating indices for cross-fitting (e.g.
#'   obtained by \code{createFolds} of \code{\link{caret}} pkg)
#' @param args_p List of arguments passed to estimate propensity score
#'   model
#' @param args_y List of arguments passed to estimate outcome model
#' @param args_y1 List of arguments passed to estimate outcome model
#'   of treated
#' @param args_y0 List of arguments passed to estimate outcome model
#'   of non-treated
#' @import glmnet
#'
#' @return Returns n x 4 matrix containing the nuisance parameters
#'
#' @export
nuisance_cf_lasso <- function( y_tr, d_tr, x_tr, index,
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
    
    # Predict the propensity scores for the current fold's test data and 
    # store them in the np matrix
    np[index[[i]],1] = predict( fit_p, x_tr[index[[i]],,drop=F], s = "lambda.min", type = "response" )
    
    # Fit a cross-validated glmnet model to estimate the expected outcome based on the covariate data 
    # in x_tr and the outcome in y_tr for a particular fold specified by index[[i]]
    fit_y = do.call( cv.glmnet, c( list( x = x_tr[-index[[i]],,drop=F],
                                         y = y_tr[-index[[i]]] ),
                                   args_y))
    
    # Predict the expected outcome for the current fold's test data and 
    # store them in the np matrix 
    np[index[[i]],2] = predict( fit_y, x_tr[index[[i]],,drop=F], s = "lambda.min" )
    
    # Fit a cross-validated glmnet model to estimate the expected outcome under the control group 
    # based on the covariate data for non-treated individuals [d_tr[-index[[i]]] == 0]
    # in x_tr and the outcome in y_tr for non-treated individuals for a particular fold specified by index[[i]]
    fit_y0 = do.call( cv.glmnet, c( list( x = x_tr[-index[[i]],,drop=F][d_tr[-index[[i]]] == 0,,drop=F],
                                          y = y_tr[-index[[i]]][d_tr[-index[[i]]] == 0] ),
                                    args_y0))
    
    # Predict the expected outcome for non-treated individuals 
    # for the current fold's test data and store them in the np matrix 
    np[index[[i]],3] = predict( fit_y0, x_tr[index[[i]],,drop=F], s = "lambda.min" )
    
    # Fit a cross-validated glmnet model to estimate the expected outcome under the treatment  group 
    # based on the covariate data for treated individuals [d_tr[-index[[i]]] == 1]
    # in x_tr and the outcome in y_tr for treated individuals for a particular fold specified by index[[i]]
    fit_y1 = do.call( cv.glmnet, c( list( x = x_tr[-index[[i]],,drop=F][d_tr[-index[[i]]] == 1,,drop=F],
                                          y = y_tr[-index[[i]]][d_tr[-index[[i]]] == 1] ),
                                    args_y1 ) )
    
    # Predict the expected outcome for treated individuals 
    # for the current fold's test data and store them in the np matrix 
    np[index[[i]],4] = predict( fit_y1, x_tr[index[[i]],,drop=F], s = "lambda.min" )
  }
  
  return(np)
  
}