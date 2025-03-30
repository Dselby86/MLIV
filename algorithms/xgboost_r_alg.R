
###################################################################################
# STEP 4: SPECIFY R-LEARNERS 
####################################################################################


# XGBOOST R-LEARNER

#' xgboost_r_alg: Implementation of XGBOOST R-Learner
#'  
#' Implemented using the \code{\link{xnie/rlearner}} package
#' 
#' @description The R-learner estimates tau by combining models that predict both the outcome and
#' the propensity score. We first fit and predict model on outcome. Then fit and predict model on 
#' treatment as outcome to calculate the propensity score. Then the function calculates the residuals 
#' by subtracting the predicted values from the actual outcomes and the predicted propensity scores 
#' from the actual treatment assignments, respectively. Then the pseudo-outcome is computed by 
#' dividing the residual of the outcome by the residual of the treatment assignment. This 
#' pseudo-outcome represents the estimated treatment effect for each observation. Then we fit 
#' the model on pseudo-outcome using the square of the residuals of the treatment assignment 
#' as weights. This adjustment in outcome helps to account for any potential confounding variables 
#' that may affect both the treatment assignment and the outcome. This weighting scheme ensures that 
#' observations with larger treatment assignment residuals have more influence in estimating the 
#' treatment effect. This approach allows us to obtain more reliable estimates of the treatment 
#' effect that are less biased by confounding variables.This approach allows us to obtain more 
#' reliable estimates of the treatment effect that are less biased by confounding variables.
#' XGBoost, or eXtreme Gradient Boosting, is a model that uses a technique called gradient 
#' boosting to build an ensemble of decision trees for both regression and classification tasks.
#' 
#' @param x_tr Matrix of training covariates (N x X matrix)
#' @param y_tr Vector of outcome values
#' @param d_tr Vector of treatment indicators in training data
#' @param x_val Matrix of validation covariates (N x X matrix)
#' @import xnie/rlearner
#'
#' @return Returns predicted IATEs of the validation sample
#'
#' @export

xgboost_r_alg = function( x_tr, y_tr, d_tr, x_val ) {
  fit_tau = rboost(x_tr, d_tr, y_tr, nthread = 1, verbose = FALSE)
  iate = predict(fit_tau, newx = x_val)
  return( iate )
}