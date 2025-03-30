#' generate_y:: Generate Y0 from X and the Predictive Model
#'
#' This function generates new outcomes given a predictive model, target R2 of the model, 
#' and empirical distribution. 
#'
#' @param dat A data frame containing the simulated set of covariates to generate Y0 for.
#' @param model The predictive model that predicts control-side outcomes given covariates. This model was typically previously trained on the real data.
#' @param R2 A numeric value representing the strength of our predictions of the outcome, obtained from the real data.
#' @param empDist A numeric vector representing the empirical distribution of the control-side outcomes, usually obtained from the real data.
#' @param treatment A character string specifying the name of the treatment variable.
#' @param y A character string specifying the name of the outcome variable.
#'
#' @return A numeric vector representing the generated outcomes (`Yobs`).
#'
#' @export
#' 
generate_y <- function(dat, model, R2, empDist, treatment, y) {
  
  # Make everything predict the control-side potential outcomes
  dat[treatment] <- 0
  
  yhat_name <- paste0(y, "_hat")
  
  # Predict the outcome given covariates using the predictive model
  dat[yhat_name] <- predict(model, newdata = dat)
  
  # Calculate the percentile rank of Y_hat by making K intervals 0 to
  # 1 and assigning each Y_hat value to the midpoint of its ranked interval
  K = nrow( dat )
  dat$r_Yhat = rank( dat[[yhat_name]] ) / K - 1/( 2*K )
  
  # Calculate the Z-score of Y_hat using the rank of Y_hat
  dat$z_Yhat <- qnorm(dat$r_Yhat)
  
  # Generate Z-scores for Y that are correlated with the z-scores of
  # Yhat with a strength of relationship of the target R2 value
  dat$z_Y <- rnorm(K, 
                   mean = sqrt(R2) * dat$z_Yhat, 
                   sd = sqrt(1-R2))
  
  # Convert the generated Z-scores back to the distribution of Y
  empDist <- sort(empDist)
  K <- length(empDist)
  dat$r_Y <- ceiling(pnorm(dat$z_Y) * K)
  
  # Calculate Yobs using the empirical distribution and the converted
  # Z-scores
  Yobs <- empDist[dat$r_Y]
  
  # Return the generated Yobs
  return(Yobs)
}
