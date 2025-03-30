#' generate_regform:: Utility Function for Generating a Regression Formula
#' 
#' This function generates a regression formula that can be passed to functions like `lm()`.
#'
#' @param outcome A character string specifying the name of the outcome variable.
#' @param variables A character vector specifying the names of the covariates.
#' @param control_only A character vector specifying the covariates that should be treated as control variables in the regression model.
#' 
#' @return A regression formula.
#'
#' @examples
#' \dontrun{
#'   outcome <- "Y"
#'   variables <- c("X1", "X2", "X3")
#'   control_only <- c("X2")
#'   formula <- generate_regform(outcome, variables, control_only)
#'   print(formula)
#' }
#'
#' @export
#' 
generate_regform <- function(outcome, variables, control_only = c()){
  vint <- setdiff(variables, control_only)
  
  ints <- paste(vint, collapse = " + ")
  if (length(control_only) > 0) {
    conts <- paste(control_only, collapse = " + ")
    
    f_string <- paste0(outcome, "~ P * (", ints, ") + ", conts)
  } else {
    f_string <- paste0(outcome, "~ P * (", ints, ")")
  }
  
  as.formula(f_string)
}
