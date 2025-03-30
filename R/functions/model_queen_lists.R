


#' model_queen_lists:: The function to create lists of models and queens
#'
#' @param include_RF Include RF models
#' @param include_LASSO Include Lasso models
#' @param include_CDML Include CDML models
#' @param include_XGBOOST Include XGBOOST models
#' @param include_BART Include BART models
#' @param include_SL Include SL models
#'
#' @return outputs list of lists including list of all models, list of selected 
#' models and list of queens
#'
#' @export
#'
model_queen_lists <- function(
                              include_RF = TRUE, 
                              include_LASSO = TRUE, 
                              include_CDML = TRUE, 
                              include_XGBOOST = TRUE, 
                              include_BART = TRUE,
                              include_SL_S = TRUE,
                              include_SL_T = TRUE,
                              make_queen_list = FALSE ) {
  
  # Create a list of all available models 
  all_models <- list(
    OLS = "OLS S",
    ATE = "ATE",
    RF = c("RF INF", "RF T", "RF MOM IPW", "RF MOM DR", "CF", "CF LC"),
    LASSO = c("LASSO INF", "LASSO T", "LASSO MOM IPW", "LASSO MOM DR", "LASSO MCM", "LASSO MCM EA", "LASSO R"),
    CDML = "CDML",
    XGBOOST = c("XGBOOST S", "XGBOOST R"),
    BART = c("BART T", "BART S"),
    'SL S' = "SL S",
    'SL T' = "SL T"
  )
  
  # Create a list of flags for selected models
  model_flags <- list(
    OLS = TRUE,
    ATE = TRUE,
    RF = include_RF,
    LASSO = include_LASSO,
    CDML = include_CDML,
    XGBOOST = include_XGBOOST,
    BART = include_BART,
    'SL S' = include_SL_S,
    'SL T' = include_SL_T
  )
  
  # Start with always included models
  model_list <- c()
  
  # Add models based on the inclusion flags
  for (model in names(all_models)) {
    if (model_flags[[model]]) {
      model_list <- c(model_list, all_models[[model]])
    }
  }
  
  # Transform all_models to a list from list of lists:
  
  # Start with an empty list
  model_pool = c()
  
  # Add all available models 
  for (model in names(all_models)) {
    model_pool <- c(model_pool, all_models[[model]])
  }
  
  # Create a list of models we want to exclude from queens list
  exclude_queen <- c("RF INF", "RF MOM DR",
                     "CF LC","LASSO INF",
                     "LASSO T","LASSO MOM IPW","LASSO MOM DR", "LASSO MCM", "LASSO MCM EA",
                     "XGBOOST S", "BART S", "SL S")
  
 
  if ( make_queen_list ) {
    
   # Create a list of queens
  queen_list <- setdiff(model_list, exclude_queen)
  
  # Return a list of queens
  return(queen_list)
  } else {
    return( model_list )
  }
  
}


if ( FALSE ) {
  
  model_queen_lists( make_queen_list = TRUE )
  
  model_queen_lists( include_baseline = FALSE, make_queen_list = FALSE )
  
}