#' make_x_matrix: Given dataset, make a design matrix out of the passed covariates
#' (NOT including treatment)
#' 
#' If you have already split the data into training and validation, it
#' will stack, make a model matrix (so the right number of factor
#' levels are shared across both sets) and then return a list of the
#' two matrices.
#' 
#' It will only create binary covariates if categorical covariates are recoded as factors
#'
#' @param data Test data
#' @param covariates List of covariates names
#' @param data_train Train data
#' 
#' @return Covariate data as list of matrices
#'
#' @export
#' 
make_x_matrix <- function(data, covariates, data_train = NULL) { 
  N <- nrow(data)
  if (!is.null(data_train)) {
    data <- bind_rows(data, data_train)
  }
  
  covariates_formula <- paste(covariates, collapse = " + ")
  
  form <- as.formula(paste0("~", covariates_formula))
  
  X <- model.matrix(form, data)[ , -1]
  if (!is.null(data_train)) {
    return(list(X_val = X[1:N,],
                X_tr = X[(N+1):nrow(X),])) 
  } else {
    return(X)
  }
}
