#' is_binary: Check if the variable is binary
#' to treatment and control using the Synthpop + Copula approach
#'
#' @param variable Variable to check 
#'
#' @return TRUE or FALSE
#'
#' @export

is_binary <- function(variable) {
  unique_values <- unique(variable)
  return(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))
}
