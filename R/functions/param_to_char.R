#' param_to_char:: Convert Parameter to Character
#'
#' This helper function converts various types of parameters to a character representation.
#'
#' @param param The parameter to be converted. It can be of various types including `NULL`, logical, numeric, character, list, or vector.
#'
#' @return A character string representing the input parameter.
#'
#' @examples
#' \dontrun{
#'   param_to_char(NULL)         # Returns "NULL"
#'   param_to_char(TRUE)         # Returns "TRUE"
#'   param_to_char(123)          # Returns "123"
#'   param_to_char("text")       # Returns "text"
#'   param_to_char(list(1, 2))   # Returns "1, 2"
#' }
#'
#' @export
#' 
param_to_char <- function(param) {
  if (is.null(param)) {
    return("NULL")
  } else if (is.logical(param)) {
    return(as.character(param))
  } else if (is.numeric(param)) {
    return(as.character(param))
  } else if (is.character(param)) {
    return(param)
  } else if (is.list(param) || is.vector(param)) {
    return(paste(param, collapse = ", "))
  } else {
    return(as.character(param))
  }
}