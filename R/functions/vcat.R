#' vcat: Conditional Verbose Message Printer
#'
#' The `vcat` function is used to print messages to the console conditionally, based on a verbosity flag.
#'
#' @param verbose A logical value indicating whether the message should be printed (TRUE) or not (FALSE).
#' @param msg A character string that represents the message you want to print. It can contain placeholders for values to be inserted using the '...' argument.
#' @param ... Additional arguments that are used to replace placeholders in the 'msg' string using 'sprintf' format.
#'
#' @return This function doesn't explicitly return a value; it prints messages to the console based on the 'verbose' flag.
#'
#' @examples
#'
#' verbose <- TRUE
#' vcat(verbose, "This is a verbose message with a number: %d", 42)
#'
#' verbose <- FALSE
#' vcat(verbose, "This message will not be printed.")
#'
#' @export
#' 
vcat <- function(verbose, msg, ...) {
  if (verbose > 0) {
    cat(sprintf(msg, ...))
    cat("\n")
  }
}