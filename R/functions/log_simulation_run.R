#' log_simulation_run:: Log simulation run parameters
#'
#' This function logs the parameters of a simulation run into a CSV file.
#' It ensures that the parameters are stored in a structured manner and can
#' be retrieved for future reference or analysis. The function handles the
#' creation of log directories and appends new entries to the existing log file.
#'
#' @param params A named list of simulation parameters to be logged. This
#'   should include details such as dataset name, outcome, simulation name,
#'   and time-related fields (start, end, and total time).
#' @param verbose An integer indicating the verbosity level. If greater than 0,
#'   messages about the logging process will be printed. Default is 1000.
#'
#' @return A character string specifying the path to the log file.
#'
#' @examples
#' \dontrun{
#'   params <- list(
#'     dataset_name = "example_dataset",
#'     outcome = "example_outcome",
#'     simulation_name = "example_simulation",
#'     start_time = "2024-12-18 14:00:00",
#'     end_time = "2024-12-18 14:30:00",
#'     total_time = "1800 seconds"
#'   )
#'   log_file_path <- log_simulation_run(params, verbose = 1000)
#'   print(glue::glue("Log saved to: {log_file_path}"))
#' }
#'
#' @export
#' 
log_simulation_run <- function(params, verbose = 1000) {
  
  # Convert parameters to a data frame for logging
  params_df <- as.data.frame(t(unlist(params)), stringsAsFactors = FALSE)
  
  # Define the log directory and file path
  log_dir <- here::here("results/logs", params$dataset_name)
  
  # Ensure the log directory exists
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE)
  }
  
  log_file_path <- file.path(log_dir, "log_file.csv")
  
  # Log the parameters to the file
  if (verbose > 0) {
    message(glue::glue("Logging simulation run to {log_file_path}"))
  }
  
  if (!file.exists(log_file_path)) {
    # Create the log file if it doesn't exist
    write.csv(params_df, log_file_path, row.names = FALSE)
  } else {
    # Append to the log file if it exists
    write.table(params_df, log_file_path, append = TRUE, sep = ",",
                col.names = FALSE, row.names = FALSE)
  }
  
  return(log_file_path)
}
