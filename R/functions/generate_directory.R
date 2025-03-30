#' generate_directory:: Create directory for storing simulation results
#'
#' This function generates a directory path for storing simulation results. 
#' If the directory does not exist, it will be created.
#'
#' @param folder_name A character string specifying the name of the main folder
#'   where results will be stored.
#' @param simulation_name An optional character string specifying the name of 
#'   the simulation. If provided, it will be included in the directory path.
#'
#' @return A character string containing the full path to the generated directory.
#'
#' @details The function constructs a directory path using the `here` package, 
#'   combining a base path (`"results/predicted_IATEs"`), the provided `folder_name`, 
#'   and `simulation_name`. If the directory does not already exist, it is created recursively.
#'
#' @export
#' 
generate_directory <- function(folder_name, outcome, simulation_name = NULL) {
  
  # Create the directory path with simulation_name's date
  dir_path <- here("results", "predicted_IATEs", folder_name, paste0("frags_", outcome, "_", simulation_name))
  
  # Ensure directory exists, if not create it
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  
  dir_path
}
