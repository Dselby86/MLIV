#' Combine Simulation Fragments and Save Results
#'
#' This function combines fragments from a simulation run stored in RDS files
#' for a given queen, outcome, and dataset. It saves the combined data into
#' a new file.
#'
#' @param dataset_name A character string specifying the dataset name.
#' @param queen A character string specifying the queen identifier (e.g., "OLS S", "BART T").
#' @param simulation_name A character string specifying the simulation run name or date.
#' @param verbose An integer (default: 1). If greater than 0, progress messages are printed.
#'
#' @return Invisibly returns a list containing the combined data (`predictedtau`, `runtime`, `failrate`) and metadata (`queen`).
#' 
#' @details
#' The function:
#' - Searches for all RDS files in the specified directory matching the queen identifier.
#' - Sorts the fragments numerically by their indices.
#' - Combines attributes (`predictedtau`, `runtime`, `failrate`) across fragments.
#' - Saves the combined data into a new directory, preserving the queen identifier.
#'
#' @examples
#' \dontrun{
#' combine_fragments_and_save(
#'   dataset_name = "my_dataset",
#'   queen = "OLS S",
#'   simulation_name = "20241230",
#'   verbose = 1
#' )
#' }
#' 
#' @export
#' 
combine_fragments_and_save <- function(dataset_name,
                                       queen,
                                       outcome,
                                       simulation_name,
                                       verbose = 1) {
  
  # Set the directory containing the RDS files
  directory_path <- here("results", "predicted_IATEs", 
                         dataset_name, paste0("frags_", outcome, "_", simulation_name))
  
  # List all RDS files in the directory
  pattern <- glue::glue("{queen}.rds$")
  rds_files <- list.files(path = directory_path, pattern = pattern, full.names = TRUE)
  
  # Sort the files numerically based on the fragment number
  rds_files <- rds_files[order(as.numeric(gsub(glue(".*fragment_(\\d+)_{queen}\\.rds$"), "\\1", basename(rds_files))))]
  
  # Read all RDS files into a list
  fragments <- lapply(rds_files, readRDS)
  num_fragments = length(fragments)
  
  if (num_fragments == 0) {
    stop(glue::glue( "No fragments of simulation loaded from {directory_path} with pattern '*{pattern}'"))
  }
  
  if (verbose > 0) {
    cat(glue::glue("Loaded {num_fragments} fragments of simulation from\n....{directory_path}\n....with pattern '*{pattern}' \n"))
    cat("\n")
  }
  
  # Initialize empty list for combined fragments
  frag_combine <- list()
  
  for (attr in c("predictedtau", "runtime", "failrate")) {
    # Fill combined_data with data from all fragments
    for (i in 1:num_fragments) {
      if (i == 1){
        combined_data <- fragments[[i]][[attr]]
      }
      else {
        combined_data = abind(combined_data,fragments[[i]][[attr]], along=1)
      }
    }
    
    frag_combine[[attr]] <- combined_data
    frag_combine$queen <- queen
  }
  
  # Create the directory path with date
  dir_path <- here("results", "combined_IATEs_by_queen", dataset_name,
                   paste0("simulation_from_", simulation_name))
  
  # Ensure directory exists, if not create it
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  
  # Save the file with appended IATEs by queen
  file_name <- file.path(dir_path, paste0(queen, "_queen_predicted_IATEs.rds"))
  saveRDS(frag_combine, file_name)
  
  if (verbose > 0) {
    dim = paste(dim(frag_combine$predictedtau), collapse = " x " )
    cat(glue::glue("Saved combined IATEs ({dim} dimension matrices) for {queen} to\n....{file_name}\n"))
    cat("\n")
  }
  
  invisible(frag_combine)
}

