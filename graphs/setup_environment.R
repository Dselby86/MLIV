
# Setup analysis environment by loading functions, etc.


source( here::here( "R/packages.R" ) )


# Load all other helper functions by sourcing all files in
# 'R/functions' folder
purrr::walk(
  list.files(here::here("R", "functions"), full.names = TRUE, pattern = "R$"),
  source
)

# Create functions that specify how many messages are displayed
if (!exists("VERBOSE")) {
  VERBOSE <- FALSE
}

scat <- function(...) {
  if (VERBOSE) {
    cat(...)
  }
}