#' no_core:: A function to remove baseline models from a dataset
#' 
#' @param df_set The dataset to filter baseline models out of 
#' @return df_set without baseline models
#' 
#' @export


no_core <- function( df_set ) {
  df_set %>% filter( !baseline )
}