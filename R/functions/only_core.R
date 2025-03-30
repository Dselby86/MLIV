#' only_core:: A function to remove everything except baseline models from a dataset
#' 
#' @param df_set The dataset to filter everything excep baseline models out of 
#' @return df_set without baseline models
#' 
#' @export


only_core <- function( df_set ) {
  df_set %>% filter( baseline )
}