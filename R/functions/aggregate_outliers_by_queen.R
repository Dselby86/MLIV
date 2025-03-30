#' aggregate_outliers_by_queen: Mean percent of outliers cut by model and queen
#'
#' Needs to be stacked performance stats with a "queen" column
#' indicating where the impacts came from.
#'
#' @param performance_stats Matrix of bias, SE, and RMSE 
#'
#' 

aggregate_outliers_by_queen = function( performance_stats ) {
  
  stopifnot( "queen" %in% colnames(performance_stats) )
  
  agg_stat = performance_stats %>% 
    filter( metric == "percent_cut") %>%
    pivot_longer( cols = any_of( ALL_MODELS ),
                  names_to = "model",
                  values_to = "value" ) %>%
    
    group_by( model, queen ) %>%
    summarise( percent_cut = ( mean( value ) * 100 ),
               .groups = "drop")
  
  return( agg_stat )
}
