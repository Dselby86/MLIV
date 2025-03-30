
#' aggregated_performance_by_queen: Aggregate performance
#' characteristics (Bias, SE, RMSE) across all test points by model by
#' queen.
#'
#' Needs to be stacked performance stats with a "queen" column
#' indicating where the impacts came from.
#'
#' @param performance_stats Matrix of bias, SE, and RMSE
#'
#'   NOTE/WARNING: Bias is squared and then averaged, which will give
#'   a semi-funky mean of of absolute bias.
#' 

aggregated_performance_by_queen <- function( performance_stats ) {
  
  stopifnot( "queen" %in% colnames(performance_stats) )
  
  agg_stat = performance_stats %>% 
    pivot_longer( cols = any_of( ALL_MODELS ),
                  names_to = "model",
                  values_to = "value" ) %>%
    filter( !( metric %in% c( "runtime", "percent_cut" ) ) ) %>%
    group_by( metric, model, queen ) %>%
    summarise( Ev = sqrt( mean( value^2 ) ),
               Q1 = quantile( value, 0.25 ),
               Q3 = quantile( value, 0.75 ),
               .groups = "drop" ) 
  
  return( agg_stat )
}
