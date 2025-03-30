
aggregate_time_by_queen = function( performance_stats ) {
  
  stopifnot( "queen" %in% colnames(performance_stats) )
  
  agg_stat = performance_stats %>% 
    filter( metric == "runtime") %>%
    pivot_longer( cols = any_of( ALL_MODELS ),
                  names_to = "model",
                  values_to = "value" ) %>%
    
    group_by( model, queen ) %>%
    summarise( runtime =mean( value ),
               .groups = "drop")
  
  return( agg_stat )
}



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

