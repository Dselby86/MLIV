
#' make_trail_set:: make a set used in creating trail plots
#' 
#' @param from_set dataset to move 'from' in trail plot
#' @param to_set dataset to move 'to' in trail plot
#' 
#' @return outputs dataset to use for the figure
#' 
#' @export
#' 
#' 
#' 
#' 
# For making trail plots ----
make_trail_set <- function( from_set, to_set ) {
  
  d_b = to_set %>%
    ungroup() %>%
    #dplyr::select( -c( set_id:train_set_size ) ) %>%
    left_join( dplyr::select( ungroup( from_set ), model, bias, se ), 
               by = "model",
               suffix = c( "", "_pre" ) )
  
  d_b
}


