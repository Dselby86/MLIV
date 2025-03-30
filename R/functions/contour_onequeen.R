#' contour_onequeen::  plot the results of models including reference models
#' 
#' @param scen_dataset dataset to filter from all_scenarios
#' @param scen_outcome outcome to filter from all_scenarios
#' @param scen_train_set_size train set size to filter from all_scenarios
#' @param scen_cov_set_size cov set size to filter from all_scenarios
#' @param plotqueen queen you want to plot
#' 
#' 
#' @return outputs ggplot with the figure
#'
#' @export
#' 
#
contour_onequeen = function(scen_dataset, scen_outcome, scen_train_set_size, scen_cov_set_size, plotqueen){ 
  scen_max = scenario_maximums %>% filter(dataset == scen_dataset & outcome == scen_outcome)%>% select(max_agg) %>% unlist()
  max_bias =  scenario_maximums %>% filter(dataset == scen_dataset & outcome == scen_outcome)%>% select(max_bias_agg) %>% unlist()
  max_se = scenario_maximums %>% filter(dataset == scen_dataset & outcome == scen_outcome)%>% select(max_se_agg) %>% unlist()
  breaks = unlist(scenario_maximums%>%filter(dataset == scen_dataset & outcome == scen_outcome)%>% select(breaks_agg))
  #use max_bias and max_se to create euclidean_distance
  euclidean_distance=make_euclidean_distance(max_bias,max_se)
  
  usedata = all_scenarios %>%
    filter(dataset ==scen_dataset)%>%
    filter(outcome ==scen_outcome)%>%
    filter(cov_set_size ==scen_cov_set_size)%>%
    filter(train_set_size ==scen_train_set_size)%>%
    mutate(baseline = model %in% c( "ATE", "OLS S", "LASSO INF", "RF INF" ))
  
  # Plot of just the one queen ----
  dd = filter( usedata,  cov_set_size == scen_cov_set_size, train_set_size == scen_train_set_size, queen == plotqueen )
  p1C <- ggplot( no_core( dd ), aes(se, bias) ) +
    contour_plot_base( max_bias = max_bias, max_se = max_se, breaks = breaks ) +
    geom_point( aes(color = type), size = 4, shape=19 ) +
    geom_point( data = only_core( dd ), aes(pch=model), col="darkgrey", fill="darkgrey", size=4 ) +
    #  geom_point( data = gbs, size = 7, shape = 1, col="red" ) +
    scale_shape_manual(breaks = c( "ATE", "OLS S", "LASSO INF", "RF INF" ),
                       values = c( 15, 23, 25, 24 ) ) +
    guides(color = guide_legend(title = "Model Type"),
           shape = guide_legend(title = "Reference") ) 
   # coord_fixed( xlim=c(0, max_se) ) 
   # theme( legend.position = "none" )
  print(p1C)
}
