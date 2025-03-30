#' scenario_trail::  plot the movement between two scenarios. Requires 'all_scenarios' object.
#'
#' @param scen_dataset dataset to filter from all_scenarios
#' @param scen_outcome outcome to filter from all_scenarios
#' @param scen_train_set_size train set size to filter from all_scenarios
#' @param scen_cov_set_size cov set size to filter from all_scenarios
#' @param additional_filter_var additional filter to use (cov_set_size or train_set_size)
#' @param additional_filter_val value of additional filter to use (e.g. "small" for cov_set_size)
#' @param trail_var  trail variable to use (cov_set_size or train_set_size)
#' @param trail_val_from "from" value of trail variable to use (e.g. "small" for cov_set_size)
#' @param trail_val_to "to" value of trail variable to use (e.g. "large" for cov_set_size)
#' 
#' 
#' 
#' @return outputs ggplot with the figure
#'
#' @export
#' 
#
scenario_trail = function(scen_dataset, scen_outcome, additional_filter_var, additional_filter_val, trail_var, trail_val_to, trail_val_from){ 
  scen_max = scenario_maximums %>% filter(dataset == scen_dataset & outcome == scen_outcome)%>% select(max_agg) %>% unlist()
  max_bias =  scenario_maximums %>% filter(dataset == scen_dataset & outcome == scen_outcome)%>% select(max_bias_agg) %>% unlist()
  max_se = scenario_maximums %>% filter(dataset == scen_dataset & outcome == scen_outcome)%>% select(max_se_agg) %>% unlist()
  breaks = unlist(scenario_maximums%>%filter(dataset == scen_dataset & outcome == scen_outcome)%>% select(breaks_agg))
  
  #use scen_max to create euclidean_distance
  #TODO: hard-coding this to .4 for now, change to be dynamic
  scen_max_bias = .4 
  scen_max_se = .4 
  euclidean_distance=make_euclidean_distance(scen_max_bias,scen_max_se)
  
  usedata = all_scenarios_agg %>%
    filter(dataset ==scen_dataset)%>%
    filter(outcome ==scen_outcome)%>%
    filter(get(additional_filter_var) == additional_filter_val)
  
    d_b = make_trail_set( filter( usedata,
                                  get(trail_var)==trail_val_from),
                          filter( usedata, 
                                  get(trail_var)==trail_val_to ) )
  
  
  pltB <-  ggplot( no_core( d_b ), aes(se, bias) ) +
    contour_plot_base( max_bias = max_bias, max_se = max_se, include_scales = FALSE ) +
    geom_segment( data= d_b, aes(x = se_pre, y = bias_pre, xend = se, yend = bias),
                  lwd = 1, col="lightgrey") +
     geom_point( data = no_core( d_b ), aes(color = type), size = 4, shape=19 ) +
     geom_point( data = only_core( d_b ), aes(pch=model), col="darkgrey", fill="darkgrey", size=4 ) +
     scale_shape_manual(breaks = c( "ATE", "OLS S", "LASSO INF", "RF INF" ),
                        values = c( 15, 23, 25, 24 ) ) +
     guides(color = guide_legend(title = "Model Type"),
            shape = guide_legend(title = "Reference") ) +
  labs(
    title = paste("Trail Plot of Aggregated Bias and SE\n for Each Algorithm \n Aggregated Across Queens \n as " , trail_var, " moves from ", trail_val_from, " to " , trail_val_to)
  )
    # removing this to avoid warning about "Coordinate System Already Present"
    # coord_fixed( xlim=c(0, max_se) ) +
     theme( legend.position = "none" )
  
  pltB
  

  pltB + ggrepel::geom_text_repel(
    aes(label = modelshort, col = type),
    size = 3, # Increased label size
    max.overlaps = Inf,
    min.segment.length = Inf,
    show.legend = FALSE
  )
}


