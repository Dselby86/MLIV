#' scenario_plot::  make a plot for a given scenario. Requires saved 'all_scenarios_agg' object. 
#'
#' @param scen_dataset dataset to filter from all_scenarios
#' @param scen_outcome outcome to filter from all_scenarios
#' @param scen_train_set_size train set size to filter from all_scenarios
#' @param scen_cov_set_size cov set size to filter from all_scenarios
#' 
#' 
#' @return outputs ggplot with the figure
#'
#' @export
#' 
#' 
scenario_plot = function(scen_dataset, scen_outcome, scen_train_set_size, scen_cov_set_size){
  
    usedata = all_scenarios_agg %>%
    filter(dataset ==scen_dataset)%>%
    filter(outcome ==scen_outcome)%>%
    filter(cov_set_size ==scen_cov_set_size)%>%
    filter(train_set_size ==scen_train_set_size)
  
  scen_max = scenario_maximums %>% filter(dataset == scen_dataset & outcome == scen_outcome)%>% select(max_agg) %>% unlist()
  max_bias =  scenario_maximums %>% filter(dataset == scen_dataset & outcome == scen_outcome)%>% select(max_bias_agg) %>% unlist()
  max_se = scenario_maximums %>% filter(dataset == scen_dataset & outcome == scen_outcome)%>% select(max_se_agg) %>% unlist()
  breaks = unlist(scenario_maximums%>%filter(dataset == scen_dataset & outcome == scen_outcome)%>% select(breaks_agg))
  
  #use scen_max to create euclidean_distance
  euclidean_distance=make_euclidean_distance(max_bias,max_se)
  
  #create the plot itself
  p <- ggplot(usedata, aes(se, bias, xmax=scen_max, ymax=scen_max)) +
    geom_contour(
      data = euclidean_distance,
      aes(x = x, y = y, z = z),
      breaks = c(breaks),
      alpha = 0.6
    ) +
    geom_point(aes(color = type, shape=type),  size = 4) +
    geom_label_repel(
      aes(label = modelshort, col = type),
      size = 3, # Increased label size
      box.padding = 0.75,
      point.padding = 0.25,
      max.overlaps = Inf,
      show.legend = FALSE
    ) +
    scale_x_continuous(limits=c(0,scen_max))+
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      text = element_text(color = "black"),
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12)
    ) +
    labs(
      title = "Mean of Aggregated Bias and SE across Queens\nfor Each Algorithm with RMSE Contouring",
      subtitle = "* ATE queen is excluded",
      col = "Model Type"
    ) +
    ylab("Bias") +    # Change X-axis title
    xlab("SE")  +     # Change Y-axis title
    scale_shape_manual(values = TYPE_SHAPE_MAP) +
    guides(color = guide_legend(title = "Model Type"),
           shape = guide_legend(title = "Model Type"))
  
  print(p)
  
  
}