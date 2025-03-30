#' scenario_allqueens::  plot the results from each queen for a given model and scenario. Requires 'all_scenarios' object.
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
#
scenario_allqueens = function(scen_dataset, scen_outcome, scen_train_set_size, scen_cov_set_size){ 
  scen_max = scenario_maximums %>% filter(dataset == scen_dataset & outcome == scen_outcome)%>% select(max_agg) %>% unlist()
  max_bias =  scenario_maximums %>% filter(dataset == scen_dataset & outcome == scen_outcome)%>% select(max_bias_agg) %>% unlist()
  max_se = scenario_maximums %>% filter(dataset == scen_dataset & outcome == scen_outcome)%>% select(max_se_agg) %>% unlist()
  breaks = unlist(scenario_maximums%>%filter(dataset == scen_dataset & outcome == scen_outcome)%>% select(breaks_agg))
  
  #use scen_max to create euclidean_distance
  #TODO: hard-coding this to .4 for now, change to be dynamic
  scen_max_bias = .4 
  scen_max_se = .4 
  euclidean_distance=make_euclidean_distance(scen_max_bias,scen_max_se)
  
  
  
  
  usedata = all_scenarios %>%
    filter(dataset ==scen_dataset)%>%
    filter(outcome ==scen_outcome)%>%
    filter(cov_set_size ==scen_cov_set_size)%>%
    filter(train_set_size ==scen_train_set_size)
  #START OF THIS
  #NC 1-23-24: add an arrow to indicate outlying CDML values, source: https://stackoverflow.com/questions/29463067/include-indication-of-extreme-outliers-in-ggplot
  #make a label for the outlier value
  valmax = .4
  
  arrowtooutliers <- usedata %>% filter(usedata$bias>valmax) %>%
    group_by(model, bias) %>%
    summarise(outlier_txt=paste0(queen, " (", round(se, 2), "," , round(bias, 2) , ")"),
              xstart=min(se, 0.8*valmax),
              xend  =min(se, valmax),
              ystart=min(bias, 0.8*valmax),
              yend=min(bias, 0.99* valmax),
              .groups = "drop")
  #TODO: make text on label smaller
  
  outlier_label = arrowtooutliers$outlier_txt

  #remove the outlier data before plotting to prevent warnings
    usedata_lim = usedata[usedata$se<=valmax&usedata$bias<=valmax,]
    p <- ggplot(usedata_lim, aes(se, bias, xmax=valmax, ymax=valmax)) +
    geom_contour(
      data = euclidean_distance,
      aes(x = x, y = y, z = z),
      breaks = breaks,
      alpha = 0.6
    ) +
    geom_point(aes(color = queen, shape=queen),  size = 2) +
    scale_x_continuous(limits=c(0,valmax )) + #scen_max_bias))+
    scale_y_continuous(limits=c(0, valmax ))+ #scen_max_se))+ #adding this to remove some outlier CDML
    facet_wrap(~model) +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      text = element_text(color = "black"),
      # plot.title = element_text(size = 16),
      # plot.subtitle = element_text(size = 12),
      # axis.title = element_text(size = 14),
      # legend.title = element_text(size = 14),
      # legend.text = element_text(size = 12),
      # strip.text = element_text(size =8),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
    ) +
    labs(
      title = "Aggregated Bias and SE by Queen\nfor Each Scenario with RMSE Contouring"
    ) +
    ylab("Bias") +    # Change X-axis title
    xlab("SE")  +     # Change Y-axis title
    scale_shape_manual(values = SHAPE_MAP) +
      #add an arrow to the outlier
      geom_text(data=arrowtooutliers, aes(x=xstart*.9, y=ystart*.95, label=outlier_label, color="2", hjust=0))+
      geom_segment(data=arrowtooutliers, aes(y=ystart,yend=yend, x=xstart, xend =xend, color="2"),arrow = arrow(length = unit(0.2,"cm")), show.legend = FALSE)
    
    guides(color = guide_legend(title = "Queen"),
           shape = guide_legend(title = "Queen"))
  print(p)
  
  # ggsave(here::here(paste(sep="_","graphs/combined/allqueens",scen_dataset, scen_outcome, scen_train_set_size, scen_cov_set_size, ".png")), p, width = 8, height = 6, units = "in", bg = 'white', limitsize=FALSE)
}
