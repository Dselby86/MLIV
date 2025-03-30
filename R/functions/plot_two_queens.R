#' plot_two_queens:  plot certain queens only
#'
#' @param scen_dataset dataset to filter from all_scenarios
#' @param scen_outcome outcome to filter from all_scenarios
#' @param queen1 first queen you want to plot
#' @param queen2 second queen you want to plot
#' 
#' 

plot_two_queens = function(scen_dataset, scen_outcome, queen1 , queen2){
  
  # Adjusting your plot
  usedata = all_scenarios %>%
    filter(dataset ==scen_dataset) %>%
    filter(outcome ==scen_outcome) %>%
    filter(queen %in% c(queen1, queen2))
  
  usedata$queensize = paste0(usedata$queen, ", ", usedata$train_set_size)
  
  #create combined_shape_map for the two queens
  combined_shape_map <- setNames(c(0, 1, 2, 15, 16, 17), 
                                 c(paste0(queen1, ", 1000"), 
                                   paste0(queen1, ", 2000"), 
                                   paste0(queen1, ", 5000"), 
                                   paste0(queen2, ", 1000"), 
                                   paste0(queen2, ", 2000"), 
                                   paste0(queen2, ", 5000")))
  
  cov_set_size_color_map = c("small"="blue", "medium"="orange", "large" = "#696969")
  
  
  scen_max = scenario_maximums%>%filter(dataset == scen_dataset & outcome == scen_outcome)%>% select(max) %>% unlist()
  scen_max_bias = scenario_maximums%>%filter(dataset == scen_dataset & outcome == scen_outcome)%>% select(max_bias) %>% unlist()
  scen_max_se = scenario_maximums%>%filter(dataset == scen_dataset & outcome == scen_outcome)%>% select(max_se) %>% unlist()
  scen_breaks = scenario_maximums%>%filter(dataset == scen_dataset & outcome == scen_outcome)%>% select(breaks) %>% unlist()
  scen_breaks = unname(scen_breaks)
  #use scen_max to create euclidean_distance
  euclidean_distance=make_euclidean_distance(scen_max_bias,scen_max_se)
  
  p <- ggplot(usedata, aes(se, bias, xmax=scen_max_bias, ymax=scen_max_se, breaks=5)) +
    geom_contour(
      data = euclidean_distance,
      aes(x = x, y = y, z = z),
      breaks =scen_breaks,
      alpha = 0.6
    ) +
    geom_point(aes(color =cov_set_size, shape=queensize ),  size = 2)  +
    facet_wrap(~ model) +  # Add facet_wrap 
    theme_minimal() +
    scale_x_continuous(limits=c(0,scen_max_bias), breaks=scen_breaks)+
    scale_y_continuous(breaks=scen_breaks)+
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      text = element_text(color = "black"),
      plot.title = element_text(size = 12),
      plot.subtitle = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      strip.text = element_text(size =8),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
    ) +
    labs(
      title = "Aggregated Bias and SE\nby Algorithm with RMSE Contouring",
      subtitle = paste0(queen1, " and ", queen2, " queens only"),
      col = "Scenario"
    ) +
    ylab("Bias") +    # Change X-axis title
    xlab("SE")  +     # Change Y-axis title
    scale_color_manual(values = cov_set_size_color_map) +
    scale_shape_manual(values = combined_shape_map) +
    guides(color = guide_legend(title = "Covariate Set Size"),
           shape = guide_legend(title = "Train Set Size"))
  
  p
  
  # ggsave(here::here(paste0("graphs/combined/ATECDMLonly.png")), p, width = 8, height = 6, units = "in", bg = 'white', limitsize=FALSE)
}