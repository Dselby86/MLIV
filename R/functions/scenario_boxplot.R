
#' scenario_boxplot::  make a plot of all queens by estimator. Requires saved 'all_scenarios' object. 
#'
#' @param scen_dataset dataset to filter from all_scenarios
#' @param scen_outcome outcome to filter from all_scenarios
#' @param scen_train_set_size train set size to filter from all_scenarios
#' @param scen_cov_set_size cov set size to filter from all_scenarios
#' @param metric metric to use in the boxplot
#' 
#' 
#' @return outputs ggplot with the figure
#'
#' @export
#' 
#' 
scenario_boxplot = function(scen_dataset, scen_outcome, scen_train_set_size, scen_cov_set_size, metric){
  usedata = all_scenarios %>%
    filter(dataset ==scen_dataset)%>%
    filter(outcome ==scen_outcome)%>%
    filter(cov_set_size ==scen_cov_set_size)%>%
    filter(train_set_size ==scen_train_set_size)
  
  usedata$queentext = paste("Queen: ", usedata$queen)
  
  gg = ggplot(usedata, aes(model, get(metric))) +
    geom_boxplot(outlier.shape = NA, col = "black") +
    geom_point(aes(text = queentext, fill=queen)) +
    scale_y_continuous(limits=c(0,0.25))+
    coord_flip() +
    labs(title = paste0(metric, " (across queens)"), y=metric) 
   
   # scale_color_manual(values = TYPE_COLOR_MAP) +
   # theme(legend.position = "right")
  
  # Convert ggplot to plotly
  interactive_plot <- ggplotly(gg, tooltip = c("queentext"))
  
  # Display the interactive plot
  interactive_plot
  
}
