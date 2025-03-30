
#' scenario_poorlyest::  make a plot of the percent poorly estimated for a given scenario. Requires saved 'all_scenarios_agg' object. 
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
scenario_poorlyest = function(scen_dataset, scen_outcome, scen_train_set_size, scen_cov_set_size){
  usedata = all_scenarios %>%
    filter(dataset ==scen_dataset)%>%
    filter(outcome ==scen_outcome)%>%
    filter(cov_set_size ==scen_cov_set_size)%>%
    filter(train_set_size ==scen_train_set_size)
  
  usedata$queentext = paste("Queen: ", usedata$queen)
  
  gg = ggplot( usedata, aes( model, percent_cut ) ) +
    geom_boxplot( outlier.shape = NA, col = "black", fill="lightgrey" ) +
    # geom_jitter( width = 0.1, size=2, col="black" ) +
    # need to use suppressWarnings() here because geom_point doesn't directly support text, but it's needed for tooltip below
    suppressWarnings(geom_point(aes(text = queentext))) +
    theme_bw() +
    coord_flip() +
    labs( title = "Percent of test units that are not well estimated (across queens)")
  
  # Convert ggplot to plotly
  interactive_plot <- ggplotly(gg,tooltip = c("queentext"))
  
  # Display the interactive plot
  interactive_plot
  
}
