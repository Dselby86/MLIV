
##create function ggplotgroupby 
#' ggplotgroupby:: A function to group by a metric and plot that metric using callggplot
#' 
#' @param usedata The simulation set to use for plotting
#' @param usemetric The metric we are looking at for these plots 
#' @param plottype type of plot, defaults to boxplot
#' @param title1 The first line of the title
#' @param title2 The second line of the title, leave NULL if no second line
#'
#' @return outputs ggplot of grouped metric
#'
#' @export

ggplotgroupby = function(usedata, 
                         usemetric, 
                         plottype = geom_boxplot(), 
                         title1, 
                         title2 = NULL){
  
  # only use these for diagnosing! removing here. 
  # usedata = pL_asap_small_2_50_no
  # usemetric = "|bias|"
  # plottype = geom_boxplot()
  # title1="7.1. Bias by simulation model, by queen type"
  # Filter data and restrict to certain columns
  usedata$value = usedata$Ev
  filtered_data = usedata %>%
    dplyr::select(metric, queen, model, value) %>%
    filter(metric == usemetric)
  # Group values by simulation type (RF, Lasso, etc) 
  print(head(filtered_data))
  ggplotgroupby_RF <- filtered_data %>%
    filter(queen %in% c("RF INF", "RF CMR", "RF MOM IPW", "RF MOM DR", "CF", "CF LC")) %>%
    group_by(metric, model) %>%
    summarize(value = mean(value, na.rm = TRUE))
  ggplotgroupby_RF$queen = "Average RF"
  
  ggplotgroupby_OLS <- filtered_data %>%
    filter(queen %in% c("OLS"))
  
  ggplotgroupby_ATE <- filtered_data %>%
    filter(queen %in% c("ATE"))
  
  ggplotgroupby_Lasso <- filtered_data %>%
    filter(queen %in% c("LASSO INF","LASSO CMR","LASSO MOM IPW","LASSO MOM DR", "LASSO MCM","LASSO MCM EA","LASSO RL")) %>%
    group_by(metric, model) %>%
    summarize(value = mean(value, na.rm = TRUE))
  
  ggplotgroupby_Lasso$queen = "Average Lasso"
  
  # Combine output and plot  
  ggplotgroupby_by_queen_group = rbind(ggplotgroupby_ATE, ggplotgroupby_OLS, ggplotgroupby_RF, ggplotgroupby_Lasso)
  print(ggplotgroupby_by_queen_group, n = 200)
  # print(head(ggplotgroupby_by_queen_group))
  callggplot(data= ggplotgroupby_by_queen_group, # data to plot
             plottype=plottype,               # plot type
             excludeCountMetric= usemetric,               #  x Maximum
             xMaximum= 10,               #  x Maximum
             title1= title1,                       # title 1
             title2= NULL)                       # title 2
  
  # Change outlier interval here to produce more helpful charts
  outliermin = 0.05
  outliermax = 0.95
  # callggplot(data=ggplotgroupby_by_queen_group, # data to plot
  #            plottype=plottype,               # plot type
  #            additional_params=scale_y_continuous(limits = quantile(ggplotgroupby_by_queen_group$value, c(outliermin,outliermax))) ,   # additional parameters, if any        
  #            title1 = title1,                       # title 1
  #            title2 = paste0("Outliers removed (n = ", prettyNum(dim(pL_asap_small_2_50_no)[1],  big.mark = ","), ", kept percentiles ",100*outliermin," to "  , 100*outliermax,") "))  #title 2
  
}

