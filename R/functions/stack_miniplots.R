#' stack_miniplots::  stack miniplots for a given dataset and outcome
#'
#' @param scen_dataset dataset to filter from all_scenarios
#' @param scen_outcome outcome to filter from all_scenarios
#' 
#' 
#' @return outputs ggplot with the figure
#'
#' @export
#' 
#' 
#' 
#' 
#' 
stack_miniplots = function(scen_dataset, scen_outcome,scen_train_set_size, scen_cov_set_size){
  
  scen_dataset= "asap"
  scen_outcome = "X16BTMCRET"

#set scen_max for asap outcome 2
  scen_max = scenario_maximums%>%filter(dataset ==scen_dataset & outcome == scen_outcome)%>% select(max_agg) %>% unlist()
  scen_max_bias = scenario_maximums%>%filter(dataset ==scen_dataset & outcome == scen_outcome)%>% select(max_bias_agg) %>% unlist()
  scen_max_se = scenario_maximums%>%filter(dataset == scen_dataset & outcome == scen_outcome)%>% select(max_se_agg) %>% unlist()
  scen_breaks = scenario_maximums%>%filter(dataset == scen_dataset  & outcome == scen_outcome)%>% select(breaks_agg) %>% unlist()
  scen_breaks = unname(scen_breaks)
#use scen_max to create euclidean_distance
euclidean_distance=make_euclidean_distance(scen_max_bias,scen_max_se)

# Adjusting your plot
# Define shapes for each model type

train_size_shape_map = c("1000"=9, "2000"=2, "5000"=3)
cov_set_size_color_map = c("small"="blue", "medium"="orange", "large" = "#696969")
usedata = all_scenarios_agg %>%
  filter(dataset ==scen_dataset & outcome ==scen_outcome
         #& train_set_size == scen_train_set_size & cov_set_size == scen_cov_set_size
         ) %>%
  mutate(train_set_size =as.factor(train_set_size))

p <- ggplot(usedata, aes(se, bias, xmax=scen_max, ymax=scen_max, breaks=5)) +
  geom_contour(
    data = euclidean_distance,
    aes(x = x, y = y, z = z),
    breaks = scen_breaks,
    alpha = 0.6
  ) +
  geom_point(aes(color =cov_set_size, shape=train_set_size ),  size = 2)  +
  facet_wrap(~ model) +  # Add facet_wrap 
  theme_minimal() +
  scale_x_continuous(limits=c(0,scen_max), breaks=scen_breaks)+
  scale_y_continuous(limits=c(0,scen_max),breaks=scen_breaks)+
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
     title = "Mean of Aggregated Bias and SE across Queens\nby Algorithm with RMSE Contouring",
     subtitle = "* ATE queen is excluded",
     col = "Scenario"
   ) +
   ylab("Bias") +    # Change X-axis title
   xlab("SE")  +     # Change Y-axis title
   scale_color_manual(values=cov_set_size_color_map)+
   scale_shape_manual(values = train_size_shape_map) +
   guides(color = guide_legend(title = "Covariate Set Size"),
          shape = guide_legend(title = "Train Set Size"))
  print(p)
}
