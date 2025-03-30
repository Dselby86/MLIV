



# Make contour plot for single queen

source( here::here( "graphs/load_simulation_results.R" ) )

source( here::here( "graphs/contour_plot_code.R" ) )


# Make contour plot ----

df_set_1 <- filter( df_agg, set_id == "ca-1-large-1000" )
df_set_2 <- filter( df_agg, set_id == "ca-1-large-2000" )

max_bias = max( df_set_1$bias, df_set_2$bias )
max_se = max( df_set_1$se, df_set_2$se )

if ( FALSE ) {
  
  # old labeling style
  
  p1 <- make_contour_plot( df_set_1, max_bias=max_bias, max_se=max_se )
  p1
  
  p2 <- make_contour_plot( df_set_2, max_bias=max_bias, max_se = max_se )
  p2
  
  
  
  
  # Alternate labeling approaches? ----
  
  p1B <- make_contour_plot( df_set_1, max_bias=max_bias, max_se=max_se, add_labels = FALSE )
  p1B + ggrepel::geom_text_repel(
    aes(label = modelshort, col = type),
    size = 2, # Increased label size
    max.overlaps = Inf,
    show.legend = FALSE
  )
  
}

gbs = filter( df_set_1, model %in% c( "ATE", "OLS S", "LASSO INF", "RF INF" ) )
gbs$model

p1C <- ggplot( no_core( df_set_1 ), aes(se, bias, xmax=max_se, ymax=max_bias) ) +
  contour_plot_base( max_bias = max_bias, max_se = max_se ) +
  geom_point( aes(color = type), size = 4, shape=19 ) +
  geom_point( data = only_core( df_set_1 ), aes(pch=model), col="darkgrey", fill="darkgrey", size=4 ) +
  #  geom_point( data = gbs, size = 7, shape = 1, col="red" ) +
  scale_shape_manual(breaks = c( "ATE", "OLS S", "LASSO INF", "RF INF" ),
                     values = c( 15, 23, 25, 24 ) ) +
  guides(color = guide_legend(title = "Model Type"),
         shape = guide_legend(title = "Reference") )
p1C


p1C + ggrepel::geom_text_repel(
  aes(label = modelshort, col = type),
  size = 3, # Increased label size
  max.overlaps = Inf,
  min.segment.length = Inf,
  show.legend = FALSE
)

ggsave( here::here( "graphs/possible_clean_plot_1.pdf" ) )




# Trying out animation ----

library(ggplot2)
library(gganimate)

df_range = bind_rows( set1 = df_set_1, set2 = df_set_2, .id = "set" ) %>%
  dplyr::select( -n, -rmse ) %>%
  group_by( model, type, modelshort )

# Plot and animate

p <- ggplot( df_range, aes(se, bias, xmax=max_se, ymax=max_bias) ) +
  geom_point(aes(color = type, shape=type),  size = 4)  +
  transition_states( set, transition_length = 2, state_length = 1, wrap=FALSE) +
  ease_aes('linear') +
  labs(title = "Scenario: {closest_state}") +
  contour_plot_base( max_bias = max_bias, max_se = max_se )
#+
#  theme( legend.position = "none" )

p


anim <- animate(p, duration = 5, fps = 10, width = 600, height = 400)

gganimate::anim_save( here::here( "graphs/initial_animation.gif") )


# Trying with alternate labeling
p <- ggplot( no_core( df_range ), aes(se, bias, xmax=max_se, ymax=max_bias) ) +
  geom_point( aes(color = type),  size = 4)  +
  contour_plot_base( max_bias = max_bias, max_se = max_se ) +
  geom_point( data = only_core( df_range ), aes(pch=model), col="darkgrey", fill="darkgrey", size=4 ) +
  #  geom_point( data = gbs, size = 7, shape = 1, col="red" ) +
  scale_shape_manual(breaks = c( "ATE", "OLS S", "LASSO INF", "RF INF" ),
                     values = c( 15, 23, 25, 24 ) ) +
  guides(color = guide_legend(title = "Model Type"),
         shape = guide_legend(title = "Reference") ) +
  transition_states( set_id, transition_length = 2, state_length = 1, wrap=FALSE) +
  ease_aes('linear') +
  labs(title = "Scenario: {closest_state}")

p

gganimate::anim_save( here::here( "graphs/change_train_animation.gif") )




if ( FALSE ) {
  # This isn't working
  
  
  # Extract the first frame
  first_frame <- anim::anim_save(anim, start_pause = 1, nframes = 1)
  
  # Extract the last frame
  last_frame <- anim_save(anim, end_pause = 1, nframes = 1)
  
  # Add labels to the first frame
  first_frame_labeled <- ggplot_build(first_frame) +
    ggrepel::geom_label_repel(
      aes( label = after_stat(ifelse(set %in% c("set1", "set2"), modelshort, NA)), color = type),
      size = 3, # Increased label size
      box.padding = 0.75,
      point.padding = 0.25,
      max.overlaps = Inf,
      show.legend = FALSE
    )   
  
  geom_label_repel(aes(label = modelshort, color = type), size = 5)
  
  # Add labels to the last frame
  last_frame_labeled <- ggplot_build(last_frame) +
    geom_label_repel(aes(label = modelshort, color = type), size = 5)
  
  # You can now save or display these labeled frames
  ggsave("first_frame_with_labels.png", plot = first_frame_labeled)
  ggsave("last_frame_with_labels.png", plot = last_frame_labeled)
  
  
  
  
}



# Animating across all queens ----


library(ggplot2)
library(gganimate)

table( df$set_id )
df_queen <- df %>% 
  group_by( model, modelshort, type, baseline, queen ) %>%
  summarise( n = n(),
             bias = sqrt( mean(bias^2 ) ),
             se = sqrt( mean( se^2 ) ),
             rmse = sqrt( mean( rmse^2 ) ), .groups = "drop" ) %>%
  group_by( model, modelshort, type )
df_queen


# Plot and animate

p <- ggplot( no_core( df_queen ), aes(se, bias, xmax=max_se, ymax=max_bias) ) +
  geom_point( aes(color = type),  size = 4)  +
  contour_plot_base( max_bias = max_bias, max_se = max_se ) +
  geom_point( data = only_core( df_queen ), aes(pch=model), col="darkgrey", fill="darkgrey", size=4 ) +
  #  geom_point( data = gbs, size = 7, shape = 1, col="red" ) +
  scale_shape_manual(breaks = c( "ATE", "OLS S", "LASSO INF", "RF INF" ),
                     values = c( 15, 23, 25, 24 ) ) +
  guides(color = guide_legend(title = "Model Type"),
         shape = guide_legend(title = "Reference") ) +
  transition_states( queen, transition_length = 2, state_length = 1, wrap=TRUE) +
  ease_aes('linear') +
  labs(title = "Performance by queen", 
       subtitle = "Queen: {closest_state}" ) 

p


anim <- animate(p, duration = 10, fps = 10, width = 600, height = 400)

gganimate::anim_save( here::here( "graphs/queen_animation.gif") )







# "Trails from last plot" plot ----



d_agg <- df %>% 
  group_by( model, modelshort, type, baseline, train_set_size ) %>%
  summarise( n = n(),
             bias = sqrt( mean(bias^2 ) ),
             se = sqrt( mean( se^2 ) ),
             rmse = sqrt( mean( rmse^2 ) ), .groups = "drop" )

d_b = make_trail_set( filter( d_agg, train_set_size == 1000),
                      filter( d_agg, train_set_size == 2000) )

plt <-  ggplot( d_b, aes(se, bias, xmax=max_se, ymax=max_bias) ) +
  geom_segment(aes(x = se_pre, y = bias_pre, xend = se, yend = bias),
               arrow = arrow(length = unit(0.2, "cm")), lwd = 1, col="darkgrey") +
  geom_point(aes(color = type, shape=type, size = as.numeric(3+1*baseline) ), fill="grey" ) + 
  contour_plot_base( max_bias = max_bias, max_se = max_se ) +
  labs( title = "Trails of Models from 1000 to 2000" ) + 
  scale_size_identity() +
  guides( size="none" )
plt


# A second version

pltB <-  ggplot( d_b, aes(se, bias, xmax=max_se, ymax=max_bias) ) +
  #geom_point(aes(color = type, shape=type, size = as.numeric(4+2*baseline) ), fill="grey" ) + 
  contour_plot_base( max_bias = max_bias, max_se = max_se ) +
  geom_segment(aes(x = se_pre, y = bias_pre, xend = se, yend = bias),
               #arrow = arrow(length = unit(0.2, "cm")),
               lwd = 1, col="lightgrey") +
  geom_point( data = no_core( d_b ), aes(color = type), size = 4, shape=19 ) +
  geom_point( data = only_core( d_b ), aes(pch=model), col="darkgrey", fill="darkgrey", size=4 ) +
  labs( title = "Trails of Models from 1000 to 2000 training size",
        subtitle = "Aggregated over scenarios and queens" ) + 
  scale_size_identity() +
  scale_shape_manual(breaks = c( "ATE", "OLS S", "LASSO INF", "RF INF" ),
                     values = c( 15, 23, 25, 24 ) ) +
  guides(color = guide_legend(title = "Model Type"),
         shape = guide_legend(title = "Reference"),
         size="none" )
pltB



pltB + ggrepel::geom_text_repel(
  aes(label = modelshort, col = type),
  size = 3, # Increased label size
  max.overlaps = Inf,
  min.segment.length = Inf,
  show.legend = FALSE
)


ggsave( here::here( "graphs/trail_1000_to_2000_training_set.pdf" ) )




# Small to Large covariate trace plot ----


d_agg <- df %>% 
  group_by( model, modelshort, type, baseline, cov_set_size ) %>%
  summarise( n = n(),
             bias = sqrt( mean(bias^2 ) ),
             se = sqrt( mean( se^2 ) ),
             rmse = sqrt( mean( rmse^2 ) ), .groups = "drop" )

d_b = make_trail_set( filter( d_agg, cov_set_size == "small" ),
                      filter( d_agg, cov_set_size == "large" ) )


pltB <-  ggplot( d_b, aes(se, bias, xmax=max_se, ymax=max_bias) ) +
  #geom_point(aes(color = type, shape=type, size = as.numeric(4+2*baseline) ), fill="grey" ) + 
  contour_plot_base( max_bias = max_bias, max_se = max_se ) +
  geom_segment(aes(x = se_pre, y = bias_pre, xend = se, yend = bias),
               #arrow = arrow(length = unit(0.2, "cm")),
               lwd = 1, col="lightgrey") +
  geom_point( data = no_core( d_b ), aes(color = type), size = 4, shape=19 ) +
  geom_point( data = only_core( d_b ), aes(pch=model), col="darkgrey", fill="darkgrey", size=4 ) +
  labs( title = "Trails from small to large covariate set sizes",
        subtitle = "Aggregated over scenarios and queens" ) + 
  scale_size_identity() +
  scale_shape_manual(breaks = c( "ATE", "OLS S", "LASSO INF", "RF INF" ),
                     values = c( 15, 23, 25, 24 ) ) +
  guides(color = guide_legend(title = "Model Type"),
         shape = guide_legend(title = "Reference"),
         size="none" )
pltB



pltB + ggrepel::geom_text_repel(
  aes(label = modelshort, col = type),
  size = 3, # Increased label size
  max.overlaps = Inf,
  min.segment.length = Inf,
  show.legend = FALSE
)


ggsave( here::here( "graphs/trail_small_to_large_covariate_set.pdf" ) )





# ATE -> CDML trace plot ----

df_queen

d_b = make_trail_set( filter( df_queen, queen == "ATE" ),
                      filter( df_queen, queen == "CDML" ) )

pltB <-  ggplot( d_b, aes(se, bias, xmax=max_se, ymax=max_bias) ) +
  #geom_point(aes(color = type, shape=type, size = as.numeric(4+2*baseline) ), fill="grey" ) + 
  contour_plot_base( max_bias = max_bias, max_se = max_se ) +
  geom_segment(aes(x = se_pre, y = bias_pre, xend = se, yend = bias),
               #arrow = arrow(length = unit(0.2, "cm")),
               lwd = 1, col="lightgrey") +
  geom_point( data = no_core( d_b ), aes(color = type), size = 4, shape=19 ) +
  geom_point( data = only_core( d_b ), aes(pch=model), col="darkgrey", fill="darkgrey", size=4 ) +
  geom_point( data = d_b, aes( x=se_pre, y=bias_pre ), color ="black", size = 0.5 ) +
  labs( title = "Trails of ATE queen to CDML queen" ) + 
  scale_size_identity() +
  scale_shape_manual(breaks = c( "ATE", "OLS S", "LASSO INF", "RF INF" ),
                     values = c( 15, 23, 25, 24 ) ) +
  guides(color = guide_legend(title = "Model Type"),
         shape = guide_legend(title = "Reference"),
         size="none" )
pltB



pltB + ggrepel::geom_text_repel(
  aes(label = modelshort, col = type),
  size = 3, # Increased label size
  max.overlaps = Inf,
  min.segment.length = Inf,
  show.legend = FALSE
)

ggsave( here::here( "graphs/trails_ATE_CDML_Queen.pdf" ) )







# All queens for each model

df_queen
220 / 20

df_queen <- mutate( df_queen, is_ATE = ifelse( queen == "ATE", "ATE", "Not ATE" ) )

ggplot( df_queen, aes(se, bias, xmax=300, ymax=300, col=is_ATE ) ) +
  facet_wrap( ~ model ) +
  contour_plot_base( max_bias = max_bias, max_se = max_bias, include_scales = FALSE ) +
  geom_point( size = 2, shape=19, alpha=0.8 ) +
  labs( title = "All queens for all models",
        subtitle = "Averaged across the 6 CA scenarios",
        color = "ATE Queen") + 
  scale_size_identity() +
  scale_color_manual(breaks = c("ATE", "Not ATE"),
                     values = c( "red", "black" ) ) +
  theme(panel.grid.minor = element_blank())
#  scale_shape_manual(breaks = c( "ATE", "OLS S", "LASSO INF", "RF INF" ),
#                     values = c( 15, 23, 25, 24 ) ) +
#  guides(color = guide_legend(title = "Model Type"),
#         shape = guide_legend(title = "Reference"),
#         size="none" )


ggsave( here::here( "graphs/all_models_all_queens.pdf" ) )




# All models for each queen
ggplot( df_queen, aes(se, bias, xmax=350, ymax=max_bias ) ) +
  facet_wrap( ~ queen ) +
  contour_plot_base( max_bias = max_bias, max_se = max_bias, include_scales = FALSE ) +
  geom_point( data = no_core( df_queen ), aes(color = type), size = 2, shape=19 ) +
  geom_point( data = only_core( df_queen ), aes(pch=model), col="darkgrey", fill="darkgrey", size=2 ) +
  labs( title = "All models grouped by queeen",
        subtitle = "Averaged across the 6 CA scenarios",
        color = "ATE Queen") + 
  scale_size_identity() +
  theme(panel.grid.minor = element_blank()) + 
  scale_size_identity() +
  scale_shape_manual(breaks = c( "ATE", "OLS S", "LASSO INF", "RF INF" ),
                     values = c( 15, 23, 25, 24 ) ) +
  guides(color = guide_legend(title = "Model Type"),
         shape = guide_legend(title = "Reference"),
         size="none" )


ggsave( here::here( "graphs/all_models_by_queen.pdf" ) )



df_queen
220 / 20

df <- mutate( df, is_ATE = ifelse( queen == "ATE", "ATE", "Not ATE" ) )

ggplot( df, aes(se, bias, xmax=max_se, ymax=max_bias, col=is_ATE, pch= ) ) +
  facet_wrap( ~ model ) +
  contour_plot_base( max_bias = max_bias, max_se = max_bias, include_scales = FALSE ) +
  geom_point( size = 1, shape=19, alpha=0.8 ) +
  labs( title = "All queens for all models across all CA sets",
        color = "ATE Queen" ) + 
  scale_size_identity() +
  scale_color_manual(breaks = c("ATE", "Not ATE"),
                     values = c( "red", "black" ) ) +
  theme(panel.grid.minor = element_blank())


df_ag <- df %>%
  group_by( model, cov_set_size, train_set_size ) %>%
  summarise( n = n(),
             bias = sqrt( mean(bias^2 ) ),
             se = sqrt( mean( se^2 ) ),
             rmse = sqrt( mean( rmse^2 ) ), .groups = "drop" ) 

df_agL <- df_ag %>%
  pivot_longer( cols = c( bias, se, rmse ), names_to = "type", values_to = "value" )
df_agL

df_ag <- mutate( df_ag,
                 train_set_size = as.factor(train_set_size),
                 cov_set_size = factor(cov_set_size, levels=c("small","medium","large" ) ) )
ggplot( df_ag, aes( cov_set_size, rmse, group = as.factor(train_set_size), col = as.factor(train_set_size) ) ) +
  facet_wrap( ~ model ) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  labs( color = "Training Set" )

ggsave( here::here( "graphs/performance_across_sets_rmse.pdf" ), width = 8, height = 5 )

ggplot( df_ag, aes( cov_set_size, bias, group = as.factor(train_set_size), col = as.factor(train_set_size) ) ) +
  facet_wrap( ~ model ) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  labs( color = "Training Set Size" )
ggsave( here::here( "graphs/performance_across_sets_bias.pdf" ), width = 8, height = 5 )

ggplot( df_ag, aes( cov_set_size, se, group = as.factor(train_set_size), col = as.factor(train_set_size) ) ) +
  facet_wrap( ~ model ) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  labs( color = "Training Set Size" )
ggsave( here::here( "graphs/performance_across_sets_bias.pdf" ), width = 8, height = 5 )



