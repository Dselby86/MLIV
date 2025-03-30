
# Functions for making contour plots



# Function to subset to baseline models or main models ----


no_core <- function( df_set ) {
  #df_set %>% filter( !model %in% c( "ATE", "OLS S", "LASSO INF", "RF INF" ) )
  df_set %>% filter( !baseline )
}
only_core <- function( df_set ) {
  #df_set %>% filter( model %in% c( "ATE", "OLS S", "LASSO INF", "RF INF" ) )
  df_set %>% filter( baseline )
}



# Function to make RMSE contour plots ----

TYPE_SHAPE_MAP <- c(
  "ATE" = 20, "OLS S" = 18, 
  "INF" = 2, "RF" = 3, "CDML" = 8,
  "LASSO" = 10, "SL" = 16,
  "XGBOOST" = 1, "BART" = 0
)

TYPE_COLOR_MAP <- c(
  "ATE" = "black", "OLS S" = "black", 
  "INF" = "darkgrey", "RF" = "#E69F00", "CDML" = "#F0E442",
  "LASSO" = "#009E73", "SL" = "#D55E00",
  "XGBOOST" = "#CC79A7", "BART" = "#0072B2" # "#56B4E9"
)
LEGEND_COLORS = setdiff( names(TYPE_COLOR_MAP), c("LASSO INF", "RF INF", "ATE", "OLS S") )

# For individual methods
SHAPE_MAP <- c(
  "ATE" = 0, "OLS S" = 1, 
  "RF INF" = 2, "RF T" = 3, "RF MOM IPW" = 4, "RF MOM DR" = 5, 
  "CF" = 6, "CF LC" = 7, "CDML" = 8, "LASSO INF" = 9, "LASSO T" = 10, "LASSO MOM IPW" = 11, 
  "LASSO MOM DR" = 12, "LASSO MCM" = 13, "LASSO MCM EA" = 14, "LASSO R" = 15, 
  "SL T" = 16, "SL S" = 17, "XGBOOST S" = 18, "XGBOOST R" = 19, "BART T" = 20, "BART S" = 21
)




contour_plot_base <- function( max_bias = 350,
                               max_se = 350,
                               step = 50,
                               include_scales = TRUE ) {
  
  euclidean_distance = expand.grid(x = seq( 0, 1.1*max_bias, length.out=100 ),
                                   y = seq(0, 1.1*max_se, length.out=100 ) )
  
  euclidean_distance$z = with(euclidean_distance, sqrt(x^2 + y^2))
  
  plot_base <- list( 
    geom_contour(
      data = euclidean_distance,
      aes(x = x, y = y, z = z),
      breaks = seq( step, min(max_bias, max_se), by=step ),
      col = "#744074", alpha = 0.5
    ),
    theme_minimal(),
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      text = element_text(color = "black"),
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12)
    ),
    labs(
      y = "Bias",     # Change X-axis title
      x = "SE" ),
    coord_fixed()
  )
  
  if ( include_scales ) {
    plot_base = append( plot_base,
                        list( labs( col = "Model Type" ),
                              scale_shape_manual(values = TYPE_SHAPE_MAP),
                              scale_color_manual(values = TYPE_COLOR_MAP, breaks = LEGEND_COLORS),
                              guides(color = guide_legend(title = "Model Type"),
                                     shape = guide_legend(title = "Model Type"))
                        ) )
  }
  plot_base
}

if ( FALSE ) {
  c = contour_plot_base()
  length( c )
  c2 = contour_plot_base( include_scales = FALSE )
  length( c2 )
}


make_contour_plot <- function( df_set,
                               max_bias = 350,
                               max_se = 350,
                               add_labels = TRUE ) {
  
  p <- ggplot( df_set, aes(se, bias, xmax=max_se, ymax=max_bias) ) +
    geom_point(aes(color = type, shape = type),  size = 4 ) + 
    contour_plot_base( max_bias = max_bias, max_se = max_se )
  
  if ( add_labels ) {
    p <- p +
      ggrepel::geom_label_repel(
        aes(label = modelshort, col = type),
        size = 3, # Increased label size
        box.padding = 0.75,
        point.padding = 0.25,
        max.overlaps = Inf,
        show.legend = FALSE
      )
    
  }
  
  p
}


# Take a plot and add contour lines to it in the underlayer so it
# doesn't cover the plot.
#
# NOT CURRENTLY USED
add_contour_lines <- function( my_plot,
                               max_bias = 350,
                               max_se = 350 ) {
  
  stop() # To ensure isn't used just yet.
  
  euclidean_distance = expand.grid(x = seq( 0, 1.1*max_bias, length.out=100 ),
                                   y = seq(0, 1.1*max_se, length.out=100 ) )
  
  euclidean_distance$z = with(euclidean_distance, sqrt(x^2 + y^2))
  
  # Get the original plot
  p <- my_plot()
  
  # Create a new layer (e.g., a smooth line)
  new_layer <-  geom_contour(
    data = euclidean_distance,
    aes(x = x, y = y, z = z),
    breaks = seq( 50, max(max_bias,max_se), by=50 )
  ) 
  
  # Combine the new layer with the original layers, putting the new layer first
  combined_layers <- c(list(new_layer), p$layers)
  
  # Rebuild the plot with the layers in the new order
  p_with_new_layer <- ggplot(p$data, p$mapping) + combined_layers
  
  p_with_new_layer
}



