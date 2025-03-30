

#' contour_plot_base:: A function to create a base on which to build
#' contour plots. This function is used by make_contour_plot.
#'
#' @param max_bias The maximum used on the bias axis
#' @param max_se  The maximum used on the SE axis
#' @param breaks The break locations for the contour lines.  If null
#'   will generate on fly via step argument.
#'
#' @return outputs plot base that can be built on to create the figure
#'   using make_contour_plot
#'
#' @export

contour_plot_base <- function( max_bias = 0.5,
                               max_se = 0.5,
                               step = 0.05,
                               breaks = NULL,
                               include_scales = TRUE ) {
  
  euclidean_distance = expand.grid(x = seq( 0, 1.1*max_bias, length.out=100 ),
                                   y = seq(0, 1.1*max_se, length.out=100 ) )
  
  euclidean_distance$z = with(euclidean_distance, sqrt(x^2 + y^2))
  
  if ( is.null( breaks ) ) {
    breaks = seq( step, min(max_bias, max_se), by=step )
  }
  
  plot_base <- list( 
    geom_contour(
      data = euclidean_distance,
      aes(x = x, y = y, z = z),
      breaks = breaks,
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
  # Old version
  contour_plot_base <- function( max_bias ,
                                 max_se ,
                                 breaks) {
    
    euclidean_distance = expand.grid(x = seq( 0, 1.1*max_bias, length.out=100 ),
                                     y = seq(0, 1.1*max_se, length.out=100 ) )
    
    euclidean_distance$z = with(euclidean_distance, sqrt(x^2 + y^2))
    
    plot_base <- list( 
      geom_contour(
        data = euclidean_distance,
        aes(x = x, y = y, z = z),
        breaks = breaks 
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
        title = "Bias and SE, averaged across Queens",
        subtitle = "* ATE queen is excluded",
        col = "Model Type",
        y = "Bias",     # Change X-axis title
        x = "SE" ),     # Change Y-axis title
      scale_shape_manual(values = TYPE_SHAPE_MAP),
      scale_color_manual(values = TYPE_COLOR_MAP, breaks = LEGEND_COLORS),
      guides(color = guide_legend(title = "Model Type"),
             shape = guide_legend(title = "Model Type")),
      coord_fixed()
    )
    plot_base
  }
}
