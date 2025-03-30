#' make_contour_plot:: A function to call contour_plot_base and make a contour plot object
#'
#' @param df_set The data used for plotting
#' @param max_bias The maximum used on the bias axis
#' @param max_se  The maximum used on the SE axis
#' @param breaks The break locations for the contour lines 
#' @param add_labels binary indicator of whether we want to add labels
#' 
#' @return outputs ggplot with the figure
#'
#' @export

make_contour_plot <- function( df_set,
                               max_bias ,
                               max_se ,
                               breaks,
                               add_labels = TRUE ) {
  
  p <- ggplot( df_set, aes(se, bias, xmax=max_se, ymax=max_bias) ) +
    geom_point(aes(color = type, shape = type),  size = 4 ) + 
    contour_plot_base( max_bias = max_bias, max_se = max_se , breaks= breaks)
  
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
