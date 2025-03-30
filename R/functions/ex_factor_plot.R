#' ex_factor_plot:: make an ex_factor_plot for given set of coefficients
#' 
#' @param cs coefficient set to use for this plot
#' 
#' @return outputs ggplot with the figure
#'
#' @export
#' 
#' 
ex_factor_plot = function(c_coef){
  
  # Plot simulation factors only
  ggplot( c_coef, aes( term, estimate, color = measure ) ) +
    geom_hline( yintercept = 0 ) +
    geom_point( position = position_dodge(width = 0.4) ) +
    geom_errorbar( aes( ymin = estimate - 2*std.error, ymax = estimate + 2*std.error ),
                   position = position_dodge(width = 0.4), 
                   width = 0 ) +
    coord_flip( clip="on") +
    theme_minimal() +
    labs( x = "", y="" ) +
  #  scale_y_continuous( limits = c( -125, 240)) +
    theme( plot.margin = unit(c(1, 0.1, 0.1, 1.5), "cm") ) +
    scale_x_discrete( labels = label_wrap(10) )
  
}