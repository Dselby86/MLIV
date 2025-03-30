#' coef_plot:: make coefficient plot for given set of coefficients
#' 
#' @param cs coefficient set to use for this plot
#' @param plotOnly change to "model" or "queen" if you only want to plot models or queens
#' 
#' @return outputs ggplot with the figure
#'
#' @export
#' 
#' 
coef_plot = function(cs, plotOnly=NULL){
  
  # Plot model and queen on single plot
  library( scales,warn.conflicts=FALSE )
  cs$termW = cs$term
  lvl = levels( cs$termW )
  levels( cs$termW ) <- str_pad(lvl, 
                                width = max(nchar(lvl)), 
                                side = "left")
  
  if (is.null(plotOnly)){
    ggplot( cs, aes( term, estimate, color = measure ) ) +
      facet_wrap( ~ queen, scales = "free" ) +
      geom_hline( yintercept = 0 ) +
      #  geom_linerange( aes( ymin = 0, ymax = estimate ),
      #                  position = position_dodge(width = 0.3),
      #                  size = 1, alpha=0.5) +
      geom_point( position = position_dodge(width = 0.4) ) +
      geom_errorbar( aes( ymin = estimate - 2*std.error, ymax = estimate + 2*std.error ),
                     position = position_dodge(width = 0.4), 
                     width = 0 ) +
      coord_flip() +
      theme_minimal() +
      labs( x = "", y="" ) +
      scale_x_discrete( labels = label_wrap(10) )
    
  }
  else if (plotOnly=="model"){
    
    c2 <- filter( cs, queen != "queen" )
    
    ggplot( c2, aes( term, estimate, color = measure ) ) +
      geom_hline( yintercept = 0 ) +
      geom_point( position = position_dodge(width = 0.4) ) +
      geom_errorbar( aes( ymin = estimate - 2*std.error, ymax = estimate + 2*std.error ),
                     position = position_dodge(width = 0.4), 
                     width = 0 ) +
      coord_flip() +
      theme_minimal() +
      labs( x = "", y="" )  +
      theme( plot.margin = unit(c(1, 0.1, 0.1, 1.5), "cm") ) +
      scale_x_discrete( labels = label_wrap(10) )
  }
  
  else if (plotOnly =="queen"){
    c3 = filter( cs, queen == "queen" )
    ggplot( c3, aes( term, estimate, color = measure ) ) +
      geom_hline( yintercept = 0 ) +
      geom_point( position = position_dodge(width = 0.4) ) +
      geom_errorbar( aes( ymin = estimate - 2*std.error, ymax = estimate + 2*std.error ),
                     position = position_dodge(width = 0.4), 
                     width = 0 ) +
      coord_flip() +
      theme_minimal() +
      labs( x = "", y="" )  +
      theme( plot.margin = unit(c(1, 0.1, 0.1, 1.5), "cm") ) +
      scale_x_discrete( labels = label_wrap(10) )
    
  }
}