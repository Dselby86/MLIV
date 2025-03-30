#' callggplot:: A function to call ggplot and save the plot with some standardized options
#'
#' @param data The simulation set to use for plotting
#' @param plottype type of ggplot (i.e. geom_boxplot() )
#' @param filterqueen the queen we are using to filter the data, leave NULL if this plot includes all queens
#' @param theme theme used in the plot, default is theme_minimal()
#' @param additional_params any additional parameters for the plot, like log-scale
#' @param xMaximum maximum x value to display, will also count number excluded and add to legend
#' @param excludeCountMetric metric to count outlier exclusions by 
#' @param title1 The first line of the title
#' @param title2 The second line of the title, leave NULL if no second line
#' 
#' @return outputs ggplot with the figure
#'
#' @export

callggplot = function (data, 
                       plottype   ,
                       filterqueen   = NULL  ,        # specify queen to filter, if any
                       theme =  theme_minimal(),
                       additional_params = NULL,
                       xMaximum = 25, #maxium x value to show
                       excludeCountMetric = "rmse",
                       title1 = NULL, 
                       title2 = NULL) {
  #set the title of the figure and the name of the image file to 'title1' or, if title2 exists, 'title1 \n title2'
  figuretitle = title1
  filename = title1
  print(head(data))
  # Extend title and filename is title2 is used
  if (!is.null(title2)) {
    figuretitle = paste0(title1,"\n", title2) 
    filename = paste0(title1, "_", title2) 
  }
  
  # If the function says to filter the data to just one queen, do that
  filtered_data = data 
  if (!is.null(filterqueen)){   
    filtered_data <- data %>%
      filter(queen == filterqueen)
  }
  
  #create labels that tell you how many outliers (values above xMaximum) were excluded in rmse only for now
  filtered_data = filtered_data %>%
    group_by(model) %>%
    mutate(excludelabel = paste0(model, ": (n= ", prettyNum(n(), big.mark=","), ", ",excludeCountMetric," excludes ", prettyNum(sum(metric == excludeCountMetric & value > xMaximum ), big.mark=","),")") )
  
  # Generally, group by metrics (except for sometimes group by queen)
   facetwrap = facet_wrap( ~ metric )
   if (facet == "queen"){
     facetwrap = facet_wrap( ~ queen )
   }
  # Create the plot for , se, rmse
  plotstogroup =  c("se", "rmse", "bias")
  if (facet == "queen"){
    plotstogroup =  c("se", "rmse", "bias", "runtime")
  }
  filtered_data_metric = filtered_data[filtered_data$metric %in% plotstogroup,]
  
  plot= ggplot( filtered_data_metric, aes( model, value  , fill = excludelabel, col=model)) +
    facetwrap +
    plottype + 
    scale_y_continuous(limits = c(0, xMaximum)) +
    scale_fill_discrete(name="Legend")+
    coord_flip() + 
    labs( title = figuretitle
          ,x = ""
          ,y = "") + 
    theme + 
    additional_params
  print(plot)
  
  # Setting up a more frequent plot for runtime, commenting out for now
  # if (facet == "metric"){
  #   filtered_data_metric = filtered_data[filtered_data$metric %in% c("runtime"),]
  #    plot= ggplot( filtered_data_metric, aes( model, value  , fill = model)) +
  #     facetwrap + 
  #     plottype + 
  #     coord_flip() + 
  #     labs( title = figuretitle
  #           ,x = "",y = "") + 
  #     theme + 
  #     additional_params
  #   print(plot)
  # }
}