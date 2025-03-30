

library( ggpubr )


## ----echo=FALSE, message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------
# Iterate over unique queens
queens <- unique(wide_Ev_agg_perf_by_queen$queen)

# Create a list to store plots
plots <- list()

# Iterate over queens
for (q in queens) {
  # Filter data for the current queen
  queen_data <- wide_Ev_agg_perf_by_queen %>% filter(queen == q)
  
  # Create plot
  
  p <- ggplot(queen_data, aes(bias, se)) +
    geom_point(aes(color = type), size = 3) +
    geom_label_repel(
      aes(label = model, col = type),
      size = 4, # Increased label size
      box.padding = 0.75,
      point.padding = 0.25,
      max.overlaps = Inf,
      show.legend = FALSE
    ) +
    geom_contour(
      data = euclidean_distance,
      aes(x = x, y = y, z = z),
      breaks = c(50, 100, 150, 200)
    ) +
    theme_minimal() +
    scale_x_continuous( limits = c( 0, 300 ), breaks = seq( 0, 200, by = 50 ) ) +
    scale_y_continuous( limits = c( 0, 300 ), breaks = seq( 0, 250, by = 50 ) ) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      text = element_text(color = "black"),
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12)
    ) +
    ggtitle(paste("Queen:", q))+
    labs(
      col = "Model Type"
    ) +
    xlab("Bias") +    # Change X-axis title
    ylab("SE")       # Change Y-axis title
  
  plots[[length(plots) + 1]] <- p
}


## ---------------------------------------------------------------------------------------------------------------------------------------------
library(ggpubr)
# Arrange plots in a grid
p = ggarrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]], plots[[6]], plots[[7]], plots[[8]],
              plots[[9]],  plots[[10]], plots[[11]], ncol=2,  common.legend = TRUE, legend="bottom")

p


## ---------------------------------------------------------------------------------------------------------------------------------------------
# RSS Presentation plot 2

p = ggarrange(plots[[1]],  plots[[6]], ncol=2,  common.legend = TRUE, legend="bottom")

p


## ---------------------------------------------------------------------------------------------------------------------------------------------
# Change to your folder
ggsave(here::here("graphs/ca/train_1000/small_cov_set/plot_1_ATE_CDML.png"), p, width = 10, height = 4, units = "in", bg = 'white')


## ---------------------------------------------------------------------------------------------------------------------------------------------
queens <- unique(wide_Ev_agg_perf_by_queen$queen)




ATE_queen_data <- wide_Ev_agg_perf_by_queen %>% filter(queen == "ATE")
CDML_queen_data <- wide_Ev_agg_perf_by_queen %>% filter(queen == "CDML")

# Combine data
combined_data <- bind_rows(ATE_queen_data, CDML_queen_data)

# Plot
ggplot(combined_data, aes(x = bias, y = se, color = queen)) +
  geom_point(size = 3) +
  geom_text(aes(label = model), vjust = -1) +
  theme_minimal() +
  labs(title = "Overlayed ATE and CDML",
       x = "Bias",
       y = "SE")

## ---------------------------------------------------------------------------------------------------------------------------------------------
p <- ggplot(combined_data, aes(bias, se)) +
  geom_point(aes(color = queen), size = 3) +
  geom_label_repel(
    aes(label = model, col = queen),
    size = 4, # Increased label size
    box.padding = 0.75,
    point.padding = 0.25,
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  geom_contour(
    data = euclidean_distance,
    aes(x = x, y = y, z = z),
    breaks = c(50, 100, 150, 200)
  ) +
  theme_minimal() +
  # scale_x_continuous( limits = c( 0, 300 ), breaks = seq( 0, 200, by = 50 ) ) +
  # scale_y_continuous( limits = c( 0, 300 ), breaks = seq( 0, 250, by = 50 ) ) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    text = element_text(color = "black"),
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  ) +
  ggtitle(paste("Queen:", q))+
  labs(
    col = "Model Type"
  ) +
  xlab("Bias") +    # Change X-axis title
  ylab("SE")       # Change Y-axis title


## ---------------------------------------------------------------------------------------------------------------------------------------------
p

## ---------------------------------------------------------------------------------------------------------------------------------------------
# Define shapes for each model type
shape_map <- c(
  "ATE" = 0, "OLS S" = 1, "RF INF" = 2, "RF T" = 3, "RF MOM IPW" = 4, "RF MOM DR" = 5, 
  "CF" = 6, "CF LC" = 7, "CDML" = 8, "LASSO INF" = 9, "LASSO T" = 10, "LASSO MOM IPW" = 11, 
  "LASSO MOM DR" = 12, "LASSO MCM" = 13, "LASSO MCM EA" = 14, "LASSO R" = 15, 
  "SL T" = 16, "SL S" = 17, "XGBOOST S" = 18, "XGBOOST R" = 19, "BART T" = 20, "BART S" = 21
)

p <- ggplot(combined_data, aes(bias, se)) +
  geom_point(aes(color = queen, shape = model), size = 3) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    text = element_text(color = "black"),
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  ) +
  ggtitle("Queen: Combined ATE and CDML") +
  labs(
    col = "Model Type",
    shape = "Model"
  ) +
  xlab("Bias") +    # Change X-axis title
  ylab("SE") +      # Change Y-axis title
  scale_shape_manual(values = shape_map)

print(p)

## ---------------------------------------------------------------------------------------------------------------------------------------------
# Define shapes for each model type
shape_map <- c(
  "ATE" = 0, "OLS S" = 1, "RF INF" = 2, "RF T" = 3, "RF MOM IPW" = 4, "RF MOM DR" = 5, 
  "CF" = 6, "CF LC" = 7, "CDML" = 8, "LASSO INF" = 9, "LASSO T" = 10, "LASSO MOM IPW" = 11, 
  "LASSO MOM DR" = 12, "LASSO MCM" = 13, "LASSO MCM EA" = 14, "LASSO R" = 15, 
  "SL T" = 16, "SL S" = 17, "XGBOOST S" = 18, "XGBOOST R" = 19, "BART T" = 20, "BART S" = 21
)

p <- ggplot(combined_data, aes(bias, se)) +
  geom_point(aes(color = queen, shape = model), size = 3) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    text = element_text(color = "black"),
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = "right",
    legend.box = "vertical"
  ) +
  ggtitle("Queen: Combined ATE and CDML") +
  labs(
    col = "Queen",
    shape = "Model"
  ) +
  xlab("Bias") +    # Change X-axis title
  ylab("SE") +      # Change Y-axis title
  scale_shape_manual(values = shape_map) +
  guides(
    shape = guide_legend(ncol = 1, order = 1), # Single-column legend for shapes on the right
    color = guide_legend(override.aes = list(size = 5), order = 2) # Separate legend for colors under the plot
  ) +
  theme(
    legend.position = "right", 
    legend.justification = c(1, 0.5), 
    legend.box.just = "center", 
    legend.box = "horizontal"
  )

print(p)

## ---------------------------------------------------------------------------------------------------------------------------------------------
library(ggplot2)
library(ggrepel)
library(cowplot)
# Define shapes for each model type
shape_map <- c(
  "ATE" = 0, "OLS S" = 1, "RF INF" = 2, "RF T" = 3, "RF MOM IPW" = 4, "RF MOM DR" = 5, 
  "CF" = 6, "CF LC" = 7, "CDML" = 8, "LASSO INF" = 9, "LASSO T" = 10, "LASSO MOM IPW" = 11, 
  "LASSO MOM DR" = 12, "LASSO MCM" = 13, "LASSO MCM EA" = 14, "LASSO R" = 15, 
  "SL T" = 16, "SL S" = 17, "XGBOOST S" = 18, "XGBOOST R" = 19, "BART T" = 20, "BART S" = 21
)

p <- ggplot(combined_data, aes(bias, se)) +
  geom_point(aes(color = queen, shape = model), size = 3) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    text = element_text(color = "black"),
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = "right",
    legend.box = "vertical"
  ) +
  ggtitle("Queen: Combined ATE and CDML") +
  labs(
    col = "Queen",
    shape = "Model"
  ) +
  xlab("Bias") +    # Change X-axis title
  ylab("SE") +      # Change Y-axis title
  scale_shape_manual(values = shape_map) +
  guides(
    shape = guide_legend(ncol = 1, title.position = "top", order = 1), # Single-column legend for shapes on the right
    color = guide_legend(nrow = 1, title.position = "top", order = 2, 
                         label.position = "bottom", position = "bottom") # Legend for colors under the plot
  ) +
  theme(
    legend.box = "vertical",
    legend.position = "right",
    legend.direction = "vertical",
    plot.margin = unit(c(1, 1, 1, 1), "cm") # add margin to fit the bottom legend
  ) +
  guides(
    color = guide_legend(
      title = "Queen", nrow = 1,
      label.position = "bottom", label.hjust = 0.5,
      override.aes = list(size = 5)
    )
  )

# Extract the color legend separately
p_color_legend <- p + theme(legend.position = "bottom") + guides(shape = "none")

# Arrange plots and legends
plot_with_legend <- plot_grid(
  p + theme(legend.position = "none"), # Main plot without legends
  get_legend(p), # Model legend on the right
  ncol = 2, rel_widths = c(1, 0.2)
)

final_plot <- plot_grid(
  plot_with_legend, # Plot with model legend
  get_legend(p_color_legend), # Queen legend at the bottom
  ncol = 1, rel_heights = c(1, 0.1)
)

print(final_plot)


## ---------------------------------------------------------------------------------------------------------------------------------------------
p_color_legend

## ---------------------------------------------------------------------------------------------------------------------------------------------
p


## ---------------------------------------------------------------------------------------------------------------------------------------------
library(ggplot2)
library(cowplot)

# Define shapes for each model type
shape_map <- c(
  "ATE" = 0, "OLS S" = 1, "RF INF" = 2, "RF T" = 3, "RF MOM IPW" = 4, "RF MOM DR" = 5, 
  "CF" = 6, "CF LC" = 7, "CDML" = 8, "LASSO INF" = 9, "LASSO T" = 10, "LASSO MOM IPW" = 11, 
  "LASSO MOM DR" = 12, "LASSO MCM" = 13, "LASSO MCM EA" = 14, "LASSO R" = 15, 
  "SL T" = 16, "SL S" = 17, "XGBOOST S" = 18, "XGBOOST R" = 19, "BART T" = 20, "BART S" = 21
)

p <- ggplot(combined_data, aes(bias, se)) +
  geom_point(aes(color = queen, shape = model), size = 3) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    text = element_text(color = "black"),
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = "right",
    legend.box = "vertical",
    plot.margin = unit(c(1, 2, 2, 1), "cm") # Added margin to fit the bottom legend
  ) +
  ggtitle("Queen: Combined ATE and CDML") +
  labs(
    col = "Queen",
    shape = "Model"
  ) +
  xlab("Bias") +
  ylab("SE") +
  scale_shape_manual(values = shape_map) +
  guides(
    shape = guide_legend(ncol = 1, title.position = "top", order = 1),
    color = guide_legend(nrow = 1, title.position = "top", order = 2, 
                         label.position = "bottom", position = "bottom")
  )

# Extract legends
legend_shape <- get_legend(p + theme(legend.position = "right"))
legend_color <- get_legend(p + theme(legend.position = "bottom") + guides(shape = "none"))

# Arrange plots and legends
plot_with_legend <- plot_grid(
  p + theme(legend.position = "none"), # Main plot without legends
  legend_shape, # Model legend on the right
  ncol = 2, rel_widths = c(1, 0.2)
)

final_plot <- plot_grid(
  plot_with_legend, # Plot with model legend
  legend_color, # Queen legend at the bottom
  ncol = 1, rel_heights = c(1, 0.1)
)

print(final_plot)

## ---------------------------------------------------------------------------------------------------------------------------------------------
queens <- unique(wide_Ev_agg_perf_by_queen$queen)




ATE_queen_data <- wide_Ev_agg_perf_by_queen %>% filter(queen == "ATE")
CDML_queen_data <- wide_Ev_agg_perf_by_queen %>% filter(queen == "CDML")

# Combine data
combined_data <- bind_rows(ATE_queen_data, CDML_queen_data)

data <- combined_data

# Load necessary libraries
library(ggplot2)
library(cowplot)
library(grid)

# Define shapes for each model
model_shapes <- c('ATE' = 16, 'OLS S' = 17, 'RF INF' = 18, 'RF T' = 20, 'RF MOM IPW' = 15, 
                  'RF MOM DR' = 7, 'CF' = 8, 'CF LC' = 9, 'CDML' = 10, 'LASSO INF' = 11, 
                  'LASSO T' = 12, 'LASSO MOM IPW' = 13, 'LASSO MOM DR' = 14, 'LASSO MCM' = 3, 
                  'LASSO MCM EA' = 4, 'LASSO R' = 5, 'XGBOOST S' = 6, 'XGBOOST R' = 21, 
                  'BART T' = 22, 'BART S' = 24)

# Create the plot
plot <- ggplot(data, aes(x = bias, y = se, color = queen, shape = model)) +
  geom_point(size = 4) +
  scale_shape_manual(values = model_shapes) +
  theme_bw() +
  labs(title = "Bias vs SE by Model and Queen", x = "Bias", y = "SE") +
  theme(plot.title = element_text(hjust = 0.5, size = 14))#, face = "bold"))

# Extract the legends
legend_shape <- get_legend(plot + guides(shape = guide_legend(ncol = 2, override.aes = list(colour = "black"))) + theme(legend.position = "right"))
legend_color <- get_legend(plot + guides(color = guide_legend(nrow = 1, title = "Queen")) + theme(legend.position = "bottom"))

# Remove legends from the plot
plot <- plot + theme(legend.position = "none")

# Create a white background canvas
background <- ggdraw() + draw_grob(rectGrob(gp=gpar(fill="white", col=NA)))

# Combine the plot and legends with adjusted spacing
combined_plot <- plot_grid(plot, legend_shape, legend_color, ncol = 1, rel_heights = c(4, 1, 1), align = 'v')

# Overlay the plot on the white background and adjust the layout
final_plot <- ggdraw() + 
  draw_grob(rectGrob(gp=gpar(fill="white", col=NA))) + 
  draw_plot(plot, 0, 0, 0.61, 1) +  # Adjust plot size
  draw_plot(legend_shape, 0.65, 0.2, 0.32, 0.6, hjust = 0, vjust = 0) +  # Adjust shape legend position
  draw_plot(legend_color, 0, -0.1, 1, 0.2, hjust = 0, vjust = 0)  # Adjust color legend position

# Print the final plot
print(final_plot)



## ---------------------------------------------------------------------------------------------------------------------------------------------
# Change to your folder
ggsave(here::here("graphs/ca/train_1000/small_cov_set/plot_1_ATE_CDML_2.png"), final_plot, width = 10, height = 5, units = "in", bg = 'white')


## ---------------------------------------------------------------------------------------------------------------------------------------------
# Define shapes for each model
type_shapes <- c('ATE' = 1, 'BART' = 2, 'CDML' = 10, 'INF' = 6, 'LASSO' = 0, 
                 'OLS' = 7, 'RF' = 8, 'XGBOOST' = 9)

# Create the plot
plot <- ggplot(data, aes(x = bias, y = se, color = queen, shape = type)) +
  geom_point(size = 4) +
  scale_shape_manual(values = type_shapes) +
  theme_bw() +
  labs(title = "Bias vs SE by Model and Queen", x = "Bias", y = "SE") +
  theme(plot.title = element_text(hjust = 0.5, size = 14))#, face = "bold"))

# Extract the legends
legend_shape <- get_legend(plot + guides(shape = guide_legend(ncol = 1,  title = "model", override.aes = list(colour = "black"))) + theme(legend.position = "right"))
legend_color <- get_legend(plot + guides(color = guide_legend(nrow = 1, title = "Queen")) + theme(legend.position = "bottom"))

# Remove legends from the plot
plot <- plot + theme(legend.position = "none")

# Create a white background canvas
background <- ggdraw() + draw_grob(rectGrob(gp=gpar(fill="white", col=NA)))

# Combine the plot and legends with adjusted spacing
combined_plot <- plot_grid(plot, legend_shape, legend_color, ncol = 1, rel_heights = c(4, 1, 1), align = 'v')

# Overlay the plot on the white background and adjust the layout
final_plot <- ggdraw() + 
  draw_grob(rectGrob(gp=gpar(fill="white", col=NA))) + 
  draw_plot(plot, 0, 0, 0.81, 1) +  # Adjust plot size
  draw_plot(legend_shape, 0.77, 0.2, 0.32, 0.6, hjust = 0, vjust = 0) +  # Adjust shape legend position
  draw_plot(legend_color, 0, -0.1, 1, 0.2, hjust = 0, vjust = 0)  # Adjust color legend position

# Print the final plot
print(final_plot)



## ---------------------------------------------------------------------------------------------------------------------------------------------
# Change to your folder
ggsave(here::here("graphs/ca/train_1000/small_cov_set/plot_1_ATE_CDML_3.png"), final_plot, width = 10, height = 5, units = "in", bg = 'white')

