###################################################################################
#                                                                                                        
#                                                                         
# Created on:   12/26/2024
# Purpose:      Density plots for raw CATE data for 1st iteration
# Authors:      Polina Polskaia
#
###################################################################################


###################################################################################
# STEP 0: SET UP 
###################################################################################


source( here::here( "analysis/setup_environment.R" ) )


###################################################################################
# STEP 1: LOAD RAW CATE WIDE MASTER DATA ----
###################################################################################


# WARNING: This is a BIG file
final_data_wide <- readRDS( here::here( "results_final/cate_master_1st_iter.rds" ) )

final_data_wide

# OPTIONAL: Cut down data to more manageable size to make rest of code faster
# (We are assuming the observations are not sorted.)
final_data_wide$observation = as.numeric( final_data_wide$observation )
final_data_wide <- final_data_wide %>%
  dplyr::filter( observation <= 1000 )



###################################################################################
# STEP 2: SUBSET THE MASTER ----
###################################################################################


# Create a list of relevant models
DEFAULT_MODEL_LIST <- model_queen_lists(include_RF = TRUE, 
                                        include_LASSO = TRUE, 
                                        include_CDML = TRUE, 
                                        include_XGBOOST = FALSE, 
                                        include_BART = TRUE,
                                        include_SL_S = FALSE,
                                        include_SL_T = FALSE,
                                        make_queen_list = FALSE) 

# And our queens
DEFAULT_QUEEN_LIST = c(
  "OLS S", "ATE", "RF T", "RF MOM IPW", "CF",
  "LASSO R", "CDML", "BART T", "SL T", "XGBOOST R")


# Subset the data to relevant queens only
filtered_data <- final_data_wide[final_data_wide$queen %in% DEFAULT_QUEEN_LIST, ]

# Subset the data to relevant models only
filtered_data <- filtered_data[filtered_data$model %in% DEFAULT_MODEL_LIST, ]

# Order data by model for plot consistency
filtered_data <- filtered_data %>%
  mutate(model = factor(model, levels = DEFAULT_MODEL_LIST)) %>%
  arrange(model)

# Order data by queen for plot consistency
filtered_data <- filtered_data %>%
  mutate(queen = factor(queen, levels = DEFAULT_QUEEN_LIST)) %>%
  arrange(queen)



# ##################################################################################
# STEP 3: CREATE FUNCTION FOR DENSITY PLOTS ----
# ##################################################################################


#' Create Density Plot by Queen
#'
#' Generates a density plot for the predicted and true Conditional Average Treatment Effects (CATE)
#' by a specified queen in the dataset. The plot is saved as a PNG file.
#'
#' @param data A data frame containing the dataset with columns: `queen`, `dataset`, `outcome`,
#'   `size_train`, `covariate_set`, `CATE_hat`, and `CATE_true`.
#' @param queen_name A character string specifying the name of the queen to filter the dataset.
#' @param dataset A character string specifying the dataset name (default: "asap").
#' @param outcome A character string specifying the outcome variable (default: "X16BTMCRET").
#' @param size_train An integer specifying the size of the training data (default: 1000).
#' @param covariate_set A character string specifying the covariate set (default: "small").
#' @param x_range A numeric vector of length 2 specifying the x-axis range (default: `NULL`).
#' @param y_range A numeric vector of length 2 specifying the y-axis range (default: `NULL`).
#'
#' @return A ggplot2 object representing the density plot.
#' The plot is also saved to the folder `analysis/raw_cate_plots/by_queen`.
#' 
#' @export
plot_densities <- function(data, 
                           queen_name = NULL,
                           model_name = NULL,
                           dataset = "asap",
                           outcome = "X16BTMCRET",
                           size_train = 1000,
                           covariate_set = "small",
                           ncol = 4,
                           x_range = NULL,
                           y_range = NULL,
                           num_bins = 12 ) {
  # Filter the dataset for the given parameters
  subset_data <- data %>%
    dplyr::filter(
      dataset == !!dataset,
      outcome == !!outcome,
      size_train == !!size_train,
      covariate_set == !!covariate_set
    ) %>%
    mutate(observation = as.numeric(observation))  # Ensure observation is numeric
  
  if ( !is.null( queen_name ) ) {
    subset_data <- subset_data %>%
      filter( queen == queen_name )
    subset_data$facet = subset_data$model
    title_stem = paste( "by model, for queen", queen_name )
  }
  if ( !is.null( model_name ) ) {
    subset_data <- subset_data %>%
      filter( model == model_name )
    subset_data$facet = subset_data$queen
    title_stem = paste( "by queen, for model", model_name )
  }
  
  bw = max( subset_data$CATE_hat ) - min( subset_data$CATE_hat )
  c = 0
  if ( !is.null( x_range ) ) {
    # windorize CATE_hat and CATE_true to x range
    subset_data <- mutate( subset_data,
                           CATE_hat = ifelse( CATE_hat < x_range[1], x_range[1] + 0.00001, CATE_hat ),
                           CATE_hat = ifelse( CATE_hat > x_range[2], x_range[2] - 0.00001, CATE_hat ),
                           CATE_true = ifelse( CATE_true < x_range[1], x_range[1] + 0.00001, CATE_true ),
                           CATE_true = ifelse( CATE_true > x_range[2], x_range[2] - 0.00001, CATE_true ) )
    bw = (x_range[2] - x_range[1])/ num_bins
    c = x_range[[1]]
    #x_range[1] = x_range[1] - 0.5*bw
    #x_range[2] = x_range[2] + 0.5*bw
  }
  
  
  # Create the plot object
  plot <- ggplot(subset_data) +
    geom_histogram(aes(x = CATE_hat, y = after_stat(density), fill = "CATE_hat"), alpha = 0.5, bins = num_bins, boundary = c) +
    geom_histogram(aes(x = CATE_true, y = after_stat(density), fill = "CATE_true"), alpha = 0.5, bins = num_bins, boundary = c) +
    #geom_density(aes(x = CATE_hat, fill = "CATE_hat"), alpha = 0.5) +
    #geom_density(aes(x = CATE_true, fill = "CATE_true"), alpha = 0.5 ) +
    scale_fill_manual(
      values = c("CATE_hat" = "blue", "CATE_true" = "orange"),
      labels = c("CATE_hat" = "Predicted CATE", "CATE_true" = "True CATE"),
      name = ""  # Set legend title to blank
    ) +
    facet_wrap( ~facet, scales = "fixed", ncol = ncol ) +  # Fixed y-axis for all plots
    labs(
      title = paste(
        "Density plot of predicted vs. true CATE ",
        title_stem, ", ", dataset, ", ", outcome, ",\n",
        "train size ", size_train, ", covariate set size ", covariate_set, ", first iteration", sep=""
      ),
      x = "Value",
      y = "Density"
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 14),  # Adjust facet title size
      legend.position = "top",
      panel.background = element_rect(fill = "white", color = NA),  # White panel background
      plot.background = element_rect(fill = "white", color = NA)   # White overall background
      
    )
  
  # Apply x_range if provided
  if (!is.null(x_range)) {
    plot <- plot + xlim(x_range)
  }
  
  # Apply y_range if provided
  if (!is.null(y_range)) {
    plot <- plot + ylim(y_range)
  }
  
  # Adjust the height of the plot based on the number of models
  num_models <- n_distinct(subset_data$facet)
  plot_height <- 4 * ceiling(num_models / ncol)  # Adjust height for number of columns
  
  # Create folder structure
  folder_path <- file.path( here::here( "analysis/raw_cate_plots" ),
                            dataset, outcome, size_train, covariate_set, "by_queen")
  dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)
  
  # Define output filename with folder structure
  if ( !is.null( queen_name ) ) {
    output_filename <- file.path(
      folder_path,
      paste0("density_plot_for_queen_", queen_name, "_", dataset, "_", outcome, "_",
             size_train, "_", covariate_set, ".png")
    )
  } else {
    output_filename <- file.path(
      folder_path,
      paste0("density_plot_for_model_", model_name, "_", dataset, "_", outcome, "_",
             size_train, "_", covariate_set, ".png")
    )
  }
  
  # Save the plot
  ggsave(output_filename, plot = plot, width = 4*(ncol), height = plot_height)
  
  return(plot)  # Return the plot object if needed
}


#' Create Density Plot by Model
#'
#' Generates a density plot for the predicted and true Conditional Average Treatment Effects (CATE)
#' for a specific model in the dataset. The plot is saved as a PNG file.
#'
#' @param data A data frame containing the dataset with columns: `model`, `dataset`, `outcome`,
#'   `size_train`, `covariate_set`, `CATE_hat`, and `CATE_true`.
#' @param model_name A character string specifying the name of the model to filter the dataset.
#' @inheritParams plot_densities
#'
#' @return A ggplot2 object representing the density plot.
#' The plot is also saved to the folder `analysis/raw_cate_plots/`.
#'
#' @export
plot_density_by_model <- function(data, model_name,
                                  dataset = "asap",
                                  outcome = "X16BTMCRET",
                                  size_train = 1000,
                                  covariate_set = "small",
                                  x_range = NULL,
                                  y_range = NULL) {
  plot_densities( data, model_name = model_name,
                  dataset = dataset,
                  outcome = outcome,
                  size_train = size_train,
                  covariate_set = covariate_set,
                  x_range = x_range,
                  y_range = y_range )
}


plot_density_by_queen <- function(data, queen_name,
                                  dataset = "asap",
                                  outcome = "X16BTMCRET",
                                  size_train = 1000,
                                  covariate_set = "small",
                                  x_range = NULL,
                                  y_range = NULL) {
  plot_densities( data, queen_name = queen_name,
                  dataset = dataset,
                  outcome = outcome,
                  size_train = size_train,
                  covariate_set = covariate_set,
                  x_range = x_range,
                  y_range = y_range )
}


if ( FALSE ) {
  plot_density_by_queen( filtered_data, "OLS S",
                         dataset = "asap",
                         outcome = "X16BTMCRET",
                         size_train = 1000,
                         covariate_set = "small",
                         x_range = c(5.955, 15.239) )
  
  plot_density_by_queen( filtered_data, "OLS S",
                         dataset = "asap",
                         outcome = "X16BTMCRET",
                         size_train = 1000,
                         covariate_set = "small",
                         x_range = c( -10, 10 ) )
  
  
  plot_density_by_model( filtered_data, "RF T" )

  # ATE as queen check
  dd = filtered_data %>%
    filter( queen == "ATE",
            dataset == "asap", 
            outcome == "X16BTMCRET",
            size_train == 1000,
            covariate_set == "small" )
  table( dd$model )
  n_unique(dd$model)
  
  summary( dd$CATE_true )
  summary( dd$CATE_hat )
  
  debug( plot_densities )
  plot_density_by_queen( filtered_data, "OLS S",
                         dataset = "asap",
                         outcome = "X16BTMCRET",
                         size_train = 1000,
                         covariate_set = "small",
                         x_range = c( -10, 20 ) )
  
}

# Look at the distribution of true CATEs for each queen for a single scenario ----

DEFAULT_QUEEN_LIST = c(
  "OLS S", "ATE", "RF T", "RF MOM IPW", "CF",
  "LASSO R", "CDML", "BART T", "SL T", "XGBOOST R")

filter( final_data_wide, queen == "LASSO MCM EA" )
queen_CATEs = filter( final_data_wide, 
                      dataset == "asap",
                      outcome == "X16BTMCRET",
                      size_train == 1000, covariate_set == "small",
                      model == "ATE" ) %>%
  dplyr::select( -model, -covariate_set, -iteration_number, -CATE_hat, -iterations, -size_train ) %>%
#%>%
  filter( queen != "ATE" )
#           queen %in% DEFAULT_QUEEN_LIST )

             
queen_CATEs

ggplot( queen_CATEs, aes( x = CATE_true, y = after_stat(density) ) ) +
  geom_histogram( bins = 30 ) +
  facet_wrap( ~queen ) +
  theme_minimal() +
  #coord_cartesian( ylim=c(0,0.005) ) +
  labs( caption = "LASSO R peek truncated for clarity. true_cate_distributions.pdf" ) +
  labs( title = "Distribution of True CATEs by Queen",
        x = "True CATE" ) +
  ggthemes::theme_tufte() +theme(
    axis.title.y = element_blank(), # Remove y-axis title
    axis.text.y = element_blank(),  # Remove y-axis text
    axis.ticks.y = element_blank()  # Remove y-axis ticks
  )
ggsave( here::here( "analysis/raw_cate_plots/true_cate_distributions.pdf" ), height = 3, width = 7 )

# ##################################################################################
# STEP 4: RUN THE DENSITY PLOTS FOR ALL SCENARIOS BY QUEEN AND MODEL ----
# ##################################################################################


# Iterate over each queen in the dataset and call the function
unique_queens <- unique(filtered_data$queen)

# Call asap, X16BTMCRET, 1000, small
for (queen in unique_queens) {
  plot_density_by_queen(filtered_data, 
                        queen,
                        dataset = "asap",
                        outcome = "X16BTMCRET",
                        size_train = 1000,
                        covariate_set = "small",
                        x_range = c(5.955, 15.239))
}

# Call asap, X16BTMCRET, 2000, small
for (queen in unique_queens) {
  plot_density_by_queen(filtered_data, 
                        queen,
                        dataset = "asap",
                        outcome = "X16BTMCRET",
                        size_train = 2000,
                        covariate_set = "small",
                        x_range = c(5.955, 15.239))
}

# Call asap, X16BTMCRET, 5000, small
for (queen in unique_queens) {
  plot_density_by_queen(filtered_data, 
                        queen,
                        dataset = "asap",
                        outcome = "X16BTMCRET",
                        size_train = 5000,
                        covariate_set = "small",
                        x_range = c(5.955, 15.239))
}

# Call asap, X16BTMCRET, 1000, medium
for (queen in unique_queens) {
  plot_density_by_queen(filtered_data, 
                        queen,
                        dataset = "asap",
                        outcome = "X16BTMCRET",
                        size_train = 1000,
                        covariate_set = "medium",
                        x_range = c(5.955, 15.239))
}

# Call asap, X16BTMCRET, 2000, medium
for (queen in unique_queens) {
  plot_density_by_queen(filtered_data, 
                        queen,
                        dataset = "asap",
                        outcome = "X16BTMCRET",
                        size_train = 2000,
                        covariate_set = "medium",
                        x_range = c(5.955, 15.239))
}

# Call asap, X16BTMCRET, 5000, medium
for (queen in unique_queens) {
  plot_density_by_queen(filtered_data, 
                        queen,
                        dataset = "asap",
                        outcome = "X16BTMCRET",
                        size_train = 5000,
                        covariate_set = "medium",
                        x_range = c(5.955, 15.239))
}


# Call asap, X16BTMCRET, 1000, large
for (queen in unique_queens) {
  plot_density_by_queen(filtered_data, 
                        queen,
                        dataset = "asap",
                        outcome = "X16BTMCRET",
                        size_train = 1000,
                        covariate_set = "large",
                        x_range = c(5.955, 15.239))
}

# Call asap, X16BTMCRET, 2000, large
for (queen in unique_queens) {
  plot_density_by_queen(filtered_data, 
                        queen,
                        dataset = "asap",
                        outcome = "X16BTMCRET",
                        size_train = 2000,
                        covariate_set = "large",
                        x_range = c(5.955, 15.239))
}

# Call asap, X16BTMCRET, 5000, large
for (queen in unique_queens) {
  plot_density_by_queen(filtered_data, 
                        queen,
                        dataset = "asap",
                        outcome = "X16BTMCRET",
                        size_train = 5000,
                        covariate_set = "large",
                        x_range = c(5.955, 15.239))
}


# Iterate over each model in the dataset and call the function ----
unique_models <- unique(filtered_data$model)

# Call asap, X16BTMCRET, 1000, small
for (model in unique_models) {
  plot_density_by_model(filtered_data, model,
                        dataset = "asap",
                        outcome = "X16BTMCRET",
                        size_train = 1000,
                        covariate_set = "small",
                        x_range = c(5.955, 15.239)
  )
}

# Call asap, X16BTMCRET, 2000, small
for (model in unique_models) {
  plot_density_by_model(filtered_data, model,
                        dataset = "asap",
                        outcome = "X16BTMCRET",
                        size_train = 2000,
                        covariate_set = "small",
                        x_range = c(5.955, 15.239)
  )
}

# Call asap, X16BTMCRET, 5000, small
for (model in unique_models) {
  plot_density_by_model(filtered_data, model,
                        dataset = "asap",
                        outcome = "X16BTMCRET",
                        size_train = 5000,
                        covariate_set = "small",
                        x_range = c(5.955, 15.239)
  )
}

# Call asap, X16BTMCRET, 1000, medium
for (model in unique_models) {
  plot_density_by_model(filtered_data, model,
                        dataset = "asap",
                        outcome = "X16BTMCRET",
                        size_train = 1000,
                        covariate_set = "medium",
                        x_range = c(5.955, 15.239)
  )
}

# Call asap, X16BTMCRET, 2000, medium
for (model in unique_models) {
  plot_density_by_model(filtered_data, model,
                        dataset = "asap",
                        outcome = "X16BTMCRET",
                        size_train = 2000,
                        covariate_set = "medium",
                        x_range = c(5.955, 15.239)
  )
}

# Call asap, X16BTMCRET, 5000, medium
for (model in unique_models) {
  plot_density_by_model(filtered_data, model,
                        dataset = "asap",
                        outcome = "X16BTMCRET",
                        size_train = 5000,
                        covariate_set = "medium",
                        x_range = c(5.955, 15.239)
  )
}

# Call asap, X16BTMCRET, 1000, large
for (model in unique_models) {
  plot_density_by_model(filtered_data, model,
                        dataset = "asap",
                        outcome = "X16BTMCRET",
                        size_train = 1000,
                        covariate_set = "large",
                        x_range = c(5.955, 15.239)
  )
}

# Call asap, X16BTMCRET, 2000, large
for (model in unique_models) {
  plot_density_by_model(filtered_data, model,
                        dataset = "asap",
                        outcome = "X16BTMCRET",
                        size_train = 2000,
                        covariate_set = "large",
                        x_range = c(5.955, 15.239)
  )
}

# Call asap, X16BTMCRET, 5000, large
for (model in unique_models) {
  plot_density_by_model(filtered_data, model,
                        dataset = "asap",
                        outcome = "X16BTMCRET",
                        size_train = 5000,
                        covariate_set = "large",
                        x_range = c(5.955, 15.239)
  )
}

# ##################################################################################


# Call asap, C16BMVDEG, 1000, small
for (queen in unique_queens) {
  plot_density_by_queen(filtered_data, 
                        queen,
                        dataset = "asap",
                        outcome = "C16BMVDEG",
                        size_train = 1000,
                        covariate_set = "small",
                        x_range = c(-0.3827, 0.7511)
  )
}

# Call asap, C16BMVDEG, 2000, small
for (queen in unique_queens) {
  plot_density_by_queen(filtered_data, 
                        queen,
                        dataset = "asap",
                        outcome = "C16BMVDEG",
                        size_train = 2000,
                        covariate_set = "small",
                        x_range = c(-0.3827, 0.7511)
  )
}

# Call asap, C16BMVDEG, 5000, small
for (queen in unique_queens) {
  plot_density_by_queen(filtered_data, 
                        queen,
                        dataset = "asap",
                        outcome = "C16BMVDEG",
                        size_train = 5000,
                        covariate_set = "small",
                        x_range = c(-0.3827, 0.7511)
  )
}

# Call asap, C16BMVDEG, 1000, medium
for (queen in unique_queens) {
  plot_density_by_queen(filtered_data, 
                        queen,
                        dataset = "asap",
                        outcome = "C16BMVDEG",
                        size_train = 1000,
                        covariate_set = "medium",
                        x_range = c(-0.3827, 0.7511)
  )
}

# Call asap, C16BMVDEG, 2000, medium
for (queen in unique_queens) {
  plot_density_by_queen(filtered_data, 
                        queen,
                        dataset = "asap",
                        outcome = "C16BMVDEG",
                        size_train = 2000,
                        covariate_set = "medium",
                        x_range = c(-0.3827, 0.7511)
  )
}

# Call asap, C16BMVDEG, 5000, medium
for (queen in unique_queens) {
  plot_density_by_queen(filtered_data, 
                        queen,
                        dataset = "asap",
                        outcome = "C16BMVDEG",
                        size_train = 5000,
                        covariate_set = "medium",
                        x_range = c(-0.3827, 0.7511)
  )
}


# Call asap, C16BMVDEG, 1000, large
for (queen in unique_queens) {
  plot_density_by_queen(filtered_data, 
                        queen,
                        dataset = "asap",
                        outcome = "C16BMVDEG",
                        size_train = 1000,
                        covariate_set = "large",
                        x_range = c(-0.3827, 0.7511)
  )
}

# Call asap, C16BMVDEG, 2000, large
for (queen in unique_queens) {
  plot_density_by_queen(filtered_data, 
                        queen,
                        dataset = "asap",
                        outcome = "C16BMVDEG",
                        size_train = 2000,
                        covariate_set = "large",
                        x_range = c(-0.3827, 0.7511)
  )
}

# Call asap, C16BMVDEG, 5000, large
for (queen in unique_queens) {
  plot_density_by_queen(filtered_data, 
                        queen,
                        dataset = "asap",
                        outcome = "C16BMVDEG",
                        size_train = 5000,
                        covariate_set = "large",
                        x_range = c(-0.3827, 0.7511)
  )
}


# Call asap, C16BMVDEG, 1000, small
for (model in unique_models) {
  plot_density_by_model(filtered_data, model,
                        dataset = "asap",
                        outcome = "C16BMVDEG",
                        size_train = 1000,
                        covariate_set = "small",
                        x_range = c(-0.3827, 0.7511)
  )
}

# Call asap, C16BMVDEG, 2000, small
for (model in unique_models) {
  plot_density_by_model(filtered_data, model,
                        dataset = "asap",
                        outcome = "C16BMVDEG",
                        size_train = 2000,
                        covariate_set = "small",
                        x_range = c(-0.3827, 0.7511)
  )
}

# Call asap, C16BMVDEG, 5000, small
for (model in unique_models) {
  plot_density_by_model(filtered_data, model,
                        dataset = "asap",
                        outcome = "C16BMVDEG",
                        size_train = 5000,
                        covariate_set = "small",
                        x_range = c(-0.3827, 0.7511)
  )
}

# Call asap, C16BMVDEG, 1000, medium
for (model in unique_models) {
  plot_density_by_model(filtered_data, model,
                        dataset = "asap",
                        outcome = "C16BMVDEG",
                        size_train = 1000,
                        covariate_set = "medium",
                        x_range = c(-0.3827, 0.7511)
  )
}

# Call asap, C16BMVDEG, 2000, medium
for (model in unique_models) {
  plot_density_by_model(filtered_data, model,
                        dataset = "asap",
                        outcome = "C16BMVDEG",
                        size_train = 2000,
                        covariate_set = "medium",
                        x_range = c(-0.3827, 0.7511)
  )
}

# Call asap, C16BMVDEG, 5000, medium
for (model in unique_models) {
  plot_density_by_model(filtered_data, model,
                        dataset = "asap",
                        outcome = "C16BMVDEG",
                        size_train = 5000,
                        covariate_set = "medium",
                        x_range = c(-0.3827, 0.7511)
  )
}

# Call asap, C16BMVDEG, 1000, large
for (model in unique_models) {
  plot_density_by_model(filtered_data, model,
                        dataset = "asap",
                        outcome = "C16BMVDEG",
                        size_train = 1000,
                        covariate_set = "large",
                        x_range = c(-0.3827, 0.7511)
  )
}

# Call asap, C16BMVDEG, 2000, large
for (model in unique_models) {
  plot_density_by_model(filtered_data, model,
                        dataset = "asap",
                        outcome = "C16BMVDEG",
                        size_train = 2000,
                        covariate_set = "large",
                        x_range = c(-0.3827, 0.7511)
  )
}

# Call asap, C16BMVDEG, 5000, large
for (model in unique_models) {
  plot_density_by_model(filtered_data, model,
                        dataset = "asap",
                        outcome = "C16BMVDEG",
                        size_train = 5000,
                        covariate_set = "large",
                        x_range = c(-0.3827, 0.7511)
  )
}


###################################################################################


# Call ca, BMEFTMEDIAN, 1000, small
for (queen in unique_queens) {
  plot_density_by_queen(filtered_data, 
                        queen,
                        dataset = "ca",
                        outcome = "BMEFTMEDIAN",
                        size_train = 1000,
                        covariate_set = "small",
                        x_range = c(-0.44964, 0.65331)
  )
}

# Call ca, BMEFTMEDIAN, 2000, small
for (queen in unique_queens) {
  plot_density_by_queen(filtered_data, 
                        queen,
                        dataset = "ca",
                        outcome = "BMEFTMEDIAN",
                        size_train = 2000,
                        covariate_set = "small",
                        x_range = c(-0.44964, 0.65331)
  )
}

# Call ca, BMEFTMEDIAN, 5000, small
for (queen in unique_queens) {
  plot_density_by_queen(filtered_data, 
                        queen,
                        dataset = "ca",
                        outcome = "BMEFTMEDIAN",
                        size_train = 5000,
                        covariate_set = "small",
                        x_range = c(-0.44964, 0.65331))
}

# Call ca, BMEFTMEDIAN, 1000, medium
for (queen in unique_queens) {
  plot_density_by_queen(filtered_data, 
                        queen,
                        dataset = "ca",
                        outcome = "BMEFTMEDIAN",
                        size_train = 1000,
                        covariate_set = "medium",
                        x_range = c(-0.44964, 0.65331))
}

# Call ca, BMEFTMEDIAN, 2000, medium
for (queen in unique_queens) {
  plot_density_by_queen(filtered_data, 
                        queen,
                        dataset = "ca",
                        outcome = "BMEFTMEDIAN",
                        size_train = 2000,
                        covariate_set = "medium",
                        x_range = c(-0.44964, 0.65331))
}

# Call ca, BMEFTMEDIAN, 5000, medium
for (queen in unique_queens) {
  plot_density_by_queen(filtered_data, 
                        queen,
                        dataset = "ca",
                        outcome = "BMEFTMEDIAN",
                        size_train = 5000,
                        covariate_set = "medium",
                        x_range = c(-0.44964, 0.65331))
}

# Call ca, BMEFTMEDIAN, 1000, large
for (queen in unique_queens) {
  plot_density_by_queen(filtered_data, 
                        queen,
                        dataset = "ca",
                        outcome = "BMEFTMEDIAN",
                        size_train = 1000,
                        covariate_set = "large",
                        x_range = c(-0.44964, 0.65331))
}

# Call ca, BMEFTMEDIAN, 2000, large
for (queen in unique_queens) {
  plot_density_by_queen(filtered_data, 
                        queen,
                        dataset = "ca",
                        outcome = "BMEFTMEDIAN",
                        size_train = 2000,
                        covariate_set = "large",
                        x_range = c(-0.44964, 0.65331))
}

# Call ca, BMEFTMEDIAN, 5000, large
for (queen in unique_queens) {
  plot_density_by_queen(filtered_data, 
                        queen,
                        dataset = "ca",
                        outcome = "BMEFTMEDIAN",
                        size_train = 5000,
                        covariate_set = "large",
                        x_range = c(-0.44964, 0.65331))
}

# Call ca, BMEFTMEDIAN, 1000, small
for (model in unique_models) {
  plot_density_by_model(filtered_data, model,
                        dataset = "ca",
                        outcome = "BMEFTMEDIAN",
                        size_train = 1000,
                        covariate_set = "small",
                        x_range = c(-0.44964, 0.65331)
  )
}

# Call ca, BMEFTMEDIAN, 2000, small
for (model in unique_models) {
  plot_density_by_model(filtered_data, model,
                        dataset = "ca",
                        outcome = "BMEFTMEDIAN",
                        size_train = 2000,
                        covariate_set = "small",
                        x_range = c(-0.44964, 0.65331)
  )
}

# Call ca, BMEFTMEDIAN, 5000, small
for (model in unique_models) {
  plot_density_by_model(filtered_data, model,
                        dataset = "ca",
                        outcome = "BMEFTMEDIAN",
                        size_train = 5000,
                        covariate_set = "small",
                        x_range = c(-0.44964, 0.65331)
  )
}

# Call ca, BMEFTMEDIAN, 1000, medium
for (model in unique_models) {
  plot_density_by_model(filtered_data, model,
                        dataset = "ca",
                        outcome = "BMEFTMEDIAN",
                        size_train = 1000,
                        covariate_set = "medium",
                        x_range = c(-0.44964, 0.65331)
  )
}

# Call ca, BMEFTMEDIAN, 2000, medium
for (model in unique_models) {
  plot_density_by_model(filtered_data, model,
                        dataset = "ca",
                        outcome = "BMEFTMEDIAN",
                        size_train = 2000,
                        covariate_set = "medium",
                        x_range = c(-0.44964, 0.65331)
  )
}

# Call ca, BMEFTMEDIAN, 5000, medium
for (model in unique_models) {
  plot_density_by_model(filtered_data, model,
                        dataset = "ca",
                        outcome = "BMEFTMEDIAN",
                        size_train = 5000,
                        covariate_set = "medium",
                        x_range = c(-0.44964, 0.65331)
  )
}

# Call ca, BMEFTMEDIAN, 1000, large
for (model in unique_models) {
  plot_density_by_model(filtered_data, model,
                        dataset = "ca",
                        outcome = "BMEFTMEDIAN",
                        size_train = 1000,
                        covariate_set = "large",
                        x_range = c(-0.44964, 0.65331)
  )
}

# Call ca, BMEFTMEDIAN, 2000, large
for (model in unique_models) {
  plot_density_by_model(filtered_data, model,
                        dataset = "ca",
                        outcome = "BMEFTMEDIAN",
                        size_train = 2000,
                        covariate_set = "large",
                        x_range = c(-0.44964, 0.65331)
  )
}

# Call ca, BMEFTMEDIAN, 5000, large
for (model in unique_models) {
  plot_density_by_model(filtered_data, model,
                        dataset = "ca",
                        outcome = "BMEFTMEDIAN",
                        size_train = 5000,
                        covariate_set = "large",
                        x_range = c(-0.44964, 0.65331)
  )
}

###################################################################################


# Call ca, Y18JBERNA_06, 1000, small
for (queen in unique_queens) {
  plot_density_by_queen(filtered_data, 
                        queen,
                        dataset = "ca",
                        outcome = "Y18JBERNA_06",
                        size_train = 1000,
                        covariate_set = "small",
                        x_range = c(79.88, 214.47)
  )
}

# Call ca, Y18JBERNA_06, 2000, small
for (queen in unique_queens) {
  plot_density_by_queen(filtered_data, 
                        queen,
                        dataset = "ca",
                        outcome = "Y18JBERNA_06",
                        size_train = 2000,
                        covariate_set = "small",
                        x_range = c(79.88, 214.47))
}

# Call ca, Y18JBERNA_06, 5000, small
for (queen in unique_queens) {
  plot_density_by_queen(filtered_data, 
                        queen,
                        dataset = "ca",
                        outcome = "Y18JBERNA_06",
                        size_train = 5000,
                        covariate_set = "small",
                        x_range = c(79.88, 214.47))
}

# Call ca, Y18JBERNA_06, 1000, medium
for (queen in unique_queens) {
  plot_density_by_queen(filtered_data, 
                        queen,
                        dataset = "ca",
                        outcome = "Y18JBERNA_06",
                        size_train = 1000,
                        covariate_set = "medium",
                        x_range = c(79.88, 214.47))
}

# Call ca, Y18JBERNA_06, 2000, medium
for (queen in unique_queens) {
  plot_density_by_queen(filtered_data, 
                        queen,
                        dataset = "ca",
                        outcome = "Y18JBERNA_06",
                        size_train = 2000,
                        covariate_set = "medium",
                        x_range = c(79.88, 214.47))
}

# Call ca, Y18JBERNA_06, 5000, medium
for (queen in unique_queens) {
  plot_density_by_queen(filtered_data, 
                        queen,
                        dataset = "ca",
                        outcome = "Y18JBERNA_06",
                        size_train = 5000,
                        covariate_set = "medium",
                        x_range = c(79.88, 214.47))
}

# Call ca, Y18JBERNA_06, 1000, large
for (queen in unique_queens) {
  plot_density_by_queen(filtered_data, 
                        queen,
                        dataset = "ca",
                        outcome = "Y18JBERNA_06",
                        size_train = 1000,
                        covariate_set = "large",
                        x_range = c(79.88, 214.47))
}

# Call ca, Y18JBERNA_06, 2000, large
for (queen in unique_queens) {
  plot_density_by_queen(filtered_data, 
                        queen,
                        dataset = "ca",
                        outcome = "Y18JBERNA_06",
                        size_train = 2000,
                        covariate_set = "large",
                        x_range = c(79.88, 214.47))
}

# Call ca, Y18JBERNA_06, 5000, large
for (queen in unique_queens) {
  plot_density_by_queen(filtered_data, 
                        queen,
                        dataset = "ca",
                        outcome = "Y18JBERNA_06",
                        size_train = 5000,
                        covariate_set = "large",
                        x_range = c(79.88, 214.47))
}



# Call ca, Y18JBERNA_06, 1000, small
for (model in unique_models) {
  plot_density_by_model(filtered_data, model,
                        dataset = "ca",
                        outcome = "Y18JBERNA_06",
                        size_train = 1000,
                        covariate_set = "small",
                        x_range = c(79.88, 214.47)
  )
}

# Call ca, Y18JBERNA_06, 2000, small
for (model in unique_models) {
  plot_density_by_model(filtered_data, model,
                        dataset = "ca",
                        outcome = "Y18JBERNA_06",
                        size_train = 2000,
                        covariate_set = "small",
                        x_range = c(79.88, 214.47)
  )
}

# Call ca, Y18JBERNA_06, 5000, small
for (model in unique_models) {
  plot_density_by_model(filtered_data, model,
                        dataset = "ca",
                        outcome = "Y18JBERNA_06",
                        size_train = 5000,
                        covariate_set = "small",
                        x_range = c(79.88, 214.47)
  )
}

# Call ca, Y18JBERNA_06, 1000, medium
for (model in unique_models) {
  plot_density_by_model(filtered_data, model,
                        dataset = "ca",
                        outcome = "Y18JBERNA_06",
                        size_train = 1000,
                        covariate_set = "medium",
                        x_range = c(79.88, 214.47)
  )
}

# Call ca, Y18JBERNA_06, 2000, medium
for (model in unique_models) {
  plot_density_by_model(filtered_data, model,
                        dataset = "ca",
                        outcome = "Y18JBERNA_06",
                        size_train = 2000,
                        covariate_set = "medium",
                        x_range = c(79.88, 214.47)
  )
}

# Call ca, Y18JBERNA_06, 5000, medium
for (model in unique_models) {
  plot_density_by_model(filtered_data, model,
                        dataset = "ca",
                        outcome = "Y18JBERNA_06",
                        size_train = 5000,
                        covariate_set = "medium",
                        x_range = c(79.88, 214.47)
  )
}


# Call ca, Y18JBERNA_06, 1000, large
for (model in unique_models) {
  plot_density_by_model(filtered_data, model,
                        dataset = "ca",
                        outcome = "Y18JBERNA_06",
                        size_train = 1000,
                        covariate_set = "large",
                        x_range = c(79.88, 214.47)
  )
}

# Call ca, Y18JBERNA_06, 2000, large
for (model in unique_models) {
  plot_density_by_model(filtered_data, model,
                        dataset = "ca",
                        outcome = "Y18JBERNA_06",
                        size_train = 2000,
                        covariate_set = "large",
                        x_range = c(79.88, 214.47)
  )
}

# Call ca, Y18JBERNA_06, 5000, large
for (model in unique_models) {
  plot_density_by_model(filtered_data, model,
                        dataset = "ca",
                        outcome = "Y18JBERNA_06",
                        size_train = 5000,
                        covariate_set = "large",
                        x_range = c(79.88, 214.47)
  )
}