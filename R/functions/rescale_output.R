#' rescale_output:: function to read in and widen simulation output
#' 
#' @param df the wide simulation output
#' @param realds_name the name of the dataset, like 'asap_subset_imputed'
#' @param treatment_name, the name of the the treatment variable
#' @param outcome_name, the name of the outcome you're exploring
#' 
#' @return A numeric vector representing the generated Z-scores.
#' 
#
rescale_output = function(df, realds_name, treatment_name, outcome_name){
  
  df = ca
  realds_name="ca_data"
  treatment_name = "TREATMNT"
  outcome_name = "Y18JBERNA_06"
  # Create a new data frame with only rows where metric is "runtime"
  df_runtime = df[df$metric == "runtime", ]
  
  # Create a new data frame without rows where metric is "runtime"
  df_without_runtime = df[df$metric != "runtime", ]
  
  # Aggregate performance by queen
  agg_perf_by_queen_df_without_runtime  = aggregate_metrics_by_queen( df_without_runtime )
  
  # Table performance
  sim_results  <- agg_perf_by_queen_df_without_runtime  %>% 
    dplyr::select( -Q1, -Q3 ) %>%
    pivot_wider( names_from = "metric",
                 values_from = "Ev" ) %>%
    mutate( rmse_check = sqrt( bias^2 + se^2 ) ) %>%
    mutate( check_difference = rmse - rmse_check ) %>%
    arrange(factor(model, levels = ALL_MODELS), factor(queen, levels = ALL_MODELS))
  
  #rescale output using control-side SD
  #get SD for control-side from real data
  realdataproj = strsplit(realds_name, "[_]")[[1]][1]
  realds_data = get(realds_name)
  
  #save the SD and nlevels for this outcome in sd_0, which we'll use to divide later
  sd_0 = sd(realds_data[
    realds_data[,c(treatment_name)]==0 # just treatment == 0 rows
    ,  # just the outcome of interest
  ][[outcome_name]], na.rm=TRUE)
  
  
  nlevels = n_distinct(realds_data[
    realds_data[,treatment_name]==0 # just treatment == 0 rows
   ,# just the outcome of interest
  ][[outcome_name]])
  
  sim_results = sim_results%>%
    mutate(bias=bias/sd_0
           , rmse = rmse/sd_0
           , se = se/sd_0
           , sd_0 = sd_0
           , nlevels = nlevels
    )
  
  sim_results[sim_results$nlevels>3, c("tx_het")] = 0.2
  sim_results[sim_results$nlevels> 0 & sim_results$nlevels<=3, c("tx_het")] = 0.1
  sim_results$outcome = outcome_name
  return (sim_results)
}
#' @examples
#' \dontrun{
#'   y <- c(1, 2, 3, 4, 5)
#'   z_scores <- convert_to_z(y)
#'   print(z_scores)
#' }
#'
#' @export
#' 
convert_to_z <- function(Y) {
  
  # Calculate the number of observations in Y
  K <- length(Y)
  
  # Calculate the percentile of each Y
  r_Y <- rank(Y, ties.method = "random")/K - 1/(2*K)
  
  # Calculate the Z-scores of Y, given percentile.
  z <- qnorm(r_Y)
  
  # Return the calculated Z-scores
  return(z)
}

