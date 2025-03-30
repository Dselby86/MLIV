#' read_and_aggregate:  reads simulation results, standardizes continuous outcomes, and aggregates to the mean across all queens
#'
#' @param relative_path A path to the RDS with the stored simulation results
#' @param sd A number specifying the standard deviation of the baseline outcome
#' @param cov_set_size A character string specifying the size of the covariate set
#' @param dataset A character string specifying the name of the dataset
#' @param outcome_index A number specifying the index of the outcome within the dataset
#' @param train_set_size A character string specifying the size of the train set used for the simulation 

read_and_aggregate = function(relative_path, 
                              sd, cov_set_size, dataset, outcome_index, train_set_size,
                              result_dir = NULL,
                              verbose = 1 ) {
  
  
  # Standardize results - comment out after computing and use load() instead
  # Load baseline data to compute standard deviations
  # bl_asap = readRDS("/data/share/cdi/MLIV/Data/Big Jobs/baseline/asap_baseline_object.rds")
  # bl_ca = readRDS("/data/share/cdi/MLIV/Data/Big Jobs/baseline/ca_baseline_object.rds")
  # 
  # # Compute standard deviations
  # # Get names of continuous variables
  # bl_asap_cont = paste0(names(bl_asap$is_binary[bl_asap$is_binary==FALSE]),"_Y0" )
  # # Select only continuous variables
  # bl_asap_cont_outcomes = select(bl_asap$Data, bl_asap_cont)
  # # Get SD
  # bl_asap_sd =apply(bl_asap_cont_outcomes,2,sd) 
  # 
  # # Same for CA
  # bl_ca_cont = paste0(names(bl_ca$is_binary[bl_ca$is_binary==FALSE]),"_Y0" )
  # bl_ca_cont_outcomes = select(bl_ca$Data, bl_ca_cont)
  # bl_ca_sd =apply(bl_ca_cont_outcomes,2,sd) 
  # 
  # # Combine CA and ASAP
  # bl_sd = data.frame(t(c(bl_asap_sd, bl_ca_sd)))
  # 
  # #Save SD dataset  
  # write_xlsx(bl_sd, path = here::here("graphs/combined/baseline_outcome_sd.xlsx"))
  
  #load pre-saved standard deviations
  bl_sd = read.xlsx(here::here("graphs/combined/baseline_outcome_sd.xlsx"))
  
  #load standard deviation depending on dataset: 
  # CA outcome 1 is Y18JBERNA_06_Y0, 
  # ASAP outcome 2 is X16BTMCRET_Y0
  # ASAP outcome 3 is SessionsEnrolled_Y0
  
  # Default scaling unless need to shift to standardize effect units
  div_sd = 1
  
  if (dataset=="ca" && outcome_index == 1){
    div_sd = bl_sd$Y18JBERNA_06_Y0
  }
  if (dataset=="asap" && outcome_index == 2){
    div_sd = bl_sd$X16BTMCRET_Y0
  }
  if (dataset=="asap" && outcome_index == 3){
    div_sd = bl_sd$SessionsEnrolled_Y0 
  }
  
  #add more outcomes here as needed 
  
  #binary variables, no standardization so just dividing by 1
  if ((dataset=="asap" && outcome_index == 1 ) || (dataset=="ca" && outcome_index == 2 ) ){
    div_sd = 1
  }
  
  #Load the raw (but aggregated) output of the simulation
  if ( is.null( result_dir ) ) {
    result_dir = paste0( "/data/share/cdi/mliv/local repos/bigjobs/MLIV/", relative_path )
  } else {
    result_dir = paste0( result_dir, relative_path )
  } 
  
  if ( verbose > 0 ) {
    cat( glue::glue( "Reading results from {result_dir}\n" ) )
    cat( "\n" )
  }
  
  df = readRDS( result_dir )
  
  
  #Standardize results using SD created above
  df = df %>% mutate(across(!c(metric, id, queen), ~ . / div_sd))
  
  # Aggregate performance (Bias, SE, RMSE) by queen
  agg_perf_by_queen = aggregated_performance_by_queen(df)
  
  # Reshape agg_perf_by_queen so metrics are the columns
  wide_Ev_agg_perf_by_queen = pivot_wider(
    data = agg_perf_by_queen,
    id_cols = c("model", "queen" ),
    names_from = metric,
    values_from = c( Ev, Q1, Q3 ),
    names_prefix = ""
  ) %>%
    rename( bias = Ev_bias, se = Ev_se, rmse = Ev_rmse )
  
  # Now percent cut and runtime
  crt <- aggregate_outliers_by_queen(df)
  rt <- aggregate_time_by_queen(df)
  rt <- left_join( rt, crt, by = c("model", "queen") )
  wide_Ev_agg_perf_by_queen <- wide_Ev_agg_perf_by_queen %>%
    left_join( rt, by = c("model", "queen") )
  
  # Order by model by queen
  wide_Ev_agg_perf_by_queen <- wide_Ev_agg_perf_by_queen  %>% 
    arrange(factor(model, levels = ALL_MODELS), factor(queen, levels = ALL_MODELS))
  
  wide_Ev_agg_perf_by_queen$cov_set_size = cov_set_size
  wide_Ev_agg_perf_by_queen$dataset = dataset
  wide_Ev_agg_perf_by_queen$outcome_index = outcome_index
  wide_Ev_agg_perf_by_queen$train_set_size = train_set_size
  
  
  # Add type column
  wide_Ev_agg_perf_by_queen = wide_Ev_agg_perf_by_queen %>%
    mutate(type = case_when(
      model == "ATE" ~ "ATE",
      model == "OLS S" ~ "OLS S",
      model %in% c("RF INF", "LASSO INF") ~ "INF",
      model %in% c("RF T", "RF MOM IPW", "RF MOM DR", "CF", "CF LC") ~ "RF",
      model %in% c("LASSO T", "LASSO MOM IPW", "LASSO MOM DR", "LASSO MCM", "LASSO MCM EA", "LASSO R") ~ "LASSO",
      model == "CDML" ~ "CDML",
      model %in% c("BART T", "BART S") ~ "BART",
      model %in% c("XGBOOST S", "XGBOOST R") ~ "XGBOOST",
      model %in% c("SL T", "SL S") ~ "SL",
      TRUE ~ "Unknown"
    ))%>%
    mutate(modelshort = case_when(
      model %in% c("OLS S", "ATE", "CDML") ~ type,
      .default = trimws(str_remove(model, type))))
  
  return (wide_Ev_agg_perf_by_queen)
}
