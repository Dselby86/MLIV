#' make_cs::  make coefficient set to use for regression plots (taken from sree_2024_plots.R)
#' 
#' @param df dataset to use for regression analysis (variables should include outcome, dataset, etc)
#' @param scen_dataset dataset to filter from all_scenarios
#' @param scen_outcome outcome to filter from all_scenarios
#' 
#' @return outputs coefficient set
#'
#' @export
#' 
#' 
make_cs = function(df, scen_dataset, scen_outcome){
  
  
  # Regression plots ----
  
  # These plots plot the results of regressing rmse, se, and bias onto
  # the simulation conditions and queen and model.
  #
  # We fit three models, and then make plots of the different families
  # of coefficients.
  
  
  # Prep data for regression by making ATE the reference category for
  # both queen and model.
  df
  df_sub <- df %>%
    dplyr::filter( outcome == scen_outcome,
                   dataset == scen_dataset ) %>%
    mutate( model = relevel( factor( model ), ref = "ATE" ),
            queen = relevel( factor( queen ), ref = "ATE" ),
            cov_set_size = factor( cov_set_size, levels = c("small","medium","large" ) ),
            N2000 = 0 + (train_set_size==2000),
    ) %>%
    dplyr::filter( !str_detect( model, "INF"))
  
  
  ## Match queen and model for "home field advantage" flag ----
  
  
  table( df_sub$model )
  table( df_sub$queen )
  
  setdiff( unique( df_sub$model ), 
           unique( df_sub$queen ) )
  
  # This tries to get which models are "at home" with which queens.
  #
  # TODO: Update this to generate a "linear model/linear queen" match.
  home <- function( model, queen ) {
    if ( model == queen ) {
      return( TRUE )
    }
    if ( model == "BART S" ) {
      return (queen == "BART T" )
    }
    if ( str_detect( model, "LASSO" ) ) {
      return( str_detect( queen, "LASSO" ) )
    }
    if ( model == "SL S" ) {
      return (queen == "SL T" )
    }
    if ( str_detect( model, "RF MOM" ) ) {
      return( queen == "RF MOM IPW" )
    }
    if ( model == "CF LC" ) {
      return( queen == "CF" )
    }
    return( FALSE )
  }
  
  # testing code
  home( df_sub$model[[4]], df_sub$queen[[4]] )
  
  # Calculate home for each model
  df_sub <- mutate( df_sub,
                    home = map2_dbl( model, queen, home ) )
  
  
  ## Fit the linear models & make result tables ----
  
  M_rmse = lm( rmse ~ 1 + model + queen + cov_set_size + N2000 + home, data = df_sub )
  M_se = lm( se ~ 1 + model + queen + cov_set_size + N2000 + home, data = df_sub )
  M_bias = lm( bias ~ 1 + model + queen + cov_set_size + N2000 + home, data = df_sub )
  
  
  
  library( broom )
  c_rmse <- tidy( M_rmse ) %>%
    dplyr::select( term, estimate, std.error )
  c_se <- tidy( M_se ) %>%
    dplyr::select( term, estimate, std.error )
  c_bias <- tidy( M_bias ) %>%
    dplyr::select( term, estimate, std.error )
  
  cs <- bind_rows( rmse = c_rmse, se = c_se, bias = c_bias, .id = "measure" )
  cs$term = str_replace( cs$term, "cov_set_size", "cov_" )
  cs$term = str_replace( cs$term, "queen", "Q " )
  cs$term = str_replace( cs$term, "model", "model " )
  table( cs$term )
  
  cs <- cs %>%
    dplyr::filter( term != "(Intercept)" ) %>%
    mutate( queen = ifelse( str_detect( term, "Q " ), "queen",
                            ifelse( str_detect( term, "model" ), "model", "coef" ) ),
            term = str_replace(term, "model |queen ", "" ) )
  
  csA = cs
  
  c_coef <- filter( csA,
                    term %in% c( "N2000", "cov_medium", "cov_large", "home", "(Intercept)" ) ) %>%
    arrange( term ) %>%
    dplyr::select( -queen ) %>%
    relocate( term )
  
  
  cs <- cs %>%
    dplyr::filter( !term %in% c( "N2000", "cov_medium", "cov_large", "home", "(Intercept)" ) )
  table( cs$term )
  
  # Sort the coefficients by performance in terms of rmse
  ord = cs %>% 
    dplyr::filter( measure =="rmse" ) %>%
    mutate( f = reorder( factor( term ), estimate ) )
  ord$f
  
  cs
  cs <- mutate( cs, 
                term = factor( term, levels = levels(ord$f) ) )
  cs
  
  levels( cs$term[ cs$queen == "queen" ] )
  
  
 return(cs) 
}