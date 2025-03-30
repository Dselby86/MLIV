

# Load the combined simulation results (aggregated across the 10,000
# unit training set).
#
# Once loaded, do a small amount of cleanup, such as making shorter
# model names.
#
# It also aggregates each scenario across queens (except ATE) for all
# scenarios to give average performance dataset as well.
#
# Resulting files are:
# - df (all results)
# - df_agg (averaged across queen)
# - df_ATE (ATE queen only)
# - asap (ASAP data only)


library( tidyverse )

source( here::here( "graphs/functions/aggregate_functions.R" ) )


# Load results ----
df = readxl::read_excel( here::here( "results_final/master_aggregated_CATE.xlsx" ) )



# Add in indicator of outcome type ----

df$outcome_type = "cont"
df$outcome_type[ df$outcome == "C16BMVDEG" | df$outcome == "Y18JBERNA_06" ] = "bin"
table( df$outcome_type )


# Clean results and make set id ----

df <- mutate( df,
              set_id = paste( dataset, str_sub( outcome_type, 0, 1 ), cov_set_size, train_set_size, sep="-" ),
              baseline = model %in% c( "ATE", "OLS S", "LASSO INF", "RF INF" ) )

names( df )
table( df$set_id )

table( df$queen )

table( df$model )

table( df$set_id, df$model )

# XGBOOST freak out ----
# TODO: Removing XGBOOST due to very odd behavior under the ATE queen.

# In particular we are seeing extreme bias estimates, indicating
# something is wrong with implementation.


ALL_MODELS <- unique( df$model )


if ( FALSE ) {
  df = filter( df, model != "XGBOOST R", model != "XGBOOST S" )
  
  
  mm <- df %>%
    dplyr::filter( set_id == "asap-1-large-5000" )

  table( mm$model, mm$queen )  
  
  mm %>%
    dplyr::filter( queen == "CF", model == "CF" )
  
  # How many duplicated rows in df?
  sum( duplicated( df ) )
  
  # Filter duplicated rows into df_dup
  df_dup <- df[ duplicated( df ), ]
  table( df_dup$set_id )

}

if ( sum( duplicated( df ) ) > 0 ) {
  warning( "Duplicated rows in master result file\n" )
  df <- df[ !duplicated( df ), ]
}


# Update naming and tags for clarity and shorter names ----

# Keep ATE and OLS as baseline, and drop labels for the baseline models
df <- mutate( df,
                  baseline = model %in% c( "ATE", "OLS S", "LASSO INF", "RF INF" ),
                  modelshort = ifelse( baseline, "", modelshort ) )


table( df$modelshort )

df$modelshort <- str_replace( df$modelshort, "MOM ", "" )
df$modelshort <- str_replace( df$modelshort, "CF LC", "CF-LC" )
df$modelshort <- str_replace( df$modelshort, "MCM EA", "EA" )

table( df$modelshort )


# Drop to the canonical set of models and queens ----


warning( "Dropping extraneous models and queens" )

DEFAULT_QUEEN_LIST = c(
  "OLS S", "ATE", "RF T", "RF MOM IPW", "CF",
  "LASSO R", "CDML", "BART T", "SL T", "XGBOOST R")

#DEFAULT_MODEL_LIST = c( "OLS S",  "ATE", "RF INF", "RF T", "RF MOM IPW", "RF MOM DR", "CF", "CF LC", 
#                      "LASSO INF", "LASSO T", "LASSO MOM IPW", "LASSO MOM DR",  "LASSO MCM",    
#                      "LASSO MCM EA", "LASSO R", "CDML", "BART T" , "BART S")

df = filter( df, queen %in% DEFAULT_QUEEN_LIST )


# Aggregate across queens, except for ATE ----

df_agg <- df %>%
  group_by( set_id, dataset, outcome, cov_set_size, train_set_size,
            model, baseline, type, modelshort, outcome_type ) %>%
  filter( queen != "ATE" ) %>%
  summarise( n = n(),
             bias = sqrt( mean(bias^2 ) ),
             se = sqrt( mean( se^2 ) ),
             rmse = sqrt( mean( rmse^2 ) ), .groups = "drop" )
df_agg

table( df_agg$set_id )



# Make the ATE dataset ----
df_ATE = filter( df, queen == "ATE" )

