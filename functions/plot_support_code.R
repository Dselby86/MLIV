
# Functions for managing simulation results to make plots


# Load functions from R/functions ----

# datapasta::vector_paste_vertical( list.files(here::here("R", "functions") ) )

funcs <- c( "contour_onequeen.R",
            "contour_plot_base.R",
            "make_contour_plot.R",
            "scenario_plot.R",
            "stack_miniplots.R" )
purrr::walk( funcs, \(x) source( here::here( "R/functions/", x ) ) )



# Function to subset to baseline models or main models ----


no_core <- function( df_set ) {
  #df_set %>% filter( !model %in% c( "ATE", "OLS S", "LASSO INF", "RF INF" ) )
  df_set %>% filter( !baseline )
}
only_core <- function( df_set ) {
  #df_set %>% filter( model %in% c( "ATE", "OLS S", "LASSO INF", "RF INF" ) )
  df_set %>% filter( baseline )
}



# Function to make RMSE contour plots ----

TYPE_SHAPE_MAP <- c(
  "ATE" = 20, "OLS S" = 18, 
  "INF" = 2, "RF" = 3, "CDML" = 8,
  "LASSO" = 10, "SL" = 16,
  "XGBOOST" = 1, "BART" = 0
)

TYPE_COLOR_MAP <- c(
  "ATE" = "black", "OLS S" = "black", 
  "INF" = "darkgrey", "RF" = "#E69F00", "CDML" = "#F0E442",
  "LASSO" = "#009E73", "SL" = "#D55E00",
  "XGBOOST" = "#CC79A7", "BART" = "#0072B2" # "#56B4E9"
)
LEGEND_COLORS = setdiff( names(TYPE_COLOR_MAP), c("LASSO INF", "RF INF", "ATE", "OLS S") )

# For individual methods
SHAPE_MAP <- c(
  "ATE" = 0, "OLS S" = 1, 
  "RF INF" = 2, "RF T" = 3, "RF MOM IPW" = 4, "RF MOM DR" = 5, 
  "CF" = 6, "CF LC" = 7, "CDML" = 8, "LASSO INF" = 9, "LASSO T" = 10, "LASSO MOM IPW" = 11, 
  "LASSO MOM DR" = 12, "LASSO MCM" = 13, "LASSO MCM EA" = 14, "LASSO R" = 15, 
  "SL T" = 16, "SL S" = 17, "XGBOOST S" = 18, "XGBOOST R" = 19, "BART T" = 20, "BART S" = 21
)




