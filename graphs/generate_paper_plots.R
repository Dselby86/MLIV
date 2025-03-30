
# Plots and Results for Simulation Paper
#
# This script makes all the plots for the Simulation Paper including
# the regression coefficient plots and the contour plots
#
# It is based on the SREE presentation script, updated to include
# other code developed since.


RESULT_DIR = here::here( "graphs/plots_paper" )

library( tidyverse )
library(ggplot2)


source( here::here( "graphs/load_simulation_results.R" ) )
#source( here::here( "graphs/contour_plot_code.R" ) )

# Load plotting functions from R/functions directory and set up some
# color maps
source( here::here( "graphs/functions/plot_support_code.R"))


# Check if the "plot_paper" directory exists, if not, create it
if (!dir.exists(here::here("graphs/plots_paper"))) {
  cat( "Creating directory for paper plots\n" )
  dir.create(here::here("graphs/plots_paper"))
}


table( df$set_id )
n_distinct( df$set_id )

table( df$outcome_type )


# What simulation scenarios do we have? ----

sets <- df %>%
  dplyr::select( outcome_type, dataset, set_id, cov_set_size, train_set_size, model, queen, ) %>%
  group_by( outcome_type, dataset, set_id, cov_set_size, train_set_size ) %>%
  mutate( n_model = n_distinct( model ),
          n_queen = n_distinct( queen ) ) %>%
  ungroup() %>%
  dplyr::select( -model, -queen ) %>%
  unique()

sets %>%
  relocate( set_id ) %>%
  arrange( dataset, outcome_type, cov_set_size, train_set_size ) %>%
  knitr::kable()

table( sets$train_set_size, sets$cov_set_size, 
       dataset = sets$dataset, outcome = sets$outcome_type )


mods <- df %>%
  group_by( model ) %>%
  summarise( n = n_distinct( set_id ) )
mods %>%
  knitr::kable()

queens <- df %>%
  group_by( queen ) %>%
  summarise( n = n_distinct( set_id ) )
queens %>%
  knitr::kable()

# Number of quees
n_distinct(df$queen)


# Look at runtimes and pruning, as initial exploration ----

# NOTE: THIS IS ALL WRONG WRONG SAD!  runtime not calculated correctly.

SLs <- df_agg %>%
  filter( model == "SL S" )
table( SLs$set_id )

# TO include superlearner we filter out 5000
run_times <- df %>%
  group_by( model ) %>%
  filter( set_id %in% SLs$set_id  ) %>%
  summarise( runt = mean( runtime ),
             sd_run = sd( runtime, na.rm = TRUE ),
             per_cut = mean( percent_cut ),
             sd_per = sd( percent_cut ),
             n = n() ) %>%
  arrange( -runt )
run_times %>%
  knitr::kable( digits = 2 )


# NOTE: The runtimes can't be correct.  SL is very low on the list.  Why?  Is it total runtime rather than time / iteration?
# QUESTION: Are the percent cut correct?

# Controlling for set
library( lme4 )
M <- lmer( runtime ~ 1 + outcome_type + cov_set_size + train_set_size + (1|set_id) + (1+cov_set_size+train_set_size|model), data = df )
arm::display(M)
ranef(M)$model


# Set up making the contour plots ----

table( df$set_id )

if ( FALSE ) {
  summary( df$bias )
  summary( df$se )
  filter( df, se > 350 )  
}

max_bias = max( df$bias )
max_se = max( df$se )

# hard coded to make all plots on same dimensions
max_bias = 0.3
max_se = 0.3


# Averaging all the continuous outcomes ----

dd <- df_agg %>%
  dplyr::filter( outcome_type == "bin" ) %>%
  group_by( model, baseline, type, modelshort,  cov_set_size, train_set_size ) %>%
  summarise( n = n(),
             bias = sqrt( mean(bias^2 ) ),
             se = sqrt( mean( se^2 ) ),
             rmse = sqrt( mean( rmse^2 ) ), .groups = "drop" )
dd

p1C <- ggplot( no_core( dd ), aes(se, bias) ) +
  contour_plot_base( max_bias = max_bias, max_se = max_se, step = 0.05, include_scales = FALSE ) +
  facet_grid( cov_set_size ~ train_set_size ) +
  geom_point( aes(color = type), size = 3, shape=19 ) +
  geom_point( data = only_core( dd ), aes(pch=model), col="darkgrey", fill="darkgrey", size=3 ) +
  #  geom_point( data = gbs, size = 7, shape = 1, col="red" ) +
  scale_shape_manual(breaks = c( "ATE", "OLS S", "LASSO INF", "RF INF" ),
                     values = c( 15, 23, 25, 24 ) ) +
  guides(color = guide_legend(title = "Model Type"),
         shape = guide_legend(title = "Reference") ) +
  coord_fixed( xlim=c(0, max_se) ) 

p1C <- p1C + ggrepel::geom_text_repel(
  aes(label = modelshort, col = type),
  size = 2, # Increased label size
  max.overlaps = Inf,
  min.segment.length = Inf,
  show.legend = FALSE
)


p1C


# Do by MODEL ----

oc <- only_core( dd )
#oc$model = NULL

dnc = no_core(dd)
dnc$mod = dnc$model

p1M <- ggplot( df_agg, aes(se,bias) ) +
  contour_plot_base( max_bias = max_bias, max_se = max_se, step = 0.05, include_scales = FALSE ) +
  facet_wrap( ~ model  ) +
  geom_point( aes(shape = as.factor(train_set_size), color = cov_set_size), size = 2 ) +
  #geom_point( data = oc, aes(pch=model), col="darkgrey", fill="darkgrey", size=3 ) +
  #  geom_point( data = gbs, size = 7, shape = 1, col="red" ) +
#  scale_shape_manual(breaks = c( "ATE", "OLS S", "LASSO INF", "RF INF" ),
#                     values = c( 15, 23, 25, 24 ) ) +
  guides(shape = guide_legend(title = "Training Size"),
         color = guide_legend(title = "Num Covariates") ) +
  coord_fixed( xlim=c(0, max_se) )
p1M






# Average Treatment effect queen exploration ----

df_ATE

filter( df_ATE, model == "BART T" )

df_ATE_sub <- df_ATE %>%
  dplyr::filter( !( model %in% c( "ATE", "RF INF", "LASSO INF", "XGBOOST R", "XGBOOST S", "SL S", "SL T" ) ) ) %>%
  dplyr::filter( outcome_type == "cont" ) %>%
  group_by( model, baseline, type, modelshort,  cov_set_size, train_set_size ) %>%
  summarise( n = n(),
             bias = sqrt( mean(bias^2 ) ),
             se = sqrt( mean( se^2 ) ),
             rmse = sqrt( mean( rmse^2 ) ), .groups = "drop" )
df_ATE_sub

ggplot( df_ATE_sub, aes( as.factor(train_set_size), se, col=cov_set_size, group=cov_set_size ) ) +
  facet_wrap( ~ model, nrow=3 ) +
  geom_hline( yintercept = c( 0.1, 0.2 ), col="darkgrey" ) +
  expand_limits( y = 0 ) +
  #geom_point( ) +
  geom_point() + geom_line() +
  theme_minimal() +
  theme( legend.position = "bottom" ) +
  labs( x = "Training Set Size", y = "Average Standard Error", title = "Estimator stability against ATE Queen",
        caption = "ATE_se.pdf")

ggsave( filename = here::here( RESULT_DIR, "ATE_se.pdf" ), width = 7, height = 4 )


# *********************
# *** NOTE: SCRIPT IS NOT UPDATED PAST THIS POINT! *** ----
# *********************


# Explore single simulation scenario ----


ca1_agg = filter(df_agg, 
                 outcome_type == 1,
                 dataset == "ca" )

# Plot of just the RF T queen ----
table( df$queen )

dd = filter( df, 
             cov_set_size == "small", train_set_size == 1000, queen == "RF T",
             dataset == "ca", outcome_type == 1 )
table( dd$set_id )
sd( dd$rmse )
summary( dd$rmse )

p1C <- ggplot( no_core( dd ), aes(se, bias) ) +
  contour_plot_base( max_bias = max_bias, max_se = max_se, step = 0.05, include_scales = FALSE ) +
  geom_point( aes(color = type), size = 4, shape=19 ) +
  geom_point( data = only_core( dd ), aes(pch=model), col="darkgrey", fill="darkgrey", size=4 ) +
  #  geom_point( data = gbs, size = 7, shape = 1, col="red" ) +
  scale_shape_manual(breaks = c( "ATE", "OLS S", "LASSO INF", "RF INF" ),
                     values = c( 15, 23, 25, 24 ) ) +
  guides(color = guide_legend(title = "Model Type"),
         shape = guide_legend(title = "Reference") ) +
  coord_fixed( xlim=c(0, max_se) ) 

p1C <- p1C + ggrepel::geom_text_repel(
  aes(label = modelshort, col = type),
  size = 3, # Increased label size
  max.overlaps = Inf,
  min.segment.length = Inf,
  show.legend = FALSE
)

p1C

IMAGE_WIDTH = 6

ggsave( filename = "graphs/plot_paper/contour_ca1_1000_small_RFT_legend.pdf", width = 7, height = 5 )



# Now make the plot without the legend

p1C + theme(legend.position = "none")

ggsave( filename = "graphs/plot_paper/contour_ca1_1000_small_RFT.pdf", width = IMAGE_WIDTH, height = 5 )



# Plot averaged across queens ----

table( df$queen )

dd = filter( ca1_agg, outcome_type == 1, 
             cov_set_size == "small", 
             train_set_size == 1000 )

p1C <- ggplot( no_core( dd ), aes(se, bias) ) +
  contour_plot_base( max_bias = max_bias, max_se = max_se, include_scales = FALSE ) +
  geom_point( aes(color = type), size = 4, shape=19 ) +
  geom_point( data = only_core( dd ), aes(pch=model), col="darkgrey", fill="darkgrey", size=4 ) +
  #  geom_point( data = gbs, size = 7, shape = 1, col="red" ) +
  scale_shape_manual(breaks = c( "ATE", "OLS S", "LASSO INF", "RF INF" ),
                     values = c( 15, 23, 25, 24 ) ) +
  guides(color = guide_legend(title = "Model Type"),
         shape = guide_legend(title = "Reference") ) +
  coord_fixed( xlim=c(0, max_se) ) +
  theme( legend.position = "none" )


p1C + ggrepel::geom_text_repel(
  aes(label = modelshort, col = type),
  size = 3, # Increased label size
  max.overlaps = Inf,
  min.segment.length = Inf,
  show.legend = FALSE
)

ggsave( filename = "graphs/plot_paper/contour_all_1000.pdf", width = IMAGE_WIDTH, height = 5 )



# Moving to large covariate set plot  ----

d_b = make_trail_set( filter( ca1_agg, 
                              train_set_size == 1000,
                              outcome_type == 1,
                              cov_set_size=="small"),
                      filter( ca1_agg, 
                              train_set_size == 1000,
                              outcome_type == 1,
                              cov_set_size=="large" ) )


pltB <-  ggplot( no_core( d_b ), aes(se, bias) ) +
  contour_plot_base( max_bias = max_bias, max_se = max_se, include_scales = FALSE ) +
  geom_segment( data= d_b, aes(x = se_pre, y = bias_pre, xend = se, yend = bias),
                #arrow = arrow(length = unit(0.2, "cm")),
                lwd = 1, col="lightgrey") +
  geom_point( data = no_core( d_b ), aes(color = type), size = 4, shape=19 ) +
  geom_point( data = only_core( d_b ), aes(pch=model), col="darkgrey", fill="darkgrey", size=4 ) +
  #labs( title = "Trails of Models from 1000 to 2000 training size",
  #      subtitle = "Aggregated over scenarios and queens" ) + 
  #scale_size_identity() +
  scale_shape_manual(breaks = c( "ATE", "OLS S", "LASSO INF", "RF INF" ),
                     values = c( 15, 23, 25, 24 ) ) +
  guides(color = guide_legend(title = "Model Type"),
         shape = guide_legend(title = "Reference") ) +
  coord_fixed( xlim=c(0, max_se) ) +
  theme( legend.position = "none" )

pltB



pltB + ggrepel::geom_text_repel(
  aes(label = modelshort, col = type),
  size = 3, # Increased label size
  max.overlaps = Inf,
  min.segment.length = Inf,
  show.legend = FALSE
)

ggsave( filename = "graphs/plot_paper/contour_trail_to_large.pdf", 
        width = IMAGE_WIDTH, height = 5 )







# Moving to large training set plot  ----

d_b = make_trail_set( filter( ca1_agg, 
                              outcome_type == 1,
                              train_set_size == 1000,
                              cov_set_size=="large"),
                      filter( ca1_agg, train_set_size == 2000, cov_set_size=="large" ) )


pltB <-  ggplot( no_core( d_b ), aes(se, bias) ) +
  contour_plot_base( max_bias = max_bias, max_se = max_se, include_scales = FALSE ) +
  geom_segment( data= d_b, aes(x = se_pre, y = bias_pre, xend = se, yend = bias),
                #arrow = arrow(length = unit(0.2, "cm")),
                lwd = 1, col="lightgrey") +
  geom_point( data = no_core( d_b ), aes(color = type), size = 4, shape=19 ) +
  geom_point( data = only_core( d_b ), aes(pch=model), col="darkgrey", fill="darkgrey", size=4 ) +
  #labs( title = "Trails of Models from 1000 to 2000 training size",
  #      subtitle = "Aggregated over scenarios and queens" ) + 
  #scale_size_identity() +
  scale_shape_manual(breaks = c( "ATE", "OLS S", "LASSO INF", "RF INF" ),
                     values = c( 15, 23, 25, 24 ) ) +
  guides(color = guide_legend(title = "Model Type"),
         shape = guide_legend(title = "Reference") ) +
  coord_fixed( xlim=c(0, max_se) ) +
  theme( legend.position = "none" )

pltB



pltB + ggrepel::geom_text_repel(
  aes(label = modelshort, col = type),
  size = 3, # Increased label size
  max.overlaps = Inf,
  min.segment.length = Inf,
  show.legend = FALSE
)

ggsave( filename = "graphs/plot_paper/contour_trail_to_2000.pdf", width = IMAGE_WIDTH, height = 5 )




# Binary outcome ----

bin <- df %>% 
  dplyr::filter( outcome_type == 2 )
table( bin$model )

filter( df, outcome_type == 2, model == "LASSO INF" ) %>%
  dplyr::select( -bias, -rmse, -se, -outcome_type )

filter( df, outcome_type == 2, model == "BART T" ) %>%
  dplyr::select( -bias, -rmse, -se, -outcome_type )

if ( FALSE ) {
  
  asap <- df %>% 
    dplyr::filter( dataset == "asap" )
  table( asap$model )
  
  group_by( model, modelshort, type, baseline ) %>%
    summarise( n = n(),
               bias = sqrt( mean(bias^2 ) ),
               se = sqrt( mean( se^2 ) ),
               rmse = sqrt( mean( rmse^2 ) ), .groups = "drop" )
  
  bin <- df_agg %>% 
    dplyr::filter( outcome_type == 2, dataset == "ca" )
  
  dd = bin
  max_bias = 0.15 #max( dd$bias )
  max_se = 0.15 #max( dd$se )
  p1C <- ggplot( no_core( dd ), aes(se, bias) ) +
    contour_plot_base( max_bias = max_bias, max_se = max_se, include_scales = FALSE, step = 0.025 ) +
    geom_point( aes(color = type), size = 4, shape=19 ) +
    geom_point( data = only_core( dd ), aes(pch=model), col="darkgrey", fill="darkgrey", size=4 ) +
    #  geom_point( data = gbs, size = 7, shape = 1, col="red" ) +
    scale_shape_manual(breaks = c( "ATE", "OLS S", "LASSO INF", "RF INF" ),
                       values = c( 15, 23, 25, 24 ) ) +
    guides(color = guide_legend(title = "Model Type"),
           shape = guide_legend(title = "Reference") ) +
    coord_fixed( xlim=c(0, max_se) ) +
    theme( legend.position = "none" )
  
  p1C + ggrepel::geom_text_repel(
    aes(label = modelshort, col = type),
    size = 3, # Increased label size
    max.overlaps = Inf,
    min.segment.length = Inf,
    show.legend = FALSE
  )
  ggsave( filename = "graphs/plot_paper/binary_ca.pdf", width = IMAGE_WIDTH, height = 5 )
  
}


# By queen ----


df <- mutate( df, is_ATE = ifelse( queen == "ATE", "ATE", "Not ATE" ) )

table( df$model )

ggplot( df, aes(se, bias, xmax=max_se, ymax=max_bias, col=is_ATE, pch= ) ) +
  facet_wrap( ~ model, nrow=3 ) +
  contour_plot_base( max_bias = max_bias, max_se = max_bias, include_scales = FALSE, step=100 ) +
  geom_point( size = 1, shape=19, alpha=0.8 ) +
  labs( #title = "All queens for all models across all CA sets",
    color = "ATE Queen" ) + 
  scale_size_identity() +
  scale_color_manual(breaks = c("ATE", "Not ATE"),
                     values = c( "red", "black" ) ) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none" )


ggsave( filename = "graphs/plot_paper/all_queen_plot.pdf", width = 11, height = 6 )





# Performance across 6 scenarios

table( ca1_agg$model )


ggplot( ca1_agg, aes( cov_set_size, rmse, group=as.factor(train_set_size), col=as.factor(train_set_size) ) ) +
  facet_wrap( ~model ) +
  geom_line() + geom_point() +
  theme_minimal() +
  theme( panel.grid.minor = element_blank() )




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
  dplyr::filter( outcome_type == 1,
                 dataset == "ca" ) %>%
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


## Make the final coefficient plots ----


# Plot model and queen on single plot
library( scales )
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

ggsave( filename = "graphs/plot_paper/coef_plot.pdf", width = 11, height = 6 )



# Plot the model coefficients only

cs$termW = cs$term
lvl = levels( cs$termW )
levels( cs$termW ) <- str_pad(lvl, 
                              width = max(nchar(lvl)), 
                              side = "left")


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
  scale_y_continuous( limits = c( -125, 240)) +
  theme( plot.margin = unit(c(1, 0.1, 0.1, 1.5), "cm") ) +
  scale_x_discrete( labels = label_wrap(10) )

ggsave( filename = "graphs/plot_paper/coef_plot.pdf",
        width = 7, height = 6 ) 


# Plot the queens only

# TODO: I don't like the shift relative to ATE.  Hard to see variation
# in the rmse across queens.  Is this ideal?

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
  scale_y_continuous( limits = c( -125, 240)) +
  theme( plot.margin = unit(c(1, 0.1, 0.1, 1.5), "cm") ) +
  scale_x_discrete( labels = label_wrap(10) )

ggsave( filename = "graphs/plot_paper/coef_plot_queen.pdf",
        width = 7, height = 6 ) 



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
  scale_y_continuous( limits = c( -125, 240)) +
  theme( plot.margin = unit(c(1, 0.1, 0.1, 1.5), "cm") ) +
  scale_x_discrete( labels = label_wrap(10) )




ggsave( filename = "graphs/plot_paper/ex_factor_plot.pdf", 
        width = 7, height = 6 ) 



# Plot everything on single plot

# Fix the ordering of factors to keep grouping

csA$term <- fct_reorder( factor( csA$term ), 
                         100 * as.numeric( as.factor( csA$queen ) ) + (csA$measure =="rmse") * csA$estimate )
levels( csA$term )
ggplot( csA, aes( term, estimate, color = measure ) ) +
  geom_hline( yintercept = 0 ) +
  geom_point( position = position_dodge(width = 0.4) ) +
  geom_errorbar( aes( ymin = estimate - 2*std.error, ymax = estimate + 2*std.error ),
                 position = position_dodge(width = 0.4), 
                 width = 0 ) +
  coord_flip( clip="on") +
  theme_minimal() +
  labs( x = "", y="" ) +
  scale_y_continuous( limits = c( -125, 240)) +
  theme( plot.margin = unit(c(1, 0.1, 0.1, 1.5), "cm") ) +
  scale_x_discrete( labels = label_wrap(10) )


