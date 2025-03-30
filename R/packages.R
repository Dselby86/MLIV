library(furrr)
options(scipen = 999)
library(here)
library (readr)
library( grf )   
library(abind)
tryCatch(
  library( causalDML ),                    # double ML based methods reviewed in Knaus (2022)
  error = function(e) {
    # Print a message if the library is not found
    message("Library not found: ", e$message)
  }
)

library( caret )  
library(purrr)


# Run by hand once to install the packages to prevent any package
# errors (change FALSE to TRUE)
if ( FALSE ) {
  install.packages("randomForest")
  install.packages( "tidyverse" )
  install.packages( "MASS" )
  install.packages( "arm" )
  install.packages( "synthpop" )
  install.packages( "labelled" )
  install.packages( "grf" )
  install.packages( "glmnet" )
  install.packages( "caret" )
  install.packages( "devtools" )

  install.packages( "abind" )

  #install.packages( "Rforestry" )

  
  
  # casualDML may not load if R cannot find fortran.  To solve this (Mac solution only):
  #   1. Install homebrew
  #   2. Use homebrew to install gcc
  #   3. Edit config file to tell R where to find fortran
  # For details, see https://stackoverflow.com/questions/69639782/installing-gfortran-on-macbook-with-apple-m1-chip-for-use-in-r
  # This samne problem may occur with other packages that require fortran, such as KRLS
  
  
  install.packages( "reshape2" )
  install.packages( "dgof" )
  install.packages( "car" )
  install.packages( "cocor" )
  install.packages( "openxlsx" )
  install.packages( "ggplot2" )
  install.packages( "ggrepel" )
  install.packages( "plotly" )
  install.packages( "formattable" )
  #install.packages( "dbarts" ) # Does not work anymore
  install.packages("remotes")
  remotes::install_github("vdorie/dbarts")
  install.packages( "fastDummies" )
  
  library( devtools ) # To install packages that are not on CRAN
  install_github( repo="MCKnaus/causalDML" )
  

  install_github( "xnie/rlearner" )
  install.packages( "roperators" )
  install.packages( "gridExtra" )
  install.packages( "SuperLearner" )
  install.packages( "purrr")
  
  install.packages("broom.mixed")
  install.packages("pander")
  install.packages("glue")
}


library( tidyverse )                    # suite of packages in for manipulating/tidying/visualizing, etc. data
library( MASS )                         # AIC, BIC functions
library( randomForest )                 # random forest models
library( arm )                          # regression and multilevel/hierarchical models
library( synthpop )                     # generates fake data
library( labelled )                     # for labels
library( grf )                          # GRF provides non-parametric methods for heterogeneous treatment effects estimation
library( glmnet )                       # lasso and elastic-net regularized generalized linear models
library( caret )                        # Regression and Classification models
require( roperators )                   # make your R code nicer with roperators
#library( Rforestry )                    # Dependent package of causalToolbox

tryCatch(
  library( causalDML ),                    # double ML based methods reviewed in Knaus (2022)
  error = function(e) {
    # Print a message if the library is not found
    message("Library not found: ", e$message)
  }
)



library( rlearner )                     # For boost-based model/learner combinations
library( formattable )                  # for coloring tables
library( plotly )                       # for interactive plots
library( ggrepel )                      # to label plots
library( reshape2 )                     # to reshape TAU matrix
library( car )                          #
library( cocor )                        # Comparing Correlations
library( openxlsx )                     # For outputting results
library( ggplot2 )                      # For plots
library( dbarts )                       # For BART models
library( fastDummies )                  # Library for Creating Dummies
library( gridExtra )                    # Combine plots
library(SuperLearner)                   # For ensemble models

library(purrr)
library(broom.mixed)                    # For presenting regression results
library(kableExtra)                     # For presenting regression results
library(pander)                         # For generating regression results Rmd

library(glue)                           # For dynamic and readable string interpolation 