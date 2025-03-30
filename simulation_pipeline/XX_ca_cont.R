

# This script times the models.
#
# NOTE:
# When running for real, the training and test set size should be
# increased to the normal levels
#


library( tidyverse )
library(furrr)
library(purrr)
library(tibble)


NUM_ITERATIONS = 1

source( here::here( "simulation_pipeline/06_simulation_driver.R") )
source( here::here( "simulation_pipeline/07_simulation_launcher.R") )

options(error = NULL)

# Get simulation scenario ready ----



# Load CA data
ca_subset_real <- readRDS("/data/share/cdi/MLIV/Data/Real/ca_subset.rds")

# Create covariate sets
ca_small_set <- c(  
  "ETHNIC",          # Ethnicity; Value Labels: 1 - Hispanic, 2 - Black, 3 - White, 4 - Asian/Native American
  "READCAT",         # 8TH GR. READ TESTS SCORE (%ILE CAT)
  "AGE",             # AGE
  "ATRATCP1",        # ATTENDANCE RATE - YEAR OF RA
  "MATHCAT",         # 8TH GR. MATH TESTS SCORE (%ILE CAT)
  "FEMALE"           # FEMALE
  
)

ca_medium_set <- c( 
  "ETHNIC",          # Ethnicity; Value Labels: 1 - Hispanic, 2 - Black, 3 - White, 4 - Asian/Native American
  "READCAT",         # 8TH GR. READ TESTS SCORE (%ILE CAT)
  "AGE",             # AGE
  "ATRATCP1",        # ATTENDANCE RATE - YEAR OF RA
  "MATHCAT",         # 8TH GR. MATH TESTS SCORE (%ILE CAT)
  "FEMALE",          # FEMALE
  "SIBDROP",         # HAS SIBLING HS DROP OUT
  "CREDYRP1",        # CREDITS EARNED (COMP): RA YEAR
  "GPAYRP1",         # GPA (CONT): RA YEAR
  "OVERAGE",         # STUDENT IS OVERAGE FOR GRADE LEVEL
  "PARENT",          # WHO STUDENT LIVES WITH K3
  "SCHLTRAN",        # TRANSFERRED SCHOOLS 2+ TIMES
  "LEP",             # STUDENT LIMITED ENGLISH PROFICIENT
  "WELFFS"           # FAMILY HAS RECEIVED WELFARE OR FS
)

ca_large_set <- c(
  # SMALL SET
  "ETHNIC",          # Ethnicity; Value Labels: 1 - Hispanic, 2 - Black, 3 - White, 4 - Asian/Native American
  "READCAT",         # 8TH GR. READ TESTS SCORE (%ILE CAT)
  "AGE",             # AGE
  "ATRATCP1",        # ATTENDANCE RATE - YEAR OF RA
  "MATHCAT",         # 8TH GR. MATH TESTS SCORE (%ILE CAT)
  "FEMALE",          # FEMALE
  
  # MEDIUM SET
  "SIBDROP",         # HAS SIBLING HS DROP OUT
  "CREDYRP1",        # CREDITS EARNED (COMP): RA YEAR
  "GPAYRP1",         # GPA (CONT): RA YEAR
  "OVERAGE",         # STUDENT IS OVERAGE FOR GRADE LEVEL
  "PARENT",          # WHO STUDENT LIVES WITH K3
  "SCHLTRAN",        # TRANSFERRED SCHOOLS 2+ TIMES
  "LEP",             # STUDENT LIMITED ENGLISH PROFICIENT
  "WELFFS",          # FAMILY HAS RECEIVED WELFARE OR FS
  
  # LARGE SET
  "edfath",          # Fathers education level K5; Values Labels: 1 - FATHER DID NOT FINISH HIGH SCHOOL, 2 - FATHER RECEIVED A GED, 3 - FATHER IS A HIGH SCHOOL GRADUATE, 4 - FATHER TOOK SOME POST-HS COURSES, 5 - FATHER IS A COLLEGE GRADUATE
  "edmoth",          # Mothers education level K5; Values Labels: 1 - MOTHER DID NOT FINISH HIGH SCHOOL, 2 - MOTHER RECEIVED A GED, 3 - MOTHER IS A HIGH SCHOOL GRADUATE, 4 - MOTHER TOOK SOME POST-HS COURSES, 5 - MOTHER IS A COLLEGE GRADUATE
  "HRSTV",           # HOURS/DAY WATCHING TV (CAT): Values Labels: 1 - HOURS/DAY WATCHING TV - < 1, 2 - HOURS/DAY WATCHING TV - 1 TO 2, 3 - HOURS/DAY WATCHING TV - 2 TO 3, 4 - HOURS/DAY WATCHING TV - MORE THN 3
  "UNSUPGT3",        # STUDENT IS UNSUPERVISED > 3HRS/DAY
  "UNSAFSCH",        # STUDENT FEELS UNSAFE AT SCHOOL
  "HRSHW",           # HOURS/WEEK SPENT ON HOMEWORK (CAT)
  "PARWORK",         # PARENTS WORK FOR PAY K5
  "PARNHS",          # NEITHER PARENT HAS HS DIPLOMA
  "OFFICE",          # SENT TO OFFICE - MISBEHAV (CAT)
  "PSEDEXP",         # PSED EXPECTATIONS AT BASELINE
  "ONEPARNT",        # RISK FACTOR: SINGLE PAR HH
  "MOVED",           # # TIMES FAM MOVED PAST 2YRS (CAT) K2; Value Labels: 1 - FAMILY HAS NOT MOVED IN PAST 2 YRS, 2 - 1 OR 2 FAMILY MOVES IN PAST 2 YRS, 3 - GE 3 FAMILY MOVES IN PAST 2 YRS
  "VEMPP"            # STUDENT EVER WORKED FOR PAY Z3361
)

# Specify treatment variable
ca_treatment <- "TREATMNT"

# Specify outcomes:
ca_outcomes <- c(
  "Y18JBERNA_06",     # YRS 1-8: AVG MONTHLY EARNINGS IN 2006 DOLLARS (LQ & EQ DATA)
  "BMEFTMEDIAN",      # EQ: EMPLOYED FULL-TIME (MOS 49-96) MORE THAN MEDIAN THRESHOLD OF EQJBMEFT
  "EQJBMEMP",         # EQ: MONTHS EMPLOYED (MOS 49-96)
  "EQJBMEFT",         # EQ: MONTHS EMPLOYED FULL-TIME (MOS 49-96)
  "EQJBERNA_06"       # EQ: AVG MONTHLY EARNINGS IN 2006 DOLLARS, MONTHS 49-96
)

###################################################################################
# STEP 2: RECODE OUTCOMES 
###################################################################################


# Create a more balanced binary outcome.

# For CA, we can use the median value of # of months employed full time as 
# the threshold to code a binary outcome that = 1
# if at or above the threshold and = 0 if below the threshold.

# Calculate the median value of the EQJBMEFT variable
median_eqjbmeft <- median(ca_subset_real$EQJBMEFT, na.rm = TRUE)

# Create a new binary variable based on the median
ca_subset_real$BMEFTMEDIAN <- ifelse(ca_subset_real$EQJBMEFT >= median_eqjbmeft, 1, 0)

###################################################################################
# STEP 3: SUBSET TO ONLY VARIABLES WE ARE INTERESTED IN
###################################################################################


# Subset to covariates, treatment, and outcomes 
ca_subset_real <- ca_subset_real[ , c(ca_outcomes, ca_large_set, ca_treatment)]

###################################################################################
# STEP 4: IMPUTE COVARIATES
###################################################################################

# IMPUTATION STRATEGY
# For continuous variables:  impute with mean
# For categorical variables: add a level for missing
# For dummy variables: 	     impute the mean

# Impute continuous
ca_subset_imputed <- ca_subset_real %>%
  mutate(ATRATCP1 = replace_na(ATRATCP1, floor(mean(ATRATCP1, na.rm = TRUE))),
         AGE = replace_na(AGE, floor(mean(AGE, na.rm = TRUE))), 
         CREDYRP1 = replace_na(CREDYRP1, floor(mean(CREDYRP1, na.rm = TRUE))),
         GPAYRP1 = replace_na(GPAYRP1, floor(mean(GPAYRP1, na.rm = TRUE)))
  )

# Impute categorical vars
ca_subset_imputed <- ca_subset_imputed %>%
  mutate(ETHNIC = replace_na(ETHNIC, 99),
         PARENT = replace_na(PARENT, 99),
         edfath = replace_na(edfath, 99),
         edmoth = replace_na(edmoth, 99),
         PARWORK= replace_na(PARWORK, 99),
         MOVED = replace_na(MOVED, 99),
         MATHCAT = replace_na(MATHCAT, 99),
         READCAT = replace_na(READCAT, 99),
         OFFICE = replace_na(OFFICE, 99),
         PSEDEXP = replace_na(PSEDEXP , 99),
         HRSHW = replace_na(HRSHW, 99),
         HRSTV = replace_na(HRSTV, 99)
  )

# Impute dummies
ca_subset_imputed <- ca_subset_imputed %>%
  mutate(LEP = ifelse(is.na(LEP), mean(LEP, na.rm = TRUE), LEP),
         ONEPARNT = ifelse(is.na(ONEPARNT), mean(ONEPARNT, na.rm = TRUE), ONEPARNT),
         FEMALE = ifelse(is.na(FEMALE), mean(FEMALE, na.rm = TRUE), FEMALE),
         PARNHS = ifelse(is.na(PARNHS), mean(PARNHS, na.rm = TRUE), PARNHS),
         WELFFS = ifelse(is.na(WELFFS), mean(WELFFS, na.rm = TRUE), WELFFS),
         UNSUPGT3 = ifelse(is.na(UNSUPGT3), mean(UNSUPGT3, na.rm = TRUE), UNSUPGT3),
         UNSAFSCH = ifelse(is.na(UNSAFSCH), mean(UNSAFSCH, na.rm = TRUE), UNSAFSCH),
         VEMPP = ifelse(is.na(VEMPP), mean(VEMPP, na.rm = TRUE), VEMPP),
         OVERAGE = ifelse(is.na(OVERAGE), mean(OVERAGE, na.rm = TRUE), OVERAGE),
         SCHLTRAN = ifelse(is.na(SCHLTRAN), mean(SCHLTRAN, na.rm = TRUE), SCHLTRAN),
         SIBDROP = ifelse(is.na(SIBDROP), mean(SIBDROP, na.rm = TRUE), SIBDROP)
  )

# N cols and rows should be the same
stopifnot(dim(ca_subset_real) == dim(ca_subset_imputed))

###################################################################################
# STEP 5: CONVERT CATEGORICAL VARIABLES TO FACTORS
###################################################################################


# Convert categorical vars to factors
ca_subset_imputed$ETHNIC <- as.factor(ca_subset_imputed$ETHNIC)
ca_subset_imputed$READCAT <- as.factor(ca_subset_imputed$READCAT)
ca_subset_imputed$MATHCAT <- as.factor(ca_subset_imputed$MATHCAT)
ca_subset_imputed$edfath <- as.factor(ca_subset_imputed$edfath)
ca_subset_imputed$PARENT <- as.factor(ca_subset_imputed$PARENT)
ca_subset_imputed$edmoth <- as.factor(ca_subset_imputed$edmoth)
ca_subset_imputed$PARWORK <- as.factor(ca_subset_imputed$PARWORK)
ca_subset_imputed$MOVED <- as.factor(ca_subset_imputed$MOVED)
ca_subset_imputed$OFFICE <- as.factor(ca_subset_imputed$OFFICE)
ca_subset_imputed$PSEDEXP <- as.factor(ca_subset_imputed$PSEDEXP)
ca_subset_imputed$HRSHW <- as.factor(ca_subset_imputed$HRSHW)
ca_subset_imputed$HRSTV <- as.factor(ca_subset_imputed$HRSTV)


# Load the data and IATEs
data <- setup_simulation_data(
                              real_data = ca_subset_imputed, 
                              dataset_name =  "ca",    
                              covariates = ca_small_set,
                              outcome = "Y18JBERNA_06",
                              treatment = "TREATMNT",
                              
                              # Optional arguments in case we need to generate the IATEs, etc.
                              large_covariate_set = ca_large_set, 
                              small_covariate_set = ca_small_set, 
                              all_outcomes = "Y18JBERNA_06",
                              queen_list = "LASSO R",
                              p_tx = NULL,
                              size_train = 2000,
                              size_test = 10000,
                              baseline_seed = 68814, 
                              baseline_N = 100000, 
                              baseline_directory_path = here::here("baseline"),
                              true_iates_directory_path = here::here("true_iates"),
                              verbose = 100 )


# Look at queen IATEs, to check
head( data$true_iates_by_queen )


# Make method to run simulation ----


run_one_simulation <- function(
                                chunk_id,
                                data,
                                queen,
                                covariates,
                                outcome,
                                treatment,
                                seed,
                                S = 3,
                                p_tx = NULL,
                                size_train = 2000,
                                size_test = 10000,
                                model_list,
                                dir_name = here::here( "results/timing" ),
                                verbose = 1000 ) {
                              
  
  current_seeds = seed + 13 * 1:S
  
  
  if ( verbose > 0 ) {
    cli::cli_alert_info( "Starting '{model_list}' with seed {seed}" )
  }
  
  
  IATE <- data$true_iates_by_queen[[queen]]
  if (is.null(IATE)) {
    message(glue::glue("No pre-generated IATEs for queen {queen} -- use generator to make and save to file"))
    return(NULL)
  } else {
    baseline_full <- add_treatment_effects(baseline = data$baselinedata,
                                           IATE = IATE,
                                           outcome = outcome,
                                           treatment = treatment, 
                                           binary_outcome = data$binary_outcome )
    
    # Create test set and separate it from the rest of the baseline data
    test_set <- baseline_full[1:size_test, ]
    baseline_remainder <- baseline_full[-(1:size_test), ]
  }
  
  # Run the simulation (using function in script 06)
  sim_results <- perform_simulation(start_iteration = 1, 
                                    end_iteration = S,
                                    test_set = test_set, 
                                    baseline_remainder = baseline_remainder,
                                    size_train = size_train, 
                                    seeds = current_seeds,
                                    p_tx = NULL, 
                                    outcome = outcome,
                                    treatment = treatment,
                                    covariates = covariates,
                                    verbose = verbose - 1,
                                    model_list = model_list,
                                    sd_y0_real = data$sd_y0_real, 
                                    ate_real = data$ate_real
  )
  
  file_name <- file.path(dir_name, paste0("fragment_", chunk_id, "_v_", queen, ".rds"))
  saveRDS(sim_results, file_name)
  
  if ( verbose > 0 ) {
    cli::cli_alert_success( "Saved results {file_name} for {chunk_id}" )
  }
  
  invisible( sim_results )
}


# Testing code ----
if ( FALSE ) {
  rs <- run_one_simulation( chunk_id = "testing",
                            data = data,
                            queen = "LASSO R",
                            covariates = ca_small_set,
                            outcome = "Y18JBERNA_06",
                            treatment = "TREATMNT",
                            seed = 4343443,
                            S = 2,  
                            p_tx = NULL,
                            size_train = 2000,
                            size_test = 10000,
                            model_list = c("ATE", "LASSO R"),
                            verbose = 100,
                            dir_name = here::here( "results" )
  )
  
  rs  
  dim( rs$runtime )
}





# Run simulation in parallel ----



# Define the list of "include" flags
model_queen_lists()
models = model_queen_lists( 
                            include_SL_T = FALSE,
                            include_SL_S = FALSE,
                            include_XGBOOST = FALSE,
                            make_queen_list = FALSE )
# models = c( models, "LASSO INF", "LASSO T", "LASSO MCM EA" )
# models <- c(
#   "OLS S", "ATE", "RF INF", "RF T", "RF MOM IPW")

models <- "SL T"
args_df = expand_grid( model_list = models,
                       chunk_id = 1:NUM_ITERATIONS ) %>%
  mutate( chunk_id = paste0( model_list, "-", chunk_id ) )
args_df
args_df = slice_sample( args_df, n=nrow(args_df) )

dir_name = generate_directory(folder_name = "timing", simulation_name = NULL )
cli::cli_alert_info("Saving results to {dir_name}." )



if ( FALSE ) {
  # testing, run in sequence for debugging.
  
  args_df = args_df[ -c(1:8), ]
  results <- pmap(args_df, run_one_simulation,
                  data = data,
                  queen = "ATE",
                  covariates =ca_small_set,
                  outcome = "Y18JBERNA_06",
                  treatment = "TREATMNT",
                  seed = 4343443,
                  S = 3,  
                  p_tx = NULL,
                  size_train = 750,
                  size_test = 100,
                  dir_name = dir_name,
                  verbose = 2 )
  
  # run_one_simulation( data = data,
  #                     queen = "ATE",
  #                     covariates = paste0( "X", 1:5 ),
  #                     outcome = "Y18JBERNA_06",
  #                     treatment = "TREATMNT",
  #                     model_list = "CF_LC",
  #                     chunk_id = "CF_LC",
  #                     seed = 4343443,
  #                     S = 3,  
  #                     p_tx = NULL,
  #                     size_train = 750,
  #                     size_test = 100,
  #                     dir_name = dir_name,
  #                     verbose = 2 )
  # 
}


cli::cli_alert_info("Working on {nrow(args_df)} tasks" )

# Use future_pmap to call the function for each row in the data frame.
# Each row corresponds to a single estimator (or small group of
# estimators in some cases).
num_cores = parallel::detectCores() - 8
num_cores
plan(multisession, workers = num_cores - 1) 

results <- future_pmap(args_df, run_one_simulation,
                       data = data,
                       queen = "LASSO R",
                       covariates = ca_small_set,
                       outcome = "Y18JBERNA_06",
                       treatment = "TREATMNT",
                       seed = 4343443,
                       S = 1,  
                       p_tx = NULL,
                       size_train = 1000,
                       size_test = 10000,
                       dir_name = dir_name,
                       verbose = 1,
                       .options = furrr_options(seed = 334334) )


dimnames( results[[1]]$runtime )



# Process simulation results for runtime ----

cli::cli_alert_info("Processing simulation results" )

# Function to load and stack arrays from RDS files using tidyverse
stack_rds_arrays <- function(dir_name) {
  # Get a list of all RDS files in the directory
  rds_files <- list.files( path = dir_name, 
                           pattern = "\\.rds$", full.names = TRUE)
  
  # Load all the RDS files into a list of lists
  all_data <- map(rds_files, readRDS)
  
  all_data = transpose( all_data )
  
  # Stack arrays on the third dimension for each of the three elements
  result_list <- map(all_data, abind, along = 3 )
  
  return(result_list)
}


res <- stack_rds_arrays( dir_name )

timings = res[[2]]
names = dimnames(timings)[[3]]
timings = timings[ , 1,  ]

# og_dims <- dim( timings )
# dim(timings) = c( og_dims[[1]], og_dims[[3]] )

stopifnot( length(timings) == length(names) )

timings = tibble( model = names,
                  time = timings )

# THE FOLLOWING CODE is when each fragment has multiple iterations, making them 3D matrices
#dim( timings )
#dimnames( timings )
#head( timings )

#as_tibble(timings) %>%
#  pivot_longer( cols = everything(), names_to = "model", values_to = "time" ) %>%


# Table of how long everything took to run ----
timings %>%
  group_by( model ) %>%
  summarise( avg = mean(time),
             sd = sd( time ),
             n = n() ) %>%
  arrange( -avg ) %>%
  knitr::kable( digits = 2 )



filter( timings, model == "SL S" )
