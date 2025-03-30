

# Load script 08
source(here::here("simulation_pipeline/08_run_simulation.R"))

###################################################################################
# STEP 1: CLEAN ASAP DATA
###################################################################################


# Load CA data
asap_subset_real <- readRDS("/data/share/cdi/MLIV/Data/Real/asap_subset.rds")

# Create covariate sets
asap_small_set <- c(
  # Small set 
  "BLRAET1" ,        # Race/ethnicity
  "BLAGENUM",        # Age
  "BLFEMALE",         # Gender (Female dummy)
  "BLWORK",          # Currently employed
  "BLCHLD",          # Has children
  "BLP50"           # Parents pay more than 50% of educational expenses
)

asap_medium_set <- c(
  # Small set 
  "BLRAET1" ,        # Race/ethnicity
  "BLAGENUM",        # Age
  "BLFEMALE",        # Gender (Female dummy)
  "BLWORK",          # Currently employed
  "BLCHLD",          # Has children
  "BLP50",           # Parents pay more than 50% of educational expenses
  
  # Medium set
  "BLC1ST",          # First in immediate family to attend college
  "BLLIV",           # Marital status and living situation
  "BLDEGS_HSDP"      # Diplomas and degrees earned
)

asap_large_set <- c(
  # Small set 
  "BLRAET1" ,        # Race/ethnicity
  "BLAGENUM",        # Age
  "BLFEMALE",        # Gender (Female dummy)
  
  # Medium set
  "BLWORK",          # Currently employed
  "BLCHLD",          # Has children
  "BLP50",           # Parents pay more than 50% of educational expenses
  "BLC1ST",          # First in immediate family to attend college
  "BLLIV",           # Marital status and living situation
  "BLDEGS_HSDP",     # Diplomas and degrees earned
  
  # Large set
  "BLDPDT",          # Date earned high school diploma
  "BLHSGR"  ,        # Highest grade completed
  "BLLANG",          # Speak a language other than English at home
  "BLDGPL2",         # Highest Degree Planned
  "BLCNUM",          # Number of children
  "BLCAGENUM",       # Age of youngest child - converted  from categorical to numeric 
  "BLDEGS_GED",      # Diplomas and degrees earned
  "BLDEGS_TECH",     # Diplomas and degrees earned
  "BLDEGS_NONE"      # Diplomas and degrees earned - not running currently, still leaving at the end for now 
  
)

# Specify treatment variable
asap_treatment <- "STRA_CODE"

# Specify outcomes:
asap_outcomes <- c(
  "C16BMVDEG",       # Earned a degree from any college (%) (CUNY + OHIO)
  "X16BTMCRET",      # Total credits earned (CUNY) ("X"-prefix because excluding repeated courses)
  "SessionsEnrolled",# Sessions enrolled (out of 12) (CUNY)  #NC: this is currently out of 17, not 12
  "AnyEnrollment" 
)

###################################################################################
# STEP 2: SUBSET TO ONLY VARIABLES WE ARE INTERESTED IN
###################################################################################


# Subset to covariates,treatment, and outcomes 
asap_subset_real <- asap_subset_real[ , c(asap_outcomes, asap_large_set, asap_treatment)]

###################################################################################
# STEP 3: IMPUTE COVARIATES
###################################################################################


# IMPUTATION STRATEGY
# For continuous variables:  impute with mean
# For categorical variables: add a level for missing
# For dummy variables: 	     impute the mean

asap_continuous <- c("BLAGENUM", "BLCAGENUM", "BLCNUM", "BLDPDT") 

asap_categorical <- c("BLRAET1", "BLLIV", "BLDGPL2", "BLLANG") 

asap_dummy <- c("BLFEMALE", "BLCHLD", "BLWORK", "BLP50", "BLC1ST",
                "BLDEGS_HSDP", "BLDEGS_GED", "BLDEGS_TECH", "BLDEGS_NONE",
                "BLHSGR") 

# Imputing continuous variables with mean
asap_subset_imputed <- asap_subset_real
asap_subset_imputed[ , asap_continuous] <- lapply(asap_subset_real[,asap_continuous], function(x)
  ifelse(is.na(x), floor(mean(x, na.rm = TRUE)), x)
)

# Impute categorical vars with 99
asap_subset_imputed[ , asap_categorical] <- lapply(asap_subset_imputed[,asap_categorical], function(x)
  ifelse(is.na(x), 99, x)
)

# Impute dummies with the mean
asap_subset_imputed[ , asap_dummy] <- lapply(asap_subset_imputed[,asap_dummy], function(x)
  ifelse(is.na(x), mean(x, na.rm = TRUE), x)
)

# Sample size check
# N cols and rows should be the same
stopifnot(dim(asap_subset_real) == dim(asap_subset_imputed))

###################################################################################
# STEP 4: CONVERT CATEGORICAL VARIABLES TO FACTORS
###################################################################################


# Convert categorical vars to factors
asap_subset_imputed$BLRAET1 <- as.factor(asap_subset_imputed$BLRAET1)
asap_subset_imputed$BLLIV <- as.factor(asap_subset_imputed$BLLIV)
asap_subset_imputed$BLHSGR <- as.factor(asap_subset_imputed$BLHSGR)
asap_subset_imputed$BLLANG <- as.factor(asap_subset_imputed$BLLANG)
asap_subset_imputed$BLCNUM <- as.factor(asap_subset_imputed$BLCNUM)
asap_subset_imputed$BLDGPL2 <- as.factor(asap_subset_imputed$BLDGPL2)

# Load data
real_asap_data <- asap_subset_imputed
asap_baseline <- asap_baseline_object
asap_baseline_data <- asap_baseline_object$Data

# Add treatment to baseline:
nval <- nrow(asap_baseline_data)
treatment <- asap_treatment

# Generate treatment with p_tx proportion treated
actual_Tx <- as.numeric(sample(nval) <= 0.5 * nval) 
asap_baseline_data[, treatment] <- actual_Tx
stopifnot(treatment %in% names(asap_baseline_data))

# Load IATES
load_asap_iates <- `asap_C16BMVDEG - Copy`

# Select one queen
CDML <- load_asap_iates[["OLS S"]]

# Add treatment effects
baseline_full <- add_treatment_effects(baseline = asap_baseline_data,
                                       IATE = CDML,
                                       outcome = "C16BMVDEG",
                                       treatment = "STRA_CODE",
                                       binary_outcome = TRUE)