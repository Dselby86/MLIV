###################################################################################
#                                                                                                        
#                                                                         
# Created on:   07/02/2024
# Purpose:      Template for calling the simulation with CA data
# Authors:      Luke Miratrix, Polina Polskaia, Nick Commins
# 
#
###################################################################################


###################################################################################
# STEP 0: SET UP 
###################################################################################


# Load script 08
source(here::here("simulation_pipeline/08_run_simulation.R"))

###################################################################################
# STEP 1: CLEAN CA DATA
###################################################################################


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
