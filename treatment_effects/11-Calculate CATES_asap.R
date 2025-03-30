

#############################################
#
# Step 0: Setup Code
#
############################################
library(here)
library(tidyverse)
library(rlang)
library(ggExtra)
library(kableExtra)
library(knitr)
library(rsample)

# Styler
library(styler)
library(lintr)

# Load previous programs
source(here::here("R/packages.R"))
source(here::here("algorithms/cf_alg.R"))
source(here::here("treatment_effects/10-call_sim_w_asap.R")) # Most Recent Simulation
#list.files(here::here("treatment_effects"))

########################################
#
# Step 1: File paths
#@NOTE: change for File
#######################################
dat_raw <- asap_subset_imputed
treatment <- asap_treatment
y_binary <- "C16BMVDEG"
y_cont <- "X16BTMCRET"
cv <- asap_large_set
factor_vars <- c("BLRAET1", "BLLIV", "BLHSGR", "BLLANG", "BLDGPL2", "BLCNUM")

cv <-cv[!(cv %in% factor_vars)]


########################################
#
#Step 2: Prepare Data set
#
#########################################

dat <- dat_raw %>%
  select( all_of (c(y_binary, y_cont, treatment, cv))) %>%
  filter(!is.na(!!sym(y_binary )),
         !is.na(!!sym(y_cont ))
  )

# Set row names for id's
dat$id <- 1:nrow(dat)

# Convert To Dummy Variables
dat


######


df <- dat


# Function to perform a split and Calculate CATES from each output
predict_cate_cf <- function(df) {
  # Split dataset into train/test split
  split <- initial_split(df, prop = 0.5)
  
  df_1 <- training(split)
  df_2 <- testing(split)
  
  # Create objects for Causal forest off Dataframe 1.
  y_cont_1 <- df_1 %>%
    select(all_of(y_cont)) %>%
    as.matrix()
  
  y_binary_1 <- df_1 %>%
    select(all_of(y_binary)) %>%
    as.matrix()
  
  t_1 <- df_1 %>%
    select(all_of(treatment)) %>%
    as.matrix()
  
  cv_1 <- df_1 %>%
    select(all_of(cv)) %>%
    as.matrix()
  
  id_1 <- df_1 %>%
    select(id)
  
  # Create Vectors for Causal forest off Dataframe 1.
  y_cont_2 <- df_2 %>%
    select(all_of(y_cont)) %>%
    as.matrix()
  
  y_binary_2 <- df_2 %>%
    select(all_of(y_binary)) %>%
    as.matrix()
  
  t_2 <- df_2 %>%
    select(all_of(treatment)) %>%
    as.matrix()
  
  cv_2 <- df_2 %>%
    select(all_of(cv)) %>%
    as.matrix()
  
  id_2 <- df_2 %>%
    select(id)
  
  # Set Causal Forest models for Continous and Binary outcomes
  fit_1_cont <- causal_forest(cv_1, y_cont_1, t_1, Y.hat = NULL, W.hat = NULL)
  fit_1_binary <- causal_forest(cv_1, y_binary_1, t_1, Y.hat = NULL, W.hat = NULL)
  
  fit_2_cont <- causal_forest(cv_2, y_cont_2, t_2, Y.hat = NULL, W.hat = NULL)
  fit_2_binary <- causal_forest(cv_2, y_binary_2, t_2, Y.hat = NULL, W.hat = NULL)
  
  # Predict Yhat for cf_1 using cf_2 model
  cate_cont_1 <- predict(fit_2_cont, cv_1)$predictions
  cate_binary_1 <- predict(fit_2_binary, cv_1)$predictions
  
  # Predict Yhat for cf_2 using cf_1 model
  cate_cont_2 <- predict(fit_2_cont, cv_2)$predictions
  cate_binary_2 <- predict(fit_2_binary, cv_2)$predictions
  
  # Build yhat dataset
  cate_1 <- cbind(id_1, cate_cont_1, cate_binary_1)
  cate_2 <- cbind(id_2, cate_cont_2, cate_binary_2)
  
  colnames(cate_1) <- c("id", "cate_cont", "cate_binary")
  colnames(cate_2) <- c("id", "cate_cont", "cate_binary")
  
  cate <- rbind(cate_1, cate_2)
  
  return(cate)
}

#####################################
#
# STEP 2: FOR Loop to Calculate CATES
# Use predict_cate_cf Function
#####################################

## NOTE: Data is organized as long dataset, and should be grouped on ID.

cate_i <- predict_cate_cf(dat)
cate_i$run <- 1

for (i in 2:10) {
  temp_cate <- predict_cate_cf(dat)
  temp_cate$run <- i
  cate_i <- rbind(cate_i, temp_cate)
}


cate_agg <- cate_i %>%
  group_by(id) %>%
  summarise(cate_cont = median(cate_cont),
            cate_binary = median(cate_binary)
  )


##########################
#
# STEP 3: Rebuild Dataset
#
##########################

dat_asap <- merge(dat, cate_agg, by = "id")

