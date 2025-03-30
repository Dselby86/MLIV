# DGP stats by queen

# CA

source(here::here("simulation_pipeline/08_run_simulation.R"))

# Load baseline data for CA
ca_baseline_object <- readRDS("/data/share/cdi/MLIV/Local repos/bigobs/MLIV/baseline/ca_baseline_object.rds")
baselinedata <- as.data.frame(ca_baseline_object[["Data"]])

# Load true tau for CA
ca_BMEFTMEDIAN <- readRDS("/data/share/cdi/MLIV/Local repos/bigobs/MLIV/true_iates/ca_BMEFTMEDIAN.rds")
ca_Y18JBERNA_06 <- readRDS("/data/share/cdi/MLIV/Local repos/bigobs/MLIV/true_iates/ca_Y18JBERNA_06.rds")

queen_list <- as.list(colnames(ca_Y18JBERNA_06))

treatment <- "TREATMNT"
set.seed(68814) #set.seed(baseline_seed)
n_baseline <- nrow(baselinedata)
p_tx <- ca_baseline_object$p_tx
actual_Tx <- as.numeric(sample(n_baseline) <= p_tx * n_baseline) # p_tx * n_baseline) 
baselinedata[, treatment] = actual_Tx



# Convert column names to a list
queen_list <- as.list(colnames(ca_Y18JBERNA_06))

# Initialize an empty list to store results
results_list <- list()

# Iterate over queen_list
for (queen in queen_list) {
  
  # Select corresponding treatment effect from ca_Y18JBERNA_06
  tr_tau <- ca_Y18JBERNA_06[[queen]]
  
  # Add treatment effects
  baseline_full_ca_Y18JBERNA_06 <- add_treatment_effects(
    baseline = baselinedata,
    IATE = tr_tau,
    outcome = "Y18JBERNA_06",
    treatment = "TREATMNT",
    binary_outcome = FALSE
  )
  
  test_set <- baseline_full_ca_Y18JBERNA_06[1:10000, ]
  baseline_remainder <- baseline_full_ca_Y18JBERNA_06[-(1:10000), ]

  # Function to compute SD and ATE for a dataset
  compute_metrics <- function(data, name) {
    if (nrow(data) > 0) {
      sd_object <- sd(data$Y18JBERNA_06, na.rm = TRUE)
      treated_mean <- mean(data$Y18JBERNA_06[data$TREATMNT == 1], na.rm = TRUE)
      control_mean <- mean(data$Y18JBERNA_06[data$TREATMNT == 0], na.rm = TRUE)
      ATE <- treated_mean - control_mean
    } else {
      sd_object <- NA
      ATE <- NA
    }
    
    return(list("Name" = name, "SD" = sd_object, "ATE" = ATE))
  }
  
  # Compute SD & ATE for all datasets
  full_metrics <- compute_metrics(baseline_full_ca_Y18JBERNA_06, "Full Dataset")
  test_metrics <- compute_metrics(test_set, "Test Set")
  remainder_metrics <- compute_metrics(baseline_remainder, "Baseline Remainder")
  
  # Store results in a list
  results_list[[queen]] <- list(
    "queen" = queen,
    "Full_Dataset" = full_metrics,
    "Test_Set" = test_metrics,
    "Baseline_Remainder" = remainder_metrics
  )
  
  # Print results with queen name
  print(paste("Queen:", queen))
  print(paste("Full Dataset - SD:", full_metrics$SD, "- ATE:", full_metrics$ATE))
  print(paste("Test Set - SD:", test_metrics$SD, "- ATE:", test_metrics$ATE))
  print(paste("Baseline Remainder - SD:", remainder_metrics$SD, "- ATE:", remainder_metrics$ATE))
  print("------------------------------------------------------")
}

# Display the final results list
print(results_list)



# ASAP

# Load baseline data 
asap_baseline_object <- readRDS("/data/share/cdi/MLIV/Local repos/bigobs/MLIV/baseline/asap_baseline_object.rds")
baselinedata <- as.data.frame(asap_baseline_object[["Data"]])

# Load true tau 
asap_C16BMVDEG <- readRDS("/data/share/cdi/MLIV/Local repos/bigobs/MLIV/true_iates/asap_C16BMVDEG.rds")
asap_X16BTMCRET <- readRDS("/data/share/cdi/MLIV/Local repos/bigobs/MLIV/true_iates/asap_X16BTMCRET.rds")

queen_list <- as.list(colnames(ca_Y18JBERNA_06))

treatment <- "STRA_CODE"
set.seed(68814) #set.seed(baseline_seed)
n_baseline <- nrow(baselinedata)
p_tx <- asap_baseline_object$p_tx
actual_Tx <- as.numeric(sample(n_baseline) <= p_tx * n_baseline) # p_tx * n_baseline) 
baselinedata[, treatment] = actual_Tx


# Initialize an empty list to store results
results_list <- list()

# Iterate over queen_list
for (queen in queen_list) {
  
  # Select corresponding treatment effect from asap_C16BMVDEG
  tr_tau <- asap_X16BTMCRET[[queen]]
  
  # Add treatment effects
  baseline_full_asap_X16BTMCRET<- add_treatment_effects(
    baseline = baselinedata,
    IATE = tr_tau,
    outcome = "X16BTMCRET",
    treatment = "STRA_CODE",
    binary_outcome = FALSE
  )
  
  test_set <- baseline_full_asap_X16BTMCRET[1:10000, ]
  baseline_remainder <- baseline_full_asap_X16BTMCRET[-(1:10000), ]
  
  # Function to compute SD and ATE for a dataset
  compute_metrics <- function(data, name) {
    if (nrow(data) > 0) {
      sd_object <- sd(data$X16BTMCRET, na.rm = TRUE)
      treated_mean <- mean(data$X16BTMCRET[data$STRA_CODE == 1], na.rm = TRUE)
      control_mean <- mean(data$X16BTMCRET[data$STRA_CODE == 0], na.rm = TRUE)
      ATE <- treated_mean - control_mean
    } else {
      sd_object <- NA
      ATE <- NA
    }
    
    return(list("Name" = name, "SD" = sd_object, "ATE" = ATE))
  }
  
  # Compute SD & ATE for all datasets
  full_metrics <- compute_metrics(baseline_full_asap_X16BTMCRET, "Full Dataset")
  test_metrics <- compute_metrics(test_set, "Test Set")
  remainder_metrics <- compute_metrics(baseline_remainder, "Baseline Remainder")
  
  # Store results in a list
  results_list[[queen]] <- list(
    "queen" = queen,
    "Full_Dataset" = full_metrics,
    "Test_Set" = test_metrics,
    "Baseline_Remainder" = remainder_metrics
  )
  
  # Print results with queen name
  print(paste("Queen:", queen))
  print(paste("Full Dataset - SD:", full_metrics$SD, "- ATE:", full_metrics$ATE))
  print(paste("Test Set - SD:", test_metrics$SD, "- ATE:", test_metrics$ATE))
  print(paste("Baseline Remainder - SD:", remainder_metrics$SD, "- ATE:", remainder_metrics$ATE))
  print("------------------------------------------------------")
}

# Display the final results list
print(results_list)

