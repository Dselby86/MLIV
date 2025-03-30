library(dplyr)
result <- master_aggregated_CATE %>%
  group_by(dataset, outcome, train_set_size, cov_set_size) %>%
  summarise(model_count = n(), .groups = "drop")

# View the result
print(result)

# Assuming master_aggregated_CATE is your data frame
result <- master_aggregated_CATE %>%
  group_by(dataset, outcome, train_set_size, cov_set_size) %>%
  summarise(unique_model_count = n_distinct(model), .groups = "drop")

# View the result
print(result)

filtered_data <- master_aggregated_CATE %>%
  filter(dataset == "ca", 
         outcome == "Y18JBERNA_06", 
         train_set_size == 1000, 
         cov_set_size == "small",
         model == "OLS S",
         queen == "OLS S")

# View the filtered data
print(filtered_data)


duplicates <- master_aggregated_CATE %>%
  select(dataset, outcome, train_set_size, cov_set_size, model, queen) %>% # Keep only relevant columns
  group_by(dataset, outcome, train_set_size, cov_set_size, model, queen) %>%
  filter(n() > 1) %>% # Filter groups with more than one row (duplicates)
  ungroup()


# Remove duplicates by keeping the first row in each group
deduplicated_data <- master_aggregated_CATE %>%
  select(dataset, outcome, train_set_size, cov_set_size, model, queen) %>% # Keep only relevant columns
  distinct(dataset, outcome, train_set_size, cov_set_size, model, queen, .keep_all = TRUE)

# View the deduplicated data
print(deduplicated_data)