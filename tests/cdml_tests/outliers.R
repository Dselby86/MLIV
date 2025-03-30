# Luke's questions:
# We see long tails, but how many outliers are actually there? Even if it is a couple hundred it is still only a small % of 10,000.
# Should we look at the plots without outliers?

models = c( "ATE", "OLS", "RF Inf","RF CMR","RF MOM IPW","RF MOM DR",
            "CF","CF LC","CDML","Lasso Inf",
            "Lasso CMR","Lasso MOM IPW","Lasso MOM DR", "Lasso MCM","Lasso MCM EA",
            "Lasso RL", "XBART" )


pL_ca_small_2_50_no <- ca_50_2_RF_LASSO %>%          # FINAL SIZE 6750000 rows 5 cols
  pivot_longer( cols = any_of( models ),
                names_to = "model",
                values_to = "value" ) %>%
  mutate( value = abs( value ),
          metric = factor( metric,
                           levels = c( "bias", "se", "rmse" ),
                           labels = c( "|bias|", "se", "rmse" )) )



pL_ca_small_2_50_no <- pL_ca_small_2_50_no %>%
  filter(!queen %in% c("RF Inf", "Lasso Inf"))


ggplot( pL_ca_small_2_50_no, aes( model, value ) ) +
  facet_wrap( ~ metric ) +
  geom_boxplot() +
  coord_flip() +
  labs( title = "Individual unit performance characteristics by simulation model,\nall queens",
        x = "",
        y = "" ) +
  theme_minimal()


ggplot( pL_ca_small_2_50_no, aes( model, value ) ) +
  facet_wrap( ~ metric ) +
  geom_boxplot() +
  coord_flip() +
  labs(     title = "Individual unit performance characteristics by simulation model,\nall queens, log scale",
            x = "",
            y = "" ) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 90)) +
  scale_y_log10()

# first approah


outliers <- pL_ca_small_2_50_no %>%
  group_by(metric, queen, model) %>%
  summarize(outliers = sum(value < quantile(value, 0.25) - 1.5 * IQR(value) | value > quantile(value, 0.75) + 1.5 * IQR(value))) %>%
  mutate(outlier_percentage = (outliers / 10000) * 100)

# Print the number of outliers
print(outliers, n = nrow(outliers))


# folder_name = paste0("/Sim_Run_2023-10-09", "/" )
# 
# output_path_name = find_path(path=output_path)[1]
# print(output_path_name)# TODO::MOVE

# Define the file path for saving the Excel file
file_path <- paste0(output_path_name, folder_name)
write.xlsx(outliers, file = paste0(file_path, "outliers_ind.xlsx"))




# Calculate the lower limit based on the 10th percentile of the "value" data
your_lower_limit <- quantile(pL_ca_small_2_50_no$value, 0.10, na.rm = TRUE)

# Calculate the upper limit based on the 90th percentile of the "value" data
your_upper_limit <- quantile(pL_ca_small_2_50_no$value, 0.90, na.rm = TRUE)

# Create boxplots without outliers and set the dynamic upper limit with color by "model"
ggplot(pL_ca_small_2_50_no, aes(model, value, fill = model)) +
  facet_wrap(~ metric) +
  geom_boxplot(outlier.shape = NA) +  # Suppress outlier points
  coord_flip() +
  labs(
    title = "Individual unit performance characteristics by simulation model,\nall queens, 10% of outliers \nremoved from both sides",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  ylim(your_lower_limit, your_upper_limit)+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))





# second approach


# Randomly subsample 20 percent of the data while maintaining proportions
sampled_data <- pL_ca_small_2_50_no %>%
  group_by(metric, queen, model) %>%
  sample_frac(0.2) %>%
  ungroup()

ggplot( sampled_data, aes( model, value ) ) +
  facet_wrap( ~ metric ) +
  geom_boxplot() +
  coord_flip() +
  labs( title = "Individual unit performance characteristics by simulation model,\nall queens",
        x = "",
        y = "" ) +
  theme_minimal()


# thord approach
# Define the percentage of the most extreme outliers to remove (1%)
percent_to_remove <- 0.05


# Remove the top 5% outliers within each group (metric, queen, model)
filtered_data <- pL_ca_small_2_50_no %>%
  group_by(metric, queen, model) %>%
  mutate(
    threshold = quantile(value, 1 - percent_to_remove)
  ) %>%
  filter(value <= threshold) %>%
  ungroup()

ggplot( filtered_data, aes( model, value ) ) +
  facet_wrap( ~ metric ) +
  geom_boxplot() +
  coord_flip() +
  labs( title = "Individual unit performance characteristics by simulation model,\nall queens",
        x = "",
        y = "" ) +
  theme_minimal()





# Group by queen type bias
filtered_data <- pL_ca_small_2_50_no %>%
  filter(metric == "|bias|")

bias_RF <- filtered_data %>%
  filter(queen %in% c("RF CMR", "RF MOM IPW", "RF MOM DR", "CF", "CF LC")) %>%
  group_by(metric, id, model) %>%
  summarize(value = mean(value, na.rm = TRUE))

bias_RF$queen = "Average RF"

bias_OLS <- filtered_data %>%
  filter(queen %in% c("OLS"))

bias_ATE <- filtered_data %>%
  filter(queen %in% c("ATE"))

bias_Lasso <- filtered_data %>%
  filter(queen %in% c(
                      "Lasso CMR","Lasso MOM IPW","Lasso MOM DR", "Lasso MCM","Lasso MCM EA",
                      "Lasso RL")) %>%
  group_by(metric, id, model) %>%
  summarize(value = mean(value, na.rm = TRUE))

bias_Lasso$queen = "Average Lasso"

bias_by_queen_group = rbind(bias_ATE, bias_OLS, bias_RF, bias_Lasso)

ggplot(bias_by_queen_group, aes(model, value, fill = model)) +
  facet_wrap( ~ queen ) +
  geom_boxplot() +
  coord_flip() +
  labs( title = "Individual unit Bias by simulation model, by queen type",
        x = "",
        y = "" ) +
  theme_minimal()+
  guides(fill = guide_legend(reverse = TRUE))


# Bias by type wo outliers

outliers <- bias_by_queen_group %>%
  group_by(queen, model) %>%
  summarize(outliers = sum(value < quantile(value, 0.25) - 1.5 * IQR(value) | value > quantile(value, 0.75) + 1.5 * IQR(value))) %>%
  mutate(outlier_percentage = (outliers / 10000) * 100)

# Print the number of outliers
print(outliers, n = nrow(outliers))


# Calculate the lower limit based on the 10th percentile of the "value" data
your_lower_limit <- quantile(bias_by_queen_group$value, 0.10, na.rm = TRUE)

# Calculate the upper limit based on the 90th percentile of the "value" data
your_upper_limit <- quantile(bias_by_queen_group$value, 0.90, na.rm = TRUE)

# Create boxplots without outliers and set the dynamic upper limit with color by "model"
ggplot(bias_by_queen_group, aes(model, value, fill = model)) +
  facet_wrap( ~ queen ) +
  geom_boxplot(outlier.shape = NA) +  # Suppress outlier points
  coord_flip() +
  labs(
    title = "Individual unit Bias by simulation model,\nby type of queen, Outliers gone",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  ylim(your_lower_limit, your_upper_limit)+
  guides(fill = guide_legend(reverse = TRUE))



# SE

filtered_data <- pL_ca_small_2_50_no %>%
  filter(metric == "se")

se_RF <- filtered_data %>%
  filter(queen %in% c("RF CMR", "RF MOM IPW", "RF MOM DR", "CF", "CF LC")) %>%
  group_by(metric, id, model) %>%
  summarize(value = mean(value, na.rm = TRUE))

se_RF$queen = "Average RF"

se_OLS <- filtered_data %>%
  filter(queen %in% c("OLS"))

se_ATE <- filtered_data %>%
  filter(queen %in% c("ATE"))

se_Lasso <- filtered_data %>%
  filter(queen %in% c(
                      "Lasso CMR","Lasso MOM IPW","Lasso MOM DR", "Lasso MCM","Lasso MCM EA",
                      "Lasso RL")) %>%
  group_by(metric, id, model) %>%
  summarize(value = mean(value, na.rm = TRUE))

se_Lasso$queen = "Average Lasso"

se_by_queen_group = rbind(se_ATE, se_OLS, se_RF, se_Lasso)

ggplot(se_by_queen_group, aes(model, value, fill = model)) +
  facet_wrap( ~ queen ) +
  geom_boxplot() +
  coord_flip() +
  labs( title = "Individual unit SE by simulation model, by queen type",
        x = "",
        y = "" ) +
  theme_minimal()+
  guides(fill = guide_legend(reverse = TRUE))





# se by type wo outliers

outliers <- se_by_queen_group %>%
  group_by(queen, model) %>%
  summarize(outliers = sum(value < quantile(value, 0.25) - 1.5 * IQR(value) | value > quantile(value, 0.75) + 1.5 * IQR(value))) %>%
  mutate(outlier_percentage = (outliers / 10000) * 100)

# Print the number of outliers
print(outliers, n = nrow(outliers))


# Calculate the lower limit based on the 10th percentile of the "value" data
your_lower_limit <- quantile(se_by_queen_group$value, 0.10, na.rm = TRUE)

# Calculate the upper limit based on the 90th percentile of the "value" data
your_upper_limit <- quantile(se_by_queen_group$value, 0.90, na.rm = TRUE)

# Create boxplots without outliers and set the dynamic upper limit with color by "model"
ggplot(se_by_queen_group, aes(model, value, fill = model)) +
  facet_wrap( ~ queen ) +
  geom_boxplot(outlier.shape = NA) +  # Suppress outlier points
  coord_flip() +
  labs(
    title = "Individual unit SE by simulation model,\nby type of queen, Outliers gone",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  ylim(your_lower_limit, your_upper_limit)+
  guides(fill = guide_legend(reverse = TRUE))




# RMSE

filtered_data <- pL_ca_small_2_50_no %>%
  filter(metric == "rmse")

rmse_RF <- filtered_data %>%
  filter(queen %in% c("RF CMR", "RF MOM IPW", "RF MOM DR", "CF", "CF LC")) %>%
  group_by(metric, id, model) %>%
  summarize(value = mean(value, na.rm = TRUE))

rmse_RF$queen = "Average RF"

rmse_OLS <- filtered_data %>%
  filter(queen %in% c("OLS"))

rmse_ATE <- filtered_data %>%
  filter(queen %in% c("ATE"))

rmse_Lasso <- filtered_data %>%
  filter(queen %in% c(
                      "Lasso CMR","Lasso MOM IPW","Lasso MOM DR", "Lasso MCM","Lasso MCM EA",
                      "Lasso RL")) %>%
  group_by(metric, id, model) %>%
  summarize(value = mean(value, na.rm = TRUE))

rmse_Lasso$queen = "Average Lasso"

rmse_by_queen_group = rbind(rmse_ATE, rmse_OLS, rmse_RF, rmse_Lasso)

ggplot(rmse_by_queen_group, aes(model, value, fill = model)) +
  facet_wrap( ~ queen ) +
  geom_boxplot() +
  coord_flip() +
  labs( title = "Individual unit RMSE by simulation model, \nby queen type",
        x = "",
        y = "" ) +
  theme_minimal()+
  guides(fill = guide_legend(reverse = TRUE))



# RMSE by type wo outliers

outliers <- rmse_by_queen_group %>%
  group_by(queen, model) %>%
  summarize(outliers = sum(value < quantile(value, 0.25) - 1.5 * IQR(value) | value > quantile(value, 0.75) + 1.5 * IQR(value))) %>%
  mutate(outlier_percentage = (outliers / 10000) * 100)

# Print the number of outliers
print(outliers, n = nrow(outliers))


# Calculate the lower limit based on the 10th percentile of the "value" data
your_lower_limit <- quantile(rmse_by_queen_group$value, 0.10, na.rm = TRUE)

# Calculate the upper limit based on the 90th percentile of the "value" data
your_upper_limit <- quantile(rmse_by_queen_group$value, 0.90, na.rm = TRUE)

# Create boxplots without outliers and set the dynamic upper limit with color by "model"
ggplot(rmse_by_queen_group, aes(model, value, fill = model)) +
  facet_wrap( ~ queen ) +
  geom_boxplot(outlier.shape = NA) +  # Suppress outlier points
  coord_flip() +
  labs(
    title = "Individual unit RMSE by simulation model,\nby type of queen, Outliers gone",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  ylim(your_lower_limit, your_upper_limit)+
  guides(fill = guide_legend(reverse = TRUE))



# Individually for each queen



filtered_data <- pL_ca_small_2_50_no %>%
  filter(queen == "ATE")

ggplot( filtered_data, aes( model, value, fill = model)) +
  facet_wrap( ~ metric ) +
  geom_boxplot() +
  coord_flip() +
  labs( title = "Individual unit performance characteristics by simulation model,\nATE as queen",
        x = "",
        y = "" ) +
  theme_minimal()+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



outliers <- filtered_data %>%
  group_by(metric, queen, model) %>%
  summarize(outliers = sum(value < quantile(value, 0.25) - 1.5 * IQR(value) | value > quantile(value, 0.75) + 1.5 * IQR(value))) %>%
  mutate(outlier_percentage = (outliers / 10000) * 100)

# Print the number of outliers
print(outliers, n = nrow(outliers))

# Calculate the lower limit based on the 10th percentile of the "value" data
your_lower_limit <- quantile(filtered_data$value, 0.10, na.rm = TRUE)

# Calculate the upper limit based on the 90th percentile of the "value" data
your_upper_limit <- quantile(filtered_data$value, 0.90, na.rm = TRUE)

# Create boxplots without outliers and set the dynamic upper limit with color by "model"
ggplot(filtered_data, aes(model, value, fill = model)) +
  facet_wrap(~ metric) +
  geom_boxplot(outlier.shape = NA) +  # Suppress outlier points
  coord_flip() +
  labs(
    title = "Individual unit performance characteristics by simulation model,\nATE as queen, Outliers gone",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  ylim(your_lower_limit, your_upper_limit)+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))




filtered_data <- pL_ca_small_2_50_no %>%
  filter(queen == "OLS")

ggplot( filtered_data, aes( model, value, fill = model)) +
  facet_wrap( ~ metric ) +
  geom_boxplot() +
  coord_flip() +
  labs( title = "Individual unit performance characteristics by simulation model,\nOLS as queen",
        x = "",
        y = "" ) +
  theme_minimal()+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



outliers <- filtered_data %>%
  group_by(metric, queen, model) %>%
  summarize(outliers = sum(value < quantile(value, 0.25) - 1.5 * IQR(value) | value > quantile(value, 0.75) + 1.5 * IQR(value))) %>%
  mutate(outlier_percentage = (outliers / 10000) * 100)

# Print the number of outliers
print(outliers, n = nrow(outliers))

# Calculate the lower limit based on the 10th percentile of the "value" data
your_lower_limit <- quantile(filtered_data$value, 0.10, na.rm = TRUE)

# Calculate the upper limit based on the 90th percentile of the "value" data
your_upper_limit <- quantile(filtered_data$value, 0.90, na.rm = TRUE)

# Create boxplots without outliers and set the dynamic upper limit with color by "model"
ggplot(filtered_data, aes(model, value, fill = model)) +
  facet_wrap(~ metric) +
  geom_boxplot(outlier.shape = NA) +  # Suppress outlier points
  coord_flip() +
  labs(
    title = "Individual unit performance characteristics by simulation model,\nOLS as queen, Outliers gone",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  ylim(your_lower_limit, your_upper_limit)+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



filtered_data <- pL_ca_small_2_50_no %>%
  filter(queen == "RF CMR")

ggplot( filtered_data, aes( model, value, fill = model)) +
  facet_wrap( ~ metric ) +
  geom_boxplot() +
  coord_flip() +
  labs( title = "Individual unit performance characteristics by simulation model,\nRF CMR as queen",
        x = "",
        y = "" ) +
  theme_minimal()+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


outliers <- filtered_data %>%
  group_by(metric, queen, model) %>%
  summarize(outliers = sum(value < quantile(value, 0.25) - 1.5 * IQR(value) | value > quantile(value, 0.75) + 1.5 * IQR(value))) %>%
  mutate(outlier_percentage = (outliers / 10000) * 100)

# Print the number of outliers
print(outliers, n = nrow(outliers))

# Calculate the lower limit based on the 10th percentile of the "value" data
your_lower_limit <- quantile(filtered_data$value, 0.10, na.rm = TRUE)

# Calculate the upper limit based on the 90th percentile of the "value" data
your_upper_limit <- quantile(filtered_data$value, 0.90, na.rm = TRUE)

# Create boxplots without outliers and set the dynamic upper limit with color by "model"
ggplot(filtered_data, aes(model, value, fill = model)) +
  facet_wrap(~ metric) +
  geom_boxplot(outlier.shape = NA) +  # Suppress outlier points
  coord_flip() +
  labs(
    title = "Individual unit performance characteristics by simulation model,\nRF CMR as queen, Outliers gone",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  ylim(your_lower_limit, your_upper_limit)+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


filtered_data <- pL_ca_small_2_50_no %>%
  filter(queen == "RF MOM IPW")

ggplot( filtered_data, aes( model, value, fill = model)) +
  facet_wrap( ~ metric ) +
  geom_boxplot() +
  coord_flip() +
  labs( title = "Individual unit performance characteristics by simulation model,\nRF MOM IPW as queen",
        x = "",
        y = "" ) +
  theme_minimal()+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))




outliers <- filtered_data %>%
  group_by(metric, queen, model) %>%
  summarize(outliers = sum(value < quantile(value, 0.25) - 1.5 * IQR(value) | value > quantile(value, 0.75) + 1.5 * IQR(value))) %>%
  mutate(outlier_percentage = (outliers / 10000) * 100)

# Print the number of outliers
print(outliers, n = nrow(outliers))

# Calculate the lower limit based on the 10th percentile of the "value" data
your_lower_limit <- quantile(filtered_data$value, 0.10, na.rm = TRUE)

# Calculate the upper limit based on the 90th percentile of the "value" data
your_upper_limit <- quantile(filtered_data$value, 0.90, na.rm = TRUE)

# Create boxplots without outliers and set the dynamic upper limit with color by "model"
ggplot(filtered_data, aes(model, value, fill = model)) +
  facet_wrap(~ metric) +
  geom_boxplot(outlier.shape = NA) +  # Suppress outlier points
  coord_flip() +
  labs(
    title = "Individual unit performance characteristics by simulation model,\nRF MOM IPW as queen, Outliers gone",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  ylim(your_lower_limit, your_upper_limit)+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



filtered_data <- pL_ca_small_2_50_no %>%
  filter(queen == "RF MOM DR")

ggplot( filtered_data, aes( model, value, fill = model)) +
  facet_wrap( ~ metric ) +
  geom_boxplot() +
  coord_flip() +
  labs( title = "Individual unit performance characteristics by simulation model,\nRF MOM DR as queen",
        x = "",
        y = "" ) +
  theme_minimal()+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))




outliers <- filtered_data %>%
  group_by(metric, queen, model) %>%
  summarize(outliers = sum(value < quantile(value, 0.25) - 1.5 * IQR(value) | value > quantile(value, 0.75) + 1.5 * IQR(value))) %>%
  mutate(outlier_percentage = (outliers / 10000) * 100)

# Print the number of outliers
print(outliers, n = nrow(outliers))

# Calculate the lower limit based on the 10th percentile of the "value" data
your_lower_limit <- quantile(filtered_data$value, 0.10, na.rm = TRUE)

# Calculate the upper limit based on the 90th percentile of the "value" data
your_upper_limit <- quantile(filtered_data$value, 0.90, na.rm = TRUE)

# Create boxplots without outliers and set the dynamic upper limit with color by "model"
ggplot(filtered_data, aes(model, value, fill = model)) +
  facet_wrap(~ metric) +
  geom_boxplot(outlier.shape = NA) +  # Suppress outlier points
  coord_flip() +
  labs(
    title = "Individual unit performance characteristics by simulation model,\nRF MOM DR as queen, Outliers gone",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  ylim(your_lower_limit, your_upper_limit)+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



filtered_data <- pL_ca_small_2_50_no %>%
  filter(queen == "CF")

ggplot( filtered_data, aes( model, value, fill = model)) +
  facet_wrap( ~ metric ) +
  geom_boxplot() +
  coord_flip() +
  labs( title = "Individual unit performance characteristics by simulation model,\nCF as queen",
        x = "",
        y = "" ) +
  theme_minimal()+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))






outliers <- filtered_data %>%
  group_by(metric, queen, model) %>%
  summarize(outliers = sum(value < quantile(value, 0.25) - 1.5 * IQR(value) | value > quantile(value, 0.75) + 1.5 * IQR(value))) %>%
  mutate(outlier_percentage = (outliers / 10000) * 100)

# Print the number of outliers
print(outliers, n = nrow(outliers))

# Calculate the lower limit based on the 10th percentile of the "value" data
your_lower_limit <- quantile(filtered_data$value, 0.10, na.rm = TRUE)

# Calculate the upper limit based on the 90th percentile of the "value" data
your_upper_limit <- quantile(filtered_data$value, 0.90, na.rm = TRUE)

# Create boxplots without outliers and set the dynamic upper limit with color by "model"
ggplot(filtered_data, aes(model, value, fill = model)) +
  facet_wrap(~ metric) +
  geom_boxplot(outlier.shape = NA) +  # Suppress outlier points
  coord_flip() +
  labs(
    title = "Individual unit performance characteristics by simulation model,\nCF as queen, Outliers gone",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  ylim(your_lower_limit, your_upper_limit)+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



filtered_data <- pL_ca_small_2_50_no %>%
  filter(queen == "CF LC")

ggplot( filtered_data, aes( model, value, fill = model)) +
  facet_wrap( ~ metric ) +
  geom_boxplot() +
  coord_flip() +
  labs( title = "Individual unit performance characteristics by simulation model,\nCF LC as queen",
        x = "",
        y = "" ) +
  theme_minimal()+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



outliers <- filtered_data %>%
  group_by(metric, queen, model) %>%
  summarize(outliers = sum(value < quantile(value, 0.25) - 1.5 * IQR(value) | value > quantile(value, 0.75) + 1.5 * IQR(value))) %>%
  mutate(outlier_percentage = (outliers / 10000) * 100)

# Print the number of outliers
print(outliers, n = nrow(outliers))

# Calculate the lower limit based on the 10th percentile of the "value" data
your_lower_limit <- quantile(filtered_data$value, 0.10, na.rm = TRUE)

# Calculate the upper limit based on the 90th percentile of the "value" data
your_upper_limit <- quantile(filtered_data$value, 0.90, na.rm = TRUE)

# Create boxplots without outliers and set the dynamic upper limit with color by "model"
ggplot(filtered_data, aes(model, value, fill = model)) +
  facet_wrap(~ metric) +
  geom_boxplot(outlier.shape = NA) +  # Suppress outlier points
  coord_flip() +
  labs(
    title = "Individual unit performance characteristics by simulation model,\nCF LC as queen, Outliers gone",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  ylim(your_lower_limit, your_upper_limit)+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))




filtered_data <- pL_ca_small_2_50_no %>%
  filter(queen == "Lasso CMR")

ggplot( filtered_data, aes( model, value, fill = model)) +
  facet_wrap( ~ metric ) +
  geom_boxplot() +
  coord_flip() +
  labs( title = "Individual unit performance characteristics by simulation model,\nLasso CMR as queen",
        x = "",
        y = "" ) +
  theme_minimal()+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))






outliers <- filtered_data %>%
  group_by(metric, queen, model) %>%
  summarize(outliers = sum(value < quantile(value, 0.25) - 1.5 * IQR(value) | value > quantile(value, 0.75) + 1.5 * IQR(value))) %>%
  mutate(outlier_percentage = (outliers / 10000) * 100)

# Print the number of outliers
print(outliers, n = nrow(outliers))

# Calculate the lower limit based on the 10th percentile of the "value" data
your_lower_limit <- quantile(filtered_data$value, 0.10, na.rm = TRUE)

# Calculate the upper limit based on the 90th percentile of the "value" data
your_upper_limit <- quantile(filtered_data$value, 0.90, na.rm = TRUE)

# Create boxplots without outliers and set the dynamic upper limit with color by "model"
ggplot(filtered_data, aes(model, value, fill = model)) +
  facet_wrap(~ metric) +
  geom_boxplot(outlier.shape = NA) +  # Suppress outlier points
  coord_flip() +
  labs(
    title = "Individual unit performance characteristics by simulation model,\nLasso CMR as queen, Outliers gone",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  ylim(your_lower_limit, your_upper_limit)+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))





filtered_data <- pL_ca_small_2_50_no %>%
  filter(queen == "Lasso MOM IPW")

ggplot( filtered_data, aes( model, value, fill = model)) +
  facet_wrap( ~ metric ) +
  geom_boxplot() +
  coord_flip() +
  labs( title = "Individual unit performance characteristics by simulation model,\nLasso MOM IPW as queen",
        x = "",
        y = "" ) +
  theme_minimal()+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))





outliers <- filtered_data %>%
  group_by(metric, queen, model) %>%
  summarize(outliers = sum(value < quantile(value, 0.25) - 1.5 * IQR(value) | value > quantile(value, 0.75) + 1.5 * IQR(value))) %>%
  mutate(outlier_percentage = (outliers / 10000) * 100)

# Print the number of outliers
print(outliers, n = nrow(outliers))

# Calculate the lower limit based on the 10th percentile of the "value" data
your_lower_limit <- quantile(filtered_data$value, 0.10, na.rm = TRUE)

# Calculate the upper limit based on the 90th percentile of the "value" data
your_upper_limit <- quantile(filtered_data$value, 0.90, na.rm = TRUE)

# Create boxplots without outliers and set the dynamic upper limit with color by "model"
ggplot(filtered_data, aes(model, value, fill = model)) +
  facet_wrap(~ metric) +
  geom_boxplot(outlier.shape = NA) +  # Suppress outlier points
  coord_flip() +
  labs(
    title = "Individual unit performance characteristics by simulation model,\nLasso MOM IPW as queen, Outliers gone",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  ylim(your_lower_limit, your_upper_limit)+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



filtered_data <- pL_ca_small_2_50_no %>%
  filter(queen == "Lasso MOM DR")

ggplot( filtered_data, aes( model, value, fill = model)) +
  facet_wrap( ~ metric ) +
  geom_boxplot() +
  coord_flip() +
  labs( title = "Individual unit performance characteristics by simulation model,\nLasso MOM DR as queen",
        x = "",
        y = "" ) +
  theme_minimal()+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))






outliers <- filtered_data %>%
  group_by(metric, queen, model) %>%
  summarize(outliers = sum(value < quantile(value, 0.25) - 1.5 * IQR(value) | value > quantile(value, 0.75) + 1.5 * IQR(value))) %>%
  mutate(outlier_percentage = (outliers / 10000) * 100)

# Print the number of outliers
print(outliers, n = nrow(outliers))

# Calculate the lower limit based on the 10th percentile of the "value" data
your_lower_limit <- quantile(filtered_data$value, 0.10, na.rm = TRUE)

# Calculate the upper limit based on the 90th percentile of the "value" data
your_upper_limit <- quantile(filtered_data$value, 0.90, na.rm = TRUE)

# Create boxplots without outliers and set the dynamic upper limit with color by "model"
ggplot(filtered_data, aes(model, value, fill = model)) +
  facet_wrap(~ metric) +
  geom_boxplot(outlier.shape = NA) +  # Suppress outlier points
  coord_flip() +
  labs(
    title = "Individual unit performance characteristics by simulation model,\nLasso MOM DR as queen, Outliers gone",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  ylim(your_lower_limit, your_upper_limit)+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



filtered_data <- pL_ca_small_2_50_no %>%
  filter(queen == "Lasso MCM")

ggplot( filtered_data, aes( model, value, fill = model)) +
  facet_wrap( ~ metric ) +
  geom_boxplot() +
  coord_flip() +
  labs( title = "Individual unit performance characteristics by simulation model,\nLasso MCM as queen",
        x = "",
        y = "" ) +
  theme_minimal()+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



outliers <- filtered_data %>%
  group_by(metric, queen, model) %>%
  summarize(outliers = sum(value < quantile(value, 0.25) - 1.5 * IQR(value) | value > quantile(value, 0.75) + 1.5 * IQR(value))) %>%
  mutate(outlier_percentage = (outliers / 10000) * 100)

# Print the number of outliers
print(outliers, n = nrow(outliers))

# Calculate the lower limit based on the 10th percentile of the "value" data
your_lower_limit <- quantile(filtered_data$value, 0.10, na.rm = TRUE)

# Calculate the upper limit based on the 90th percentile of the "value" data
your_upper_limit <- quantile(filtered_data$value, 0.90, na.rm = TRUE)

# Create boxplots without outliers and set the dynamic upper limit with color by "model"
ggplot(filtered_data, aes(model, value, fill = model)) +
  facet_wrap(~ metric) +
  geom_boxplot(outlier.shape = NA) +  # Suppress outlier points
  coord_flip() +
  labs(
    title = "Individual unit performance characteristics by simulation model,\nLasso MCM as queen, Outliers gone",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  ylim(your_lower_limit, your_upper_limit)+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


filtered_data <- pL_ca_small_2_50_no %>%
  filter(queen == "Lasso MCM EA")

ggplot( filtered_data, aes( model, value, fill = model)) +
  facet_wrap( ~ metric ) +
  geom_boxplot() +
  coord_flip() +
  labs( title = "Individual unit performance characteristics by simulation model,\nLasso MCM EA as queen",
        x = "",
        y = "" ) +
  theme_minimal()+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))




outliers <- filtered_data %>%
  group_by(metric, queen, model) %>%
  summarize(outliers = sum(value < quantile(value, 0.25) - 1.5 * IQR(value) | value > quantile(value, 0.75) + 1.5 * IQR(value))) %>%
  mutate(outlier_percentage = (outliers / 10000) * 100)

# Print the number of outliers
print(outliers, n = nrow(outliers))

# Calculate the lower limit based on the 10th percentile of the "value" data
your_lower_limit <- quantile(filtered_data$value, 0.10, na.rm = TRUE)

# Calculate the upper limit based on the 90th percentile of the "value" data
your_upper_limit <- quantile(filtered_data$value, 0.90, na.rm = TRUE)

# Create boxplots without outliers and set the dynamic upper limit with color by "model"
ggplot(filtered_data, aes(model, value, fill = model)) +
  facet_wrap(~ metric) +
  geom_boxplot(outlier.shape = NA) +  # Suppress outlier points
  coord_flip() +
  labs(
    title = "Individual unit performance characteristics by simulation model,\nLasso MCM EA as queen, Outliers gone",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  ylim(your_lower_limit, your_upper_limit)+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



filtered_data <- pL_ca_small_2_50_no %>%
  filter(queen == "Lasso RL")

ggplot( filtered_data, aes( model, value, fill = model)) +
  facet_wrap( ~ metric ) +
  geom_boxplot() +
  coord_flip() +
  labs( title = "Individual unit performance characteristics by simulation model,\nLasso RL as queen",
        x = "",
        y = "" ) +
  theme_minimal()+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))




outliers <- filtered_data %>%
  group_by(metric, queen, model) %>%
  summarize(outliers = sum(value < quantile(value, 0.25) - 1.5 * IQR(value) | value > quantile(value, 0.75) + 1.5 * IQR(value))) %>%
  mutate(outlier_percentage = (outliers / 10000) * 100)

# Print the number of outliers
print(outliers, n = nrow(outliers))

# Calculate the lower limit based on the 10th percentile of the "value" data
your_lower_limit <- quantile(filtered_data$value, 0.10, na.rm = TRUE)

# Calculate the upper limit based on the 90th percentile of the "value" data
your_upper_limit <- quantile(filtered_data$value, 0.90, na.rm = TRUE)

# Create boxplots without outliers and set the dynamic upper limit with color by "model"
ggplot(filtered_data, aes(model, value, fill = model)) +
  facet_wrap(~ metric) +
  geom_boxplot(outlier.shape = NA) +  # Suppress outlier points
  coord_flip() +
  labs(
    title = "Individual unit performance characteristics by simulation model,\nLasso RL as queen, Outliers gone",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  ylim(your_lower_limit, your_upper_limit)+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# AVERAGE 
# 
# folder_name = paste0("/Sim_Run_2023-10-09", "/" )
# 
# output_path_name = find_path(path=output_path)[1]
# print(output_path_name)# TODO::MOVE
# 
# # Define the file path for saving the Excel file
# file_path <- paste0(output_path_name, folder_name)

# Overall performance
models = c( "ATE", "OLS", "RF Inf","RF CMR","RF MOM IPW","RF MOM DR",
            "CF","CF LC","CDML","Lasso Inf",
            "Lasso CMR","Lasso MOM IPW","Lasso MOM DR", "Lasso MCM","Lasso MCM EA",
            "Lasso RL", "XBART" )


agg_perf_ca_50_2_RF_LASSO = aggregate_metrics(ca_50_2_RF_LASSO)


# Table performance
tbl_perf_ca_50_2_RF_LASSO <- agg_perf_ca_50_2_RF_LASSO %>% 
  dplyr::select( -Q1, -Q3 ) %>%
  pivot_wider( names_from = "metric",
               values_from = "Ev" )
knitr::kable(tbl_perf_ca_50_2_RF_LASSO, digits=2 )


#write.xlsx(tbl_perf_ca_50_2_RF_LASSO, file = paste0(file_path, "tbl_perf_ca_50_2_RF_LASSO.xlsx"))



agg_perf_ca_50_2_RF_LASSO <- agg_perf_ca_50_2_RF_LASSO %>%
  mutate( metric = factor( metric,
                           levels = c( "bias", "se", "rmse" ),
                           labels = c( "|bias|", "se", "rmse" )),
          model = fct_reorder(model, Ev^2 ) )
# # Define the desired order of levels for the "model" variable
desired_order <- c("ATE", "OLS", "RF Inf", "RF CMR", "RF MOM IPW", "RF MOM DR",
                   "CF", "CF LC", "CDML", "Lasso Inf", "Lasso CMR", "Lasso MOM IPW",
                   "Lasso MOM DR", "Lasso MCM", "Lasso MCM EA", "Lasso RL", "XBART")

agg_perf_ca_50_2_RF_LASSO$model <- factor(agg_perf_ca_50_2_RF_LASSO$model, levels = rev(desired_order), ordered = TRUE)

ggplot( agg_perf_ca_50_2_RF_LASSO, aes( model, Ev, fill = model)) +
  facet_wrap( ~ metric ) +
  geom_bar( stat="identity", width=1 ) +
  coord_flip() +
  theme_minimal() + 
  # scale_y_log10() +
  scale_fill_manual(values = scales::hue_pal()(length(unique(agg_perf_ca_50_2_RF_LASSO$model)))) +  # Manually set fill colors
  
  labs( title="Average performance across all queens" )+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))




# Overall performance by queen 
agg_perf_by_queen_ca_50_2_RF_LASSO = aggregate_metrics_by_queen(ca_50_2_RF_LASSO)



# Table performance
tbl_perf_by_queen_ca_50_2_RF_LASSO <- agg_perf_by_queen_ca_50_2_RF_LASSO %>% 
  dplyr::select( -Q1, -Q3 ) %>%
  pivot_wider( names_from = "metric",
               values_from = "Ev" )
knitr::kable(tbl_perf_by_queen_ca_50_2_RF_LASSO, digits=2 )


# write.xlsx(tbl_perf_by_queen_ca_50_2_RF_LASSO, file = paste0(file_path, "tbl_perf_by_queen_ca_50_2_RF_LASSO.xlsx"))



agg_perf_by_queen_ca_50_2_RF_LASSO <- agg_perf_by_queen_ca_50_2_RF_LASSO %>%
  mutate( metric = factor( metric,
                           levels = c( "bias", "se", "rmse" ),
                           labels = c( "|bias|", "se", "rmse" )),
          model = fct_reorder(model, Ev^2 ) )
# Increase the height to make the plot taller


# Set custom width and height for the plot (in inches)
plot_width <- 8.5  # Width of US letter size paper
plot_height <- 11  # Height of US letter size paper

# # Define the desired order of levels for the "model" variable
desired_order <- c("ATE", "OLS", "RF Inf", "RF CMR", "RF MOM IPW", "RF MOM DR",
                   "CF", "CF LC", "CDML", "Lasso Inf", "Lasso CMR", "Lasso MOM IPW",
                   "Lasso MOM DR", "Lasso MCM", "Lasso MCM EA", "Lasso RL", "XBART")

agg_perf_by_queen_ca_50_2_RF_LASSO$queen <- factor(agg_perf_by_queen_ca_50_2_RF_LASSO$queen, levels = rev(desired_order), ordered = TRUE)

# Create the plot with custom size
gg <- ggplot(agg_perf_by_queen_ca_50_2_RF_LASSO, aes(model, Ev, fill = queen)) +
  facet_wrap(~metric) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  coord_flip() +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the title
    axis.title.y = element_blank(),  # Remove the y-axis title
    axis.text.y = element_text(size = 8),  # Adjust y-axis text size
    strip.text = element_text(size = 8),  # Adjust facet labels size
    plot.background = element_blank(),  # Remove plot background
    plot.margin = unit(c(5, 5, 5, 5), "pt"),  # Adjust margins
    axis.text.x = element_text(angle = 90, hjust = 1)  # Rotate x-axis labels vertically
  ) +
  scale_fill_manual(values = scales::hue_pal()(length(unique(agg_perf_by_queen_ca_50_2_RF_LASSO$queen)))) +  # Manually set fill colors
  
  labs(title = "Average performance by model by queen")+
  guides(fill = guide_legend(reverse = TRUE))



# Use ggsave to save the ggplot object to a file
ggsave(filename = paste0(file_path, "long_plot.png"), plot = gg, width = plot_width, height = plot_height, units = "in")





# By queen type
# Define the desired order of levels for the "model" variable
desired_order <- c("ATE", "OLS", "RF Inf", "RF CMR", "RF MOM IPW", "RF MOM DR",
                   "CF", "CF LC", "CDML", "Lasso Inf", "Lasso CMR", "Lasso MOM IPW",
                   "Lasso MOM DR", "Lasso MCM", "Lasso MCM EA", "Lasso RL", "XBART")


# Group by queen type bias
filtered_data <- agg_perf_by_queen_ca_50_2_RF_LASSO %>%
  filter(metric == "|bias|")

bias_RF <- filtered_data %>%
  filter(queen %in% c("RF CMR", "RF MOM IPW", "RF MOM DR", "CF", "CF LC")) %>%
  group_by(metric, model) %>%
  summarize(value = mean(Ev, na.rm = TRUE))

bias_RF$queen = "Average RF"

bias_OLS <- filtered_data %>%
  filter(queen %in% c("OLS"))

bias_OLS$value = bias_OLS$Ev
bias_OLS <- bias_OLS[, !(colnames(bias_OLS) %in% c("Ev", "Q1", "Q3"))]

bias_ATE <- filtered_data %>%
  filter(queen %in% c("ATE"))

bias_ATE$value = bias_ATE$Ev
bias_ATE <- bias_ATE[, !(colnames(bias_ATE) %in% c("Ev", "Q1", "Q3"))]


bias_Lasso <- filtered_data %>%
  filter(queen %in% c(
                      "Lasso CMR","Lasso MOM IPW","Lasso MOM DR", "Lasso MCM","Lasso MCM EA",
                      "Lasso RL")) %>%
  group_by(metric, model) %>%
  summarize(value = mean(Ev, na.rm = TRUE))

bias_Lasso$queen = "Average Lasso"

bias_by_queen_group = rbind(bias_ATE, bias_OLS, bias_RF, bias_Lasso)
bias_by_queen_group$model <- factor(bias_by_queen_group$model, levels = rev(desired_order), ordered = TRUE)
desired_order_2 <- c("ATE", "OLS", "Average RF", "Average Lasso")
bias_by_queen_group$queen <- factor(bias_by_queen_group$queen, levels = desired_order_2, ordered = TRUE)

ggplot( bias_by_queen_group, aes( model, value, fill = model)) +
  facet_wrap( ~ queen ) +
  geom_bar( stat="identity", width=1 ) +
  coord_flip() +
  theme_minimal() + 
  # scale_y_log10() +
  scale_fill_manual(values = scales::hue_pal()(length(desired_order))) +  # Manually set fill colors
  
  labs( title="Average bias, by simulation model, \nby queen type" )+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# by model type


# Group by queen type bias
filtered_data <- agg_perf_by_queen_ca_50_2_RF_LASSO %>%
  filter(metric == "|bias|")

bias_RF <- filtered_data %>%
  filter(model %in% c("RF Inf", "RF CMR", "RF MOM IPW", "RF MOM DR", "CF", "CF LC")) %>%
  group_by(metric, queen) %>%
  summarize(value = mean(Ev, na.rm = TRUE))

bias_RF$model = "Average RF"

bias_OLS <- filtered_data %>%
  filter(model %in% c("OLS"))

bias_OLS$value = bias_OLS$Ev
bias_OLS <- bias_OLS[, !(colnames(bias_OLS) %in% c("Ev", "Q1", "Q3"))]

bias_ATE <- filtered_data %>%
  filter(model %in% c("ATE"))

bias_ATE$value = bias_ATE$Ev
bias_ATE <- bias_ATE[, !(colnames(bias_ATE) %in% c("Ev", "Q1", "Q3"))]


bias_Lasso <- filtered_data %>%
  filter(model %in% c(
    "Lasso Inf", "Lasso CMR","Lasso MOM IPW","Lasso MOM DR", "Lasso MCM","Lasso MCM EA",
    "Lasso RL")) %>%
  group_by(metric, queen) %>%
  summarize(value = mean(Ev, na.rm = TRUE))

bias_Lasso$model = "Average Lasso"

bias_by_queen_group = rbind(bias_ATE, bias_OLS, bias_RF, bias_Lasso)
bias_by_queen_group$queen <- factor(bias_by_queen_group$queen, levels = rev(desired_order), ordered = TRUE)
desired_order_2 <- c("ATE", "OLS", "Average RF", "Average Lasso")
bias_by_queen_group$model <- factor(bias_by_queen_group$model, levels = desired_order_2, ordered = TRUE)

ggplot( bias_by_queen_group, aes( queen, value, fill = queen)) +
  facet_wrap( ~ model ) +
  geom_bar( stat="identity", width=1 ) +
  coord_flip() +
  theme_minimal() + 
  # scale_y_log10() +
  scale_fill_manual(values = scales::hue_pal()(length(desired_order))) +  # Manually set fill colors
  
  labs( title="Average bias, by queen, by model type" )+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))






filtered_data <- agg_perf_by_queen_ca_50_2_RF_LASSO %>%
  filter(metric == "se")

se_RF <- filtered_data %>%
  filter(queen %in% c( "RF CMR", "RF MOM IPW", "RF MOM DR", "CF", "CF LC")) %>%
  group_by(metric, model) %>%
  summarize(value = mean(Ev, na.rm = TRUE))

se_RF$queen = "Average RF"

se_OLS <- filtered_data %>%
  filter(queen %in% c("OLS"))

se_OLS$value = se_OLS$Ev
se_OLS <- se_OLS[, !(colnames(se_OLS) %in% c("Ev", "Q1", "Q3"))]

se_ATE <- filtered_data %>%
  filter(queen %in% c("ATE"))

se_ATE$value = se_ATE$Ev
se_ATE <- se_ATE[, !(colnames(se_ATE) %in% c("Ev", "Q1", "Q3"))]


se_Lasso <- filtered_data %>%
  filter(queen %in% c(
                      "Lasso CMR","Lasso MOM IPW","Lasso MOM DR", "Lasso MCM","Lasso MCM EA",
                      "Lasso RL")) %>%
  group_by(metric, model) %>%
  summarize(value = mean(Ev, na.rm = TRUE))

se_Lasso$queen = "Average Lasso"

se_by_queen_group = rbind(se_ATE, se_OLS, se_RF, se_Lasso)
se_by_queen_group$model <- factor(se_by_queen_group$model, levels = rev(desired_order), ordered = TRUE)

desired_order_2 <- c("ATE", "OLS", "Average RF", "Average Lasso")
bias_by_queen_group$queen <- factor(bias_by_queen_group$queen, levels = desired_order_2, ordered = TRUE)

ggplot( se_by_queen_group, aes( model, value, fill = model)) +
  facet_wrap( ~ queen ) +
  geom_bar( stat="identity", width=1 ) +
  coord_flip() +
  theme_minimal() + 
  # scale_y_log10() +
  scale_fill_manual(values = scales::hue_pal()(length(desired_order))) +  # Manually set fill colors
  
  labs( title="Average SE, by simulation model, \nby queen type" )+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# by est model



filtered_data <- agg_perf_by_queen_ca_50_2_RF_LASSO %>%
  filter(metric == "se")

se_RF <- filtered_data %>%
  filter(model %in% c( "RF Inf", "RF CMR", "RF MOM IPW", "RF MOM DR", "CF", "CF LC")) %>%
  group_by(metric, queen) %>%
  summarize(value = mean(Ev, na.rm = TRUE))

se_RF$model = "Average RF"

se_OLS <- filtered_data %>%
  filter(model %in% c("OLS"))

se_OLS$value = se_OLS$Ev
se_OLS <- se_OLS[, !(colnames(se_OLS) %in% c("Ev", "Q1", "Q3"))]

se_ATE <- filtered_data %>%
  filter(model %in% c("ATE"))

se_ATE$value = se_ATE$Ev
se_ATE <- se_ATE[, !(colnames(se_ATE) %in% c("Ev", "Q1", "Q3"))]


se_Lasso <- filtered_data %>%
  filter(model %in% c(
    "Lasso Inf", "Lasso CMR","Lasso MOM IPW","Lasso MOM DR", "Lasso MCM","Lasso MCM EA",
    "Lasso RL")) %>%
  group_by(metric, queen) %>%
  summarize(value = mean(Ev, na.rm = TRUE))

se_Lasso$model = "Average Lasso"

se_by_queen_group = rbind(se_ATE, se_OLS, se_RF, se_Lasso)
se_by_queen_group$queen <- factor(se_by_queen_group$queen, levels = rev(desired_order), ordered = TRUE)
desired_order_2 <- c("ATE", "OLS", "Average RF", "Average Lasso")
bias_by_queen_group$model <- factor(bias_by_queen_group$model, levels = desired_order_2, ordered = TRUE)

ggplot( se_by_queen_group, aes( queen, value, fill = queen)) +
  facet_wrap( ~ model ) +
  geom_bar( stat="identity", width=1 ) +
  coord_flip() +
  theme_minimal() + 
  # scale_y_log10() +
  scale_fill_manual(values = scales::hue_pal()(length(desired_order))) +  # Manually set fill colors
  
  labs( title="Average SE, by queen, \nby model type" )+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))




filtered_data <- agg_perf_by_queen_ca_50_2_RF_LASSO %>%
  filter(metric == "rmse")

rmse_RF <- filtered_data %>%
  filter(queen %in% c( "RF CMR", "RF MOM IPW", "RF MOM DR", "CF", "CF LC")) %>%
  group_by(metric, model) %>%
  summarize(value = mean(Ev, na.rm = TRUE))

rmse_RF$queen = "Average RF"

rmse_OLS <- filtered_data %>%
  filter(queen %in% c("OLS"))

rmse_OLS$value = rmse_OLS$Ev
rmse_OLS <- rmse_OLS[, !(colnames(rmse_OLS) %in% c("Ev", "Q1", "Q3"))]

rmse_ATE <- filtered_data %>%
  filter(queen %in% c("ATE"))

rmse_ATE$value = rmse_ATE$Ev
rmse_ATE <- rmse_ATE[, !(colnames(rmse_ATE) %in% c("Ev", "Q1", "Q3"))]


rmse_Lasso <- filtered_data %>%
  filter(queen %in% c(
                      "Lasso CMR","Lasso MOM IPW","Lasso MOM DR", "Lasso MCM","Lasso MCM EA",
                      "Lasso RL")) %>%
  group_by(metric, model) %>%
  summarize(value = mean(Ev, na.rm = TRUE))

rmse_Lasso$queen = "Average Lasso"

rmse_by_queen_group = rbind(rmse_ATE, rmse_OLS, rmse_RF, rmse_Lasso)

rmse_by_queen_group$model <- factor(rmse_by_queen_group$model, levels = rev(desired_order), ordered = TRUE)

desired_order_2 <- c("ATE", "OLS", "Average RF", "Average Lasso")
bias_by_queen_group$queen <- factor(bias_by_queen_group$queen, levels = desired_order_2, ordered = TRUE)


ggplot( rmse_by_queen_group, aes( model, value, fill = model)) +
  facet_wrap( ~ queen ) +
  geom_bar( stat="identity", width=1 ) +
  coord_flip() +
  theme_minimal() + 
  # scale_y_log10() +
  scale_fill_manual(values = scales::hue_pal()(length(desired_order))) +  # Manually set fill colors
  labs( title="Average RMSE, by simulation model, \nby queen type" )+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



# by est model

filtered_data <- agg_perf_by_queen_ca_50_2_RF_LASSO %>%
  filter(metric == "rmse")

rmse_RF <- filtered_data %>%
  filter(model %in% c( "RF Inf", "RF CMR", "RF MOM IPW", "RF MOM DR", "CF", "CF LC")) %>%
  group_by(metric, queen) %>%
  summarize(value = mean(Ev, na.rm = TRUE))

rmse_RF$model = "Average RF"

rmse_OLS <- filtered_data %>%
  filter(model %in% c("OLS"))

rmse_OLS$value = rmse_OLS$Ev
rmse_OLS <- rmse_OLS[, !(colnames(rmse_OLS) %in% c("Ev", "Q1", "Q3"))]

rmse_ATE <- filtered_data %>%
  filter(model %in% c("ATE"))

rmse_ATE$value = rmse_ATE$Ev
rmse_ATE <- rmse_ATE[, !(colnames(rmse_ATE) %in% c("Ev", "Q1", "Q3"))]


rmse_Lasso <- filtered_data %>%
  filter(model %in% c(
    "Lasso Inf", "Lasso CMR","Lasso MOM IPW","Lasso MOM DR", "Lasso MCM","Lasso MCM EA",
    "Lasso RL")) %>%
  group_by(metric, queen) %>%
  summarize(value = mean(Ev, na.rm = TRUE))

rmse_Lasso$model = "Average Lasso"

rmse_by_queen_group = rbind(rmse_ATE, rmse_OLS, rmse_RF, rmse_Lasso)

rmse_by_queen_group$queen <- factor(rmse_by_queen_group$queen, levels = rev(desired_order), ordered = TRUE)

desired_order_2 <- c("ATE", "OLS", "Average RF", "Average Lasso")
bias_by_queen_group$model <- factor(bias_by_queen_group$model, levels = desired_order_2, ordered = TRUE)


ggplot( rmse_by_queen_group, aes( queen, value, fill = queen)) +
  facet_wrap( ~ model ) +
  geom_bar( stat="identity", width=1 ) +
  coord_flip() +
  theme_minimal() + 
  # scale_y_log10() +
  scale_fill_manual(values = scales::hue_pal()(length(desired_order))) +  # Manually set fill colors
  labs( title="Average RMSE, by queen, \nby model type" )+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



# By ind queen


filtered_data <- agg_perf_by_queen_ca_50_2_RF_LASSO %>%
  filter(queen == "ATE")

ggplot( filtered_data, aes( model, Ev, fill = model)) +
  facet_wrap( ~ metric ) +
  geom_bar( stat="identity", width=1 ) +
  coord_flip() +
  theme_minimal() + 
  # scale_y_log10() +
  labs( title="Average performance, ATE as queen" )+
  guides(fill = guide_legend(reverse = TRUE))

filtered_data <- agg_perf_by_queen_ca_50_2_RF_LASSO %>%
  filter(queen == "OLS")
filtered_data$model <- factor(filtered_data$model, levels = rev(desired_order), ordered = TRUE)

ggplot( filtered_data, aes( model, Ev, fill = model)) +
  facet_wrap( ~ metric ) +
  geom_bar( stat="identity", width=1 ) +
  coord_flip() +
  theme_minimal() + 
  # scale_y_log10() +
  scale_fill_manual(values = scales::hue_pal()(length(desired_order))) +  # Manually set fill colors
  labs( title="Average performance, OLS as queen" )+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))





filtered_data <- agg_perf_by_queen_ca_50_2_RF_LASSO %>%
  filter(queen == "RF CMR")
filtered_data$model <- factor(filtered_data$model, levels = rev(desired_order), ordered = TRUE)

ggplot( filtered_data, aes( model, Ev, fill = model)) +
  facet_wrap( ~ metric ) +
  geom_bar( stat="identity", width=1 ) +
  coord_flip() +
  theme_minimal() + 
  # scale_y_log10() +
  scale_fill_manual(values = scales::hue_pal()(length(desired_order))) +  # Manually set fill colors
  
  labs( title="Average performance, RF CMR as queen" )+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))





filtered_data <- agg_perf_by_queen_ca_50_2_RF_LASSO %>%
  filter(queen == "RF MOM IPW")
filtered_data$model <- factor(filtered_data$model, levels = rev(desired_order), ordered = TRUE)

ggplot( filtered_data, aes( model, Ev, fill = model)) +
  facet_wrap( ~ metric ) +
  geom_bar( stat="identity", width=1 ) +
  coord_flip() +
  theme_minimal() + 
  # scale_y_log10() +
  scale_fill_manual(values = scales::hue_pal()(length(desired_order))) +  # Manually set fill colors
  
  labs( title="Average performance, \nRF MOM IPW as queen" )+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



filtered_data <- agg_perf_by_queen_ca_50_2_RF_LASSO %>%
  filter(queen == "RF MOM DR")
filtered_data$model <- factor(filtered_data$model, levels = rev(desired_order), ordered = TRUE)

ggplot( filtered_data, aes( model, Ev, fill = model)) +
  facet_wrap( ~ metric ) +
  geom_bar( stat="identity", width=1 ) +
  coord_flip() +
  theme_minimal() + 
  # scale_y_log10() +
  scale_fill_manual(values = scales::hue_pal()(length(desired_order))) +  # Manually set fill colors
  
  labs( title="Average performance, \nRF MOM DR as queen" )+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



filtered_data <- agg_perf_by_queen_ca_50_2_RF_LASSO %>%
  filter(queen == "CF")
filtered_data$model <- factor(filtered_data$model, levels = rev(desired_order), ordered = TRUE)

ggplot( filtered_data, aes( model, Ev, fill = model)) +
  facet_wrap( ~ metric ) +
  geom_bar( stat="identity", width=1 ) +
  coord_flip() +
  theme_minimal() + 
  # scale_y_log10() +
  scale_fill_manual(values = scales::hue_pal()(length(desired_order))) +  # Manually set fill colors
  
  labs( title="Average performance, CF as queen" )+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



filtered_data <- agg_perf_by_queen_ca_50_2_RF_LASSO %>%
  filter(queen == "CF LC")
filtered_data$model <- factor(filtered_data$model, levels = rev(desired_order), ordered = TRUE)

ggplot( filtered_data, aes( model, Ev, fill = model)) +
  facet_wrap( ~ metric ) +
  geom_bar( stat="identity", width=1 ) +
  coord_flip() +
  theme_minimal() + 
  # scale_y_log10() +
  scale_fill_manual(values = scales::hue_pal()(length(desired_order))) +  # Manually set fill colors
  labs( title="Average performance, CF LC as queen" )+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))






filtered_data <- agg_perf_by_queen_ca_50_2_RF_LASSO %>%
  filter(queen == "Lasso CMR")
filtered_data$model <- factor(filtered_data$model, levels = rev(desired_order), ordered = TRUE)

ggplot( filtered_data, aes( model, Ev, fill = model)) +
  facet_wrap( ~ metric ) +
  geom_bar( stat="identity", width=1 ) +
  coord_flip() +
  theme_minimal() + 
  # scale_y_log10() +
  scale_fill_manual(values = scales::hue_pal()(length(desired_order))) +  # Manually set fill colors
  
  labs( title="Average performance, \nLasso CMR as queen" )+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



filtered_data <- agg_perf_by_queen_ca_50_2_RF_LASSO %>%
  filter(queen == "Lasso MOM IPW")
filtered_data$model <- factor(filtered_data$model, levels = rev(desired_order), ordered = TRUE)

ggplot( filtered_data, aes( model, Ev, fill = model)) +
  facet_wrap( ~ metric ) +
  geom_bar( stat="identity", width=1 ) +
  coord_flip() +
  theme_minimal() + 
  # scale_y_log10() +
  scale_fill_manual(values = scales::hue_pal()(length(desired_order))) +  # Manually set fill colors
  
  labs( title="Average performance, \nLasso MOM IPW as queen" )+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



filtered_data <- agg_perf_by_queen_ca_50_2_RF_LASSO %>%
  filter(queen == "Lasso MOM DR")
filtered_data$model <- factor(filtered_data$model, levels = rev(desired_order), ordered = TRUE)

ggplot( filtered_data, aes( model, Ev, fill = model)) +
  facet_wrap( ~ metric ) +
  geom_bar( stat="identity", width=1 ) +
  coord_flip() +
  theme_minimal() + 
  # scale_y_log10() +
  scale_fill_manual(values = scales::hue_pal()(length(desired_order))) +  # Manually set fill colors
  
  labs( title="Average performance, \nLasso MOM DR as queen" )+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


filtered_data <- agg_perf_by_queen_ca_50_2_RF_LASSO %>%
  filter(queen == "Lasso MCM")
filtered_data$model <- factor(filtered_data$model, levels = rev(desired_order), ordered = TRUE)

ggplot( filtered_data, aes( model, Ev, fill = model)) +
  facet_wrap( ~ metric ) +
  geom_bar( stat="identity", width=1 ) +
  coord_flip() +
  theme_minimal() + 
  # scale_y_log10() +
  scale_fill_manual(values = scales::hue_pal()(length(desired_order))) +  # Manually set fill colors
  
  labs( title="Average performance, Lasso MCM as queen" )+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))




filtered_data <- agg_perf_by_queen_ca_50_2_RF_LASSO %>%
  filter(queen == "Lasso MCM EA")
filtered_data$model <- factor(filtered_data$model, levels = rev(desired_order), ordered = TRUE)

ggplot( filtered_data, aes( model, Ev, fill = model)) +
  facet_wrap( ~ metric ) +
  geom_bar( stat="identity", width=1 ) +
  coord_flip() +
  theme_minimal() + 
  # scale_y_log10() +
  scale_fill_manual(values = scales::hue_pal()(length(desired_order))) +  # Manually set fill colors
  
  labs( title="Average performance, \nLasso MCM EA as queen" )+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

filtered_data <- agg_perf_by_queen_ca_50_2_RF_LASSO %>%
  filter(queen == "Lasso RL")
filtered_data$model <- factor(filtered_data$model, levels = rev(desired_order), ordered = TRUE)

ggplot( filtered_data, aes( model, Ev, fill = model)) +
  facet_wrap( ~ metric ) +
  geom_bar( stat="identity", width=1 ) +
  coord_flip() +
  theme_minimal() + 
  # scale_y_log10() +
  scale_fill_manual(values = scales::hue_pal()(length(desired_order))) +  # Manually set fill colors
  
  labs( title="Average performance, \nLasso RL as queen" )+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))





