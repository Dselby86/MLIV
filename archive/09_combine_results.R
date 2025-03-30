###################################################################################
#                                                                                                        
#                                                                         
# Created on:   11/05/2024
# Purpose:      Create a function that aggregates and combines the standardized simulation results and saved as combined_all
# Authors:      Luke Miratrix, Polina Polskaia, Nick Commins
# 
#
###################################################################################

# Load script 08
source(here::here("simulation_pipeline/08_run_simulation.R"))

#######################################
#Call the function 'read_and_aggregate' to read in asap and ca data for different covariate set sizes (small, medium and large) and different train set sizes (1000, 2000, 5000). Suffixes of _noSL and _SL indicate that the simulation excluded or included super-learner (SL) models

#read in baseline data
#standardize simulation output

if ( FALSE ) {
  # Debug and play with read_and_aggregate function
  read_and_aggregate( cov_set_size = "small", dataset = "ca", outcome_index = 1, train_set_size = "1000", relative_path = "results/aggregated_IATEs/ca/simulation_from_080524/aggregated_IATEs_data.rds")
}


#ca outcome 1, train set size 1000
ca_1_1000_small_noSL = read_and_aggregate(cov_set_size = "small", dataset = "ca", outcome_index = 1, train_set_size = "1000", relative_path = "results/aggregated_IATEs/ca/simulation_from_080524/aggregated_IATEs_data.rds")

ca_1_1000_small_SL = read_and_aggregate(cov_set_size = "small", dataset = "ca", outcome_index = 1, train_set_size = "1000", relative_path = "results/aggregated_IATEs/ca/simulation_from_091024/aggregated_IATEs_data.rds")  %>% filter(model %in% c("SL S", "SL T"))

ca_1_1000_medium_noSL = read_and_aggregate(cov_set_size = "medium", dataset = "ca", outcome_index = 1, train_set_size = "1000", relative_path = "results/aggregated_IATEs/ca/simulation_from_080624/aggregated_IATEs_data.rds")

ca_1_1000_medium_SL = read_and_aggregate(cov_set_size = "medium", dataset = "ca", outcome_index = 1, train_set_size = "1000", relative_path = "results/aggregated_IATEs/ca/simulation_from_091124/aggregated_IATEs_data.rds")%>% filter(model %in% c("SL S", "SL T"))

ca_1_1000_large_noSL = read_and_aggregate(cov_set_size = "large", dataset = "ca", outcome_index = 1, train_set_size = "1000", relative_path = "results/aggregated_IATEs/ca/simulation_from_080724/aggregated_IATEs_data.rds")

ca_1_1000_large_SL = read_and_aggregate(cov_set_size = "large", dataset = "ca", outcome_index = 1, train_set_size = "1000", relative_path = "results/aggregated_IATEs/ca/simulation_from_091624/aggregated_IATEs_data.rds")%>% filter(model %in% c("SL S", "SL T"))


#ca outcome 1, train set size 2000
ca_1_2000_small_noSL = read_and_aggregate(cov_set_size = "small", dataset = "ca", outcome_index = 1, train_set_size = "2000", relative_path = "results/aggregated_IATEs/ca/simulation_from_080824/aggregated_IATEs_data.rds")

ca_1_2000_small_SL = read_and_aggregate(cov_set_size = "small", dataset = "ca", outcome_index = 1, train_set_size = "2000", relative_path = "results/aggregated_IATEs/ca/simulation_from_091824/aggregated_IATEs_data.rds")%>% filter(model %in% c("SL S", "SL T"))

ca_1_2000_medium_noSL = read_and_aggregate(cov_set_size = "medium", dataset = "ca", outcome_index = 1, train_set_size = "2000", relative_path = "results/aggregated_IATEs/ca/simulation_from_081424/aggregated_IATEs_data.rds")

ca_1_2000_medium_SL = read_and_aggregate(cov_set_size = "medium", dataset = "ca", outcome_index = 1, train_set_size = "2000", relative_path = "results/aggregated_IATEs/ca/simulation_from_092424/aggregated_IATEs_data.rds")%>% filter(model %in% c("SL S", "SL T"))

ca_1_2000_large_noSL = read_and_aggregate(cov_set_size = "large", dataset = "ca", outcome_index = 1, train_set_size = "2000", relative_path = "results/aggregated_IATEs/ca/simulation_from_082224/aggregated_IATEs_data.rds")

ca_1_2000_large_SL = read_and_aggregate(cov_set_size = "large", dataset = "ca", outcome_index = 1, train_set_size = "2000", relative_path = "results/aggregated_IATEs/ca/simulation_from_092524/aggregated_IATEs_data.rds")%>% filter(model %in% c("SL S", "SL T"))

#ca outcome 1, train set size 5000 (no SL only)
ca_1_5000_small_noSL = read_and_aggregate(cov_set_size = "small", dataset = "ca", outcome_index = 1, train_set_size = "5000", relative_path = "results/aggregated_IATEs/ca/simulation_from_103024_2108/aggregated_IATEs_data.rds")

ca_1_5000_medium_noSL = read_and_aggregate(cov_set_size = "medium", dataset = "ca", outcome_index = 1, train_set_size = "5000", relative_path = "results/aggregated_IATEs/ca/simulation_from_110124_0123/aggregated_IATEs_data.rds")

ca_1_5000_large_noSL = read_and_aggregate(cov_set_size = "large", dataset = "ca", outcome_index = 1, train_set_size = "5000", relative_path = "results/aggregated_IATEs/ca/simulation_from_110224_0041/aggregated_IATEs_data.rds")

#more simulations
#big job 9










#ca outcome 2, train set size 1000 (only small, no SL)
ca_2_1000_small = read_and_aggregate(cov_set_size = "small", dataset = "ca", outcome_index = 2, train_set_size = "1000", relative_path = "results/aggregated_IATEs/ca/simulation_from_112224_2039/aggregated_IATEs_data.rds")
#this one above was also run and results stored in simulation_from_082824, using the latest run here

ca_2_1000_medium = read_and_aggregate(cov_set_size = "medium", dataset = "ca", outcome_index = 2, train_set_size = "1000", relative_path = "results/aggregated_IATEs/ca/simulation_from_112324_1144/aggregated_IATEs_data.rds")
ca_2_1000_large = read_and_aggregate(cov_set_size = "large", dataset = "ca", outcome_index = 2, train_set_size = "1000", relative_path = "results/aggregated_IATEs/ca/simulation_from_112324_2251/aggregated_IATEs_data.rds")

ca_2_2000_small = read_and_aggregate(cov_set_size = "small", dataset = "ca", outcome_index = 2, train_set_size = "2000", relative_path = "results/aggregated_IATEs/ca/simulation_from_112424_1434/aggregated_IATEs_data.rds")
ca_2_2000_medium = read_and_aggregate(cov_set_size = "medium", dataset = "ca", outcome_index = 2, train_set_size = "2000", relative_path = "results/aggregated_IATEs/ca/simulation_from_120324_1223/aggregated_IATEs_data.rds")
ca_2_2000_large = read_and_aggregate(cov_set_size = "large", dataset = "ca", outcome_index = 2, train_set_size = "2000", relative_path = "results/aggregated_IATEs/ca/simulation_from_120324_2258/aggregated_IATEs_data.rds")

ca_2_5000_small = read_and_aggregate(cov_set_size = "small", dataset = "ca", outcome_index = 2, train_set_size = "5000", relative_path = "results/aggregated_IATEs/ca/simulation_from_120424_1017/aggregated_IATEs_data.rds")
ca_2_5000_medium = read_and_aggregate(cov_set_size = "medium", dataset = "ca", outcome_index = 2, train_set_size = "5000", relative_path = "results/aggregated_IATEs/ca/simulation_from_120424_2318/aggregated_IATEs_data.rds")
ca_2_5000_large = read_and_aggregate(cov_set_size = "large", dataset = "ca", outcome_index = 2, train_set_size = "5000", relative_path = "results/aggregated_IATEs/ca/simulation_from_121024_1112/aggregated_IATEs_data.rds")

ca_1_5000_large_noSL = read_and_aggregate(cov_set_size = "large", dataset = "ca", outcome_index = 1, train_set_size = "5000", relative_path = "results/aggregated_IATEs/ca/simulation_from_110224_0041/aggregated_IATEs_data.rds")

#asap outcome 2, train set size 1000 (no SL)
asap_2_1000_small_noSL = read_and_aggregate(cov_set_size = "small", dataset = "asap", outcome_index = 2, train_set_size = "1000", relative_path = "results/aggregated_IATEs/asap/simulation_from_100224/aggregated_IATEs_data.rds")

asap_2_1000_medium_noSL = read_and_aggregate(cov_set_size = "medium", dataset = "asap", outcome_index = 2, train_set_size = "1000", relative_path = "results/aggregated_IATEs/asap/simulation_from_100824/aggregated_IATEs_data.rds")

asap_2_1000_large_noSL = read_and_aggregate(cov_set_size = "large", dataset = "asap", outcome_index = 2, train_set_size = "1000", relative_path = "results/aggregated_IATEs/asap/simulation_from_101124/aggregated_IATEs_data.rds")

#asap outcome 2, train set size 2000 (no SL)
asap_2_2000_small_noSL = read_and_aggregate(cov_set_size = "small", dataset = "asap", outcome_index = 2, train_set_size = "2000", relative_path = "results/aggregated_IATEs/asap/simulation_from_101024/aggregated_IATEs_data.rds")

asap_2_2000_medium_noSL = read_and_aggregate(cov_set_size = "medium", dataset = "asap", outcome_index = 2, train_set_size = "2000", relative_path = "results/aggregated_IATEs/asap/simulation_from_101124/aggregated_IATEs_data.rds")

asap_2_2000_large_noSL = read_and_aggregate(cov_set_size = "large", dataset = "asap", outcome_index = 2, train_set_size = "2000", relative_path = "results/aggregated_IATEs/asap/simulation_from_101424/aggregated_IATEs_data.rds")

#asap outcome 2, train set size 5000 
asap_2_5000_small_noSL = read_and_aggregate(cov_set_size = "small", dataset = "asap", outcome_index = 2, train_set_size = "5000", relative_path = "results/aggregated_IATEs/asap/simulation_from_110224_2045/aggregated_IATEs_data.rds")

asap_2_5000_medium_noSL = read_and_aggregate(cov_set_size = "medium", dataset = "asap", outcome_index = 2, train_set_size = "5000", relative_path = "results/aggregated_IATEs/asap/simulation_from_110324_1058/aggregated_IATEs_data.rds")

asap_2_5000_large_noSL = read_and_aggregate(cov_set_size = "large", dataset = "asap", outcome_index = 2, train_set_size = "5000", relative_path = "results/aggregated_IATEs/asap/simulation_from_111924_0931/aggregated_IATEs_data.rds")

#asap outcome 1
asap_1_1000_small_noSL = read_and_aggregate(cov_set_size = "small", dataset = "asap", outcome_index = 1, train_set_size = "1000", relative_path = "results/aggregated_IATEs/asap/simulation_from_100224/aggregated_IATEs_data.rds")

asap_1_1000_medium_noSL = read_and_aggregate(cov_set_size = "medium", dataset = "asap", outcome_index =1, train_set_size = "1000", relative_path = "results/aggregated_IATEs/asap/simulation_from_111324_1105/aggregated_IATEs_data.rds")

asap_1_1000_large_noSL = read_and_aggregate(cov_set_size = "large", dataset = "asap", outcome_index = 1, train_set_size = "1000", relative_path = "results/aggregated_IATEs/asap/simulation_from_111324_2039/aggregated_IATEs_data.rds")

asap_1_2000_small_noSL = read_and_aggregate(cov_set_size = "small", dataset = "asap", outcome_index = 1, train_set_size = "2000", relative_path = "results/aggregated_IATEs/asap/simulation_from_111424_0926/aggregated_IATEs_data.rds")

asap_1_2000_medium_noSL = read_and_aggregate(cov_set_size = "medium", dataset = "asap", outcome_index =1, train_set_size = "2000", relative_path = "results/aggregated_IATEs/asap/simulation_from_111424_2352/aggregated_IATEs_data.rds")

asap_1_2000_large_noSL = read_and_aggregate(cov_set_size = "large", dataset = "asap", outcome_index = 1, train_set_size = "2000", relative_path = "results/aggregated_IATEs/asap/simulation_from_111824_1151/aggregated_IATEs_data.rds")

asap_1_5000_small_noSL = read_and_aggregate(cov_set_size = "small", dataset = "asap", outcome_index = 1, train_set_size = "5000", relative_path = "results/aggregated_IATEs/asap/simulation_from_112024_0936/aggregated_IATEs_data.rds")

asap_1_5000_medium_noSL = read_and_aggregate(cov_set_size = "medium", dataset = "asap", outcome_index =1, train_set_size = "5000", relative_path = "results/aggregated_IATEs/asap/simulation_from_112124_1207/aggregated_IATEs_data.rds")

asap_1_5000_large_noSL = read_and_aggregate(cov_set_size = "large", dataset = "asap", outcome_index = 1, train_set_size = "5000", relative_path = "results/aggregated_IATEs/asap/simulation_from_112224_0056/aggregated_IATEs_data.rds")


combined_all = rbind(ca_1_1000_small_noSL, ca_1_1000_small_SL,
                     ca_1_1000_medium_noSL, ca_1_1000_medium_SL, 
                     ca_1_1000_large_noSL, ca_1_1000_large_SL,
                     ca_1_2000_small_noSL, ca_1_2000_small_SL,
                     ca_1_2000_medium_noSL, ca_1_2000_medium_SL,
                     ca_1_2000_large_noSL, ca_1_2000_large_SL,
                     ca_1_5000_small_noSL,
                     ca_1_5000_medium_noSL,
                     ca_1_5000_large_noSL,
                     ca_2_1000_small, 
                     ca_2_1000_medium, 
                     ca_2_1000_large, 
                     ca_2_2000_small,
                     ca_2_2000_medium,
                     ca_2_2000_large, 
                     ca_2_5000_small,
                     ca_2_5000_medium,
                     ca_2_5000_large, 
                     asap_2_1000_small_noSL, 
                     asap_2_1000_medium_noSL, 
                     asap_2_1000_large_noSL, 
                     asap_2_2000_small_noSL,
                     asap_2_2000_medium_noSL,
                     asap_2_2000_large_noSL, 
                     asap_2_5000_small_noSL,
                     asap_2_5000_medium_noSL,
                     asap_2_5000_large_noSL,
                     asap_1_5000_large_noSL,
                     #asap_1_1000_small_noSL, 
                     asap_1_1000_medium_noSL, 
                     asap_1_1000_large_noSL, 
                     asap_1_2000_small_noSL,
                     asap_1_2000_medium_noSL,
                     asap_1_2000_large_noSL, 
                     asap_1_5000_small_noSL,
                     asap_1_5000_medium_noSL,
                     asap_1_5000_large_noSL) %>%
  
  #specify order for cov_set_size
  mutate(cov_set_size = factor(cov_set_size, levels = c("small", "medium", "large")))

write.xlsx(combined_all, file = here::here("graphs/combined/combined_all.xlsx"))
