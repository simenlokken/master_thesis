# SOURCE SCRIPTS 

source("Thesis code/multiadjusted_and_crude_hr_all_surveys_no_change_in_pa.R")
source("Thesis code/multiadjusted_and_crude_hr_all_surveys_no_change_in_pa_socioeconomic_stratified.R")

# DATA PREPARATION FOR ALL SURVEYS, i.e., constructing a categorical occupational PA variable

# HUNT 1

hunt_1_cleaned_data <- hunt_1_cleaned_data |>
  rename(heavy_physical_work = wor_heav_nt1blq2) |> 
  mutate(occupational_pa = case_when(
    heavy_physical_work %in% c("Aldri, eller nesten aldri", "Ganske sjelden") ~ "low",
    heavy_physical_work %in% c("Ja, nesten alltid", "Ganske ofte") ~ "high")
  )

# HUNT 2

hunt_2_cleaned_data <- hunt_2_cleaned_data |>
  rename(heavy_physical_work = wor_heav_nt2blq2) |> 
  mutate(occupational_pa = case_when(
    heavy_physical_work %in% c("Aldri, eller nesten aldri", "Ganske sjelden") ~ "low",
    heavy_physical_work %in% c("Ja, nesten alltid", "Ganske ofte") ~ "high")
  )

# HUNT 3

hunt_3_cleaned_data <- hunt_3_cleaned_data |>
  rename(heavy_physical_work = wor_heav_nt3blq2) |> 
  mutate(occupational_pa = case_when(
    heavy_physical_work %in% c("Aldri, eller nesten aldri", "Ganske sjelden") ~ "low",
    heavy_physical_work %in% c("Ja, nesten alltid", "Ganske ofte") ~ "high")
  )

# HUNT 4

hunt_4_cleaned_data <- hunt_4_cleaned_data |>
  rename(heavy_physical_work = wor_heav_nt4blq2) |> 
  mutate(occupational_pa = case_when(
    heavy_physical_work %in% c("Aldri, eller nesten aldri", "Ganske sjelden") ~ "low",
    heavy_physical_work %in% c("Ja, nesten alltid", "Ganske ofte") ~ "high")
  )

# SURVIVAL ANALYSIS

# Using Cox Regression functions created from previous analyses

# HUNT 1

hunt_1_cox_reg_multi_occu_strat <- run_cox_reg_multi(hunt_1_cleaned_data, occupational_pa)

hunt_1_cox_reg_crude_occu_strat <- run_cox_reg_crude(hunt_1_cleaned_data, occupational_pa)

# HUNT 2

hunt_2_cox_reg_multi_occu_strat <- run_cox_reg_multi(hunt_2_cleaned_data, occupational_pa)

hunt_2_cox_reg_crude_occu_strat <- run_cox_reg_crude(hunt_2_cleaned_data, occupational_pa)

# HUNT 3

hunt_3_cox_reg_multi_occu_strat <- run_cox_reg_multi(hunt_3_cleaned_data, occupational_pa)

hunt_3_cox_reg_crude_occu_strat <- run_cox_reg_crude(hunt_3_cleaned_data, occupational_pa)

# HUNT 4

hunt_4_cox_reg_multi_occu_strat <- run_cox_reg_multi(hunt_4_cleaned_data, occupational_pa)

hunt_4_cox_reg_crude_occu_strat <- run_cox_reg_crude(hunt_4_cleaned_data, occupational_pa)

# FOLLOW-UP 

# HUNT 1, multi-adjusted and crude

hunt_1_f_up_time_multi_occu_strat <- calculate_follow_up_time_multi(hunt_1_cleaned_data, strata = occupational_pa)

hunt_1_f_up_time_crude_occu_strat <- calculate_follow_up_time_crude(hunt_1_cleaned_data, strata = occupational_pa)

# HUNT 2, multi-adjusted and crude

hunt_2_f_up_time_multi_occu_strat <- calculate_follow_up_time_multi(hunt_2_cleaned_data, strata = occupational_pa)

hunt_2_f_up_time_crude_occu_strat <- calculate_follow_up_time_crude(hunt_2_cleaned_data, strata = occupational_pa)

# HUNT 3, multi-adjusted and crude

hunt_3_f_up_time_multi_occu_strat <- calculate_follow_up_time_multi(hunt_3_cleaned_data, strata = occupational_pa)

hunt_3_f_up_time_crude_occu_strat <- calculate_follow_up_time_crude(hunt_3_cleaned_data, strata = occupational_pa)

# HUNT 4, multi-adjusted and crude

hunt_4_f_up_time_multi_occu_strat <- calculate_follow_up_time_multi(hunt_4_cleaned_data, strata = occupational_pa)

hunt_4_f_up_time_crude_occu_strat <- calculate_follow_up_time_crude(hunt_4_cleaned_data, strata = occupational_pa)