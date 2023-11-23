# SOURCE SCRIPTS 

source("code/functions.R")

# LOAD PACKAGES

library(tidyverse)
library(survival)

# DATA PREPARATION FOR ALL SURVEYS, i.e., constructing a categorical occupational PA variable

# HUNT 1

hunt_1_cleaned_data_occu_strat <- hunt_1_cleaned_data |>
  rename(heavy_physical_work = wor_heav_nt1blq2) |> 
  mutate(occupational_pa = case_when(
    heavy_physical_work %in% c("Aldri, eller nesten aldri", "Ganske sjelden") ~ "low",
    heavy_physical_work %in% c("Ja, nesten alltid", "Ganske ofte") ~ "high")
  )

# HUNT 2

hunt_2_cleaned_data_occu_strat <- hunt_2_cleaned_data |>
  rename(heavy_physical_work = wor_heav_nt2blq2) |> 
  mutate(occupational_pa = case_when(
    heavy_physical_work %in% c("Aldri, eller nesten aldri", "Ganske sjelden") ~ "low",
    heavy_physical_work %in% c("Ja, nesten alltid", "Ganske ofte") ~ "high")
  )

# HUNT 3

hunt_3_cleaned_data_occu_strat <- hunt_3_cleaned_data |>
  rename(heavy_physical_work = wor_heav_nt3blq2) |> 
  mutate(occupational_pa = case_when(
    heavy_physical_work %in% c("Aldri, eller nesten aldri", "Ganske sjelden") ~ "low",
    heavy_physical_work %in% c("Ja, nesten alltid", "Ganske ofte") ~ "high")
  )

# HUNT 4

hunt_4_cleaned_data_occu_strat <- hunt_4_cleaned_data |>
  rename(heavy_physical_work = wor_heav_nt4blq2) |> 
  mutate(occupational_pa = case_when(
    heavy_physical_work %in% c("Aldri, eller nesten aldri", "Ganske sjelden") ~ "low",
    heavy_physical_work %in% c("Ja, nesten alltid", "Ganske ofte") ~ "high")
  )

# SURVIVAL ANALYSIS

# HUNT 1

hunt_1_cox_reg_multi_occu_strat <- run_cox_reg_multi(hunt_1_cleaned_data_occu_strat, occupational_pa)

hunt_1_cox_reg_crude_occu_strat <- run_cox_reg_crude(hunt_1_cleaned_data_occu_strat, occupational_pa)

# HUNT 2

hunt_2_cox_reg_multi_occu_strat <- run_cox_reg_multi(hunt_2_cleaned_data_occu_strat, occupational_pa)

hunt_2_cox_reg_crude_occu_strat <- run_cox_reg_crude(hunt_2_cleaned_data_occu_strat, occupational_pa)

# HUNT 3

hunt_3_cox_reg_multi_occu_strat <- run_cox_reg_multi(hunt_3_cleaned_data_occu_strat, occupational_pa)

hunt_3_cox_reg_crude_occu_strat <- run_cox_reg_crude(hunt_3_cleaned_data_occu_strat, occupational_pa)

# HUNT 4

hunt_4_cox_reg_multi_occu_strat <- run_cox_reg_multi(hunt_4_cleaned_data_occu_strat, occupational_pa)

hunt_4_cox_reg_crude_occu_strat <- run_cox_reg_crude(hunt_4_cleaned_data_occu_strat, occupational_pa)

# SUMMARY STATS FROM MODELS

# Multi-adjusted model

occupational_pa <- c("low", "high")

for (class in occupational_pa) {
  calculate_num_of_participants_strat(
    dataframes = c("hunt_1_cleaned_data_occu_strat", "hunt_2_cleaned_data_occu_strat", "hunt_3_cleaned_data_occu_strat", "hunt_4_cleaned_data_occu_strat"), 
    covariates = c("age", "pa_hrs_per_week", "follow_up_time_in_years", "death_all_cause", "bp_diastolic", "bp_systolic",
                   "bmi", "packs_of_smoke_per_year", "sex", "alcohol_usage", "heart_infarction", "occupational_pa"),
    strata = class
  )
}

for (class in occupational_pa) {
  calculate_person_years_follow_up_strat(
    dataframes =c("hunt_1_cleaned_data_occu_strat", "hunt_2_cleaned_data_occu_strat", "hunt_3_cleaned_data_occu_strat", "hunt_4_cleaned_data_occu_strat"),
    covariates = c("age", "pa_hrs_per_week", "follow_up_time_in_years", "death_all_cause", "bp_diastolic", "bp_systolic",
                   "bmi", "packs_of_smoke_per_year", "sex", "alcohol_usage", "heart_infarction", "occupational_pa"),
    strata = class
  )
}

# Crude model

for (class in occupational_pa) {
  calculate_num_of_participants_strat(
    dataframes = c("hunt_1_cleaned_data_occu_strat", "hunt_2_cleaned_data_occu_strat", "hunt_3_cleaned_data_occu_strat", "hunt_4_cleaned_data_occu_strat"), 
    covariates = c("pa_hrs_per_week", "age", "follow_up_time_in_years", "death_all_cause", "occupational_pa"),
    strata = class
  )
}

for (class in occupational_pa) {
  calculate_person_years_follow_up_strat(
    dataframes =c("hunt_1_cleaned_data_occu_strat", "hunt_2_cleaned_data_occu_strat", "hunt_3_cleaned_data_occu_strat", "hunt_4_cleaned_data_occu_strat"),
    covariates = c("pa_hrs_per_week", "age", "follow_up_time_in_years", "death_all_cause", "occupational_pa"),
    strata = class
  )
}