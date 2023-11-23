# SOURCE

source("code/functions.R")

# LOAD PACKAGES

library(tidyverse)
library(survival)

# HUNT 1 SURVIVAL ANALYSIS

# Create data frame

hunt_1_cleaned_data <- full_cleaned_data |> 
  process_hunt_1_no_change()
  
# Multi-adjusted Cox model, adjusted for BP, BMI, smoking (cont), age and sex

hunt_1_cox_reg_multi <- run_cox_reg_multi(hunt_1_cleaned_data, strata = NULL)

# Crude Cox model, adjusted for age

hunt_1_cox_reg_crude <- run_cox_reg_crude(hunt_1_cleaned_data, strata = NULL)

# HUNT 2 SURVIVAL ANALYSIS

# Create data frame

hunt_2_cleaned_data <- full_cleaned_data |> 
  process_hunt_2_no_change()

# Multi-adjusted model, adjusted for BP, BMI, smoking (cont), age and sex

hunt_2_cox_reg_multi <- run_cox_reg_multi(hunt_2_cleaned_data, strata = NULL)

# Crude model

hunt_2_cox_reg_crude <- run_cox_reg_crude(hunt_2_cleaned_data, strata = NULL)

# HUNT 3 SURVIVAL ANALYSIS

# Create data frame

hunt_3_cleaned_data <- full_cleaned_data |>
  process_hunt_3_no_change()

# Multi-adjusted Cox model, BP, BMI, smoking, age and sex

hunt_3_cox_reg_multi <- run_cox_reg_multi(hunt_3_cleaned_data, strata = NULL)

# Crude model, adjusted for age

hunt_3_cox_reg_crude <- run_cox_reg_crude(hunt_3_cleaned_data, strata = NULL)

# HUNT 4 SURVIVAL ANALYSIS

# Create data frame

hunt_4_cleaned_data <- full_cleaned_data |> 
  process_hunt_4_no_change()

# Multi-adjusted Cox model, BP, BMI, smoking, age and sex

hunt_4_cox_reg_multi <- run_cox_reg_multi(hunt_4_cleaned_data, strata = NULL)
                              
# Crude model, adjusted for age

hunt_4_cox_reg_crude <- run_cox_reg_crude(hunt_4_cleaned_data, strata = NULL)

# SUMMARY STATS FROM MODELS

# Functions (names are descriptive)

calculate_num_of_participants <- function(dataframes, covariates) {
  for (dataframe in dataframes) {
    print(
      get(dataframe) %>% 
        select(all_of(covariates)) %>% 
        drop_na() %>% 
        count()
    )
  }
}

calculate_num_of_deaths <- function(dataframes, covariates) {
  for (dataframe in dataframes) {
    print(
      get(dataframe) %>% 
        select(all_of(covariates)) %>% 
        drop_na() %>% 
        filter(death_all_cause == 2) |> # Death is coded as 2
        count()
    )
  }
}

calculate_person_years_follow_up <- function(dataframes, covariates) {
  for (dataframe in dataframes) {
    print(
      get(dataframe) |> 
        select(all_of(covariates)) %>% 
        drop_na() %>% 
        summarise(person_years = sum(follow_up_time_in_years))
    )
  }
}

# Multi-adjusted 

calculate_num_of_participants(
  dataframes = c("hunt_1_cleaned_data", "hunt_2_cleaned_data", "hunt_3_cleaned_data", "hunt_4_cleaned_data"), 
  covariates = c("age", "pa_hrs_per_week", "follow_up_time_in_years", "death_all_cause", "bp_diastolic", "bp_systolic",
                 "bmi", "packs_of_smoke_per_year", "sex", "alcohol_usage", "heart_infarction")
)

calculate_num_of_deaths(
  dataframes = c("hunt_1_cleaned_data", "hunt_2_cleaned_data", "hunt_3_cleaned_data", "hunt_4_cleaned_data"),
  covariates = c("age", "pa_hrs_per_week", "follow_up_time_in_years", "death_all_cause", "bp_diastolic", "bp_systolic",
                 "bmi", "packs_of_smoke_per_year", "sex", "alcohol_usage", "heart_infarction")
)

calculate_person_years_follow_up(
  dataframes = c("hunt_1_cleaned_data", "hunt_2_cleaned_data", "hunt_3_cleaned_data", "hunt_4_cleaned_data"), 
  covariates = c("age", "pa_hrs_per_week", "follow_up_time_in_years", "death_all_cause", "bp_diastolic", "bp_systolic",
                 "bmi", "packs_of_smoke_per_year", "sex", "alcohol_usage", "heart_infarction")
)

# Crude

calculate_num_of_participants(
  dataframes = c("hunt_1_cleaned_data", "hunt_2_cleaned_data", "hunt_3_cleaned_data", "hunt_4_cleaned_data"),
  covariates = c("follow_up_time_in_years", "death_all_cause", "age", "pa_hrs_per_week")
)

calculate_num_of_deaths(
  dataframes = c("hunt_1_cleaned_data", "hunt_2_cleaned_data", "hunt_3_cleaned_data", "hunt_4_cleaned_data"),
  covariates = c("follow_up_time_in_years", "death_all_cause", "age", "pa_hrs_per_week")
)

calculate_person_years_follow_up(
  dataframes = c("hunt_1_cleaned_data", "hunt_2_cleaned_data", "hunt_3_cleaned_data", "hunt_4_cleaned_data"), 
  covariates = c("follow_up_time_in_years", "death_all_cause", "age", "pa_hrs_per_week")
)

# Multi-adjusted model

tibble(
  survey = c("HUNT 1", "HUNT 2", "HUNT 3", "HUNT 4"),
  number_of_participants = c(33926,50931, 36118, 38430),
  num_of_deaths = c(17011, 13992, 4024, 405),
  person_years_follow_up = c(931806, 1093581, 458556, 102768),
  incidence_rate = (num_of_deaths) / (person_years_follow_up) * 1000,
  multi_hr = c(1.040, 0.946, 0.948, 0.881),
  ci_low = c(1.030, 0.933, 0.928, 0.822),
  ci_high = c(1.050, 0.958, 0.968, 0.945)
)

# Crude model

tibble(
  survey = c("HUNT 1", "HUNT 2", "HUNT 3", "HUNT 4"),
  number_of_participants = c(35080, 54397, 38456, 44627),
  num_of_deaths = c(17984, 15362, 4705, 604),
  person_years_follow_up = c(953010, 1160780, 485785, 119545),
  incidence_rate = (num_of_deaths) / (person_years_follow_up) * 1000,
  crude_hr = c(1.070, 0.920, 0.951, 0.846),
  ci_low = c(1.060, 0.909, 0.932, 0.797),
  ci_high = c(1.080, 0.932, 0.970, 0.898)
)