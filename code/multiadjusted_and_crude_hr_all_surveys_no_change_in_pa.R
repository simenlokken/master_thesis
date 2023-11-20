# SOURCE

source("code/pre_analysis_data_preparation.R") # Cleaning script
source("code/functions.R") # A set of my own functions

# LOAD PACKAGES

library(tidyverse)
library(survival)

# HUNT 1 SURVIVAL ANALYSIS

# Create data frame

hunt_1_cleaned_data <- full_cleaned_data |> 
  process_hunt_1()

# Multi-adjusted Cox model, adjusted for BP, BMI, smoking (cont), age and sex

hunt_1_cox_reg_multi <- coxph(Surv(follow_up_time_in_years_h1, death_all_cause) ~ pa_hrs_per_week_h1 +
                                bp_diastolic_h1 + bp_systolic_h1 + bmi_h1 + packs_of_smoke_per_year_h1 +
                                age + sex, data = hunt_1_cleaned_data)

# Crude Cox model, adjusted for age

hunt_1_cox_reg_crude <- coxph(Surv(follow_up_time_in_years_h1, death_all_cause) ~ pa_hrs_per_week_h1 + age,
                              data = hunt_1_cleaned_data)

# HUNT 1 FOLLOW-UP 

# Follow-up time (person-years) for multi-adjusted and crude model

hunt_1_f_up_time_multi <- calculate_follow_up_time(hunt_1_cleaned_data, covariates = c(age, pa_hrs_per_week_h1),
                                                   end_date_death = end_date_death, 
                                                   participation_date = participation_date_h1)

hunt_1_cox_reg_crude_covariates <- c(age, sex, pa_hrs_per_week)
  
hunt_1_f_up_time_crude <- calculate_follow_up_time(hunt_1_cleaned_data, c(age, pa_hrs_per_week_h1),
                                                   end_date_death = end_date_death, 
                                                   participation_date = participation_date_h1)

# HUNT 2 SURVIVAL ANALYSIS

# Create data frame

hunt_2_cleaned_data <- full_cleaned_data |> 
  process_hunt_2()

# Multi-adjusted model, adjusted for BP, BMI, smoking (cont), age and sex

hunt_2_cox_reg_multi <- coxph(Surv(follow_up_time_in_years, death_all_cause) ~ pa_minutes_per_week +
                                bp_systolic + bp_diastolic + bmi +
                                packs_of_smoke_per_year + sex + age, data = hunt_2_cleaned_data)

# Crude model

hunt_2_cox_reg_crude <- coxph(Surv(follow_up_time_in_years, death_all_cause) ~ pa_minutes_per_week + age,
                        data = hunt_2_cleaned_data)

# FOLLOW-UP

# Follow-up time (person-years) for multi-adjusted and crude model

hunt_2_f_up_time_multi <- calculate_follow_up_time_multi(hunt_2_cleaned_data)

hunt_2_f_up_time_crude <- calculate_follow_up_time_crude(hunt_2_cleaned_data)

# HUNT 3 SURVIVAL ANALYSIS

# Create data frame

full_cleaned_data |>
  process_hunt_3()

# Multi-adjusted Cox model, BP, BMI, smoking, age and sex

hunt_3_cox_reg_multi <- coxph(Surv(follow_up_time_in_years, death_all_cause) ~ pa_minutes_per_week +
                                bp_diastolic + bp_systolic + bmi +
                                packs_of_smoke_per_year + age + sex, data = hunt_3_cleaned_data)

# Crude model, adjusted for age

hunt_3_cox_reg_crude <- coxph(Surv(follow_up_time_in_years, death_all_cause) ~ pa_minutes_per_week + age,
                              data = hunt_3_cleaned_data)

# FOLLOW-UP

# Follow-up time (person-years) for multi-adjusted and crude model

hunt_3_f_up_time_multi <- calculate_follow_up_time_multi(hunt_3_cleaned_data)

hunt_3_f_up_time_crude <- calculate_follow_up_time_crude(hunt_3_cleaned_data)

# HUNT 4 SURVIVAL ANALYSIS

# Create data frame

full_cleaned_data |> 
  process_hunt_4()

# Cox Regression

hunt_4_cox_reg_multi <- coxph(Surv(follow_up_time_in_years, death_all_cause) ~ pa_minutes_per_week + 
                                bp_diastolic + bp_systolic + bmi + packs_of_smoke_per_year + age + sex, 
                              data = hunt_4_cleaned_data)

hunt_4_cox_reg_crude <- coxph(Surv(follow_up_time_in_years, death_all_cause) ~ pa_minutes_per_week + age,
                              data = hunt_4_cleaned_data)

# FOLLOW-UP

# Follow-up time (person-years) for multi-adjusted and crude model

hunt_4_f_up_time_multi <- calculate_follow_up_time_multi(hunt_4_cleaned_data)

hunt_4_f_up_time_crude <- calculate_follow_up_time_crude(hunt_4_cleaned_data)

# SUMMARY STATS FROM MODELS

# Multi-adjusted model

sum_stats_cox_reg_multi <- tibble(
  survey = c("HUNT 1", "HUNT 2", "HUNT 3", "HUNT 4"),
  number_of_participants = c(14903,27282, 16964, 19069),
  num_of_deaths = c(6835,7776, 2013, 319),
  person_years_follow_up = c(426751.8, 584618.3, 214476.6, 51191.76),
  incidence_rate = (num_of_deaths) / (person_years_follow_up) * 1000,
)

# Crude model

sum_stats_cox_reg_crude <- tibble(
  survey = c("HUNT 1", "HUNT 2", "HUNT 3", "HUNT 4"),
  number_of_participants = c(35080, 54397, 38456, 44627),
  num_of_deaths = c(17984, 15362, 4705, 604),
  person_years_follow_up = c(953768.7, 1161724.2, 486275.2, 119997.5),
  incidence_rate = (num_of_deaths) / (person_years_follow_up) * 1000,
)