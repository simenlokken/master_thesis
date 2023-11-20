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

# HUNT 1 FOLLOW-UP 

# Follow-up time (person-years) for multi-adjusted and crude model

hunt_1_follow_up_time_multi <- calculate_follow_up_time(dataframe = hunt_1_cleaned_data, 
                                                        covariates = c("age", "pa_hrs_per_week", "follow_up_time_in_years", 
                                                                       "death_all_cause", "bp_diastolic", "bp_systolic",
                                                                       "bmi", "packs_of_smoke_per_year", "sex"), 
                                                        end_date_death = end_date_death, 
                                                        participation_date = participation_date
)

hunt_1_follow_up_time_crude <- calculate_follow_up_time(dataframe = hunt_1_cleaned_data, 
                                                        covariates = c("follow_up_time_in_years", "death_all_cause", 
                                                                       "age", "pa_hrs_per_week"), 
                                                        end_date_death = end_date_death, 
                                                        participation_date = participation_date
)

# HUNT 2 SURVIVAL ANALYSIS

# Create data frame

hunt_2_cleaned_data <- full_cleaned_data |> 
  process_hunt_2_no_change()

# Multi-adjusted model, adjusted for BP, BMI, smoking (cont), age and sex

hunt_2_cox_reg_multi <- run_cox_reg_crude(hunt_2_cleaned_data, strata = NULL)

# Crude model

hunt_2_cox_reg_crude <- run_cox_reg_crude(hunt_2_cleaned_data, strata = NULL)

# FOLLOW-UP

# Follow-up time (person-years) for multi-adjusted and crude model

hunt_2_follow_up_time_multi <- calculate_follow_up_time(dataframe = hunt_2_cleaned_data, 
                                                        covariates = c("age", "pa_hrs_per_week", "follow_up_time_in_years", 
                                                                       "death_all_cause", "bp_diastolic", "bp_systolic",
                                                                       "bmi", "packs_of_smoke_per_year", "sex"), 
                                                        end_date_death = end_date_death, 
                                                        participation_date = participation_date
)

hunt_2_follow_up_time_crude <- calculate_follow_up_time(dataframe = hunt_2_cleaned_data, 
                                                        covariates = c("follow_up_time_in_years", "death_all_cause", 
                                                                       "age", "pa_hrs_per_week"), 
                                                        end_date_death = end_date_death, 
                                                        participation_date = participation_date
)

# HUNT 3 SURVIVAL ANALYSIS

# Create data frame

hunt_3_cleaned_data <- full_cleaned_data |>
  process_hunt_3_no_change()

# Multi-adjusted Cox model, BP, BMI, smoking, age and sex

hunt_3_cox_reg_multi <- run_cox_reg_multi(hunt_3_cleaned_data, strata = NULL)

# Crude model, adjusted for age

hunt_3_cox_reg_crude <- run_cox_reg_crude(hunt_3_cleaned_data, strata = NULL)

# FOLLOW-UP

# Follow-up time (person-years) for multi-adjusted and crude model

hunt_3_follow_up_time_multi <- calculate_follow_up_time(dataframe = hunt_3_cleaned_data, 
                                                        covariates = c("age", "pa_hrs_per_week", "follow_up_time_in_years", 
                                                                       "death_all_cause", "bp_diastolic", "bp_systolic",
                                                                       "bmi", "packs_of_smoke_per_year", "sex"), 
                                                        end_date_death = end_date_death, 
                                                        participation_date = participation_date
)

hunt_3_follow_up_time_crude <- calculate_follow_up_time(dataframe = hunt_3_cleaned_data, 
                                                        covariates = c("follow_up_time_in_years", "death_all_cause", 
                                                                       "age", "pa_hrs_per_week"), 
                                                        end_date_death = end_date_death, 
                                                        participation_date = participation_date
)

# HUNT 4 SURVIVAL ANALYSIS

# Create data frame

hunt_4_cleaned_data <- full_cleaned_data |> 
  process_hunt_4_no_change()

# Multi-adjusted Cox model, BP, BMI, smoking, age and sex

hunt_4_cox_reg_multi <- run_cox_reg_multi(hunt_4_cleaned_data, strata = NULL)
                              
# Crude model, adjusted for age

hunt_4_cox_reg_crude <- run_cox_reg_crude(hunt_4_cleaned_data, strata = NULL)

# FOLLOW-UP

# Follow-up time (person-years) for multi-adjusted and crude model

hunt_4_follow_up_time_multi <- calculate_follow_up_time(dataframe = hunt_4_cleaned_data, 
                                                        covariates = c("age", "pa_hrs_per_week", "follow_up_time_in_years", 
                                                                       "death_all_cause", "bp_diastolic", "bp_systolic",
                                                                       "bmi", "packs_of_smoke_per_year", "sex"), 
                                                        end_date_death = end_date_death, 
                                                        participation_date = participation_date
)

hunt_4_follow_up_time_crude <- calculate_follow_up_time(dataframe = hunt_4_cleaned_data, 
                                                        covariates = c("follow_up_time_in_years", "death_all_cause", 
                                                                       "age", "pa_hrs_per_week"), 
                                                        end_date_death = end_date_death, 
                                                        participation_date = participation_date
)

# SUMMARY STATS FROM MODELS

# Multi-adjusted model

sum_stats_cox_reg_multi_no_change <- tibble(
  survey = c("HUNT 1", "HUNT 2", "HUNT 3", "HUNT 4"),
  number_of_participants = c(14903,27282, 16964, 19069),
  num_of_deaths = c(6835,7776, 2013, 319),
  person_years_follow_up = c(426751.8, 584618.3, 214476.6, 51191.76),
  incidence_rate = (num_of_deaths) / (person_years_follow_up) * 1000,
)

# Crude model

sum_stats_cox_reg_crude_no_change <- tibble(
  survey = c("HUNT 1", "HUNT 2", "HUNT 3", "HUNT 4"),
  number_of_participants = c(35080, 54397, 38456, 44627),
  num_of_deaths = c(17984, 15362, 4705, 604),
  person_years_follow_up = c(953768.7, 1161724.2, 486275.2, 119997.5),
  incidence_rate = (num_of_deaths) / (person_years_follow_up) * 1000,
)
