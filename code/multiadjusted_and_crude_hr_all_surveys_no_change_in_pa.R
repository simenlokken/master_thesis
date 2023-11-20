# SOURCE

source("code/functions.R")

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
                                age + sex, data = hunt_1_cleaned_data
) |> 
  broom::tidy(exponentiate = TRUE)

# Crude Cox model, adjusted for age

hunt_1_cox_reg_crude <- coxph(Surv(follow_up_time_in_years_h1, death_all_cause) ~ pa_hrs_per_week_h1 + age,
                              data = hunt_1_cleaned_data
) |> 
  broom::tidy(exponentiate = TRUE)

# HUNT 1 FOLLOW-UP 

# Follow-up time (person-years) for multi-adjusted and crude model

hunt_1_follow_up_time_multi <- calculate_follow_up_time(dataframe = hunt_1_cleaned_data, 
                                                        covariates = c("age", "pa_hrs_per_week_h1", "follow_up_time_in_years_h1", 
                                                                       "death_all_cause", "bp_diastolic_h1", "bp_systolic_h1",
                                                                       "bmi_h1", "packs_of_smoke_per_year_h1", "sex"), 
                                                        end_date_death = end_date_death, 
                                                        participation_date = participation_date_h1
)

hunt_1_follow_up_time_crude <- calculate_follow_up_time(dataframe = hunt_1_cleaned_data, 
                                                        covariates = c("follow_up_time_in_years_h1", "death_all_cause", 
                                                                       "age", "pa_hrs_per_week_h1"), 
                                                        end_date_death = end_date_death, 
                                                        participation_date = participation_date_h1
)

# HUNT 2 SURVIVAL ANALYSIS

# Create data frame

hunt_2_cleaned_data <- full_cleaned_data |> 
  process_hunt_2()

# Multi-adjusted model, adjusted for BP, BMI, smoking (cont), age and sex

hunt_2_cox_reg_multi <- coxph(Surv(follow_up_time_in_years_h2, death_all_cause) ~ pa_hrs_per_week_h2 +
                                bp_systolic_h2 + bp_diastolic_h2 + bmi_h2 +
                                packs_of_smoke_per_year_h2 + sex + age, data = hunt_2_cleaned_data
) |> 
  broom::tidy(exponentiate = TRUE)

# Crude model

hunt_2_cox_reg_crude <- coxph(Surv(follow_up_time_in_years_h2, death_all_cause) ~ pa_hrs_per_week_h2 + age,
                        data = hunt_2_cleaned_data
) |> 
  broom::tidy(exponentiate = TRUE)

# FOLLOW-UP

# Follow-up time (person-years) for multi-adjusted and crude model

hunt_2_follow_up_time_multi <- calculate_follow_up_time(dataframe = hunt_2_cleaned_data, 
                                                        covariates = c("age", "pa_hrs_per_week_h2", "follow_up_time_in_years_h2", 
                                                                       "death_all_cause", "bp_diastolic_h2", "bp_systolic_h2",
                                                                       "bmi_h2", "packs_of_smoke_per_year_h2", "sex"), 
                                                        end_date_death = end_date_death, 
                                                        participation_date = participation_date_h2
)

hunt_2_follow_up_time_crude <- calculate_follow_up_time(dataframe = hunt_2_cleaned_data, 
                                                        covariates = c("follow_up_time_in_years_h2", "death_all_cause", 
                                                                       "age", "pa_hrs_per_week_h2"), 
                                                        end_date_death = end_date_death, 
                                                        participation_date = participation_date_h2
)

# HUNT 3 SURVIVAL ANALYSIS

# Create data frame

hunt_3_cleaned_data <- full_cleaned_data |>
  process_hunt_3()

# Multi-adjusted Cox model, BP, BMI, smoking, age and sex

hunt_3_cox_reg_multi <- coxph(Surv(follow_up_time_in_years_h3, death_all_cause) ~ pa_hrs_per_week_h3 +
                                bp_diastolic_h3 + bp_systolic_h3 + bmi_h3 +
                                packs_of_smoke_per_year_h3 + age + sex, data = hunt_3_cleaned_data
) |> 
  broom::tidy(exponentiate = TRUE)

# Crude model, adjusted for age

hunt_3_cox_reg_crude <- coxph(Surv(follow_up_time_in_years_h3, death_all_cause) ~ pa_hrs_per_week_h3 + age,
                              data = hunt_3_cleaned_data
) |> 
  broom::tidy(exponentiate = TRUE)

# FOLLOW-UP

# Follow-up time (person-years) for multi-adjusted and crude model

hunt_3_follow_up_time_multi <- calculate_follow_up_time(dataframe = hunt_3_cleaned_data, 
                                                        covariates = c("age", "pa_hrs_per_week_h3", "follow_up_time_in_years_h3", 
                                                                       "death_all_cause", "bp_diastolic_h3", "bp_systolic_h3",
                                                                       "bmi_h3", "packs_of_smoke_per_year_h3", "sex"), 
                                                        end_date_death = end_date_death, 
                                                        participation_date = participation_date_h3
)

hunt_3_follow_up_time_crude <- calculate_follow_up_time(dataframe = hunt_3_cleaned_data, 
                                                        covariates = c("follow_up_time_in_years_h3", "death_all_cause", 
                                                                       "age", "pa_hrs_per_week_h3"), 
                                                        end_date_death = end_date_death, 
                                                        participation_date = participation_date_h3
)

# HUNT 4 SURVIVAL ANALYSIS

# Create data frame

hunt_4_cleaned_data <- full_cleaned_data |> 
  process_hunt_4()

# Cox Regression

hunt_4_cox_reg_multi <- coxph(Surv(follow_up_time_in_years_h4, death_all_cause) ~ pa_hrs_per_week_h4 + 
                                bp_diastolic_h4 + bp_systolic_h4 + bmi_h4 + packs_of_smoke_per_year_h4 + age + sex, 
                              data = hunt_4_cleaned_data
                              
) |> 
  broom::tidy(exponentiate = TRUE)

hunt_4_cox_reg_crude <- coxph(Surv(follow_up_time_in_years_h4, death_all_cause) ~ pa_hrs_per_week_h4 + age,
                              data = hunt_4_cleaned_data
) |> 
  broom::tidy(exponentiate = TRUE)

# FOLLOW-UP

# Follow-up time (person-years) for multi-adjusted and crude model

hunt_4_follow_up_time_multi <- calculate_follow_up_time(dataframe = hunt_4_cleaned_data, 
                                                        covariates = c("age", "pa_hrs_per_week_h4", "follow_up_time_in_years_h4", 
                                                                       "death_all_cause", "bp_diastolic_h4", "bp_systolic_h4",
                                                                       "bmi_h4", "packs_of_smoke_per_year_h4", "sex"), 
                                                        end_date_death = end_date_death, 
                                                        participation_date = participation_date_h4
)

hunt_4_follow_up_time_crude <- calculate_follow_up_time(dataframe = hunt_4_cleaned_data, 
                                                        covariates = c("follow_up_time_in_years_h4", "death_all_cause", 
                                                                       "age", "pa_hrs_per_week_h4"), 
                                                        end_date_death = end_date_death, 
                                                        participation_date = participation_date_h4
)

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