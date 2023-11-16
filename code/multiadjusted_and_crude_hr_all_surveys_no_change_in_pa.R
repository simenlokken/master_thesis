# SOURCE

source("pre_analysis_data_preparation.R")

# LOAD PACKAGES

library(tidyverse)
library(survival)

# HUNT 1 DATA PREPARATION

hunt_1_cleaned_data <- full_cleaned_data |> 
  select(contains(match = "nt1"), age, sex, death_all, end_date_death) |> 
  rename(
    exercise_duration = exe_du_nt1blq2, 
    exercise_frequency_per_week = exe_f_nt1blq2,
    death_all_cause = death_all,
    participation_date = part_dat_nt1blq1,
    bp_diastolic = bp_dias2_nt1blm,
    bp_systolic = bp_syst2_nt1blm,
    bmi = bmi_nt1blm,
    packs_of_smoke_per_year = smo_pack_yrs_x_nt1blq2
  ) |> 
  drop_na( # To be able to use these variables inside the coming mutate function
    exercise_duration, exercise_frequency_per_week, age, death_all_cause,
    sex, participation_date
    ) |> 
  mutate(
    minutes_duration_each_exercise_bout = case_when(
      exercise_duration == "Mindre enn 15 minutter" ~ 7.5,
      exercise_duration == "16-30 minutter" ~ 22.5,
      exercise_duration == "30 minutter-1 time" ~ 45,
      exercise_duration == "Mer enn 1 time" ~ 75
    ),
    frequency_per_week = case_when(
      exercise_frequency_per_week == "En gang i uka" ~ 1,
      exercise_frequency_per_week == "2-3 ganger i uka" ~ 2.5,
      exercise_frequency_per_week == "Omtrent hver dag" ~ 5
    ),
    pa_minutes_per_week = minutes_duration_each_exercise_bout * frequency_per_week,
    follow_up_time_in_years = round(as.numeric(interval(participation_date, end_date_death) / dyears(1)), 1),
    death_all_cause = as.numeric(death_all_cause), # Surv() input needs to be numeric
    packs_of_smoke_per_year = ifelse(is.na(packs_of_smoke_per_year), 0, packs_of_smoke_per_year) # Compute all smoking NA's to 0
    )

# HUNT 1 SURVIVAL ANALYSIS

# Multi-adjusted Cox model, adjusted for BP, BMI, smoking (cont), age and sex

hunt_1_cox_reg_multi <- coxph(Surv(follow_up_time_in_years, death_all_cause) ~ pa_minutes_per_week +
                                bp_diastolic + bp_systolic + bmi + packs_of_smoke_per_year +
                                age + sex, data = hunt_1_cleaned_data)

# Crude Cox model, adjusted for age

hunt_1_cox_reg_crude <- coxph(Surv(follow_up_time_in_years, death_all_cause) ~ pa_minutes_per_week + age,
                              data = hunt_1_cleaned_data)

# FOLLOW-UP 

# Create a function that calculates follow-up time for both models

calculate_follow_up_time_multi <- function(dataframe, strata = NULL) {
  dataframe |> 
    select(
      participation_date, end_date_death, bp_diastolic, bp_systolic,bmi, 
      packs_of_smoke_per_year, sex, age, death_all_cause, follow_up_time_in_years,
      {{strata}}
      ) |> 
    drop_na() |> 
    mutate(person_years = (end_date_death - participation_date) / 365) |> 
    summarize(person_years = sum(person_years)) |> 
    pull()
}

calculate_follow_up_time_crude <- function(dataframe, strata = NULL) {
  dataframe |> 
    select(
      participation_date, end_date_death, death_all_cause,
      follow_up_time_in_years, {{strata}}
    ) |> 
    drop_na() |> 
    mutate(person_years = (end_date_death - participation_date) / 365) |> 
    summarize(person_years = sum(person_years)) |> 
    pull()
}

# Follow-up time (person-years) for multi-adjusted and crude model

hunt_1_f_up_time_multi <- calculate_follow_up_time_multi(hunt_1_cleaned_data)

hunt_1_f_up_time_crude <- calculate_follow_up_time_crude(hunt_1_cleaned_data)

# HUNT 2 DATA PREPARATION

hunt_2_cleaned_data <- full_cleaned_data |> 
  select(contains(match = "nt2"), age, sex, end_date_death, death_all) |> 
  rename(
    exercise_time_per_week = exe_lig_du_ly_nt2blq1,
    death_all_cause = death_all,
    participation_date = part_dat_nt2blq1,
    packs_of_smoke_per_year = smo_pack_yrs_x_nt2blq1,
    bp_systolic = bp_syst_mn23_nt2blm,
    bp_diastolic = bp_dias_mn23_nt2blm,
    bmi = bmi_nt2blm
  ) |> 
  drop_na(exercise_time_per_week, end_date_death, participation_date, death_all_cause) |> 
  mutate(
    pa_minutes_per_week = case_when(
      exercise_time_per_week == "Ingen" ~ 0,
      exercise_time_per_week == "Under 1 time" ~ 30,
      exercise_time_per_week == "1-2 timer" ~ 90,
      exercise_time_per_week == "3 timer eller mer" ~ 210
    ),
    follow_up_time_in_years = round(as.numeric(interval(participation_date, end_date_death) / dyears(1)), 1),
    death_all_cause = as.numeric(death_all_cause),
    packs_of_smoke_per_year = ifelse(is.na(packs_of_smoke_per_year), 0, packs_of_smoke_per_year)
  )

# HUNT 2 SURVIVAL ANALYSIS

# Multi-adjusted model, adjusted for BP, BMI, smoking (cont), age and sex

hunt_2_cox_reg_multi <- coxph(Surv(follow_up_time_in_years, death_all_cause) ~ pa_hrs_per_week +
                                bp_systolic + bp_diastolic + bmi +
                                packs_of_smoke_per_year + sex + age, data = hunt_2_cleaned_data)

# Crude model

hunt_2_cox_reg_crude <- coxph(Surv(follow_up_time_in_years, death_all_cause) ~ pa_hrs_per_week + age,
                        data = hunt_2_cleaned_data)

# FOLLOW-UP

# Follow-up time (person-years) for multi-adjusted and crude model

hunt_2_f_up_time_multi <- calculate_follow_up_time_multi(hunt_2_cleaned_data)

hunt_2_f_up_time_crude <- calculate_follow_up_time_crude(hunt_2_cleaned_data)

# HUNT 3 DATA PREPARATION

hunt_3_cleaned_data <- full_cleaned_data |> 
  select(contains(match = "nt3"), age, sex, death_all, end_date_death) |> 
  rename(
    exercise_duration = exe_du_nt3blq1,
    exercise_frequency_per_week = exe_f_nt3blq1,
    death_all_cause = death_all,
    participation_date = part_dat_nt3blq1,
    bp_diastolic = bp_dias_mn23_nt3blm,
    bp_systolic = bp_syst_mn23_nt3blm,
    bmi = bmi_nt3blm,
    packs_of_smoke_per_year = smo_pack_yrs_x_nt3blq1
  ) |> 
  drop_na( # To be able to use these variables inside the coming mutate function
    exercise_duration, exercise_frequency_per_week, age, death_all_cause,
    sex, participation_date
  ) |> 
  mutate(
    minutes_duration_each_exercise_bout = case_when(
      exercise_duration == "Mindre enn 15 minutter" ~ 7.5, #?
      exercise_duration == "15-29 minutter" ~ 22.5,
      exercise_duration == "30 minutter - 1 time" ~ 45,
      exercise_duration == "Mer enn 1 time" ~ 75
    ),
    frequency_per_week = case_when(
      exercise_frequency_per_week == "En gang i uka" ~ 1,
      exercise_frequency_per_week == "2-3 ganger i uka" ~ 2.5, #?
      exercise_frequency_per_week == "Omtrent hver dag" ~ 5 #?
    ),
    pa_minutes_per_week = minutes_duration_each_exercise_bout * frequency_per_week,
    follow_up_time_in_years = round(as.numeric(interval(participation_date, end_date_death) / dyears(1)), 1),
    death_all_cause = as.numeric(death_all_cause),
    packs_of_smoke_per_year = ifelse(is.na(packs_of_smoke_per_year), 0, packs_of_smoke_per_year)
  )

# HUNT 3 SURVIVAL ANALYSIS

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

# HUNT 4 DATA PREPARATION

hunt_4_cleaned_data <- full_cleaned_data |> 
  select(contains(match = "nt4"), age, sex, end_date_death, death_all) |> 
  rename(
    exercise_duration = exe_du_nt4blq1,
    exercise_frequency_per_week = exe_f_nt4blq1,
    death_all_cause = death_all,
    participation_date = part_dat_nt4blq1,
    bp_diastolic = bp_dias_mn23_nt4blm,
    bp_systolic = bp_syst_mn23_nt4blm,
    bmi = bmi_nt4blm,
    packs_of_smoke_per_year = smo_pack_yrs_x_nt4blq1
  ) |> 
  drop_na( # To be able to use these variables inside the coming mutate function
    exercise_duration, exercise_frequency_per_week, age, death_all_cause,
    sex, participation_date
  ) |>
  mutate(
    minutes_duration_each_exercise_bout = case_when(
      exercise_duration == "Mindre enn 15 minutter" ~ 7.5,
      exercise_duration == "15-29 minutter" ~ 22.5,
      exercise_duration == "30-60 minutter" ~ 45,
      exercise_duration == "Mer enn 60 minutter" ~ 75
    ),
    frequency_per_week = case_when(
      exercise_frequency_per_week == "En gang i uka" ~ 1,
      exercise_frequency_per_week == "2-3 ganger i uka" ~ 2.5,
      exercise_frequency_per_week == "Omtrent hver dag" ~ 5
    ),
    pa_minutes_per_week = minutes_duration_each_exercise_bout * frequency_per_week,
    follow_up_time_in_years = round(as.numeric(interval(participation_date, end_date_death) / dyears(1)), 1),
    death_all_cause = as.numeric(death_all_cause),
    packs_of_smoke_per_year = ifelse(is.na(packs_of_smoke_per_year), 0, packs_of_smoke_per_year)
  )

# HUNT 4 SURVIVAL ANALYSIS

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