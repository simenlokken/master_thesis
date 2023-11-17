# SOURCE

source("pre_analysis_data_preparation.R")

# LOAD PACKAGES

library(tidyverse)
library(survival)

# DATA CLEANING FUNCTIONS

# Create functions that processes data for all surveys

process_hunt_1 <- function(dataframe) {
  
  dataframe |> 
    
    # Select all variables 
    
    select(contains(match = "nt1"), age, sex, death_all, end_date_death) |> 
    
    # Rename HUNT 1 variables
    
    rename(
      exercise_duration_h1 = exe_du_nt1blq2, 
      exercise_frequency_per_week_h1 = exe_f_nt1blq2,
      death_all_cause = death_all,
      participation_date_h1 = part_dat_nt1blq1,
      bp_diastolic_h1 = bp_dias2_nt1blm,
      bp_systolic_h1 = bp_syst2_nt1blm,
      bmi_h1 = bmi_nt1blm,
      packs_of_smoke_per_year_h1 = smo_pack_yrs_x_nt1blq2
      
      # Mutate HUNT 1 variables
      
    ) |> 
    drop_na( # To be able to use these variables inside the coming mutate function
      exercise_duration_h1, exercise_frequency_per_week_h1, age, death_all_cause,
      sex, participation_date_h1
    ) |> 
    mutate(
      minutes_duration_each_exercise_bout_h1 = case_when(
        exercise_duration_h1 == "Mindre enn 15 minutter" ~ 7.5,
        exercise_duration_h1 == "16-30 minutter" ~ 22.5,
        exercise_duration_h1 == "30 minutter-1 time" ~ 45,
        exercise_duration_h1 == "Mer enn 1 time" ~ 75
      ),
      frequency_per_week_h1 = case_when(
        exercise_frequency_per_week_h1 == "En gang i uka" ~ 1,
        exercise_frequency_per_week_h1 == "2-3 ganger i uka" ~ 2.5,
        exercise_frequency_per_week_h1 == "Omtrent hver dag" ~ 5
      ),
      pa_hrs_per_week_h1 = (minutes_duration_each_exercise_bout_h1 * frequency_per_week_h1) / 60,
      follow_up_time_in_years_h1 = round(as.numeric(interval(participation_date_h1, end_date_death) / dyears(1)), 1),
      death_all_cause = as.numeric(death_all_cause), # Surv() input needs to be numeric
      packs_of_smoke_per_year_h1 = ifelse(is.na(packs_of_smoke_per_year_h1), 0, packs_of_smoke_per_year_h1) # Compute all smoking NA's to 0
    )
  
} # HUNT 1

process_hunt_2 <- function(dataframe) {
  
  dataframe |> 
  
    # Select relevant variables
  
    select(contains(match = "nt2"), age, sex, death_all, end_date_death) |>
    
    # HUNT 2 renaming of variables
    
    rename(
      exercise_time_per_week_h2 = exe_lig_du_ly_nt2blq1,
      participation_date_h2 = part_dat_nt2blq1,
      death_all_cause = death_all,
      packs_of_smoke_per_year_h2 = smo_pack_yrs_x_nt2blq1,
      bp_systolic_h2 = bp_syst_mn23_nt2blm,
      bp_diastolic_h2 = bp_dias_mn23_nt2blm,
      bmi_h2 = bmi_nt2blm
    ) |> 
    
    drop_na( # To be able to use these variables inside the coming mutate function
      exercise_time_per_week_h2, age, death_all_cause,
      sex, participation_date_h2
    ) |> 
    
    # HUNT 2 mutating variables
    
    mutate(
      pa_minutes_per_week_h2 = case_when(
        exercise_time_per_week_h2 == "Ingen" ~ 0,
        exercise_time_per_week_h2 == "Under 1 time" ~ 30,
        exercise_time_per_week_h2 == "1-2 timer" ~ 90,
        exercise_time_per_week_h2 == "3 timer eller mer" ~ 210
      ),
      follow_up_time_in_years_h2 = round(as.numeric(interval(participation_date_h2, end_date_death) / dyears(1)), 1),
      packs_of_smoke_per_year_h2 = ifelse(is.na(packs_of_smoke_per_year_h2), 0, packs_of_smoke_per_year_h2)
    )
} # HUNT 2

process_hunt_3 <- function(dataframe) {
  
  dataframe |> 
    
    # Select relevant variables
    
    select(contains(match = "nt3"), age, sex, death_all, end_date_death) |>
    
    # Rename HUNT 3 variables
    
    rename(
      exercise_duration_h3 = exe_du_nt3blq1,
      exercise_frequency_per_week_h3 = exe_f_nt3blq1,
      participation_date_h3 = part_dat_nt3blq1,
      death_all_cause = death_all,
      bp_diastolic_h3 = bp_dias_mn23_nt3blm,
      bp_systolic_h3 = bp_syst_mn23_nt3blm,
      bmi_h3 = bmi_nt3blm,
      packs_of_smoke_per_year_h3 = smo_pack_yrs_x_nt3blq1
    ) |> 
    
    # Mutate HUNT 3 variables
    
    drop_na( # To be able to use these variables inside the coming mutate function
      exercise_duration_h3, exercise_frequency_per_week_h3, age, death_all_cause,
      sex, participation_date_h3
    ) |> 
    mutate(
      minutes_duration_each_exercise_bout_h3 = case_when(
        exercise_duration_h3 == "Mindre enn 15 minutter" ~ 7.5, #?
        exercise_duration_h3 == "15-29 minutter" ~ 22.5,
        exercise_duration_h3 == "30 minutter - 1 time" ~ 45,
        exercise_duration_h3 == "Mer enn 1 time" ~ 75
      ),
      frequency_per_week_h3 = case_when(
        exercise_frequency_per_week_h3 == "En gang i uka" ~ 1,
        exercise_frequency_per_week_h3 == "2-3 ganger i uka" ~ 2.5, #?
        exercise_frequency_per_week_h3 == "Omtrent hver dag" ~ 5 #?
      ),
      pa_hrs_per_week_h3 = (minutes_duration_each_exercise_bout_h3 * frequency_per_week_h3) / 60,
      follow_up_time_in_years_h3 = round(as.numeric(interval(participation_date_h3, end_date_death) / dyears(1)), 1),
      death_all_cause = as.numeric(death_all_cause),
      packs_of_smoke_per_year_h3 = ifelse(is.na(packs_of_smoke_per_year_h3), 0, packs_of_smoke_per_year_h3)
    )
  
} # HUNT 3

process_hunt_4 <- function(dataframe) {
  
  dataframe |> 
  
  # Select relevant variables
  
  select(contains(match = "nt4"), age, sex, death_all, end_date_death) |>
    
    # Rename HUNT 4 variables
    
    rename(
      exercise_duration_h4 = exe_du_nt4blq1,
      exercise_frequency_per_week_h4 = exe_f_nt4blq1,
      death_all_cause = death_all,
      participation_date_h4 = part_dat_nt4blq1,
      bp_diastolic_h4 = bp_dias_mn23_nt4blm,
      bp_systolic_h4 = bp_syst_mn23_nt4blm,
      bmi_h4 = bmi_nt4blm,
      packs_of_smoke_per_year_h4 = smo_pack_yrs_x_nt4blq1
    ) |>
      
      # Mutate HUNT 4 variables
    
    drop_na( # To be able to use these variables inside the coming mutate function
      exercise_duration_h4, exercise_frequency_per_week_h4, age, death_all_cause,
      sex, participation_date_h4
    ) |>
    mutate(
      minutes_duration_each_exercise_bout = case_when(
        exercise_duration_h4 == "Mindre enn 15 minutter" ~ 7.5,
        exercise_duration_h4 == "15-29 minutter" ~ 22.5,
        exercise_duration_h4 == "30-60 minutter" ~ 45,
        exercise_duration_h4 == "Mer enn 60 minutter" ~ 75
      ),
      frequency_per_week = case_when(
        exercise_frequency_per_week_h4 == "En gang i uka" ~ 1,
        exercise_frequency_per_week_h4 == "2-3 ganger i uka" ~ 2.5,
        exercise_frequency_per_week_h4 == "Omtrent hver dag" ~ 5
      ),
      pa_hrs_per_week_h4 = (minutes_duration_each_exercise_bout * frequency_per_week) / 60,
      follow_up_time_in_years_h4 = round(as.numeric(interval(participation_date_h4, end_date_death) / dyears(1)), 1),
      death_all_cause = as.numeric(death_all_cause),
      packs_of_smoke_per_year_h4 = ifelse(is.na(packs_of_smoke_per_year_h4), 0, packs_of_smoke_per_year_h4)
    )
  
} # HUNT 4

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

# FOLLOW-UP 

# Create a function that calculates follow-up time for both models

calculate_follow_up_time_multi <- function(dataframe, strata = NULL, covariates ) {
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

calculate_follow_up_time <- function(dataframe, strata = NULL, covariates, participation_date) {
  dataframe |> 
    select(strata, !!(covariates), {{end_date_death}}, {{participation_date}}) |> 
    drop_na() |> 
    mutate(person_years = ({{end_date_death}} - {{participation_date}}) / 365) |> 
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