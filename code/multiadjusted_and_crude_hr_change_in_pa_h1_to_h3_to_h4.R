## This script contains the analyses for change in PA across HUNT 1, HUNT 3 and UNT 4.

source("code/functions.R")

library(tidyverse)
library(survival)

# DATA PROCESSING FOR HUNT 1, 3 AND 4

# This data is filtered so that all participants have data on PA, death, BP, BMI, smoking, heart infarction and alcohol usage for three
# HUNT surveys. Note that heart infarction history is only filtered out in HUNT 4 because if you have reported in earlier, you will also
# report it in HUNT 4.

hunt_1_3_4_cleaned_change_in_pa <- full_cleaned_data |> 
  select(contains(match = c("nt1", "nt3", "nt4")), age, sex, death_all, end_date_death) |>
  
  # Process HUNT 1 data, change names and mutate necessary variables
  
  rename(
    exercise_duration_h1 = exe_du_nt1blq2, 
    exercise_frequency_per_week_h1 = exe_f_nt1blq2,
    death_all_cause = death_all, # Static variable for all surveys, only changed in this processing part
    participation_date_h1 = part_dat_nt1blq1,
    bp_diastolic_h1 = bp_dias2_nt1blm,
    bp_systolic_h1 = bp_syst2_nt1blm,
    bmi_h1 = bmi_nt1blm,
    packs_of_smoke_per_year_h1 = smo_pack_yrs_x_nt1blq2,
    alcohol_usage_h1 = alc_fl2w_nt1blq2,
  ) |> 
    drop_na(
      exercise_duration_h1, exercise_frequency_per_week_h1, age, sex, 
      death_all_cause, participation_date_h1, bp_diastolic_h1, bp_systolic_h1, bmi_h1,
      alcohol_usage_h1
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
    ) |> 
  
  # Process HUNT 3 data, change names and mutate necessary variables
  
  rename(
    exercise_duration_h3 = exe_du_nt3blq1,
    exercise_frequency_per_week_h3 = exe_f_nt3blq1,
    participation_date_h3 = part_dat_nt3blq1,
    bp_diastolic_h3 = bp_dias_mn23_nt3blm,
    bp_systolic_h3 = bp_syst_mn23_nt3blm,
    bmi_h3 = bmi_nt3blm,
    packs_of_smoke_per_year_h3 = smo_pack_yrs_x_nt3blq1,
    alcohol_usage_h3 = alc_tot_unit_w_nt3blq1,
    heart_infarction_h3 = car_inf_ev_nt3blq1
  ) |>
  drop_na(
    exercise_duration_h3, exercise_frequency_per_week_h3, age, death_all_cause,
    sex, participation_date_h3, alcohol_usage_h3, heart_infarction_h3, 
  ) |> 
  mutate(
    minutes_duration_each_exercise_bout_h3 = case_when(
      exercise_duration_h3 == "Mindre enn 15 minutter" ~ 7.5,
      exercise_duration_h3 == "15-29 minutter" ~ 22.5,
      exercise_duration_h3 == "30 minutter - 1 time" ~ 45,
      exercise_duration_h3 == "Mer enn 1 time" ~ 75
    ),
    frequency_per_week_h3 = case_when(
      exercise_frequency_per_week_h3 == "En gang i uka" ~ 1,
      exercise_frequency_per_week_h3 == "2-3 ganger i uka" ~ 2.5,
      exercise_frequency_per_week_h3 == "Omtrent hver dag" ~ 5
    ),
    pa_hrs_per_week_h3 = (minutes_duration_each_exercise_bout_h3 * frequency_per_week_h3) / 60,
    follow_up_time_in_years_h3 = round(as.numeric(interval(participation_date_h3, end_date_death) / dyears(1)), 1),
    death_all_cause = as.numeric(death_all_cause),
    packs_of_smoke_per_year_h3 = ifelse(is.na(packs_of_smoke_per_year_h3), 0, packs_of_smoke_per_year_h3)
  ) |> 
  
  # Process HUNT 4 data, change names and mutate necessary variables
  
  rename(
    exercise_duration_h4 = exe_du_nt4blq1,
    exercise_frequency_per_week_h4 = exe_f_nt4blq1,
    participation_date_h4 = part_dat_nt4blq1,
    bp_diastolic_h4 = bp_dias_mn23_nt4blm,
    bp_systolic_h4 = bp_syst_mn23_nt4blm,
    bmi_h4 = bmi_nt4blm,
    packs_of_smoke_per_year_h4 = smo_pack_yrs_x_nt4blq1,
    alcohol_usage_h4 = alc_tot_unit_w_nt4blq1,
    heart_infarction_h4 = car_inf_ev_nt4blq1
  ) |>
  drop_na(
    exercise_duration_h4, exercise_frequency_per_week_h4, age, death_all_cause,
    sex, participation_date_h4, alcohol_usage_h4, heart_infarction_h4, bp_systolic_h4,
    bp_diastolic_h4, bmi_h4
  ) |>
  mutate(
    minutes_duration_each_exercise_bout_h4 = case_when(
      exercise_duration_h4 == "Mindre enn 15 minutter" ~ 7.5,
      exercise_duration_h4 == "15-29 minutter" ~ 22.5,
      exercise_duration_h4 == "30-60 minutter" ~ 45,
      exercise_duration_h4 == "Mer enn 60 minutter" ~ 75
    ),
    frequency_per_week_h4 = case_when(
      exercise_frequency_per_week_h4 == "En gang i uka" ~ 1,
      exercise_frequency_per_week_h4 == "2-3 ganger i uka" ~ 2.5,
      exercise_frequency_per_week_h4 == "Omtrent hver dag" ~ 5
    ),
    pa_hrs_per_week_h4 = (minutes_duration_each_exercise_bout_h4 * frequency_per_week_h4) / 60,
    follow_up_time_in_years_h4 = round(as.numeric(interval(participation_date_h4, end_date_death) / dyears(1)), 1),
    death_all_cause = as.numeric(death_all_cause),
    packs_of_smoke_per_year_h4 = ifelse(is.na(packs_of_smoke_per_year_h4), 0, packs_of_smoke_per_year_h4)
  ) |> 
  
  # Select variables of interest for the analysis
  
  select(contains(match = c("h1", "h2", "h3", "h4")), age, sex, death_all_cause, end_date_death) |> 
  
  # Construct a follow-up time variable for all surveys
  
  mutate(total_follow_up_time = end_date_death - participation_date_h1)


