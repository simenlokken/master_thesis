## This script contains the analyses for change in PA across HUNT 1, HUNT 3 and UNT 4.

source("code/functions.R")

library(tidyverse)
library(survival)

# DATA PROCESSING FOR HUNT 1, 3 AND 4

hunt_1_3_4_cleaned_change_in_pa <- full_cleaned_data |> 
  select(contains(match = c("nt1", "nt3", "nt4")), age, sex, death_all, end_date_death) |>
  
  # Process HUNT 1 data, change names and mutate necessary variables
  
  rename(
    exercise_duration_h1 = exe_du_nt1blq2, 
    exercise_frequency_per_week_h1 = exe_f_nt1blq2,
    death_all_cause = death_all, # Static variable for all surveys, only changed in this processing part
    participation_date_h1 = part_dat_nt1blq1,
  ) |> 
  drop_na(exercise_duration_h1, exercise_frequency_per_week_h1, end_date_death, death_all_cause
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
    follow_up_time_in_years_h1 = round(as.numeric(interval(participation_date_h1, end_date_death) / dyears(1)), 1)
  ) |> 
  
  # Process HUNT 3 data, change names and mutate necessary variables
  
  rename(
    exercise_duration_h3 = exe_du_nt3blq1,
    exercise_frequency_per_week_h3 = exe_f_nt3blq1,
    participation_date_h3 = part_dat_nt3blq1
  ) |> 
  drop_na(exercise_duration_h3, exercise_frequency_per_week_h3) |> 
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
  ) |> 
  
  # Process HUNT 4 data, change names and mutate necessary variables
  
  rename(
    exercise_duration_h4 = exe_du_nt4blq1,
    exercise_frequency_per_week_h4 = exe_f_nt4blq1,
    participation_date_h4 = part_dat_nt4blq1,
  ) |>
  drop_na(exercise_duration_h4, exercise_frequency_per_week_h4) |>
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
  ) |> 
  
  # Select variables of interest for the analysis
  
  select(contains(match = c("h1", "h3", "h4")), age, sex, death_all_cause, end_date_death) |> 
  
  # Construct a follow-up time variable for all surveys, i.e., by subtracting end date from the participation date in HUNT 1
  
  mutate(total_follow_up_time = end_date_death - participation_date_h1)