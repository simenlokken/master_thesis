# LOAD PACKAGES

library(tidyverse)

# DATA PREPARATION

# Data preparation, change in LTPA from HUNT 1 to HUNT 2

hunt_1_to_2_cleaned_data <- full_cleaned_data |> 
  
  # HUNT 1 renaming variables
  
  rename(
    exercise_duration_h1 = exe_du_nt1blq2, 
    exercise_frequency_per_week_h1 = exe_f_nt1blq2,
    death_all_cause = death_all, # Only one variable for all surveys
    participation_date_h1 = part_dat_nt1blq1,
    bp_diastolic_h1 = bp_dias2_nt1blm,
    bp_systolic_h1 = bp_syst2_nt1blm,
    bmi_h1 = bmi_nt1blm,
    packs_of_smoke_per_year_h1 = smo_pack_yrs_x_nt1blq2
  ) |> 
  
  drop_na( # To be able to use these variables inside the coming mutate function
    exercise_duration_h1, exercise_frequency_per_week_h1, age, death_all_cause,
    sex, participation_date_h1
  ) |> 
  
  # HUNT 1 mutating variables
  
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
    pa_minutes_per_week_h1 = minutes_duration_each_exercise_bout_h1 * frequency_per_week_h1,
    follow_up_time_in_years_h1 = round(as.numeric(interval(participation_date_h1, end_date_death) / dyears(1)), 1),
    death_all_cause = as.numeric(death_all_cause), # Surv() input needs to be numeric
    packs_of_smoke_per_year_h1 = ifelse(is.na(packs_of_smoke_per_year_h1), 0, packs_of_smoke_per_year_h1) # Compute all smoking NA's to 0
    ) |> 
  
  # HUNT 2 remaning of variables
  
  rename(
    exercise_time_per_week_h2 = exe_lig_du_ly_nt2blq1,
    participation_date_h2 = part_dat_nt2blq1,
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
    death_all_cause = as.numeric(death_all_cause),
    packs_of_smoke_per_year_h2 = ifelse(is.na(packs_of_smoke_per_year_h2), 0, packs_of_smoke_per_year_h2)
  ) |>
  
  # Creating a variable calculating the difference between HUNT 1 and HUNT 2
  
  mutate(pa_minutes_per_week_diff_h1_h2 = pa_minutes_per_week_h2 - pa_minutes_per_week_h1)