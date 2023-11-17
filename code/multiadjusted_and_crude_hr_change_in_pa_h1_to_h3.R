# LOAD PACKAGES

library(tidyverse)

source("code/multiadjusted_and_crude_hr_change_in_pa_h1_to_h2.R")

# DATA PREPARATION

# Data preparation, change in LTPA from HUNT 1 to HUNT 3

# Create a function for processing HUNT 3 variables

process_hunt_3_variables <- function(dataframe) {
  
  dataframe |> 
    
    # Rename HUNT 3 variables
    
    rename(
      exercise_duration_h3 = exe_du_nt3blq1,
      exercise_frequency_per_week_h3 = exe_f_nt3blq1,
      participation_date_h3 = part_dat_nt3blq1,
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
        pa_minutes_per_week_h3 = minutes_duration_each_exercise_bout_h3 * frequency_per_week_h3,
        follow_up_time_in_years_h3 = round(as.numeric(interval(participation_date_h3, end_date_death) / dyears(1)), 1),
        death_all_cause = as.numeric(death_all_cause),
        packs_of_smoke_per_year_h3 = ifelse(is.na(packs_of_smoke_per_year_h3), 0, packs_of_smoke_per_year_h3)
      )
  
}

# Re-use function for cleaning HUNT 1 variables

source("code/multiadjusted_and_crude_hr_change_in_pa_h1_to_h2.R")

# Create dataframe

hunt_1_to_3_cleaned_data <- full_cleaned_data |> 
  process_hunt_1_variables() |> 
  process_hunt_3_variables() |> 
  mutate(pa_minutes_per_week_diff_h1_h3 = pa_minutes_per_week_h3 - pa_minutes_per_week_h1)

# EDA ON HUNT 1 AND HUNT 2 DATA

# Plot histograms for HUNT 1, HUNT 2 and diff between them

columns_h1_to_h3 <- c("pa_minutes_per_week_h1", "pa_minutes_per_week_h3", "pa_minutes_per_week_diff_h1_h3")

for (column in columns_h1_to_h3) {
  print(
    plot_histogram(hunt_1_to_3_cleaned_data, !!sym(column), 30)
  )
}
