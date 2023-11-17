# LOAD PACKAGES

library(tidyverse)

# SOURCE

source("code/multiadjusted_and_crude_hr_all_surveys_no_change_in_pa_socioeconomic_stratified.R")

# DATA PREPARATION

# Data preparation, change in LTPA from HUNT 1 to HUNT 2

# Create a function for cleaning HUNT 1 variables for later use

process_hunt_2_variables <- function(dataframe) {
  
  # Rename HUNT 2 variables
  
  dataframe |> 
  
    # HUNT 2 renaming of variables
    
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
      packs_of_smoke_per_year_h2 = ifelse(is.na(packs_of_smoke_per_year_h2), 0, packs_of_smoke_per_year_h2)
    )
}

# Perform the processing and create a new dataframe

hunt_1_to_2_cleaned_data <- full_cleaned_data |> 
  process_hunt_1_variables() |> 
  process_hunt_2_variables() |> 
  mutate(pa_minutes_per_week_diff_h1_h2 = pa_minutes_per_week_h2 - pa_minutes_per_week_h1)

# EDA ON HUNT 1 AND HUNT 2 DATA

# Histogram function

plot_histogram <- function(dataframe, x, bins = NULL) {
  dataframe |> 
    ggplot(aes(x = {{x}})) +
    geom_histogram(bins = bins)
}

# Plot histograms for HUNT 1, HUNT 2 and diff between them

columns_h1_to_h1 <- c("pa_minutes_per_week_h1", "pa_minutes_per_week_h2", "pa_minutes_per_week_diff_h1_h2")

for (column in columns_h1_to_h1) {
  print(
    plot_histogram(hunt_1_to_2_cleaned_data, !!sym(column), 20)
  )
}

# Get unique values of each column

unique(sort(hunt_1_to_2_cleaned_data$pa_minutes_per_week_h1))
unique(sort(hunt_1_to_2_cleaned_data$pa_minutes_per_week_h2))
unique(sort(hunt_1_to_2_cleaned_data$pa_minutes_per_week_diff_h1_h2))
