# LOAD PACKAGES

library(tidyverse)

source("code/multiadjusted_and_crude_hr_change_in_pa_h1_to_h2.R")

# DATA PREPARATION

# Data preparation, change in LTPA from HUNT 1 to HUNT 3

# Create a function for processing HUNT 3 variables

# Re-use function for cleaning HUNT 1 variables

source("code/multiadjusted_and_crude_hr_change_in_pa_h1_to_h2.R")

# Create data frame

hunt_1_to_3_cleaned_data <- full_cleaned_data |> 
  process_hunt_1_variables() |> 
  process_hunt_3_variables() |> 
  mutate(pa_minutes_per_week_diff_h1_h3 = pa_minutes_per_week_h3 - pa_minutes_per_week_h1)

# EDA ON HUNT 1 AND HUNT 3 DATA

