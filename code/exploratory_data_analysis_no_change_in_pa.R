## This script contains an exploratory data analysis on the variables used in the Regression analyses

# LOAD PACKAGES

library(tidyverse)

# PLOT HISTOGRAMS OVER ALL CONTINUOUS COVARIATES

# Histogram function

plot_histograms <- function(dataframe, covariates, survey) {
  
  for (covariate in covariates) {
    print(
      ggplot(dataframe, aes(x = !!sym(covariate))) +
      geom_histogram(bins = 20, fill = "steelblue", color = "black") +
      labs(x = covariate, y = "Count", title = paste("Histogram of", covariate, "for", survey)) +
      theme_minimal()
    )
  }
}

# HUNT 1

plot_histograms(hunt_1_cleaned_data, covariates = c("pa_hrs_per_week", "age", "follow_up_time_in_years", "bp_diastolic",
                                                    "bp_systolic","bmi", "packs_of_smoke_per_year"), 
                survey = "HUNT 1")

# HUNT 2

plot_histograms(hunt_2_cleaned_data, covariates = c("pa_hrs_per_week", "age", "follow_up_time_in_years", "bp_diastolic",
                                                    "bp_systolic","bmi", "packs_of_smoke_per_year", "alcohol_usage"),
                survey = "HUNT 2")

# HUNT 3

plot_histograms(hunt_3_cleaned_data, covariates = c("pa_hrs_per_week", "age", "follow_up_time_in_years", "bp_diastolic",
                                                    "bp_systolic","bmi", "packs_of_smoke_per_year", "alcohol_usage"),
                survey = "HUNT 3")

# HUNT 4

plot_histograms(hunt_4_cleaned_data, covariates = c("pa_hrs_per_week", "age", "follow_up_time_in_years", "bp_diastolic",
                                                    "bp_systolic","bmi", "packs_of_smoke_per_year", "alcohol_usage"),
                survey = "HUNT 4")

# COUNT OCURRENCE OF CATEGORICAL COVARIATES

# Function to count occurence of categorical covariates

count_occurence <- function(dataframe, covariate) {
  dataframe %>%
    drop_na(!!sym(covariate)) |> 
    group_by(!!sym(covariate)) %>%
    summarise(count = n()) %>%
    arrange(desc(count))
}

# HUNT 1

count_occurence(hunt_1_cleaned_data, "heart_infarction")

# HUNT 2

count_occurence(hunt_2_cleaned_data, "heart_infarction")

# HUNT 3

count_occurence(hunt_3_cleaned_data, "heart_infarction")

# HUNT 4

count_occurence(hunt_4_cleaned_data, "heart_infarction")