## This script contains an exploratory data analysis on the variables used in the regression analyses

# LOAD PACKAGES

library(tidyverse)
library(patchwork)

source("code/functions.R")

# PLOT HISTOGRAMS OVER ALL CONTINUOUS COVARIATES

# Histogram function

plot_histograms <- function(dataframe, covariates, survey = NULL, bins = 50) {
  plot_list <- list()
  
  for (covariate in covariates) {
    plot <- ggplot(dataframe, aes(x = !!sym(covariate))) +
      geom_histogram(bins = bins, fill = "steelblue", color = "black", alpha = 0.8) +
      labs(x = covariate, y = "Count", title = paste("Histogram of", covariate, "for", survey)) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank()
    )
    
    plot_list <- c(plot_list, list(plot))
  }
  return(plot_list)
}

# Plot all covariates for all surveys

# HUNT 1

plot_histograms(hunt_1_cleaned_data, covariates = c("pa_hrs_per_week", "age", "follow_up_time_in_years", "bp_diastolic",
                                                    "bp_systolic","bmi", "packs_of_smoke_per_year"), 
                survey = "HUNT 1"
)

# HUNT 2

plot_histograms(hunt_2_cleaned_data, covariates = c("pa_hrs_per_week", "age", "follow_up_time_in_years", "bp_diastolic",
                                                    "bp_systolic","bmi", "packs_of_smoke_per_year", "alcohol_usage"),
                survey = "HUNT 2"
)

# HUNT 3

plot_histograms(hunt_3_cleaned_data, covariates = c("pa_hrs_per_week", "age", "follow_up_time_in_years", "bp_diastolic",
                                                    "bp_systolic","bmi", "packs_of_smoke_per_year", "alcohol_usage"),
                survey = "HUNT 3"
)

# HUNT 4

plot_histograms(hunt_4_cleaned_data, covariates = c("pa_hrs_per_week", "age", "follow_up_time_in_years", "bp_diastolic",
                                                    "bp_systolic","bmi", "packs_of_smoke_per_year", "alcohol_usage"),
                survey = "HUNT 4"
)

# COUNT OCURRENCE OF CATEGORICAL COVARIATES

# Function to count occurence of categorical covariates. This also works without a for loop.

count_occurence <- function(dataframe, covariate) {
  dataframe %>%
    drop_na(!!sym(covariate)) |> 
    group_by(!!sym(covariate)) %>%
    summarise(count = n()) |> 
    arrange(desc(count))
}

# HUNT 1-4

dataframes <- c("hunt_1_cleaned_data", "hunt_2_cleaned_data", "hunt_3_cleaned_data", "hunt_4_cleaned_data")

for (i in dataframes) {
  result <- count_occurence(get(i), covariate = "heart_infarction")
  print(result)
}

# Plotting follow-up time for all surveys

plot_histograms <- function(dataframes, covariate, bins = 30) {
  plot_list <- list()
  
  for (df in dataframes) {
    plot <- ggplot(df, aes(x = !!sym(covariate))) +
      geom_histogram(bins = bins, fill = "steelblue", color = "black", alpha = 0.8) +
      labs(x = covariate, y = "Count", title = paste("Histogram of", covariate)) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank())
    
    plot_list <- c(plot_list, list(plot))
  }
  
  return(plot_list)
}

dataframes <- list(hunt_1_cleaned_data, hunt_2_cleaned_data, hunt_3_cleaned_data, hunt_4_cleaned_data)

covariate <- "follow_up_time_in_years"

histograms <- plot_histograms(dataframes, covariate, bins = 30)

combined_plots <- wrap_plots(histograms, ncol = 1)

combined_plots