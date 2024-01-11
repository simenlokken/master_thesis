# LOAD PACKAGES

library(tidyverse)

# Source HUNT data

source("code/functions.R")

# Number of daily smokers, all surveys

count_daily_smokers <- function(dataframe) {
  dataframe |> 
    group_by(socioeconomic_class) |> 
    drop_na(socioeconomic_class) |> 
    filter(smo_status == "Daglig røyker") |> 
    count(smo_status)
}

# Re-use this list for coming functions

hunt_data_socio_strat <- list(
  hunt_1_cleaned_data_socio_strat,
  hunt_2_cleaned_data_socio_strat,
  hunt_3_cleaned_data_socio_strat,
  hunt_4_cleaned_data_socio_strat
)

map_df(hunt_data, count_daily_smokers)

# Number of people w/ BMI > 30, all surveys

count_bmi_above_30 <- function(dataframe) {
  dataframe |> 
    group_by(socioeconomic_class) |> 
    drop_na(socioeconomic_class) |> 
    filter(bmi >= 30) |> 
    count()
}

map_df(hunt_data_socio_strat, count_bmi_above_30)

# Number of low socioeconomic class

count_high_occu_pa <- function(dataframe) {
  dataframe |> 
    drop_na() |> 
    group_by(occupational_pa) |> 
    count()
}

hunt_data_occu_pa <- list(
  hunt_1_cleaned_data_occu_strat,
  hunt_2_cleaned_data_occu_strat,
  hunt_3_cleaned_data_occu_strat,
  hunt_4_cleaned_data_occu_strat
)

map_df(hunt_data_occu_pa, count_high_occu_pa)

# HUNT 1-4 summary stats

calc_sum_stat <- function(dataframe) {
  dataframe |> 
    group_by(socioeconomic_class) |> 
    drop_na(socioeconomic_class, age, bp_systolic, pa_hrs_per_week, bmi) |> 
    summarise(
      mean_age = mean(age),
      sd_age = sd(age),
      mean_sbp = round(mean(bp_systolic), 1),
      sd_sbp = round(sd(bp_systolic), 1),
      mean_ltpa = round(mean(pa_hrs_per_week), 1),
      sd_ltpa = round(sd(pa_hrs_per_week), 1),
      mean_bmi = round(mean(bmi), 1),
      sd_bmi = round(sd(bmi), 1)
    )
}

map_df(hunt_data_socio_strat, calc_sum_stat)
