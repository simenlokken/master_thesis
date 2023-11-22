## This script contains data for the table on descriptive characteristics in all four HUNT surveys without change in LTPA

# Create functions

# Numerical summary stats

calculate_numerical_summary_statistics <- function(dataframe) {
  
  dataframe |> 
    drop_na(
      age, sex, pa_hrs_per_week, follow_up_time_in_years, death_all_cause, 
      bp_systolic, bp_diastolic, bmi, packs_of_smoke_per_year
    ) |> 
    summarize(
      mean_age = mean(age),
      sd_age = sd(age),
      mean_bmi = mean(bmi),
      sd_bmi = sd(bmi),
      mean_sbp = mean(bp_systolic),
      sd_sbp = sd(bp_systolic),
      mean_dbp = mean(bp_diastolic),
      sd_dbp = sd(bp_diastolic),
      mean_smo = mean(packs_of_smoke_per_year),
      sd_smo = sd(packs_of_smoke_per_year),
      mean_pa = mean(pa_hrs_per_week),
      sd_pa = sd(pa_hrs_per_week),
    )
  
}

# Count group occurence

count_group_occurence <- function(dataframe, grouping_variable) {
  
  dataframe |> 
    drop_na({{ grouping_variable }}) |> 
    group_by({{ grouping_variable }}) |>
    count()
  
}

# HUNT 1

calculate_numerical_summary_statistics(hunt_1_cleaned_data)

count_group_occurence(hunt_1_cleaned_data_socio_strat, grouping_variable = socioeconomic_class)
hunt_1_sum_socio <- c(4164, 19976, 9665)
hunt_1_sum_socio / sum(hunt_1_sum_socio)

count_group_occurence(hunt_1_cleaned_data_occu_strat, grouping_variable = occupational_pa)
hunt_1_sum_occu <- c(10628, 13371)
hunt_1_sum_occu / sum(hunt_1_sum_occu)

# BMI >= 30 kg

hunt_1_cleaned_data |> 
  drop_na(bmi) |> 
  filter(bmi >= 30)

3118 / 34996

# HUNT 2

# Numerical stats

calculate_numerical_summary_statistics(hunt_2_cleaned_data)

count_group_occurence(hunt_2_cleaned_data_socio_strat, grouping_variable = socioeconomic_class)
hunt_2_sum_socio <- c(18175, 23712, 11126)
hunt_2_sum_socio / sum(hunt_2_sum_socio)

count_group_occurence(hunt_2_cleaned_data_occu_strat, grouping_variable = occupational_pa)
hunt_2_sum_occu <- c(13552, 17602)
hunt_2_sum_occu / sum(hunt_2_sum_occu)

# BMI >= 30 kg

hunt_2_cleaned_data |> 
  drop_na(bmi) |> 
  filter(bmi >= 30)

8736 / 53830

# HUNT 3

calculate_numerical_summary_statistics(hunt_3_cleaned_data)

count_group_occurence(hunt_3_cleaned_data_socio_strat, grouping_variable = socioeconomic_class)
hunt_3_sum_socio <- c(7509, 15167, 13926)
hunt_3_sum_socio / sum(hunt_3_sum_socio)

count_group_occurence(hunt_3_cleaned_data_occu_strat, grouping_variable = occupational_pa)
hunt_3_sum_occu <- c(13797, 1961)
hunt_3_sum_occu / sum(hunt_3_sum_occu)

# BMI >= 30 kg

hunt_3_cleaned_data |> 
  drop_na(bmi) |> 
  filter(bmi >= 30)

7954 / 38240
  
# HUNT 4

calculate_numerical_summary_statistics(hunt_4_cleaned_data)  

count_group_occurence(hunt_4_cleaned_data_socio_strat, grouping_variable = socioeconomic_class)
hunt_4_sum_socio <- c(13558, 12084, 18827)
hunt_4_sum_socio / sum(hunt_4_sum_socio)

count_group_occurence(hunt_4_cleaned_data_occu_strat, grouping_variable = occupational_pa)
hunt_4_sum_occu <- c(8467, 1973)
hunt_4_sum_occu / sum(hunt_4_sum_occu)

hunt_4_cleaned_data |> 
  drop_na(bmi) |> 
  filter(bmi >= 30)

9637 / 43367