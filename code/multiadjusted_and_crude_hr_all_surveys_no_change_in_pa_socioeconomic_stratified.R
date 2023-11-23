# SOURCE SCRIPTS

source("code/functions.R")

# LOAD PACKAGES 

library(tidyverse)
library(survival)

# DATA PREPARATION FOR ALL HUNT SURVEYS, i.e., adding a socioeconomic variable to the data. The earlier data preparation done are sourced in.

# HUNT 1

hunt_1_cleaned_data_socio_strat <- hunt_1_cleaned_data |> 
  rename(education_level = educ_nt1blq2) |> 
  mutate(socioeconomic_class = as.factor(case_when( # Constructing a socioeconomic variable
    education_level %in% c("7-årig folkeskole eller kortere", "9-årig grunnskole", "Real- eller middelskole, grunnskolen 10.år", "Framhalds- eller fortsettelsesskole") ~ "low",
    education_level %in% c("Ett- eller toårig videregående skole", "Artium, økonomisk gymnas eller almenfaglig retning i vgs.") ~ "medium",
    education_level %in% c("Høyskole eller universitet, 4 år eller mer", "Høyskole eller universitet, mindre enn 4 år") ~ "high"))
  )

# HUNT 2

hunt_2_cleaned_data_socio_strat <- hunt_2_cleaned_data |> 
  rename(education_level = educ_nt2blq1) |> 
  mutate(socioeconomic_class = as.factor(case_when(
    education_level %in% c("Grunnskole 7-10 år, framhaldsskole, folkehøgskole") ~ "low",
    education_level %in% c("Artium, øk.gymnas, allmennfaglig retning i videregående skole", "Realskole, middelskole, yrkesskole 1-2 årig videregående skole") ~ "medium",
    education_level %in% c("Høgskole/universitet, 4 år eller mer", "Høgskole/universitet, mindre enn 4 år") ~ "high"))
  )

# HUNT 3

hunt_3_cleaned_data_socio_strat <- hunt_3_cleaned_data |> 
  rename(occupational_type = wor_tit_isco1_nt3bli) |> 
  mutate(socioeconomic_class = as.factor(case_when(
    occupational_type %in% c("Yrker uten krav til utdanning", "Yrker innen jordbruk, skogbruk og fiske", "Prosess- og maskinoperatører, transportarbeidere mv.") ~ "low",
    occupational_type %in% c("Kontor- og kundeserviceyrker", "Salgs-, service- og omsorgsyrker", "Håndverkere o.l.", "Militære yrker og uoppgitt") ~ "medium",
    occupational_type %in% c("Administrative ledere og politikere", "Akademiske yrker", "Yrker med kortere høyskole- og universitetsutdanning og teknikere") ~ "high"))
  )

# HUNT 4

hunt_4_cleaned_data_socio_strat <- hunt_4_cleaned_data |> 
  rename(education_level = educ_nt4blq1) |> 
  mutate(
    socioeconomic_class = as.factor(case_when(
      education_level %in% c("Grunnskole", "Fagbrev eller svennebrev") ~ "low",
      education_level %in% c("3 år i videregående skole", "1-2årig videregående skole") ~ "medium",
      education_level %in% c("Høyskole/universitet, 4 år eller mer", "Høyskole/universitet, mindre enn 4 år") ~ "high"))
  )

# SURVIVAL ANALYSIS

# HUNT 1, stratified multi-adjusted and crude Cox models

hunt_1_cox_reg_multi_socio_strat <- run_cox_reg_multi(hunt_1_cleaned_data_socio_strat, socioeconomic_class) # Multi-adjusted

hunt_1_cox_reg_crude_socio_strat <- run_cox_reg_crude(hunt_1_cleaned_data_socio_strat, socioeconomic_class) # Crude

# HUNT 2, stratified multi-adjusted and crude models

hunt_2_cox_reg_multi_socio_strat <- run_cox_reg_multi(hunt_2_cleaned_data_socio_strat, socioeconomic_class) # Multi-adjusted

hunt_2_cox_reg_crude_socio_strat <- run_cox_reg_crude(hunt_2_cleaned_data_socio_strat, socioeconomic_class) # Crude

# HUNT 3, stratified multi-adjusted and crude models

hunt_3_cox_reg_multi_socio_strat <- run_cox_reg_multi(hunt_3_cleaned_data_socio_strat, socioeconomic_class) # Multi-adjusted

hunt_3_cox_reg_crude_socio_strat <- run_cox_reg_crude(hunt_3_cleaned_data_socio_strat, socioeconomic_class) # Crude

# HUNT 4, stratified multi-adjusted and crude models

hunt_4_cox_reg_multi_socio_strat <- run_cox_reg_multi(hunt_4_cleaned_data_socio_strat, socioeconomic_class) # Multi-adjusted

hunt_4_cox_reg_crude_socio_strat <- run_cox_reg_crude(hunt_4_cleaned_data_socio_strat, socioeconomic_class) # Crude

# SUMMARY STATS FROM MODELS

# Multi-adjusted model

socioeconomic_class <- c("low", "medium", "high")

for (class in socioeconomic_class) {
    calculate_num_of_participants_strat(
      dataframes = c("hunt_1_cleaned_data_socio_strat", "hunt_2_cleaned_data_socio_strat", "hunt_3_cleaned_data_socio_strat", "hunt_4_cleaned_data_socio_strat"), 
      covariates = c("age", "pa_hrs_per_week", "follow_up_time_in_years", "death_all_cause", "bp_diastolic", "bp_systolic",
                     "bmi", "packs_of_smoke_per_year", "sex", "alcohol_usage", "heart_infarction", "socioeconomic_class"),
      strata = class
    )
}

for (class in socioeconomic_class) {
  calculate_person_years_follow_up_strat(
    dataframes = c("hunt_1_cleaned_data_socio_strat", "hunt_2_cleaned_data_socio_strat", "hunt_3_cleaned_data_socio_strat", "hunt_4_cleaned_data_socio_strat"),
    covariates = c("age", "pa_hrs_per_week", "follow_up_time_in_years", "death_all_cause", "bp_diastolic", "bp_systolic",
                   "bmi", "packs_of_smoke_per_year", "sex", "alcohol_usage", "heart_infarction", "socioeconomic_class"),
    strata = class
  )
}

# Crude model

for (class in socioeconomic_class) {
  calculate_num_of_participants_strat(
    dataframes = c("hunt_1_cleaned_data_socio_strat", "hunt_2_cleaned_data_socio_strat", "hunt_3_cleaned_data_socio_strat", "hunt_4_cleaned_data_socio_strat"), 
    covariates = c("pa_hrs_per_week", "age", "follow_up_time_in_years", "death_all_cause", "socioeconomic_class"),
    strata = class
  )
}

for (class in socioeconomic_class) {
  calculate_person_years_follow_up_strat(
    dataframes = c("hunt_1_cleaned_data_socio_strat", "hunt_2_cleaned_data_socio_strat", "hunt_3_cleaned_data_socio_strat", "hunt_4_cleaned_data_socio_strat"),
    covariates = c("pa_hrs_per_week", "age", "follow_up_time_in_years", "death_all_cause", "socioeconomic_class"),
    strata = class
  )
}