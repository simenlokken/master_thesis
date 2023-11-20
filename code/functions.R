## This script contains all the functions used for the analyses. The analyses scripts may also contain other functions, but these are smaller.

# DATA PROCESSING FUNCTIONS

# These functions prepares the data for each HUNT survey for the analyses related to change in LTPA

process_hunt_1 <- function(dataframe) {
  
  dataframe |> 
    
    # Select all variables 
    
    select(contains(match = "nt1"), age, sex, death_all, end_date_death) |> 
    
    # Rename HUNT 1 variables
    
    rename(
      exercise_duration_h1 = exe_du_nt1blq2, 
      exercise_frequency_per_week_h1 = exe_f_nt1blq2,
      death_all_cause = death_all,
      participation_date_h1 = part_dat_nt1blq1,
      bp_diastolic_h1 = bp_dias2_nt1blm,
      bp_systolic_h1 = bp_syst2_nt1blm,
      bmi_h1 = bmi_nt1blm,
      packs_of_smoke_per_year_h1 = smo_pack_yrs_x_nt1blq2
      
      # Mutate HUNT 1 variables
      
    ) |> 
    drop_na( # To be able to use these variables inside the coming mutate function
      exercise_duration_h1, exercise_frequency_per_week_h1, age, death_all_cause,
      sex, participation_date_h1
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
      follow_up_time_in_years_h1 = round(as.numeric(interval(participation_date_h1, end_date_death) / dyears(1)), 1),
      death_all_cause = as.numeric(death_all_cause), # Surv() input needs to be numeric
      packs_of_smoke_per_year_h1 = ifelse(is.na(packs_of_smoke_per_year_h1), 0, packs_of_smoke_per_year_h1) # Compute all smoking NA's to 0
    )
  
}

process_hunt_2 <- function(dataframe) {
  
  dataframe |> 
    
    # Select relevant variables
    
    select(contains(match = "nt2"), age, sex, death_all, end_date_death) |>
    
    # HUNT 2 renaming of variables
    
    rename(
      exercise_time_per_week_h2 = exe_lig_du_ly_nt2blq1,
      participation_date_h2 = part_dat_nt2blq1,
      death_all_cause = death_all,
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
      pa_hrs_per_week_h2 = case_when(
        exercise_time_per_week_h2 == "Ingen" ~ 0,
        exercise_time_per_week_h2 == "Under 1 time" ~ 0.5,
        exercise_time_per_week_h2 == "1-2 timer" ~ 1.5,
        exercise_time_per_week_h2 == "3 timer eller mer" ~ 3.5
      ),
      follow_up_time_in_years_h2 = round(as.numeric(interval(participation_date_h2, end_date_death) / dyears(1)), 1),
      packs_of_smoke_per_year_h2 = ifelse(is.na(packs_of_smoke_per_year_h2), 0, packs_of_smoke_per_year_h2),
      death_all_cause = as.numeric(death_all_cause)
    )
}

process_hunt_3 <- function(dataframe) {
  
  dataframe |> 
    
    # Select relevant variables
    
    select(contains(match = "nt3"), age, sex, death_all, end_date_death) |>
    
    # Rename HUNT 3 variables
    
    rename(
      exercise_duration_h3 = exe_du_nt3blq1,
      exercise_frequency_per_week_h3 = exe_f_nt3blq1,
      participation_date_h3 = part_dat_nt3blq1,
      death_all_cause = death_all,
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
      pa_hrs_per_week_h3 = (minutes_duration_each_exercise_bout_h3 * frequency_per_week_h3) / 60,
      follow_up_time_in_years_h3 = round(as.numeric(interval(participation_date_h3, end_date_death) / dyears(1)), 1),
      death_all_cause = as.numeric(death_all_cause),
      packs_of_smoke_per_year_h3 = ifelse(is.na(packs_of_smoke_per_year_h3), 0, packs_of_smoke_per_year_h3)
    )
  
}

process_hunt_4 <- function(dataframe) {
  
  dataframe |> 
    
    # Select relevant variables
    
    select(contains(match = "nt4"), age, sex, death_all, end_date_death) |>
    
    # Rename HUNT 4 variables
    
    rename(
      exercise_duration_h4 = exe_du_nt4blq1,
      exercise_frequency_per_week_h4 = exe_f_nt4blq1,
      death_all_cause = death_all,
      participation_date_h4 = part_dat_nt4blq1,
      bp_diastolic_h4 = bp_dias_mn23_nt4blm,
      bp_systolic_h4 = bp_syst_mn23_nt4blm,
      bmi_h4 = bmi_nt4blm,
      packs_of_smoke_per_year_h4 = smo_pack_yrs_x_nt4blq1
    ) |>
    
    # Mutate HUNT 4 variables
    
    drop_na( # To be able to use these variables inside the coming mutate function
      exercise_duration_h4, exercise_frequency_per_week_h4, age, death_all_cause,
      sex, participation_date_h4
    ) |>
    mutate(
      minutes_duration_each_exercise_bout = case_when(
        exercise_duration_h4 == "Mindre enn 15 minutter" ~ 7.5,
        exercise_duration_h4 == "15-29 minutter" ~ 22.5,
        exercise_duration_h4 == "30-60 minutter" ~ 45,
        exercise_duration_h4 == "Mer enn 60 minutter" ~ 75
      ),
      frequency_per_week = case_when(
        exercise_frequency_per_week_h4 == "En gang i uka" ~ 1,
        exercise_frequency_per_week_h4 == "2-3 ganger i uka" ~ 2.5,
        exercise_frequency_per_week_h4 == "Omtrent hver dag" ~ 5
      ),
      pa_hrs_per_week_h4 = (minutes_duration_each_exercise_bout * frequency_per_week) / 60,
      follow_up_time_in_years_h4 = round(as.numeric(interval(participation_date_h4, end_date_death) / dyears(1)), 1),
      death_all_cause = as.numeric(death_all_cause),
      packs_of_smoke_per_year_h4 = ifelse(is.na(packs_of_smoke_per_year_h4), 0, packs_of_smoke_per_year_h4)
    )
  
}

# COX REGRESSION FUNCTION

# This function performs a Cox regression and puts it into a tidied format

run_cox_reg <- function(dataframe, strata = NULL, follow_up_time, covariates) {
  
  result <- dataframe |> 
    group_by({{strata}}) |> 
    drop_na({{strata}}) |> 
    nest() |> 
    mutate(test_results = map(.x = data, 
                              .f = ~ coxph(Surv({{follow_up_time}}, death_all_cause) ~ covariates, data =.x) |> 
                                broom::tidy(conf.int = TRUE, exponentiate = TRUE))
           
    ) |> 
    unnest(test_results) |> 
    select(strata, term, estimate, std.error, conf.low, conf.high) |> 
    ungroup()
  
  return(result)
  
}

run_cox_reg_crude <- function(dataframe, strata = NULL, follow_up_time) {
  
  result <- dataframe |> 
    group_by({{strata}}) |> 
    drop_na({{strata}}) |> 
    nest() |> 
    mutate(test_results = map(.x = data,
                              .f = ~ coxph(Surv({{follow_up_time}}, death_all_cause) ~ {{covariates}},
                                           data = .x) |> 
                                broom::tidy(exponentiate = TRUE, conf.int = TRUE))
           ) |> 
    unnest(test_results) |> 
    select({{strata}}, term, estimate, std.error, conf.low, conf.high) |> 
    ungroup()
  
}

# FOLLOW-UP TIME CALCULATION FUNCTION

# This function performs a follow-up time calculation

calculate_follow_up_time <- function(dataframe, covariates, strata = NULL, end_date_death, participation_date) {
  
  dataframe |> 
    select(({{covariates}}), {{participation_date}}, {{end_date_death}}) |> 
    drop_na() |> 
    mutate(person_years = (end_date_death - {{participation_date}}) / 365) |> 
    summarize(person_years = sum(person_years)) |> 
    pull()
  
}