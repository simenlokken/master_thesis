# SOURCE

source("multiadjusted_and_crude_hr_all_surveys_no_change_in_pa.R")

# DATA PREPARATION FOR ALL HUNT SURVEYS, i.e., adding a socioeconomic variable to the data. The earlier data preparation done are sourced in.

# HUNT 1

hunt_1_cleaned_data <- hunt_1_cleaned_data |> 
  rename(education_level = educ_nt1blq2) |> 
  mutate(socioeconomic_class = as.factor(case_when( # Constructing a socioeconomic variable
    education_level %in% c("7-årig folkeskole eller kortere", "9-årig grunnskole", "Real- eller middelskole, grunnskolen 10.år", "Framhalds- eller fortsettelsesskole") ~ "low",
    education_level %in% c("Ett- eller toårig videregående skole", "Artium, økonomisk gymnas eller almenfaglig retning i vgs.") ~ "medium",
    education_level %in% c("Høyskole eller universitet, 4 år eller mer", "Høyskole eller universitet, mindre enn 4 år") ~ "high"))
  )

# HUNT 2

hunt_2_cleaned_data <- hunt_2_cleaned_data |> 
  rename(education_level = educ_nt2blq1) |> 
  mutate(socioeconomic_class = as.factor(case_when(
    education_level %in% c("Grunnskole 7-10 år, framhaldsskole, folkehøgskole") ~ "low",
    education_level %in% c("Artium, øk.gymnas, allmennfaglig retning i videregående skole", "Realskole, middelskole, yrkesskole 1-2 årig videregående skole") ~ "medium",
    education_level %in% c("Høgskole/universitet, 4 år eller mer", "Høgskole/universitet, mindre enn 4 år") ~ "high"))
  )

# HUNT 3

hunt_3_cleaned_data <- hunt_3_cleaned_data |> 
  rename(occupational_type = wor_tit_isco1_nt3bli) |> 
  mutate(socioeconomic_class = as.factor(case_when(
    occupational_type %in% c("Yrker uten krav til utdanning", "Yrker innen jordbruk, skogbruk og fiske", "Prosess- og maskinoperatører, transportarbeidere mv.") ~ "low",
    occupational_type %in% c("Kontor- og kundeserviceyrker", "Salgs-, service- og omsorgsyrker", "Håndverkere o.l.", "Militære yrker og uoppgitt") ~ "medium",
    occupational_type %in% c("Administrative ledere og politikere", "Akademiske yrker", "Yrker med kortere høyskole- og universitetsutdanning og teknikere") ~ "high"))
  )

# HUNT 4

hunt_4_cleaned_data <- hunt_4_cleaned_data |> 
  rename(education_level = educ_nt4blq1) |> 
  mutate(
    socioeconomic_class = as.factor(case_when(
      education_level %in% c("Grunnskole", "Fagbrev eller svennebrev") ~ "low",
      education_level %in% c("3 år i videregående skole", "1-2årig videregående skole") ~ "medium",
      education_level %in% c("Høyskole/universitet, 4 år eller mer", "Høyskole/universitet, mindre enn 4 år") ~ "high"))
  )

# SURVIVAL ANALYSIS

# Creating two functions for easier iteration through all Cox analyses and efficiency. This is why all variable names across the separate surveys were matched, to be able to easily create use functions

run_cox_reg_multi <- function(dataframe, strata) { # Multi-adjusted
  
  result <- dataframe |> 
    group_by({{ strata }}) |> 
    drop_na({{ strata }}) |> 
    nest() |> 
    mutate(test_results = map(.x = data, 
                              .f = ~ coxph(Surv(follow_up_time_in_years, death_all_cause) ~ pa_minutes_per_week +
                                             bp_diastolic + bp_systolic + bmi + packs_of_smoke_per_year +
                                             age + sex, data =.x) |> 
                                broom::tidy(conf.int = TRUE, exponentiate = TRUE))
           
    ) |> 
    unnest(test_results) |> 
    select({{ strata }}, term, estimate, std.error, conf.low, conf.high) |> 
    ungroup()
  
  return(result)
  
} # Multi-adjusted

run_cox_reg_crude <- function(dataframe, strata) { # Crude
  
  result <- dataframe |> 
    group_by({{ strata}}) |> # strata needs to be in double curly brackets so R understands it should look inside the dataframe
    drop_na({{ strata}} ) |> 
    nest() |> 
    mutate(test_results = map(.x = data, 
                              .f = ~ coxph(Surv(follow_up_time_in_years, death_all_cause) ~ pa_minutes_per_week + 
                                             age, data =.x) |> 
                                broom::tidy(conf.int = TRUE, exponentiate = TRUE))
           
    ) |> 
    unnest(test_results) |>  
    select({{ strata }}, term, estimate, std.error, conf.low, conf.high) |> 
    ungroup()
  
  return(result)
  
} # Crude

# HUNT 1, stratified multi-adjusted and crude Cox models

hunt_1_cox_reg_multi_socio_strat <- run_cox_reg_multi(hunt_1_cleaned_data, socioeconomic_class) # Multi-adjusted

hunt_1_cox_reg_crude_socio_strat <- run_cox_reg_crude(hunt_1_cleaned_data, socioeconomic_class) # Crude

# HUNT 2, stratified multi-adjusted and crude models

hunt_2_cox_reg_multi_socio_strat <- run_cox_reg_multi(hunt_2_cleaned_data, socioeconomic_class) # Multi-adjusted

hunt_2_cox_reg_crude_socio_strat <- run_cox_reg_crude(hunt_2_cleaned_data, socioeconomic_class) # Crude

# HUNT 3, stratified multi-adjusted and crude models

hunt_3_cox_reg_multi_socio_strat <- run_cox_reg_multi(hunt_3_cleaned_data, socioeconomic_class) # Multi-adjusted

hunt_3_cox_reg_crude_socio_strat <- run_cox_reg_crude(hunt_3_cleaned_data, socioeconomic_class) # Crude

# HUNT 4, stratified multi-adjusted and crude models

hunt_4_cox_reg_multi_socio_strat <- run_cox_reg_multi(hunt_4_cleaned_data, socioeconomic_class) # Multi-adjusted

hunt_4_cox_reg_crude_socio_strat <- run_cox_reg_crude(hunt_4_cleaned_data, socioeconomic_class) # Crude

# FOLLOW-UP 

# HUNT 1, multi-adjusted and crude

hunt_1_f_up_time_multi_socio_strat <- calculate_follow_up_time_multi(hunt_1_cleaned_data, strata = socioeconomic_class)

hunt_1_f_up_time_crude_socio_strat <- calculate_follow_up_time_crude(hunt_1_cleaned_data, strata = socioeconomic_class)

# HUNT 2, multi-adjusted and crude

hunt_2_f_up_time_multi_socio_strat <- calculate_follow_up_time_multi(hunt_2_cleaned_data, strata = socioeconomic_class)

hunt_2_f_up_time_crude_socio_strat <- calculate_follow_up_time_crude(hunt_2_cleaned_data, strata = socioeconomic_class)

# HUNT 3, multi-adjusted and crude

hunt_3_f_up_time_multi_socio_strat <- calculate_follow_up_time_multi(hunt_3_cleaned_data, strata = socioeconomic_class)

hunt_3_f_up_time_crude_socio_strat <- calculate_follow_up_time_crude(hunt_3_cleaned_data, strata = socioeconomic_class)

# HUNT 4, multi-adjusted and crude

hunt_4_f_up_time_multi_socio_strat <- calculate_follow_up_time_multi(hunt_4_cleaned_data, strata = socioeconomic_class)

hunt_4_f_up_time_crude_socio_strat <- calculate_follow_up_time_crude(hunt_4_cleaned_data, strata = socioeconomic_class)