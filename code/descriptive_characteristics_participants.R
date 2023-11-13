# LOAD PACKAGES

library(tidyverse)

# Source HUNT data

source("Thesis Code/pre_analysis_data_preparation.R")

# HUNT 1, no change

full_cleaned_data |> 
  select(sex, bp_syst2_nt1blm, bp_dias2_nt1blm, bmi_nt1blm, birth_year, smo_pack_yrs_x_nt1blq2,
         age) |>
  drop_na() |> 
  summarize(
    mean_age = mean(age),
    sd_age = sd(birth_year),
    mean_bmi = mean(bmi_nt1blm),
    sd_bmi = sd(bmi_nt1blm),
    mean_bp_sys = mean(bp_syst2_nt1blm),
    sd_bp_sys = sd(bp_syst2_nt1blm),
    mean_bp_dia = mean(bp_dias2_nt1blm),
    sd_bp_dia = sd(bp_dias2_nt1blm),
    mean_smo = mean(smo_pack_yrs_x_nt1blq2),
    sd_smo = sd(smo_pack_yrs_x_nt1blq2),
  )

# HUNT 2, no change

full_cleaned_data |> 
  select(sex, bp_syst_mn23_nt2blm, bp_dias_mn23_nt2blm, bmi_nt2blm, age, smo_pack_yrs_x_nt2blq1) |>
  drop_na() |> 
  summarize(
    mean_age = mean(age),
    sd_age = sd(age),
    mean_bmi = mean(bmi_nt2blm),
    sd_bmi = sd(bmi_nt2blm),
    mean_bp_sys = mean(bp_syst_mn23_nt2blm),
    sd_bp_sys = sd(bp_syst_mn23_nt2blm),
    mean_bp_dia = mean(bp_dias_mn23_nt2blm),
    sd_bp_dia = sd(bp_dias_mn23_nt2blm),
    mean_smo = mean(smo_pack_yrs_x_nt2blq1),
    sd_smo = sd(smo_pack_yrs_x_nt2blq1)
  )


# HUNT 3, no change

full_cleaned_data |> 
  select(sex, bp_syst_mn23_nt3blm, bp_dias_mn23_nt3blm, bmi_nt3blm, age, smo_pack_yrs_x_nt3blq1) |>
  drop_na() |> 
  summarize(
    mean_age = mean(age),
    sd_age = sd(age),
    mean_bmi = mean(bmi_nt3blm),
    sd_bmi = sd(bmi_nt3blm),
    mean_bp_sys = mean(bp_syst_mn23_nt3blm),
    sd_bp_sys = sd(bp_syst_mn23_nt3blm),
    mean_bp_dia = mean(bp_dias_mn23_nt3blm),
    sd_bp_dia = sd(bp_dias_mn23_nt3blm),
    mean_smo = mean(smo_pack_yrs_x_nt3blq1),
    sd_smo = sd(smo_pack_yrs_x_nt3blq1)
  )

# HUNT 4, no change

full_cleaned_data |> 
  select(sex, bp_syst_mn23_nt4blm, bp_dias_mn23_nt4blm, bmi_nt4blm, age, smo_pack_yrs_x_nt4blq1) |>
  drop_na() |> 
  summarize(
    mean_age = mean(age),
    sd_age = sd(age),
    mean_bmi = mean(bmi_nt4blm),
    sd_bmi = sd(bmi_nt4blm),
    mean_bp_sys = mean(bp_syst_mn23_nt4blm),
    sd_bp_sys = sd(bp_syst_mn23_nt4blm),
    mean_bp_dia = mean(bp_dias_mn23_nt4blm),
    sd_bp_dia = sd(bp_dias_mn23_nt4blm),
    mean_smo = mean(smo_pack_yrs_x_nt4blq1),
    sd_smo = sd(smo_pack_yrs_x_nt4blq1)
  )
