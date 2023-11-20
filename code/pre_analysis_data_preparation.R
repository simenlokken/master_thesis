## This script has to be run before running any other scripts related to the thesis.

## The script loads the data from a private NTNU (NICE) directory and cleans it before resetting working directory.
## This does not clean the data for specific analyses, that is done in each pipeline and not saved to the full dataset.

prepare_data <- function() {
    
  # SET WORKING DIRECTORY TO NICE AREA
  
  setwd(Sys.getenv("MASTER_THESIS_FILE_DIRECTORY"))
  
  # LOAD DATA
  
  full_data <- readr::read_csv("full_data.csv")
  
  # CLEAN DATA
  
  # Un-capitalize column names
  
  full_cleaned_data <- full_data |> 
    janitor::clean_names()
  
  # Change all variables to the correct type (i.e., factor, int, dbl etc.)
  
  pre_cleaning_variable_types <- map(full_cleaned_data, class) # Iterates through all columns and retrieves class
  
  full_cleaned_data <- full_cleaned_data |>
    mutate_if(is.character, as.factor)
  
  # Manually change those few columns who should in fact be character or dates back again, while manually factor some doubles
  
  list_of_unique_values_for_each_variable <- map(full_cleaned_data, unique)
  
  full_cleaned_data <- full_cleaned_data |> 
    mutate(birth_date = as.character(birth_date), 
           end_date_death = as.character(end_date_death),
           end_date_icd8910 = as.character(end_date_icd8910),
           end_date_link = as.character(end_date_link),
           w22_0389_lopenr_person = as.character(w22_0389_lopenr_person),
           death_all = as.factor(death_all),
           death_all_icd = as.factor(death_all_icd),
           death_cvd = as.factor(death_cvd),
           death_ihd = as.factor(death_ihd),
           death_stroke = as.factor(death_stroke),
           death_ihd = as.factor(death_ihd))
  
  # Change all date variables to date format
  
  full_cleaned_data <- full_cleaned_data |> 
    mutate(part_dat_nt1blq1 = dmy(part_dat_nt1blq1),
           part_dat_nt2blq1 = dmy(part_dat_nt2blq1),
           part_dat_nt3blq1 = dmy(part_dat_nt3blq1),
           part_dat_nt4blq1 = dmy(part_dat_nt4blq1),
           end_date_death = dmy(end_date_death),
           end_date_link = dmy(end_date_link),
           end_date_icd8910 = dmy(end_date_icd8910),
           eof_date_death = dmy(eof_date_death)
    )
  
  # Compute participant age
  
  full_cleaned_data <- full_cleaned_data |> 
    mutate(age = year(end_date_death) - birth_year)
  
  # RESET WORKING DIRECTORY
  
  setwd(Sys.getenv("MASTER_THESIS_WORKING_DIRECTORY"))
  
  # Returns the cleaned data frame

  return(full_cleaned_data)
  
}

full_cleaned_data <- prepare_data()
