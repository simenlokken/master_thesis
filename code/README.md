# Code

All scripts for the analyses has an intuitive file name. For example, the script that contains Cox regressions without considering change in LTPA for all HUNT surveys is called **multiadjusted_and_crude_hr_all_surveys_no_change_in_pa.R**.

I have mainly wrapped everything into functions (such as data processing for each survey, specialized Cox regression functions, etc.) which are located in the **functions.R** script. To understand what's been done, use this script actively. Each script needs to call these functions before it can run. The functions are rather intuitive, and as of now, there are no documentation rather than the comments in the script itself. 