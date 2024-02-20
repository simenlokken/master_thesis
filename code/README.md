# Code

## Folder Information

There are mainly four scripts that are of interest for the analyses done in this thesis:

1. data_preparation.qmd
2. functions.qmd
3. no_change_in_pa.qmd
4. change_in_pa.qmd

The first scripts loads the data and performs some basic processing. Worth mentioning is that it distinguishes the different HUNT surveys in to separate data frames which are used in the analyses in third script. This script is also the link between the data processing done in Stata and R (some of the processing is done in Stata). It is indicated explicitly when Stata is used instead of R.

The second script contains all the functions I have created and used in the thesis, and this script is especially important for data processing related to the analyses in script 3 (no_change_in_pa.qmd).

The third and fourth script is where I've performed the actual analyses (Cox regressions) but also performed some minor data processing.

## So, how to use these scripts?

I have mainly wrapped everything into functions (such as data processing for each survey, specialized Cox regression functions, etc.) which are located in the **functions.qmd** script. It is a bit of a hassle to store functions in a .qmd file, but for readability I choose that. Use this script actively together with script 1 (data_preparation.qmd) to understand what's been done. Unfortunately, there is no other documentation available than the functions itself at this point. However, I think they are quite intuitive (written in explicit Tidyverse style).