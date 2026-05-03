# This script merges residency
# data with hospital level CMS
# mortality data using fuzzy
# string matching on hospital names
# within state

# author: Hussein Hadah
# first created: March 18, 2025
# last updated: March 18, 2026

# load residency data
# residency <- read_dta(file.path(datasets, "cleaned_residency_medicaid.dta"))

# load CMS data (long format — one row per hospital-year)
mortality_data <- read_dta(file.path(raw, "mortality_wide.dta"))

mortality_data |> 
  glimpse()

look_for(mortality_data)        # nice overview: names, labels, and value labels together
