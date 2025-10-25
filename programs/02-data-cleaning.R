#-----------------------------------------
# This script cleans and prepares
# cleans and prepares the data for analysis
#
# Authors: Hussain Hadah
#
# date: October 26th, 2025
# last updated: October 26th, 2025
#-----------------------------------------

# open residency data
residency_data <- read_dta(file.path(raw, "2010_2019_residency_programs.dta"))

residency_data |> 
  glimpse()
  
# open medicaid expansion data
medicaid_data <- read_dta(file.path(raw, "expansion_status.dta"))

medicaid_data |> 
  glimpse()

# Merge residency data with medicaid expansion data
merged_data <- residency_data |> 
  left_join(medicaid_data, by = c("state"))

merged_data |> 
  glimpse()


# reshape data to long format
long_data <- merged_data |>
  pivot_longer(
    cols = matches("^(quota|matched)_\\d{4}$"),
    names_to = c("type", "year"),
    names_pattern = "(quota|matched)_(\\d{4})",
    values_to = "value"
  ) |>
  pivot_wider(
    names_from = "type",
    values_from = "value"
  ) |> 
  mutate(
    year = as.integer(year),
    quota = as.numeric(quota),
    matched = as.numeric(matched),
    unmatched = quota - matched,
    year_expanded = as.integer(year_expanded),
    medicaid_expansion = case_when(
      !is.na(year_expanded) & year >= year_expanded ~ 1,
      TRUE ~ 0
    )
  )

long_data |> 
  glimpse()

# census data
install.packages("tidycensus")
install.packages("wru")
library(tidycensus)
library(wru)

total_population_10 <- get_decennial(
  geography = "state", 
  variables = "P001001",
  year = 2010
)

# convert state FIPS to state abbreviation
total_population_10 <- total_population_10 |> 
  mutate(
    state = as_state_abbreviation(GEOID)
  )

long_data <- long_data |> 
  left_join(
    total_population_10 |> select(state, total_population_10 = value),
    by = "state"
  )
long_data |>
  glimpse()

# calculate matches, quotas, and unmatches per 100k population
long_data <- long_data |> 
  mutate(
    matched_per_100k = (matched / total_population_10) * 100000,
    quota_per_100k = (quota / total_population_10) * 100000,
    unmatched_per_100k = (unmatched / total_population_10) * 100000
  )

long_data |>
  glimpse()
datasummary_skim(long_data)
# Save cleaned data
write_dta(long_data, file.path(datasets, "cleaned_residency_medicaid.dta"))

# data by program
program_long_data <- long_data |> 
  group_by(state, institution_code, year) |>
  summarize(
    matched = sum(matched, na.rm = TRUE),
    quota = sum(quota, na.rm = TRUE),
    unmatched = sum(unmatched, na.rm = TRUE),
    total_population_10 = first(total_population_10),
    city = first(city),
    expansion_state = first(expansion_state),
    year_expanded = first(year_expanded),
    medicaid_expansion = first(medicaid_expansion)
  ) |> 
  ungroup() |> 
  mutate(
    treated_state = case_when(
      expansion_state == "Yes" ~ 1L,
      expansion_state == "No" ~ 0L,
      TRUE ~ NA_integer_
    ),
    post_expansion = case_when(
      treated_state == 1L & !is.na(year_expanded) & year >= year_expanded ~ 1L,
      TRUE ~ 0L
    ),
    treated_post = treated_state * post_expansion,
    program_id = paste(state, institution_code, sep = "_")
  )
# create per 100k variables
program_long_data <- program_long_data |> 
  mutate(
    matched_per_100k = (matched / total_population_10) * 100000,
    quota_per_100k = (quota / total_population_10) * 100000,
    unmatched_per_100k = (unmatched / total_population_10) * 100000
  )

datasummary_skim(long_data)
# Save cleaned program-level data
write_dta(program_long_data, file.path(datasets, "cleaned_program_residency_medicaid.dta"))
