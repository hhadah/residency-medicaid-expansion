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

# Save cleaned data
write_dta(long_data, file.path(datasets, "cleaned_residency_medicaid.dta"))

# data by program
program_long_data <- long_data |> 
  group_by(state, institution_code, year) |>
  summarize(
    matched = sum(matched, na.rm = TRUE),
    quota = sum(quota, na.rm = TRUE),
    unmatched = sum(unmatched, na.rm = TRUE),
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

# Save cleaned program-level data
write_dta(program_long_data, file.path(datasets, "cleaned_program_residency_medicaid.dta"))

summary(program_long_data$matched)
table(program_long_data$matched)
