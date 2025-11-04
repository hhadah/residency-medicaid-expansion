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

residency_data %>%
  glimpse()

#-----------------------------------------
# 1. Build hospital/site lookup (unique per institution_code)
#-----------------------------------------
site_lookup <- residency_data %>%
  select(
    institution_code,
    state,
    city,
    institution_name
  ) %>%
  distinct() %>%
  mutate(
    geocode_query = paste(institution_name, city, state, sep = ", ")
  )

#-----------------------------------------
# 2. Geocode using Mapbox (forward geocoding)
#    - You already have a Mapbox token. We'll set it in the env,
#      which tidygeocoder will automatically read.
#-----------------------------------------
Sys.setenv(MAPBOX_API_KEY = "pk.eyJ1IjoiaGhhZGFoIiwiYSI6ImNtaDd1N2NkYzBueGcya29kOWpzMTRqY3oifQ.zpRnaAfLGm06Kz8rs6RuFA")

site_geo_mapbox <- site_lookup %>%
  geocode(
    address = geocode_query,
    method  = "mapbox",
    lat     = latitude,
    long    = longitude,
    full_results = TRUE
  )

# Quick check of geocoding success
site_geo_mapbox %>%
  summarise(
    total_sites      = n(),
    geocoded_sites   = sum(!is.na(latitude) & !is.na(longitude)),
    pct_geocoded     = mean(!is.na(latitude) & !is.na(longitude)) * 100
  )

#-----------------------------------------
# 3. Reverse geocode each successful coordinate
#    to pull ZIP/postal code info
#-----------------------------------------
site_zip_raw <- site_geo_mapbox %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  reverse_geocode(
    lat = latitude,
    long = longitude,
    method = "mapbox",
    address = "rev_address",
    full_results = TRUE
  )

#-----------------------------------------
# 4. Clean & collapse geocoding output
#    - Extract a ZIP code
#    - Keep only lat / lon / zip per institution_code
#    - Drop obviously bad coordinates
#    - Deduplicate (1 row per institution_code)
#-----------------------------------------

geo_lookup <- site_zip_raw %>%
  mutate(
    # Mapbox sometimes returns a column literally named "properties.override:postcode"
    postcode_raw      = .data[["properties.override:postcode"]],
    # Fallback: try to regex a 5-digit ZIP from the freeform reverse address string
    postcode_from_rev = str_extract(rev_address, "\\b\\d{5}\\b"),
    # Choose best available ZIP
    zip_code          = coalesce(postcode_raw, postcode_from_rev)
  ) %>%
  select(
    institution_code,
    latitude,
    longitude,
    zip_code
  ) %>%
  # Keep only plausible CONUS-ish points (this kicks out Spain, ocean, etc.)
  filter(
    longitude > -130, longitude < -60,
    latitude  >  24,  latitude  <  50
  ) %>%
  group_by(institution_code) %>%
  slice(1) %>%        # if multiple matches for same institution_code, keep first
  ungroup()

# Optional sanity check: how many institutions now have clean coords
geo_lookup %>%
  summarise(
    n_institutions          = n(),
    pct_with_zip            = mean(!is.na(zip_code)) * 100,
    pct_with_coords         = mean(!is.na(latitude) & !is.na(longitude)) * 100
  )

#-----------------------------------------
# 5. Merge geocodes (lat/lon/zip) back to the residency_data
#    RESULT: one row per specialty/program/year in residency_data,
#    with hospital coordinates added.
#-----------------------------------------
residency_geo <- residency_data %>%
  left_join(
    geo_lookup,
    by = "institution_code"
  )

residency_geo %>%
  glimpse()

# open medicaid expansion data
medicaid_data <- read_dta(file.path(raw, "expansion_status.dta"))

medicaid_data |> 
  glimpse()

# Merge residency data with medicaid expansion data
merged_data <- residency_geo |> 
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
    ),
    zip_code = zip_code
  )

long_data |> 
  glimpse()

# census data
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
    medicaid_expansion = first(medicaid_expansion),
    zip_code = as.numeric(first(zip_code))
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