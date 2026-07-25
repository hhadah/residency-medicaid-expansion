#-----------------------------------------
# This script cleans and prepares
# cleans and prepares the data for analysis
#
# Authors: Hussain Hadah
#
# date: October 26th, 2025
# last updated: January 27th, 2025
#-----------------------------------------

# open residency data
residency_data <- read_dta(file.path(raw, "2010_2019_residency_programs.dta"))

residency_data |>
  glimpse()

#-----------------------------------------
# 1. Build hospital/site lookup (unique per institution_code)
#-----------------------------------------
hospitals_lookup <- residency_data |>
  select(
    institution_code,
    institution_name,
    city,
    state
  ) |>
  distinct()

site_lookup <- hospitals_lookup |>
  mutate(
    # --- IMPROVEMENT 1: Programmatic Name Cleaning ---
    # Standardize names to help geocoder avoid bad matches (like MD Anderson in Crockett, TX).
    institution_name_clean = institution_name |>
      str_replace("^U ", "University of ") |>       # Fixes "U Texas" -> "University of Texas"
      str_replace_all(c(
        "\\bCtr\\b" = "Center",                     # Fixes "Med Ctr"
        "\\bHosp\\b" = "Hospital",                  # Fixes "Gen Hosp"
        "\\bMed\\b" = "Medical",                    # Fixes "Med Ctr"
        "\\bSyst?\\b" = "System"
      )),
    
    # Use the cleaned name for the query
    geocode_query = paste(institution_name_clean, city, state, sep = ", ")
  )

#-----------------------------------------
# 2. Geocode using Mapbox (forward geocoding)
#-----------------------------------------
# MAPBOX_API_KEY must be set in ~/.Renviron (same convention as
# CENSUS_API_KEY in 03-state-year-population.R). Never hardcode it here.
if (Sys.getenv("MAPBOX_API_KEY") == "") {
  stop("MAPBOX_API_KEY is not set. Add it to ~/.Renviron and restart R.")
}

site_geo_mapbox <- site_lookup |>
  geocode(
    address = geocode_query,
    method  = "mapbox",
    lat     = latitude,
    long    = longitude,
    full_results = TRUE
  )

# Quick check of geocoding success
site_geo_mapbox |>
  summarise(
    total_sites      = n(),
    geocoded_sites   = sum(!is.na(latitude) & !is.na(longitude)),
    pct_geocoded     = mean(!is.na(latitude) & !is.na(longitude)) * 100
  )

#-----------------------------------------
# 3. Reverse geocode each successful coordinate
#-----------------------------------------
site_zip_raw <- site_geo_mapbox |>
  filter(!is.na(latitude) & !is.na(longitude)) |>
  reverse_geocode(
    lat = latitude,
    long = longitude,
    method = "mapbox",
    address = "rev_address",
    full_results = FALSE
  )

site_zip_raw |>
  glimpse()

#-----------------------------------------
# 4. Clean & collapse geocoding output
#-----------------------------------------

geo_lookup <- site_zip_raw |>
  mutate(
    # --- IMPROVEMENT 2: Robust Zip Extraction (Fix for 'No zip codes added') ---
    # Strategy: Extract ALL 5-digit sequences from the address string.
    # Logic: In an address like "21855 Oxnard St, Woodland Hills, California 91367",
    #        the street number (21855) comes first, and the Zip (91367) comes last.
    #        So we simply take the LAST 5-digit number found.
    
    # 1. Find all 5-digit matches
    zip_matches = str_extract_all(rev_address, "\\b\\d{5}\\b"),
    
    # 2. Take the last match from the list (or NA if none found)
    zip_code = map_chr(zip_matches, function(x) {
      if (length(x) > 0) tail(x, 1) else NA_character_
    })
  ) |>
  select(
    institution_code,
    latitude,
    longitude,
    zip_code
  ) |>
  # Keep only plausible CONUS-ish points
  filter(
    longitude > -130, longitude < -60,
    latitude  >  24,  latitude  <  50
  ) |>
  group_by(institution_code) |>
  slice(1) |>        # if multiple matches for same institution_code, keep first
  ungroup()

# Optional sanity check
geo_lookup |>
  summarise(
    n_institutions          = n(),
    pct_with_zip            = mean(!is.na(zip_code)) * 100,
    pct_with_coords         = mean(!is.na(latitude) & !is.na(longitude)) * 100
  )

# merge with hospital lookup to see city/state
hospitals_zip <- geo_lookup |>
  left_join(
    hospitals_lookup,
    by = "institution_code"
  ) |>
  select(institution_code, institution_name, city, state, latitude, longitude, zip_code)

hospitals_zip |>
  glimpse()  

# Manually enter zip codes for any missing ones (if known)
hospitals_zip <- hospitals_zip |>
  mutate(
    zip_code = case_when(
      institution_code == 1023 ~ "91206",  # Glendale Adventist Med Ctr
      institution_code == 1572 ~ "93291",  # Kaweah Delta Health Care District
      institution_code == 3069 ~ "81501",  # St Marys Hospital
      institution_code == 3120 ~ "02301",  # Harvard South Shore
      TRUE ~ zip_code
    )
  )
write_csv(hospitals_zip, file.path(raw, "hospitals_geocoded.csv"))

#-----------------------------------------
# 5. Merge geocodes (lat/lon/zip) back to the residency_data
#    RESULT: one row per specialty/program/year in residency_data,
#    with hospital coordinates added.
#-----------------------------------------
residency_geo <- residency_data |>
  left_join(
    geo_lookup,
    by = "institution_code"
  )

residency_geo |>
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

#-----------------------------------------
# Merge residency data with rural-urban
# classification data
#-----------------------------------------
ruca_2020_data <-  read_csv(file.path(raw, "ruca-2020.csv"))

ruca_2010_data <- read_csv(file.path(raw, "ruca-2010.csv"))

names(ruca_2010_data)
names(ruca_2020_data)

# rename columns in ruca 2010
ruca_2010_data <- ruca_2010_data |> 
  rename(
    zip_code = ZIP_CODE,
    ruca_1 = RUCA1,
    ruca_2 = RUCA2
  )

# rename columns in ruca 2020
ruca_2020_data <- ruca_2020_data |> 
  rename(
    zip_code = ZIPCode,
    ruca_1 = PrimaryRUCA,
    ruca_2 = SecondaryRUCA
  )

# select relevant columns
# and append data
# add year column
ruca_2010_data <- ruca_2010_data |> 
  select(zip_code, ruca_1, ruca_2) |> 
  mutate(year = 2010)

ruca_2020_data <- ruca_2020_data |> 
  select(zip_code, ruca_1, ruca_2) |> 
  mutate(year = 2020)

ruca_data <- bind_rows(ruca_2010_data, ruca_2020_data)
ruca_data |> 
  glimpse()

ruca_merged <- ruca_data |>
  pivot_wider(
    names_from = year,
    values_from = c(ruca_1, ruca_2),
    names_glue = "{.value}_{year}" # Creates names like ruca1_2010, ruca1_2020
  ) |> 
  arrange(zip_code)
# merge with long_data
long_data <- long_data |> 
  left_join(
    ruca_merged,
    by = "zip_code"
  )

#-----------------------------------------
# Merge with specialty classification
# simplified crosswalk. Merge crosswalk
# using `specialty_code` variable
#-----------------------------------------
specialty_crosswalk <- read_dta(file.path(raw, "program_simplified.dta")) |> 
  select(
    specialty_code,
    gen_specialty_alt
  )
specialty_crosswalk |> 
  glimpse()

# merge with long_data
long_data <- long_data |> 
  left_join(
    specialty_crosswalk,
    by = "specialty_code"
  )

long_data |> 
  glimpse()

#-----------------------------------------
# Count NA values in ruca_1_2010 and 
# gen_specialty_alt
#-----------------------------------------
na_ruca_2010 <- sum(is.na(long_data$ruca_1_2010))
na_specialty <- sum(is.na(long_data$gen_specialty_alt))

#-----------------------------------------
# Save cleaned data
#-----------------------------------------
write_dta(long_data, file.path(datasets, "cleaned_residency_medicaid.dta"))

#-----------------------------------------
# Create aggregated data by program
#-----------------------------------------

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
    gen_specialty_alt = first(gen_specialty_alt),
    rural_urban_2010 = first(ruca_1_2010),
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