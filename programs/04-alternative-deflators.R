# =============================================================================
# Alternative state x year deflators (ACS 1-year, 2010-2019)
# ---------------------------------------------------------------------------
# Referee response (editorial decision 2026-07-24, MUST-5 / cluster F4). The
# headline outcome divides by contemporary total population, which is itself
# plausibly post-treatment (interstate migration). The methods referee asks for
# the effect under at least three alternative deflators. This script pulls two
# demographic alternatives from the ACS:
#   - pop_1864    : population aged 18-64 (B01001 age-sex bins)
#   - pop_u150fpl : population below 150% of the federal poverty line
#                   (C17002; 150% is the closest ACS cut to the 138% Medicaid
#                   expansion threshold, noted as such in the paper)
# A non-demographic scale (state GDP / total inpatient discharges) is flagged
# in data/raw/state-gdp-README.md pending a BEA pull.
# Uses tidycensus with CENSUS_API_KEY (already in ~/.Renviron), as 03-state-year-population.R.
#
# Output: data/datasets/state_year_deflators.dta (state, year, pop_1864, pop_u150fpl)
# =============================================================================

suppressMessages({
  library(tidycensus)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(haven)
})

topdir   <- here::here()
datasets <- file.path(topdir, "data", "datasets")

years <- 2010:2019
name_to_abb <- c(setNames(state.abb, state.name), "District of Columbia" = "DC")

# B01001 bins covering ages 18-64: male _007.._019, female _031.._043
vars_1864 <- sprintf("B01001_%03d", c(7:19, 31:43))
# C17002 income-to-poverty bins under 1.50: _002 (<.50) .. _005 (1.25-1.49)
vars_fpl  <- sprintf("C17002_%03d", 2:5)

pull_year <- function(y) {
  df <- try(
    get_acs(geography = "state", variables = c(vars_1864, vars_fpl),
            year = y, survey = "acs1"),
    silent = TRUE
  )
  if (inherits(df, "try-error")) {
    warning("ACS pull failed for ", y, ": ", attr(df, "condition")$message)
    return(NULL)
  }
  df |>
    mutate(group = if_else(variable %in% vars_1864, "pop_1864", "pop_u150fpl")) |>
    group_by(NAME, group) |>
    summarise(value = sum(estimate), .groups = "drop") |>
    pivot_wider(names_from = group, values_from = value) |>
    transmute(state = unname(name_to_abb[NAME]), year = y, pop_1864, pop_u150fpl)
}

message("Pulling ACS 1-year alternative deflators for ", min(years), "-", max(years), " ...")
defl <- map_dfr(years, pull_year) |> filter(!is.na(state))

message("Rows: ", nrow(defl), " | states: ", n_distinct(defl$state),
        " | years: ", paste(range(defl$year), collapse = "-"))
stopifnot(nrow(defl) > 0)

out <- file.path(datasets, "state_year_deflators.dta")
write_dta(defl, out)
message("Wrote ", out)
