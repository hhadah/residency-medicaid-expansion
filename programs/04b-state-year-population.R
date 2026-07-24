# =============================================================================
# Build a STATE x YEAR population panel (ACS 1-year, 2010-2019)
# ---------------------------------------------------------------------------
# The cleaned residency panel scales matched positions by the 2010 decennial
# state population -- a time-invariant denominator that is absorbed by the
# hospital fixed effects, so "per 100,000" and "levels" differ only through the
# population weights, not through any real population normalization. This pulls
# a year-varying state population so the outcome can be re-scaled by contemporary
# population as a robustness check (referee comment C4). Uses tidycensus with
# CENSUS_API_KEY (already in ~/.Renviron).
#
# Output: data/datasets/state_year_population.dta  (state, year, pop_yr)
# =============================================================================

suppressMessages({
  library(tidycensus)
  library(dplyr)
  library(purrr)
  library(haven)
})

topdir  <- "/Users/hhadah/Projects/GiT/residency-medicaid-expansion"
datasets <- file.path(topdir, "data", "datasets")
dir.create(datasets, showWarnings = FALSE, recursive = TRUE)

years <- 2010:2019

# Full state name -> 2-letter abbreviation (residency panel uses abbreviations)
name_to_abb <- c(setNames(state.abb, state.name), "District of Columbia" = "DC")

message("Pulling ACS 1-year total population (B01003_001) for ", min(years), "-", max(years), " ...")

pop <- map_dfr(years, function(y) {
  df <- try(
    get_acs(geography = "state", variables = "B01003_001",
            year = y, survey = "acs1"),
    silent = TRUE
  )
  if (inherits(df, "try-error")) {
    warning("ACS pull failed for ", y, ": ", attr(df, "condition")$message)
    return(NULL)
  }
  df %>%
    transmute(state = unname(name_to_abb[NAME]),
              year = y,
              pop_yr = estimate)
})

pop <- pop %>% filter(!is.na(state))

message("Rows: ", nrow(pop), " | states: ", dplyr::n_distinct(pop$state),
        " | years: ", paste(range(pop$year), collapse = "-"))
stopifnot(nrow(pop) > 0)

out <- file.path(datasets, "state_year_population.dta")
write_dta(pop, out)
message("Wrote ", out)
