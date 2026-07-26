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

topdir  <- here::here()
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

# ---------------------------------------------------------------------------
# Extension (2026-07-25, long-panel module scripts 90-92): a 2000-2019 series
# for the 10-pre-period event study and the pre-ACA placebo. ACS 1-year does
# not exist before 2005, so: decennial 2000 (P001001) anchors 2000, ACS 1-year
# covers 2005-2019, and 2001-2004 are linear interpolations between the 2000
# decennial and 2005 ACS values (disclosed in the figure notes).
# Output: data/datasets/state_year_population_2000_2019.dta
# ---------------------------------------------------------------------------
message("Building 2000-2019 series (decennial 2000 + interpolation + ACS 1-year) ...")

dec2000 <- get_decennial(geography = "state", variables = "P001001", year = 2000) %>%
  transmute(state = unname(name_to_abb[NAME]), year = 2000L, pop_yr = value) %>%
  filter(!is.na(state))

acs_0509 <- map_dfr(2005:2009, function(y) {
  df <- try(get_acs(geography = "state", variables = "B01003_001",
                    year = y, survey = "acs1"), silent = TRUE)
  if (inherits(df, "try-error")) {
    warning("ACS pull failed for ", y)
    return(NULL)
  }
  df %>% transmute(state = unname(name_to_abb[NAME]), year = y, pop_yr = estimate)
}) %>% filter(!is.na(state))

interp <- dec2000 %>%
  select(state, p2000 = pop_yr) %>%
  inner_join(acs_0509 %>% filter(year == 2005) %>% select(state, p2005 = pop_yr),
             by = "state") %>%
  tidyr::crossing(year = 2001:2004) %>%
  mutate(pop_yr = p2000 + (p2005 - p2000) * (year - 2000) / 5) %>%
  select(state, year, pop_yr)

pop_long <- bind_rows(dec2000, interp, acs_0509, pop) %>%
  arrange(state, year)

stopifnot(nrow(pop_long) == dplyr::n_distinct(pop_long$state) * 20)
out2 <- file.path(datasets, "state_year_population_2000_2019.dta")
write_dta(pop_long, out2)
message("Wrote ", out2, " (", nrow(pop_long), " rows)")
