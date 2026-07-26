# =============================================================================
# 2000-2019 ESTIMATION PANELS -- the datasets ALL Stata analysis runs on
# ---------------------------------------------------------------------------
# Builds, from the wide institution x specialty file created by script 05
# (2000_2019_residency_programs.dta):
#
#   1. panel_2000_2019_estimation.dta   -- institution x year (primary panel)
#   2. panel_2000_2019_specialty.dta    -- institution x specialty-group x year
#   3. output/tables/entry-exit-by-year.csv -- entry/exit by year x expansion
#
# Construction rules (referee-round lessons, applied uniformly):
#   - NA-aware aggregation: an institution-year (or group-year) whose source
#     rows are all missing aggregates to NA, not zero.
#   - Activity-window coding: institution-years outside [first year with any
#     positive quota/matched, last year with any positive] are missing in
#     matched_na/quota_na; zero-filled variants (matched_zf/quota_zf) kept.
#   - Specialty groups come from data/raw/program_simplified.dta
#     (specialty_code -> gen_specialty_alt); the handful of unmapped OCR-era
#     codes (0.05% of total quota) are pooled as "Other".
#   - rural_urban_2010 (RUCA via geocoded ZIP) and the NRMP->CCN crosswalk
#     are merged from the 2010-2019 pipeline; both are unavailable for
#     institutions that exited before 2010 (1.8% of matched volume) and are
#     missing for those rows by construction.
#
# Inputs: 2000_2019_residency_programs.dta (script 05),
#         cleaned_program_residency_medicaid.dta (script 02; state expansion
#           status, 2010 weights, RUCA),
#         data/raw/program_simplified.dta (specialty map),
#         data/datasets/institution_ccn_crosswalk.csv (script 10; optional),
#         state_year_population_2000_2019.dta (script 03)
# =============================================================================

suppressMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readr)
})

topdir   <- here::here()
datasets <- file.path(topdir, "data", "datasets")
rawdir   <- file.path(topdir, "data", "raw")
tabdir   <- file.path(topdir, "output", "tables")

wide <- read_dta(file.path(datasets, "2000_2019_residency_programs.dta"))
prog <- read_dta(file.path(datasets, "cleaned_program_residency_medicaid.dta"))
spec_map <- read_dta(file.path(rawdir, "program_simplified.dta")) |>
  distinct(specialty_code, gen_specialty_alt)

sum_na <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)

# ---- long at institution x specialty x year --------------------------------
long_spec <- wide |>
  select(state, institution_code, specialty_code, matches("^(quota|matched)_20")) |>
  pivot_longer(matches("^(quota|matched)_20"),
               names_to = c(".value", "year"),
               names_pattern = "(quota|matched)_(\\d{4})") |>
  mutate(year = as.integer(year)) |>
  left_join(spec_map, by = "specialty_code") |>
  mutate(gen_specialty_alt = if_else(is.na(gen_specialty_alt), "Other", gen_specialty_alt))

n_other <- long_spec |> filter(gen_specialty_alt == "Other") |> distinct(specialty_code) |> nrow()
cat("Specialty codes pooled as 'Other':", n_other, "\n")

# ---- institution x year -----------------------------------------------------
long <- long_spec |>
  group_by(state, institution_code, year) |>
  summarize(quota = sum_na(quota), matched = sum_na(matched), .groups = "drop")

stopifnot(nrow(long) == dplyr::n_distinct(long$institution_code) * 20)

windows <- long |>
  filter((quota > 0 | matched > 0) & !(is.na(quota) & is.na(matched))) |>
  group_by(institution_code) |>
  summarize(first_active = min(year), last_active = max(year), .groups = "drop")

panel <- long |>
  inner_join(windows, by = "institution_code") |>
  mutate(
    in_window  = year >= first_active & year <= last_active,
    matched_na = if_else(in_window, matched, NA_real_),
    quota_na   = if_else(in_window, quota,   NA_real_),
    matched_zf = coalesce(matched, 0),
    quota_zf   = coalesce(quota,   0),
    sas_window_entrant = first_active >= 2016,
    balanced_full = first_active <= 2000 & last_active >= 2019
  )

cat("Institutions:", n_distinct(panel$institution_code),
    "| institution-years:", nrow(panel),
    "| outside activity window:", sum(!panel$in_window),
    sprintf("(%.1f%%)\n", 100 * mean(!panel$in_window)))

# ---- state-level attributes + institution-level merges ---------------------
states <- prog |>
  distinct(state, year_expanded, treated_state, total_population_10) |>
  mutate(state = toupper(trimws(state)))
stopifnot(nrow(states) == n_distinct(states$state))

ruca <- prog |>
  distinct(institution_code, rural_urban_2010)

ccn_path <- file.path(datasets, "institution_ccn_crosswalk.csv")
ccn <- if (file.exists(ccn_path)) {
  read_csv(ccn_path, show_col_types = FALSE) |>
    distinct(institution_code, provider_ccn, match_source)
} else {
  cat("NOTE: institution_ccn_crosswalk.csv not found; provider_ccn left unmerged.\n")
  tibble(institution_code = double(), provider_ccn = character(), match_source = character())
}

pop <- read_dta(file.path(datasets, "state_year_population_2000_2019.dta"))

panel <- panel |>
  mutate(state = toupper(trimws(state))) |>
  left_join(states, by = "state") |>
  left_join(ruca,   by = "institution_code") |>
  left_join(ccn,    by = "institution_code") |>
  left_join(pop,    by = c("state", "year"))

n_nostate <- sum(is.na(panel$total_population_10))
cat("Rows without expansion/weight merge (dropped):", n_nostate, "\n")
panel <- panel |> filter(!is.na(total_population_10))
stopifnot(!any(is.na(panel$pop_yr)))
cat("RUCA available for",
    n_distinct(panel$institution_code[!is.na(panel$rural_urban_2010)]), "institutions;",
    "CCN link for", n_distinct(panel$institution_code[!is.na(panel$provider_ccn)]), "\n")

write_dta(panel, file.path(datasets, "panel_2000_2019_estimation.dta"))
cat("Wrote", file.path(datasets, "panel_2000_2019_estimation.dta"), "\n")

# ---- specialty-group x institution x year panel ----------------------------
spec_panel <- long_spec |>
  group_by(state, institution_code, gen_specialty_alt, year) |>
  summarize(quota = sum_na(quota), matched = sum_na(matched), .groups = "drop") |>
  inner_join(windows, by = "institution_code") |>
  mutate(
    in_window  = year >= first_active & year <= last_active,
    matched_na = if_else(in_window, matched, NA_real_),
    quota_na   = if_else(in_window, quota,   NA_real_)
  ) |>
  mutate(state = toupper(trimws(state))) |>
  left_join(states, by = "state") |>
  left_join(pop,    by = c("state", "year")) |>
  filter(!is.na(total_population_10))

write_dta(spec_panel, file.path(datasets, "panel_2000_2019_specialty.dta"))
cat("Wrote", file.path(datasets, "panel_2000_2019_specialty.dta"),
    "(", nrow(spec_panel), "rows )\n")

# ---- entry/exit table by year x expansion status ---------------------------
inst <- panel |>
  distinct(institution_code, first_active, last_active, sas_window_entrant, treated_state)

entry_exit_table <- bind_rows(
  inst |> filter(first_active > 2000) |>
    count(year = first_active, treated_state, sas_window_entrant) |>
    mutate(margin = "entry"),
  inst |> filter(last_active < 2019) |>
    count(year = last_active, treated_state) |>
    mutate(margin = "exit", sas_window_entrant = NA)
) |>
  arrange(margin, year, treated_state)

write_csv(entry_exit_table, file.path(tabdir, "entry-exit-by-year.csv"))
cat("Wrote", file.path(tabdir, "entry-exit-by-year.csv"), "\n")
