# =============================================================================
# Entry/exit panel: undo the zero-fill at the extensive margin
# ---------------------------------------------------------------------------
# Referee response (editorial decision 2026-07-24, MUST-7 / cluster F6). The
# estimation panel is exactly balanced (859 x 10) because aggregation zero-
# filled institution-years absent from the NRMP report. The raw wide file
# (data/raw/2010_2019_residency_programs.dta) is also zero-filled, but it
# carries `hospital_first_appears` and `program_first_offered`, which identify
# the entry margin directly. This script:
#   1. Computes each institution's first-appearance year and last active year
#      (last year with any positive quota or matched across its programs).
#   2. Produces an entry/exit table by year x expansion status, flagging
#      Single Accreditation System-window entrants (2016-2019) as the closest
#      available proxy for formerly-AOA programs (no public AOA list merged
#      yet -- see data/raw/acgme-validation-README.md).
#   3. Writes a program-panel variant where institution-years OUTSIDE the
#      [first_appears, last_active] window are coded missing (not zero);
#      zeros INSIDE the window are retained as genuine zeros.
#
# Outputs: data/datasets/program_panel_entry_exit.dta
#          output/tables/entry-exit-by-year.csv
# =============================================================================

suppressMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readr)
})

topdir   <- "/Users/hhadah/Projects/GiT/residency-medicaid-expansion"
datasets <- file.path(topdir, "data", "datasets")
tabdir   <- file.path(topdir, "output", "tables")

raw  <- read_dta(file.path(topdir, "data", "raw", "2010_2019_residency_programs.dta"))
prog <- read_dta(file.path(datasets, "cleaned_program_residency_medicaid.dta"))

# ---- long totals per institution-year (from the raw wide file) --------------
long <- raw |>
  select(state, institution_code, matches("^(quota|matched)_20")) |>
  pivot_longer(matches("^(quota|matched)_20"),
               names_to = c(".value", "year"), names_pattern = "(quota|matched)_(\\d{4})") |>
  mutate(year = as.integer(year)) |>
  group_by(state, institution_code, year) |>
  summarize(quota = sum(quota, na.rm = TRUE),
            matched = sum(matched, na.rm = TRUE), .groups = "drop")

inst <- raw |>
  group_by(institution_code) |>
  summarize(first_appears = min(hospital_first_appears, na.rm = TRUE), .groups = "drop")

last_active <- long |>
  filter(quota > 0 | matched > 0) |>
  group_by(institution_code) |>
  summarize(last_active = max(year), .groups = "drop")

windows <- inst |>
  left_join(last_active, by = "institution_code") |>
  mutate(last_active = if_else(is.na(last_active), 2010L, as.integer(last_active)),
         sas_window_entrant = first_appears >= 2016)

cat("Institutions:", nrow(windows),
    "| entering after 2010:", sum(windows$first_appears > 2010),
    "| exiting before 2019:", sum(windows$last_active < 2019),
    "| SAS-window entrants (2016+):", sum(windows$sas_window_entrant), "\n")

# ---- entry/exit table by year x expansion status ---------------------------
exp_status <- prog |>
  distinct(institution_code, treated_state)

ee <- windows |>
  left_join(exp_status, by = "institution_code") |>
  left_join(long |> group_by(institution_code) |>
              summarize(total_matched_2019 = matched[year == 2019][1], .groups = "drop"),
            by = "institution_code")

entry_exit_table <- bind_rows(
  ee |> filter(first_appears > 2010) |>
    count(year = first_appears, treated_state, sas_window_entrant) |>
    mutate(margin = "entry"),
  ee |> filter(last_active < 2019) |>
    count(year = last_active, treated_state) |>
    mutate(margin = "exit", sas_window_entrant = NA)
) |>
  arrange(margin, year, treated_state)

# positions at entering institutions, by year of entry
entrant_positions <- ee |>
  filter(first_appears > 2010) |>
  group_by(treated_state) |>
  summarize(n_entrants = n(),
            matched_2019_at_entrants = sum(total_matched_2019, na.rm = TRUE),
            .groups = "drop")
cat("Matched positions (2019) at post-2010 entrant institutions:\n")
print(entrant_positions)

write_csv(entry_exit_table, file.path(tabdir, "entry-exit-by-year.csv"))

# ---- program-panel variant with missing-not-zero outside the window --------
panel <- prog |>
  left_join(windows, by = "institution_code") |>
  mutate(in_window   = year >= first_appears & year <= last_active,
         matched_na  = if_else(in_window, matched, NA_real_),
         quota_na    = if_else(in_window, quota, NA_real_),
         balanced_full = first_appears == 2010 & last_active == 2019)

cat("Institution-years recoded missing (outside activity window):",
    sum(!panel$in_window), "of", nrow(panel), "\n")
cat("Institutions active in all ten years:", n_distinct(panel$institution_code[panel$balanced_full]), "\n")

write_dta(panel, file.path(datasets, "program_panel_entry_exit.dta"))
cat("Wrote", file.path(datasets, "program_panel_entry_exit.dta"), "\n")
