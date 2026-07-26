# This script merges the appended GME
# funding panel with each state's Medicaid
# expansion status, adding a treated flag
# and event-time (years relative to expansion).
#
# author: Hussein Hadah
# first created: July 13, 2026
# last updated: July 13, 2026

#---------------------------------------------------------------
# Load inputs
#---------------------------------------------------------------
# GME funding panel (built by 05-append-gme-funding.R)
gme_panel <- read_dta(file.path(datasets, "gme_funding_panel.dta"))

# Medicaid expansion status (one row per state)
#   state           : 2-letter code
#   expansion_state : "Yes"/"No"
#   year_expanded   : year the state expanded (missing if never)
expansion <- read_dta(file.path(raw, "expansion_status.dta")) |>
  mutate(state = toupper(str_trim(state)))

cat("GME panel rows:", nrow(gme_panel), "\n")
cat("Expansion-status states:", nrow(expansion), "\n")

#---------------------------------------------------------------
# Check state coverage before merging
#---------------------------------------------------------------
gme_states       <- sort(unique(gme_panel$state))
expansion_states <- sort(unique(expansion$state))

in_gme_only <- setdiff(gme_states, expansion_states)
if (length(in_gme_only) > 0) {
  cat("\nStates in GME panel but NOT in expansion status (will be NA):\n  ",
      paste(in_gme_only, collapse = ", "), "\n")
}

#---------------------------------------------------------------
# Merge on state (left join keeps every GME hospital-year)
#---------------------------------------------------------------
gme_expansion <- gme_panel |>
  left_join(expansion, by = "state") |>
  mutate(
    # Clean up the expansion indicator into a 0/1 flag
    expanded_ever = case_when(
      expansion_state == "Yes" ~ 1L,
      expansion_state == "No"  ~ 0L,
      TRUE                     ~ NA_integer_
    ),
    # Treated in a given fiscal year = state had expanded by then
    post_expansion = case_when(
      is.na(year_expanded)              ~ 0L,
      fiscal_year >= year_expanded      ~ 1L,
      TRUE                              ~ 0L
    ),
    # Event time: years since (or until) expansion; NA for never-expanders
    event_time = if_else(
      is.na(year_expanded), NA_real_, fiscal_year - year_expanded
    )
  )

#---------------------------------------------------------------
# Merge diagnostics
#---------------------------------------------------------------
cat("\n--- Merge summary ---\n")
cat("Rows after merge:", nrow(gme_expansion), "\n")
cat("Rows matched to an expansion status:",
    sum(!is.na(gme_expansion$expansion_state)), "\n")
cat("Rows unmatched (state not in expansion file):",
    sum(is.na(gme_expansion$expansion_state)), "\n\n")

gme_expansion |>
  distinct(state, expansion_state, year_expanded) |>
  arrange(expansion_state, year_expanded, state) |>
  print(n = Inf)

#---------------------------------------------------------------
# Save merged dataset
#---------------------------------------------------------------
write_dta(gme_expansion, file.path(datasets, "gme_funding_expansion.dta"))
cat("\nSaved merged dataset to:",
    file.path(datasets, "gme_funding_expansion.dta"), "\n")
