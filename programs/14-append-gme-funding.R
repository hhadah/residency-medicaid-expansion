# This script appends all the annual
# GME (Graduate Medical Education) funding
# spreadsheets in data/raw/gme-funding into
# a single hospital-year panel and renames
# the columns to self-explanatory names.
#
# Source: CMS Research, Statistics, Data & Systems
#         "RGC GME Data" (one .xls/.xlsx per fiscal year)
#
# author: Hussein Hadah
# first created: July 13, 2026
# last updated: July 13, 2026

# readxl is not loaded in 01-packages-wds.R, so ensure it is available
pacman::p_load(readxl)

gme_dir <- file.path(raw, "gme-funding")

#---------------------------------------------------------------
# Self-explanatory column names
#---------------------------------------------------------------
# Every file (old .xls with per-state sheets and newer .xlsx with a
# single sheet) shares the SAME 20 columns in the SAME order, only the
# header labels differ. We therefore read by position and assign our
# own names rather than relying on the file headers.
#
# Original labels (old  ->  new file):
#   Fiscal Year               / FY          -> fiscal_year
#   Provider Number           / CCN         -> provider_ccn
#   Hospital Name             / NAME        -> hospital_name
#   ST                        / STATE       -> state
#   Fiscal Year Begin Date    / BEGIN DATE  -> fy_begin_date
#   Fiscal Year End Date      / END DATE    -> fy_end_date
#   Report Status Code        / STATUS      -> report_status
#   DME                       / DGME        -> dgme_payment
#   IME                       / IME         -> ime_payment
#   GME                       / GME         -> total_gme_payment
#   Prim. care FTE            / PC FTES     -> primary_care_fte
#   Non-Prim. care FTE        / NON-PC FTES -> non_primary_care_fte
#   Updated Prim. care PRA    / PC PRA      -> primary_care_pra
#   Updated Non-Prim. care PRA/ NON-PC PRA  -> non_primary_care_pra
#   Total DME Resident Cap    / DGME CAP    -> dgme_resident_cap
#   # of DME FTEs             / DGME FTES   -> dgme_ftes
#   Total IME Resident Cap    / IME CAP     -> ime_resident_cap
#   # of IME FTEs             / IME FTES    -> ime_ftes
#   # of Beds                 / BEDS        -> num_beds
#   months                    / MONTHS      -> months_covered

gme_colnames <- c(
  "fiscal_year", "provider_ccn", "hospital_name", "state",
  "fy_begin_date", "fy_end_date", "report_status",
  "dgme_payment", "ime_payment", "total_gme_payment",
  "primary_care_fte", "non_primary_care_fte",
  "primary_care_pra", "non_primary_care_pra",
  "dgme_resident_cap", "dgme_ftes",
  "ime_resident_cap", "ime_ftes",
  "num_beds", "months_covered"
)

# Numeric columns (everything except identifiers, dates, and status)
numeric_cols <- c(
  "fiscal_year", "dgme_payment", "ime_payment", "total_gme_payment",
  "primary_care_fte", "non_primary_care_fte",
  "primary_care_pra", "non_primary_care_pra",
  "dgme_resident_cap", "dgme_ftes",
  "ime_resident_cap", "ime_ftes",
  "num_beds", "months_covered"
)

#---------------------------------------------------------------
# Reader for a single annual file
#---------------------------------------------------------------
# - Old .xls files store every provider on the "National" sheet
#   (plus redundant per-state sheets); newer .xlsx files have a
#   single sheet. Prefer "National" when present, else the first sheet.
# - Read everything as text so Stata-style missing markers (".")
#   are preserved, then convert numerics ourselves.

read_gme_file <- function(path) {
  sheets    <- excel_sheets(path)
  use_sheet <- if ("National" %in% sheets) "National" else sheets[1]

  raw_dat <- read_excel(
    path,
    sheet     = use_sheet,
    col_names = FALSE,
    col_types = "text",
    skip      = 1               # drop the header row (labels vary by year)
  )

  # Guard against an unexpected layout
  if (ncol(raw_dat) != length(gme_colnames)) {
    stop(sprintf("%s has %d columns, expected %d",
                 basename(path), ncol(raw_dat), length(gme_colnames)))
  }

  names(raw_dat) <- gme_colnames

  raw_dat |>
    mutate(
      # Treat "." and blank strings as missing
      across(everything(), ~ na_if(str_trim(.x), ".")),
      across(everything(), ~ na_if(.x, "")),
      # Convert numeric columns
      across(all_of(numeric_cols), as.numeric),
      # Standardize state to uppercase 2-letter code
      state = toupper(str_trim(state)),
      # Parse fiscal-year dates (mm/dd/yyyy)
      fy_begin_date = mdy(fy_begin_date),
      fy_end_date   = mdy(fy_end_date),
      # Track the source file for auditing
      source_file   = basename(path)
    )
}

#---------------------------------------------------------------
# Read and append every file
#---------------------------------------------------------------
gme_files <- list.files(
  gme_dir,
  pattern    = "\\.xlsx?$",
  full.names = TRUE
) |> sort()

cat("Found", length(gme_files), "GME funding files to append.\n")

gme_panel <- gme_files |>
  map(read_gme_file) |>
  bind_rows() |>
  # Keep only rows with a real provider record
  filter(!is.na(provider_ccn)) |>
  arrange(fiscal_year, state, provider_ccn)

#---------------------------------------------------------------
# Summary
#---------------------------------------------------------------
cat("\n--- Appended GME panel ---\n")
cat("Total rows:", nrow(gme_panel), "\n")
cat("Fiscal years:", paste(range(gme_panel$fiscal_year, na.rm = TRUE),
                           collapse = " - "), "\n")
cat("Unique providers:", n_distinct(gme_panel$provider_ccn), "\n")
cat("States/territories:", n_distinct(gme_panel$state), "\n\n")

gme_panel |>
  count(fiscal_year) |>
  print(n = Inf)

#---------------------------------------------------------------
# Save appended panel
#---------------------------------------------------------------
write_dta(gme_panel, file.path(datasets, "gme_funding_panel.dta"))
cat("\nSaved appended panel to:",
    file.path(datasets, "gme_funding_panel.dta"), "\n")
