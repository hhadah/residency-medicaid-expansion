# This script merges the program-level
# residency panel with hospital-level GME
# funding. The two datasets use DIFFERENT
# hospital identifiers:
#   - residency data: institution_code (AMA/ACGME, numeric)
#   - funding data  : provider_ccn (CMS Certification Number)
#
# We therefore link them through an
# institution_code -> CMS CCN crosswalk built in two layers:
#   1. Reuse the zip-anchored crosswalk already produced by
#      08-merge-residency-cms.R (institution_name -> providerid),
#      which links each institution to a CMS CCN using name + state + zip.
#      This is the high-confidence layer.
#   2. For institutions not covered by layer 1, fuzzy-match the
#      institution name to the funding hospital name within state
#      (the funding data has no zip, so this layer is name+state only).
# The CCN then joins the residency panel to funding by CCN and year.
#
# author: Hussein Hadah
# first created: July 13, 2026
# last updated: July 13, 2026

pacman::p_load(stringdist)

#---------------------------------------------------------------
# Load inputs
#---------------------------------------------------------------
# Main analysis panel (program x year) -- the merge target
program <- read_dta(file.path(datasets, "cleaned_program_residency_medicaid.dta"))

# Hospital-level GME funding panel (built by 14-append-gme-funding.R)
funding <- read_dta(file.path(datasets, "gme_funding_panel.dta"))

# Existing zip-anchored crosswalk (built by 08-merge-residency-cms.R):
# supplies institution_code -> providerid (CMS CCN)
res_cms <- read_dta(file.path(datasets, "residency_cms_merged.dta"))

# Full residency file supplies institution NAMES (the program panel has
# only institution_code) for the fuzzy fallback layer
full_res <- read_dta(file.path(datasets, "cleaned_residency_medicaid.dta"))

cat("Program panel rows:", nrow(program),
    "| unique institutions:", n_distinct(program$institution_code), "\n")

#---------------------------------------------------------------
# CCN normalization
#---------------------------------------------------------------
# The funding data stores CCNs zero-padded to 6 characters (e.g. "010033").
# The crosswalk's providerid is unpadded (e.g. "10033"). Pad both to align.
pad_ccn <- function(x) str_pad(as.character(x), width = 6, pad = "0")

#---------------------------------------------------------------
# Aggregate funding to one row per CCN x fiscal year
#---------------------------------------------------------------
# A hospital can file several cost-report segments in a year; sum the dollar
# payments and resident FTEs, average the rate/stock measures. (Mirrors the
# collapse used in 16-gme-funding-event-study.do.)
funding_year <- funding |>
  mutate(provider_ccn = pad_ccn(provider_ccn)) |>
  group_by(provider_ccn, fiscal_year) |>
  summarise(
    gme_hospital_name    = first(hospital_name),
    gme_state            = first(state),
    dgme_payment         = sum(dgme_payment, na.rm = TRUE),
    ime_payment          = sum(ime_payment, na.rm = TRUE),
    total_gme_payment    = sum(total_gme_payment, na.rm = TRUE),
    primary_care_fte     = sum(primary_care_fte, na.rm = TRUE),
    non_primary_care_fte = sum(non_primary_care_fte, na.rm = TRUE),
    dgme_ftes            = sum(dgme_ftes, na.rm = TRUE),
    ime_ftes             = sum(ime_ftes, na.rm = TRUE),
    primary_care_pra     = mean(primary_care_pra, na.rm = TRUE),
    non_primary_care_pra = mean(non_primary_care_pra, na.rm = TRUE),
    dgme_resident_cap    = mean(dgme_resident_cap, na.rm = TRUE),
    ime_resident_cap     = mean(ime_resident_cap, na.rm = TRUE),
    num_beds             = mean(num_beds, na.rm = TRUE),
    months_covered       = sum(months_covered, na.rm = TRUE),
    .groups = "drop"
  )

funding_ccns <- unique(funding_year$provider_ccn)

#---------------------------------------------------------------
# Name-cleaning function (mirrors 08-merge-residency-cms.R)
#---------------------------------------------------------------
# Standardizes hospital names for matching: lowercase, expand common
# abbreviations, drop punctuation/filler, squish whitespace.
clean_hospital_name <- function(x) {
  x <- tolower(x)
  x <- str_replace_all(x, "\\b(inc|llc|l\\.?l\\.?c|the|of|and|&|at|a|an)\\b", " ")

  abbrevs <- c(
    "\\bmed ctr\\b" = "medical center", "\\breg med\\b" = "regional medical",
    "\\breg\\b" = "regional", "\\bmed\\b" = "medical", "\\bctr\\b" = "center",
    "\\bctrs\\b" = "centers", "\\bhosp\\b" = "hospital", "\\bhosps\\b" = "hospitals",
    "\\bhlth\\b" = "health", "\\bhealthcare\\b" = "health care", "\\bsys\\b" = "system",
    "\\bfdn\\b" = "foundation", "\\bfnd\\b" = "foundation", "\\bgen\\b" = "general",
    "\\buniv\\b" = "university", "\\baffil\\b" = "affiliated", "\\baffl\\b" = "affiliated",
    "\\bsom\\b" = "school medicine", "\\bcom\\b" = "college medicine", "\\bprog\\b" = "program",
    "\\bprogs\\b" = "programs", "\\bres\\b" = "residency", "\\bfam\\b" = "family",
    "\\bprac\\b" = "practice", "\\bed\\b" = "education", "\\bco\\b" = "county",
    "\\bmem\\b" = "memorial", "\\bcomm\\b" = "community", "\\bsurg\\b" = "surgery",
    "\\brehab\\b" = "rehabilitation", "\\bpsych\\b" = "psychiatric", "\\bpeds\\b" = "pediatrics",
    "\\bst\\b" = "saint", "\\bmt\\b" = "mount", "\\bft\\b" = "fort", "\\bso\\b" = "south",
    "\\bn\\b" = "north", "\\bw\\b" = "west", "\\be\\b" = "east", "\\bnw\\b" = "northwest",
    "\\bne\\b" = "northeast", "\\bsw\\b" = "southwest", "\\bse\\b" = "southeast",
    "\\bva\\b" = "veterans affairs", "\\bu\\b" = "university", "\\bil\\b" = "illinois",
    "\\bwi\\b" = "wisconsin", "\\bcinn\\b" = "cincinnati", "\\bcol\\b" = "college",
    "\\bcoll\\b" = "college", "\\bsch\\b" = "school", "\\bhsc\\b" = "health science center",
    "\\blij\\b" = "long island jewish", "\\bnslij\\b" = "north shore long island jewish",
    "\\bnyp\\b" = "new york presbyterian", "\\bnymc\\b" = "new york medical college",
    "\\bnyu\\b" = "new york university", "\\busc\\b" = "university southern california",
    "\\bucla\\b" = "university california los angeles", "\\buc\\b" = "university california",
    "\\bucsf\\b" = "university california san francisco", "\\blsu\\b" = "louisiana state university",
    "\\blsuhsc\\b" = "louisiana state university health science center",
    "\\bupmc\\b" = "university pittsburgh medical center",
    "\\bnycomec\\b" = "new york college osteopathic medicine",
    "\\bneomed\\b" = "northeast ohio medical university",
    "\\bahec\\b" = "area health education center",
    "\\buams\\b" = "university arkansas medical sciences",
    "\\bopti\\b" = "osteopathic postdoctoral training institution"
  )
  for (i in seq_along(abbrevs)) x <- str_replace_all(x, names(abbrevs)[i], abbrevs[i])

  x <- str_replace_all(x, "[[:punct:]]", " ")
  x <- str_squish(x)
  x
}

#---------------------------------------------------------------
# Institution universe (with names + state for matching)
#---------------------------------------------------------------
institutions <- program |>
  distinct(institution_code, state) |>
  left_join(
    full_res |> distinct(institution_code, institution_name),
    by = "institution_code"
  ) |>
  mutate(state_upper = toupper(str_trim(state)))

#---------------------------------------------------------------
# Layer 1: reuse the zip-anchored crosswalk (institution -> CCN)
#---------------------------------------------------------------
xwalk_zip <- res_cms |>
  distinct(institution_code, providerid) |>
  filter(!is.na(providerid)) |>
  mutate(provider_ccn = pad_ccn(providerid)) |>
  # keep only links whose CCN actually appears in the funding data
  filter(provider_ccn %in% funding_ccns) |>
  distinct(institution_code, provider_ccn) |>
  # enforce one CCN per institution (crosswalk is already 1:1, but be safe)
  group_by(institution_code) |>
  slice_head(n = 1) |>
  ungroup() |>
  mutate(match_source = "zip_anchored_xwalk")

cat("\nLayer 1 (zip-anchored crosswalk) links:", nrow(xwalk_zip), "institutions\n")

#---------------------------------------------------------------
# Layer 2: fuzzy name+state match for the remainder
#---------------------------------------------------------------
# Candidate pool = each CCN's hospital name(s) in the funding data
funding_candidates <- funding_year |>
  distinct(provider_ccn, gme_hospital_name, gme_state) |>
  mutate(
    name_clean  = clean_hospital_name(gme_hospital_name),
    state_upper = toupper(str_trim(gme_state))
  )

# Institutions still needing a link
unmatched_inst <- institutions |>
  filter(!institution_code %in% xwalk_zip$institution_code,
         !is.na(institution_name)) |>
  mutate(name_clean = clean_hospital_name(institution_name))

cat("Institutions needing fuzzy match:", nrow(unmatched_inst), "\n")

# For each unmatched institution, pick the best within-state name match.
# Threshold is tight (0.15) because there is no zip anchor to guard against
# false positives among similarly named hospitals.
fuzzy_link <- function(inst_df, cand_df, max_dist = 0.15) {
  out <- list()
  for (i in seq_len(nrow(inst_df))) {
    st   <- inst_df$state_upper[i]
    nm   <- inst_df$name_clean[i]
    cand <- cand_df |> filter(state_upper == st)
    if (nrow(cand) == 0 || is.na(nm) || nm == "") next

    d    <- stringdist::stringdist(nm, cand$name_clean, method = "jw")
    best <- which.min(d)
    if (length(best) == 0 || is.na(d[best])) next

    if (d[best] <= max_dist) {
      out[[i]] <- tibble(
        institution_code = inst_df$institution_code[i],
        provider_ccn     = cand$provider_ccn[best],
        jw_distance      = d[best],
        institution_name = inst_df$institution_name[i],
        gme_hospital_name = cand$gme_hospital_name[best]
      )
    }
  }
  bind_rows(out)
}

fuzzy_matches <- fuzzy_link(unmatched_inst, funding_candidates)

xwalk_fuzzy <- fuzzy_matches |>
  distinct(institution_code, provider_ccn) |>
  group_by(institution_code) |>
  slice_head(n = 1) |>
  ungroup() |>
  mutate(match_source = "fuzzy_name_state")

cat("Layer 2 (fuzzy name+state) links:", nrow(xwalk_fuzzy), "institutions\n")

# Show fuzzy matches for manual review (worst first)
if (nrow(fuzzy_matches) > 0) {
  cat("\n--- Fuzzy institution -> hospital matches (review quality) ---\n")
  fuzzy_matches |>
    arrange(desc(jw_distance)) |>
    select(institution_name, gme_hospital_name, jw_distance) |>
    print(n = Inf)
}

#---------------------------------------------------------------
# Combine crosswalk layers (one CCN per institution)
#---------------------------------------------------------------
crosswalk <- bind_rows(xwalk_zip, xwalk_fuzzy) |>
  group_by(institution_code) |>
  slice_head(n = 1) |>          # layer 1 rows come first -> preferred
  ungroup()

n_inst_total   <- n_distinct(institutions$institution_code)
n_inst_matched <- n_distinct(crosswalk$institution_code)
cat("\n--- Crosswalk coverage ---\n")
cat("Institutions linked to a CCN:", n_inst_matched, "/", n_inst_total,
    sprintf("(%.1f%%)\n", 100 * n_inst_matched / n_inst_total))
crosswalk |> count(match_source) |> print()

#---------------------------------------------------------------
# Merge funding into the residency program panel
#---------------------------------------------------------------
# institution_code -> CCN (crosswalk), then CCN + year -> funding.
# Many residency programs share one hospital-year of funding, so this is a
# many-to-one join (funding is unique by provider_ccn x fiscal_year).
program_funded <- program |>
  left_join(crosswalk, by = "institution_code") |>
  left_join(
    funding_year,
    by = c("provider_ccn" = "provider_ccn", "year" = "fiscal_year")
  )

#---------------------------------------------------------------
# Merge diagnostics
#---------------------------------------------------------------
n_rows          <- nrow(program_funded)
n_rows_ccn      <- sum(!is.na(program_funded$provider_ccn))
n_rows_funded   <- sum(!is.na(program_funded$total_gme_payment))

cat("\n--- Merge summary ---\n")
cat("Program-year rows:", n_rows, "\n")
cat("Rows with a linked CCN:", n_rows_ccn,
    sprintf("(%.1f%%)\n", 100 * n_rows_ccn / n_rows))
cat("Rows with actual funding data for that CCN-year:", n_rows_funded,
    sprintf("(%.1f%%)\n", 100 * n_rows_funded / n_rows))
cat("  (a linked CCN may still lack funding in a given year, e.g. the\n",
    "   hospital filed no cost report or the program predates it)\n")

#---------------------------------------------------------------
# Save merged dataset and crosswalk
#---------------------------------------------------------------
write_dta(program_funded, file.path(datasets, "program_residency_gme_funding.dta"))
cat("\nSaved merged dataset to:",
    file.path(datasets, "program_residency_gme_funding.dta"), "\n")

write_csv(crosswalk, file.path(datasets, "institution_ccn_crosswalk.csv"))
cat("Saved institution->CCN crosswalk to:",
    file.path(datasets, "institution_ccn_crosswalk.csv"), "\n")
