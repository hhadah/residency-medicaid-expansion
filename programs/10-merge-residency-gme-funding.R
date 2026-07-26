# This script merges the program-level
# residency (NRMP) panel with hospital-level
# GME funding. The two datasets use DIFFERENT
# hospital identifiers:
#   - residency data: institution_code (NRMP, numeric)
#   - funding data  : provider_ccn (CMS Certification Number)
#
# There is no public NRMP -> CCN crosswalk, so we build one by
# name+location matching. To match on LOCATION (the funding data has no
# zip), we bring in the CMS Provider of Services (POS) file, which supplies
# each CCN's hospital name, state, ZIP, and teaching status. We then run a
# ZIP-ANCHORED name match (mirrors 08-merge-residency-cms.R) between NRMP
# institutions (which have zip) and POS CCNs, restricted to CCNs that appear
# in the funding data. The old 08 crosswalk is used as a fallback layer.
#
# POS source: Sacarny cleaned Provider of Services panel (pos_lastyear.dta),
#   https://sacarny.com/data/  (one row per CCN, last observed year).
#   Downloaded to data/raw/provider-of-services/.
#
# author: Hussein Hadah
# first created: July 13, 2026
# last updated: July 13, 2026

pacman::p_load(stringdist)

#---------------------------------------------------------------
# Load inputs
#---------------------------------------------------------------
program  <- read_dta(file.path(datasets, "cleaned_program_residency_medicaid.dta"))
funding  <- read_dta(file.path(datasets, "gme_funding_panel.dta"))
full_res <- read_dta(file.path(datasets, "cleaned_residency_medicaid.dta"))   # institution names + zip
res_cms  <- read_dta(file.path(datasets, "residency_cms_merged.dta"))         # old 08 crosswalk (fallback)
pos      <- read_dta(file.path(raw, "provider-of-services/pos_lastyear.dta")) # CCN -> name/zip/teaching

cat("Program panel rows:", nrow(program),
    "| unique institutions:", n_distinct(program$institution_code), "\n")

#---------------------------------------------------------------
# Helpers
#---------------------------------------------------------------
# CCN normalization: zero-pad to 6 characters ("10033" -> "010033")
pad_ccn <- function(x) str_pad(as.character(x), width = 6, pad = "0")

# 5-digit zip as character (handles ZIP+4 and dropped leading zeros)
clean_zip <- function(x) {
  x <- str_extract(as.character(x), "\\d+")
  str_pad(str_sub(x, 1, 5), width = 5, pad = "0")
}

# Name standardization (mirrors 08-merge-residency-cms.R)
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
  str_squish(x)
}

#---------------------------------------------------------------
# Aggregate funding to one row per CCN x fiscal year
#---------------------------------------------------------------
# Sum dollar payments / FTEs across a hospital's multiple cost-report segments
# in a year; average the rate/stock measures. (Mirrors 16-...event-study.do.)
funding_year <- funding |>
  mutate(provider_ccn = pad_ccn(provider_ccn)) |>
  group_by(provider_ccn, fiscal_year) |>
  summarise(
    gme_hospital_name    = first(hospital_name),
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
# Build the POS reference: CCN -> name, zip, state, teaching
#---------------------------------------------------------------
# Restrict to CCNs that appear in the funding data (these are the only useful
# match targets, and this naturally keeps GME/teaching hospitals).
pos_ref <- pos |>
  transmute(
    provider_ccn = pad_ccn(pn),
    pos_name     = name,
    name_clean   = clean_hospital_name(name),
    state_upper  = toupper(str_trim(state)),
    zip5         = clean_zip(zip),
    zip3         = str_sub(zip5, 1, 3),
    residents,
    teaching     = (resprog_ama %in% 1 | resprog_aoa %in% 1 |
                    resprog_oth %in% 1 | resprog_ada %in% 1 |
                    (!is.na(residents) & residents > 0))
  ) |>
  filter(provider_ccn %in% funding_ccns) |>
  distinct(provider_ccn, .keep_all = TRUE)

# Save the cleaned POS crosswalk for reuse / auditing
write_dta(pos_ref, file.path(datasets, "pos_ccn_reference.dta"))
cat("Built POS reference: ", nrow(pos_ref), " funding CCNs with zip/name",
    " (", sum(pos_ref$teaching), " flagged teaching)\n", sep = "")

#---------------------------------------------------------------
# Institution universe (NRMP institution -> name, state, zip)
#---------------------------------------------------------------
institutions <- program |>
  distinct(institution_code, state) |>
  left_join(
    full_res |>
      distinct(institution_code, institution_name, zip_code),
    by = "institution_code"
  ) |>
  mutate(
    name_clean  = clean_hospital_name(institution_name),
    state_upper = toupper(str_trim(state)),
    zip5        = clean_zip(zip_code),
    zip3        = str_sub(zip5, 1, 3)
  )

#---------------------------------------------------------------
# Zip-anchored name match: NRMP institution -> CCN (via POS)
#---------------------------------------------------------------
# For each institution: best Jaro-Winkler name match within its state, with
# the zip used to VALIDATE weaker name matches. Acceptance is graduated:
#   - near-exact name (jw <= .15): accept regardless of zip
#   - moderate name  (jw <= .35): require same 3-digit zip
#   - loose name     (jw <= .45): require exact 5-digit zip
# Ties in name distance break toward the zip-matching candidate.
match_institution <- function(inst_i, cand_df) {
  st  <- inst_i$state_upper
  nm  <- inst_i$name_clean
  iz5 <- inst_i$zip5
  iz3 <- inst_i$zip3
  cand <- cand_df |> filter(state_upper == st)
  if (nrow(cand) == 0 || is.na(nm) || nm == "") return(NULL)

  cand <- cand |>
    mutate(
      d = stringdist::stringdist(nm, name_clean, method = "jw"),
      zip_tier = case_when(
        !is.na(iz5) & !is.na(zip5) & zip5 == iz5 ~ 3L,  # exact zip
        !is.na(iz3) & !is.na(zip3) & zip3 == iz3 ~ 2L,  # zip3
        TRUE                                     ~ 1L   # state only
      )
    ) |>
    arrange(d, desc(zip_tier))

  best <- cand[1, ]
  accept <- (best$d <= 0.15) |
            (best$d <= 0.35 & best$zip_tier >= 2L) |
            (best$d <= 0.45 & best$zip_tier == 3L)
  if (!accept) return(NULL)

  tibble(
    institution_code = inst_i$institution_code,
    provider_ccn     = best$provider_ccn,
    institution_name = inst_i$institution_name,
    pos_name         = best$pos_name,
    jw_distance      = best$d,
    zip_match        = c("state_only", "zip3", "exact_zip")[best$zip_tier]
  )
}

pos_matches <- map_dfr(seq_len(nrow(institutions)),
                       ~ match_institution(institutions[.x, ], pos_ref))

xwalk_pos <- pos_matches |>
  distinct(institution_code, provider_ccn, .keep_all = TRUE) |>
  group_by(institution_code) |>
  slice_head(n = 1) |>
  ungroup() |>
  mutate(match_source = "pos_zip_anchored")

cat("\nPOS zip-anchored matches:", nrow(xwalk_pos), "institutions\n")
xwalk_pos |> count(zip_match) |> print()

#---------------------------------------------------------------
# Fallback layer: old 08 crosswalk for institutions POS did not match
#---------------------------------------------------------------
xwalk_08 <- res_cms |>
  distinct(institution_code, providerid) |>
  filter(!is.na(providerid)) |>
  mutate(provider_ccn = pad_ccn(providerid)) |>
  filter(provider_ccn %in% funding_ccns,
         !institution_code %in% xwalk_pos$institution_code) |>
  distinct(institution_code, provider_ccn) |>
  group_by(institution_code) |>
  slice_head(n = 1) |>
  ungroup() |>
  mutate(match_source = "old08_fallback")

cat("Fallback (old 08) links for institutions POS missed:", nrow(xwalk_08), "\n")

#---------------------------------------------------------------
# Combine crosswalk layers (POS preferred)
#---------------------------------------------------------------
crosswalk <- bind_rows(
    xwalk_pos |> select(institution_code, provider_ccn, match_source, jw_distance, zip_match),
    xwalk_08  |> select(institution_code, provider_ccn, match_source)
  ) |>
  group_by(institution_code) |>
  slice_head(n = 1) |>
  ungroup()

n_inst_total   <- n_distinct(institutions$institution_code)
n_inst_matched <- n_distinct(crosswalk$institution_code)
cat("\n--- Crosswalk coverage ---\n")
cat("Institutions linked to a CCN:", n_inst_matched, "/", n_inst_total,
    sprintf("(%.1f%%)\n", 100 * n_inst_matched / n_inst_total))
crosswalk |> count(match_source) |> print()

#---------------------------------------------------------------
# Validation: agreement between POS match and the old 08 crosswalk
#---------------------------------------------------------------
compare <- xwalk_pos |>
  inner_join(
    res_cms |> distinct(institution_code, providerid) |>
      filter(!is.na(providerid)) |> mutate(ccn08 = pad_ccn(providerid)),
    by = "institution_code"
  )
cat("\n--- POS vs. old-08 agreement (institutions matched by both) ---\n")
cat("Both methods present:", nrow(compare),
    "| agree on CCN:", sum(compare$provider_ccn == compare$ccn08),
    sprintf("(%.1f%%)\n", 100 * mean(compare$provider_ccn == compare$ccn08)))
disagree <- compare |> filter(provider_ccn != ccn08)
if (nrow(disagree) > 0) {
  cat("Disagreements (POS zip-anchored is usually the correct one):\n")
  disagree |>
    select(institution_name, pos_name, provider_ccn, ccn08, jw_distance, zip_match) |>
    print(n = Inf)
}

#---------------------------------------------------------------
# Merge funding into the residency program panel
#---------------------------------------------------------------
program_funded <- program |>
  left_join(crosswalk, by = "institution_code") |>
  left_join(funding_year, by = c("provider_ccn" = "provider_ccn", "year" = "fiscal_year"))

#---------------------------------------------------------------
# Merge diagnostics
#---------------------------------------------------------------
n_rows        <- nrow(program_funded)
n_rows_ccn    <- sum(!is.na(program_funded$provider_ccn))
n_rows_funded <- sum(!is.na(program_funded$total_gme_payment))
cat("\n--- Merge summary ---\n")
cat("Program-year rows:", n_rows, "\n")
cat("Rows with a linked CCN:", n_rows_ccn,
    sprintf("(%.1f%%)\n", 100 * n_rows_ccn / n_rows))
cat("Rows with actual funding data for that CCN-year:", n_rows_funded,
    sprintf("(%.1f%%)\n", 100 * n_rows_funded / n_rows))

#---------------------------------------------------------------
# Save merged dataset and crosswalk
#---------------------------------------------------------------
write_dta(program_funded, file.path(datasets, "program_residency_gme_funding.dta"))
cat("\nSaved merged dataset to:",
    file.path(datasets, "program_residency_gme_funding.dta"), "\n")

write_csv(crosswalk, file.path(datasets, "institution_ccn_crosswalk.csv"))
cat("Saved institution->CCN crosswalk to:",
    file.path(datasets, "institution_ccn_crosswalk.csv"), "\n")

#---------------------------------------------------------------
# Review list: links NOT confirmed by an exact/near zip anchor
#---------------------------------------------------------------
# Flags the lower-confidence links for manual audit: any match that is
# state-only (no zip corroboration) or fell back to the old 08 crosswalk.
# Includes the institution and matched hospital names side by side, plus the
# institution's total GME funding (largest first) to prioritize review.
funding_by_ccn <- funding_year |>
  group_by(provider_ccn) |>
  summarise(total_gme_all_years = sum(total_gme_payment, na.rm = TRUE),
            .groups = "drop")

review_list <- crosswalk |>
  filter(match_source == "old08_fallback" |
         (match_source == "pos_zip_anchored" & zip_match == "state_only")) |>
  left_join(institutions |> distinct(institution_code, institution_name, state, zip5),
            by = "institution_code") |>
  left_join(pos_ref |> select(provider_ccn, pos_name, pos_zip5 = zip5),
            by = "provider_ccn") |>
  left_join(funding_by_ccn, by = "provider_ccn") |>
  transmute(
    institution_code, institution_name, state,
    provider_ccn, matched_hospital = pos_name,
    inst_zip = zip5, ccn_zip = pos_zip5,
    match_source, zip_match, jw_distance,
    total_gme_all_years
  ) |>
  arrange(desc(total_gme_all_years))

write_csv(review_list, file.path(datasets, "crosswalk_review_list.csv"))
cat("Saved", nrow(review_list),
    "lower-confidence links to review:",
    file.path(datasets, "crosswalk_review_list.csv"), "\n")
