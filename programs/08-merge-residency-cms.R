# This script merges residency
# data with hospital level CMS
# readmission data using fuzzy
# string matching on hospital names
# within state

# author: Hussein Hadah
# first created: March 18, 2025
# last updated: March 18, 2026

# load residency data
residency <- read_dta(file.path(datasets, "cleaned_residency_medicaid.dta"))

# load CMS data (long format — one row per hospital-year)
cms <- read_dta(file.path(raw, "readmission_wide.dta"))

library(labelled)
var_label(cms)       # all variable labels
val_labels(cms)      # all value labels (the coded categories)
look_for(cms)        # nice overview: names, labels, and value labels together

# |>
#     select(
#       providerid, hospitalname, address, city, state, zipcode, year,
#       ami_score, cabg_score, copd_score, hf_score, hip_knee_score,
#       hosp_wide_score, pn_score, stk_score
#     )

# convert score columns to numeric
score_cols <- c("ami_score", "cabg_score", "copd_score", "hf_score",
                "hip_knee_score", "hosp_wide_score", "pn_score", "stk_score")

cms <- cms |>
  mutate(across(all_of(score_cols), as.numeric))

# glimpse both datasets
residency |> glimpse()
cms |> glimpse()

#---------------------------------------------------------------
# Name-cleaning function
#---------------------------------------------------------------
# Standardizes hospital names for matching by:
#   - converting to lowercase
#   - expanding common abbreviations
#   - removing punctuation, extra whitespace, and filler words

clean_hospital_name <- function(x) {
  x <- tolower(x)

  # Remove common suffixes/filler words
  x <- str_replace_all(x, "\\b(inc|llc|l\\.?l\\.?c|the|of|and|&|at|a|an)\\b", " ")

  # Expand abbreviations (order matters — longer patterns first)
  abbrevs <- c(
    "\\bmed ctr\\b"    = "medical center",
    "\\breg med\\b"    = "regional medical",
    "\\breg\\b"        = "regional",
    "\\bmed\\b"        = "medical",
    "\\bctr\\b"        = "center",
    "\\bctrs\\b"       = "centers",
    "\\bhosp\\b"       = "hospital",
    "\\bhosps\\b"      = "hospitals",
    "\\bhlth\\b"       = "health",
    "\\bhealthcare\\b" = "health care",
    "\\bsys\\b"        = "system",
    "\\bfdn\\b"        = "foundation",
    "\\bfnd\\b"        = "foundation",
    "\\bgen\\b"        = "general",
    "\\buniv\\b"       = "university",
    "\\baffil\\b"      = "affiliated",
    "\\baffl\\b"       = "affiliated",
    "\\bsom\\b"        = "school medicine",
    "\\bcom\\b"        = "college medicine",
    "\\bprog\\b"       = "program",
    "\\bprogs\\b"      = "programs",
    "\\bres\\b"        = "residency",
    "\\bfam\\b"        = "family",
    "\\bprac\\b"       = "practice",
    "\\bed\\b"         = "education",
    "\\bco\\b"         = "county",
    "\\bmem\\b"        = "memorial",
    "\\bcomm\\b"       = "community",
    "\\bsurg\\b"       = "surgery",
    "\\brehab\\b"      = "rehabilitation",
    "\\bpsych\\b"      = "psychiatric",
    "\\bpeds\\b"       = "pediatrics",
    "\\bst\\b"         = "saint",
    "\\bmt\\b"         = "mount",
    "\\bft\\b"         = "fort",
    "\\bso\\b"         = "south",
    "\\bn\\b"          = "north",
    "\\bw\\b"          = "west",
    "\\be\\b"          = "east",
    "\\bnw\\b"         = "northwest",
    "\\bne\\b"         = "northeast",
    "\\bsw\\b"         = "southwest",
    "\\bse\\b"         = "southeast",
    "\\bva\\b"         = "veterans affairs",
    "\\bu\\b"          = "university",
    "\\bil\\b"         = "illinois",
    "\\bwi\\b"         = "wisconsin",
    "\\bcinn\\b"       = "cincinnati",
    "\\bcol\\b"        = "college",
    "\\bcoll\\b"       = "college",
    "\\bsch\\b"        = "school",
    "\\bhsc\\b"        = "health science center",
    "\\blij\\b"        = "long island jewish",
    "\\bnslij\\b"      = "north shore long island jewish",
    "\\bnyp\\b"        = "new york presbyterian",
    "\\bnymc\\b"       = "new york medical college",
    "\\bnyu\\b"        = "new york university",
    "\\busc\\b"        = "university southern california",
    "\\bucla\\b"       = "university california los angeles",
    "\\buc\\b"         = "university california",
    "\\bucsf\\b"       = "university california san francisco",
    "\\blsu\\b"        = "louisiana state university",
    "\\blsuhsc\\b"     = "louisiana state university health science center",
    "\\bupmc\\b"       = "university pittsburgh medical center",
    "\\bnycomec\\b"    = "new york college osteopathic medicine",
    "\\bneomed\\b"     = "northeast ohio medical university",
    "\\bahec\\b"       = "area health education center",
    "\\buams\\b"       = "university arkansas medical sciences",
    "\\bopti\\b"       = "osteopathic postdoctoral training institution"
  )

  for (i in seq_along(abbrevs)) {
    x <- str_replace_all(x, names(abbrevs)[i], abbrevs[i])
  }

  # Remove punctuation (hyphens, slashes, periods, commas, parentheses)
  x <- str_replace_all(x, "[[:punct:]]", " ")

  # Collapse multiple spaces
  x <- str_squish(x)

  return(x)
}

#---------------------------------------------------------------
# Clean names and standardize zip codes in both datasets
#---------------------------------------------------------------

# Standardize zip codes to 5-digit character strings
clean_zip <- function(x) {
  x <- as.character(x)
  # Extract first 5 digits (handles ZIP+4 like "10001-1234")
  x <- str_extract(x, "\\d{5}")
  return(x)
}

# Get unique institution-level info from residency data
residency_institutions <- residency |>
  distinct(institution_name, state, zip_code) |>
  mutate(
    name_clean = clean_hospital_name(institution_name),
    state_upper = toupper(state),
    zip5 = clean_zip(zip_code)
  )

# Get unique hospital-level info from CMS data
cms_hospitals <- cms |>
  distinct(hospitalname, state, zipcode, providerid) |>
  mutate(
    name_clean = clean_hospital_name(hospitalname),
    state_upper = toupper(state),
    zip5 = clean_zip(zipcode)
  )

cat("Residency institutions:", nrow(residency_institutions), "\n")
cat("CMS hospitals:", nrow(cms_hospitals), "\n")

#---------------------------------------------------------------
# Step 1: Exact match on cleaned name + state
#---------------------------------------------------------------
exact_matches <- residency_institutions |>
  inner_join(
    cms_hospitals,
    by = c("name_clean", "state_upper"),
    suffix = c("_res", "_cms"),
    relationship = "many-to-many"
  ) |>
  mutate(match_type = "exact", jw_distance = 0)

cat("Exact matches:", n_distinct(exact_matches$institution_name),
    "out of", nrow(residency_institutions), "institutions\n")

#---------------------------------------------------------------
# Step 2: Zip-code-anchored fuzzy match for remaining
#---------------------------------------------------------------
# Strategy: for each unmatched residency institution, find CMS
# hospitals in the same or nearby zip code (first 3 digits),
# then pick the best name match among those candidates.

unmatched_res <- residency_institutions |>
  filter(!institution_name %in% exact_matches$institution_name)

cat("Unmatched after exact:", nrow(unmatched_res), "\n")

# Add 3-digit zip prefix for broader geographic matching
unmatched_res <- unmatched_res |>
  mutate(zip3 = str_sub(zip5, 1, 3))

cms_hospitals <- cms_hospitals |>
  mutate(zip3 = str_sub(zip5, 1, 3))

fuzzy_match_by_zip <- function(res_df, cms_df, max_name_dist = 0.35) {
  results <- list()

  for (i in seq_len(nrow(res_df))) {
    res_name  <- res_df$name_clean[i]
    res_state <- res_df$state_upper[i]
    res_zip5  <- res_df$zip5[i]
    res_zip3  <- res_df$zip3[i]

    # ---- Candidate pool: same state + nearby zip ----
    # First try exact 5-digit zip match within state
    candidates <- cms_df |>
      filter(state_upper == res_state, zip5 == res_zip5)

    # If too few candidates, broaden to 3-digit zip prefix
    if (nrow(candidates) < 3 & !is.na(res_zip3)) {
      candidates <- cms_df |>
        filter(state_upper == res_state, zip3 == res_zip3)
    }

    # If still no candidates, fall back to state-level
    if (nrow(candidates) == 0) {
      candidates <- cms_df |>
        filter(state_upper == res_state)
    }

    if (nrow(candidates) == 0) next

    # ---- Name similarity within candidates ----
    dists <- stringdist::stringdist(res_name, candidates$name_clean, method = "jw")

    best_idx  <- which.min(dists)
    best_dist <- dists[best_idx]

    # Flag whether the match was zip-anchored or state-level fallback
    best_zip5 <- candidates$zip5[best_idx]
    zip_match <- case_when(
      !is.na(res_zip5) & !is.na(best_zip5) & res_zip5 == best_zip5 ~ "exact_zip",
      !is.na(res_zip3) & !is.na(best_zip5) & str_sub(best_zip5, 1, 3) == res_zip3 ~ "zip3",
      TRUE ~ "state_only"
    )

    if (best_dist <= max_name_dist) {
      results[[i]] <- tibble(
        institution_name = res_df$institution_name[i],
        state_res        = res_state,
        zip_res          = res_zip5,
        hospitalname     = candidates$hospitalname[best_idx],
        providerid       = candidates$providerid[best_idx],
        state_cms        = candidates$state_upper[best_idx],
        zip_cms          = best_zip5,
        name_clean_res   = res_name,
        name_clean_cms   = candidates$name_clean[best_idx],
        jw_distance      = best_dist,
        zip_match        = zip_match,
        match_type       = "fuzzy"
      )
    }
  }

  bind_rows(results)
}

fuzzy_matches <- fuzzy_match_by_zip(unmatched_res, cms_hospitals)

cat("Fuzzy matches found:", nrow(fuzzy_matches), "\n")
cat("  - exact zip:", sum(fuzzy_matches$zip_match == "exact_zip"), "\n")
cat("  - zip3 match:", sum(fuzzy_matches$zip_match == "zip3"), "\n")
cat("  - state only:", sum(fuzzy_matches$zip_match == "state_only"), "\n")

#---------------------------------------------------------------
# Step 3: Combine match results into a crosswalk
#---------------------------------------------------------------

# Build crosswalk from exact matches
exact_crosswalk <- exact_matches |>
  select(institution_name, hospitalname, providerid, match_type, jw_distance)

# Build crosswalk from fuzzy matches
fuzzy_crosswalk <- fuzzy_matches |>
  select(institution_name, hospitalname, providerid, match_type, jw_distance,
         zip_res, zip_cms, zip_match)

# Combine into one crosswalk
crosswalk <- bind_rows(
    exact_crosswalk,
    fuzzy_crosswalk |> select(institution_name, hospitalname, providerid, match_type, jw_distance)
  ) |>
  # If a residency institution matched to multiple CMS hospitals,
  # keep the closest match
  group_by(institution_name) |>
  slice_min(jw_distance, n = 1, with_ties = FALSE) |>
  ungroup()

cat("Total matched institutions:", nrow(crosswalk),
    "out of", nrow(residency_institutions), "\n")

# Print fuzzy matches for manual review (sorted by worst first)
cat("\n--- Fuzzy matches (review for quality) ---\n")
fuzzy_crosswalk |>
  arrange(desc(jw_distance)) |>
  select(institution_name, hospitalname, jw_distance, zip_res, zip_cms, zip_match) |>
  print(n = Inf)

#---------------------------------------------------------------
# Step 4: Merge CMS scores into residency panel (long format)
#---------------------------------------------------------------

# Keep only residency institutions that matched to a CMS hospital
residency_merged <- residency |>
  inner_join(
    crosswalk |> select(institution_name, hospitalname, providerid, match_type),
    by = "institution_name"
  ) |>
  # Join CMS scores by providerid + year (avoids many-to-many from
  # duplicate hospital names across different provider IDs)
  inner_join(
    cms |> select(providerid, year, all_of(score_cols)),
    by = c("providerid", "year")
  )

cat("\n--- Merge summary ---\n")
cat("Rows in residency data:", nrow(residency), "\n")
cat("Rows after merge (matched only):", nrow(residency_merged), "\n")
cat("Unique institutions matched:",
    n_distinct(residency_merged$institution_name), "\n")

# Tabulate match types
residency_merged |>
  count(match_type) |>
  print()

#---------------------------------------------------------------
# Step 5: Save merged dataset and crosswalk
#---------------------------------------------------------------
write_dta(residency_merged, file.path(datasets, "residency_cms_merged.dta"))
cat("Saved merged dataset to:", file.path(datasets, "residency_cms_merged.dta"), "\n")

# # Save crosswalk for reference/manual review
# write_csv(crosswalk, file.path(datasets, "hospital_name_crosswalk.csv"))
# cat("Saved crosswalk to:", file.path(datasets, "hospital_name_crosswalk.csv"), "\n")

# # Save detailed fuzzy matches with zip info for auditing
# write_csv(fuzzy_crosswalk, file.path(datasets, "fuzzy_matches_detail.csv"))
# cat("Saved fuzzy match details to:", file.path(datasets, "fuzzy_matches_detail.csv"), "\n")
