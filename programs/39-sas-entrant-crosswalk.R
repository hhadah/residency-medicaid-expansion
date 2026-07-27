# ==============================================================================
# 39-sas-entrant-crosswalk.R
# Classify post-2010 NRMP entrant institutions as (i) AOA->ACGME Single
# Accreditation System (SAS) migrants or (ii) genuinely new sponsors, by
# matching entrants against the ACGME ADS public list of programs that applied
# for accreditation under the SAS (parsed by 38-parse-sas-transition.py).
#
# Desk review 2026-07-26, return condition: "classify every post-2010 NRMP
# entrant as (i) a genuinely new sponsoring institution or (ii) a pre-existing
# AOA-accredited sponsor migrating under the Single Accreditation System."
#
# Inputs : data/datasets/2000_2019_residency_programs.dta (names, city, state)
#          data/datasets/panel_2000_2019_estimation.dta   (first_active)
#          data/raw/sas_transition_programs.csv           (script 38)
# Outputs: data/datasets/sas_entrant_classification.csv
#            institution_code, state, institution_name, city, first_active,
#            sas_migrant, best_sas_match, jw_dist, city_match, match_band
#
# Matching: within-state Jaro-Winkler on normalized names, with SAS compound
# names ("School/Hospital Program") split into parts; distance <= 0.12 or
# (distance <= 0.20 and city match) => sas_migrant = 1. The 0.12-0.20 band
# without a city match is written out for manual review (match_band = "review").
# Run AFTER 38; re-run before 28's genuine-entrant specs.
# ==============================================================================

if (!exists("datasets")) {
  git_mdir     <- here::here()
  datasets     <- file.path(git_mdir, "data", "datasets")
  source(file.path(git_mdir, "programs", "01-packages-wds.r"))
}
if (!exists("raw", mode = "character")) raw <- file.path(here::here(), "data", "raw")
library(stringdist)

expand_abbrev <- function(x) {
  x |>
    stringr::str_replace_all("\\breg\\b", "regional") |>
    stringr::str_replace_all("\\bmed\\b", "medical") |>
    stringr::str_replace_all("\\bctrs?\\b", "center") |>
    stringr::str_replace_all("\\bmem\\b", "memorial") |>
    stringr::str_replace_all("\\bsys\\b", "system") |>
    stringr::str_replace_all("\\bhosps?\\b", "hospital") |>
    stringr::str_replace_all("\\buniv\\b", "university") |>
    stringr::str_replace_all("\\bdept\\b", "department") |>
    stringr::str_replace_all("\\bsom\\b", "school of medicine") |>
    stringr::str_replace_all("\\bst\\b", "saint")
}

normalize_name <- function(x) {
  x |>
    tolower() |>
    # mangled en-dashes in NRMP names come through as multibyte junk
    iconv(from = "", to = "ASCII", sub = " ") |>
    stringr::str_replace_all("[^a-z0-9/ ]", " ") |>
    expand_abbrev() |>
    stringr::str_replace_all(
      paste0("\\b(program|the|of|at|and|college|university|school|",
             "osteopathic|medicine|medical|center|health|healthcare|",
             "hospital|regional|community|system|education|consortium|",
             "opti|inc|llc|for|graduate)\\b"), " ") |>
    stringr::str_squish()
}

token_overlap <- function(a, b) {
  ta <- unique(strsplit(a, " ")[[1]]); tb <- unique(strsplit(b, " ")[[1]])
  ta <- ta[nchar(ta) > 2]; tb <- tb[nchar(tb) > 2]
  shared <- length(intersect(ta, tb))
  # a single shared generic token ("valley", "mercy") is not evidence;
  # require at least two shared tokens for a full-overlap score
  if (length(ta) == 0 || length(tb) == 0 || shared < 2) return(0)
  shared / min(length(ta), length(tb))
}

# --- NRMP entrants ------------------------------------------------------------
inst_names <- haven::read_dta(file.path(datasets, "2000_2019_residency_programs.dta")) |>
  dplyr::distinct(institution_code, state, institution_name, city) |>
  dplyr::mutate(state = toupper(stringr::str_trim(state))) |>
  dplyr::group_by(institution_code, state) |>
  dplyr::slice(1) |>
  dplyr::ungroup()

panel <- haven::read_dta(file.path(datasets, "panel_2000_2019_estimation.dta")) |>
  dplyr::distinct(institution_code, state, first_active) |>
  dplyr::mutate(state = toupper(stringr::str_trim(state)))

entrants <- panel |>
  dplyr::filter(first_active >= 2011) |>
  dplyr::left_join(inst_names, by = c("institution_code", "state")) |>
  dplyr::mutate(
    name_norm = normalize_name(institution_name),
    city_norm = tolower(stringr::str_trim(city))
  )

# --- SAS applicant institutions (split compound names; extract acronyms) ------
sas_raw <- readr::read_csv(file.path(raw, "sas_transition_programs.csv"),
                           show_col_types = FALSE)

acronyms <- sas_raw |>
  dplyr::mutate(acr = stringr::str_extract(program_name, "\\(([A-Z]{4,})\\)"),
                acr = stringr::str_remove_all(acr, "[()]")) |>
  dplyr::filter(!is.na(acr)) |>
  dplyr::transmute(sas_name = program_name, sas_norm = tolower(acr),
                   sas_city = tolower(stringr::str_trim(city)),
                   state = toupper(state))

sas <- sas_raw |>
  dplyr::mutate(part = stringr::str_split(program_name, "/")) |>
  tidyr::unnest(part) |>
  dplyr::transmute(
    sas_name  = stringr::str_squish(part),
    sas_norm  = normalize_name(part),
    sas_city  = tolower(stringr::str_trim(city)),
    state     = toupper(state)
  ) |>
  dplyr::bind_rows(acronyms) |>
  dplyr::filter(sas_norm != "") |>
  dplyr::distinct(sas_norm, state, .keep_all = TRUE)

# --- best within-state match --------------------------------------------------
match_one <- function(nn, cc, st) {
  cand <- sas[sas$state == st, ]
  if (nrow(cand) == 0 || is.na(nn) || nn == "") {
    return(tibble::tibble(best_sas_match = NA_character_, jw_dist = NA_real_,
                          tok_overlap = 0, city_match = FALSE))
  }
  d   <- stringdist::stringdist(nn, cand$sas_norm, method = "jw", p = 0.1)
  tov <- vapply(cand$sas_norm, token_overlap, numeric(1), a = nn)
  # rank by JW but let a full token-subset match win outright
  i <- if (any(tov == 1)) which(tov == 1)[which.min(d[tov == 1])] else which.min(d)
  tibble::tibble(best_sas_match = cand$sas_name[i], jw_dist = d[i],
                 tok_overlap = tov[i],
                 city_match = identical(cc, cand$sas_city[i]) && !is.na(cc) && cc != "")
}

res <- entrants |>
  dplyr::bind_cols(
    purrr::pmap_dfr(list(entrants$name_norm, entrants$city_norm, entrants$state),
                    match_one)
  ) |>
  dplyr::mutate(
    osteo_name = stringr::str_detect(tolower(institution_name),
                                     "opti|osteopath|nycomec"),
    match_band = dplyr::case_when(
      osteo_name                                        ~ "osteo_name",
      !is.na(jw_dist) & jw_dist <= 0.12                 ~ "match",
      !is.na(tok_overlap) & tok_overlap >= 0.99         ~ "match_tokens",
      !is.na(jw_dist) & jw_dist <= 0.20 &  city_match   ~ "match_city",
      !is.na(jw_dist) & jw_dist <= 0.20 & !city_match   ~ "review",
      TRUE                                              ~ "no_match"
    ),
    sas_migrant = as.integer(match_band %in%
                               c("osteo_name", "match", "match_tokens", "match_city"))
  ) |>
  dplyr::arrange(jw_dist)

readr::write_csv(
  res |> dplyr::select(institution_code, state, institution_name, city,
                       first_active, sas_migrant, best_sas_match, jw_dist,
                       tok_overlap, city_match, osteo_name, match_band),
  file.path(datasets, "sas_entrant_classification.csv")
)

cat("post-2010 entrants:", nrow(res), "\n")
print(table(res$match_band))
cat("classified SAS migrants:", sum(res$sas_migrant), "\n")
cat("by first_active >= 2016 (old timing proxy):\n")
print(with(res, table(proxy_2016 = first_active >= 2016, sas_migrant)))
