# ------------------------------------------------------------------
# 23-make-2000-2019-residency-panel.R
# Standardize the 2000-2009 NRMP extraction against the clean
# 2010-2019 file and build data/datasets/2000_2019_residency_programs.dta
# with the same structure (institution x specialty, wide years).
#
# Inputs:
#   data/datasets/nrmp-programs-2000-2009.csv   (program-level extraction)
#   data/raw/2010_2019_residency_programs.dta   (clean baseline)
# Outputs:
#   data/datasets/2000_2019_residency_programs.dta
#   data/datasets/nrmp-2000-2009-standardization-crosswalk.csv
# Conventions carried from the 2010-2019 file:
#   - one row per institution x specialty_code (3-digit NRMP specialty),
#     quota/matched summed across program subtypes (C/P/A/M)
#   - 0 = institution existed in panel but no positions that year
#   - NA = data not observed: 2002 for institutions hit by the missing
#     GA/HI/ID book pages; 2010-2019 for Puerto Rico (absent from the
#     2010-2019 source file)
# ------------------------------------------------------------------

library(here)
suppressMessages({
  library(dplyr)
  library(tidyr)
  library(haven)
  library(readr)
  library(stringr)
})

ext <- read_csv(here("data", "datasets", "nrmp-programs-2000-2009.csv"),
                show_col_types = FALSE,
                col_types = cols(.default = "c"))
base <- read_dta(here("data", "raw", "2010_2019_residency_programs.dta"))

ext <- ext %>%
  mutate(quota = as.integer(quota),
         matched = as.integer(matched),
         year = as.integer(year))

# ---- 1. specialty code -------------------------------------------------
# 2002-2009: embedded in the 9-char code (chars 5-7).
# 2000-2001: from program text via the modal text->code mapping observed
# in 2002-2009, plus fallback rules for old-only vocabulary.
norm_txt <- function(x) {
  x <- toupper(x)
  x <- str_replace_all(x, "[^A-Z]+", " ")
  x <- str_squish(x)
  # typewriter margin artifacts: stray single-letter leading tokens
  str_remove(x, "^([A-Z] )+")
}

modern <- ext %>% filter(year >= 2002, nchar(code) == 9) %>%
  mutate(spec = as.integer(substr(code, 5, 7)),
         ptxt = norm_txt(program))

txt_map <- modern %>%
  count(ptxt, spec) %>%
  group_by(ptxt) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(ptxt, spec)

fallback_spec <- function(ptxt) {
  case_when(
    str_detect(ptxt, "^FAM") ~ 120L,
    str_detect(ptxt, "^FAMILY") ~ 120L,
    str_detect(ptxt, "^MEDICINE PEDIATRICS") ~ 700L,
    str_detect(ptxt, "^INT MED|^INTERNAL MED|^MEDICINE") ~ 140L,
    str_detect(ptxt, "^PEDS|^PEDIATRIC") ~ 320L,
    str_detect(ptxt, "^SURGERY|^GENERAL SURG|^SURG") ~ 440L,
    str_detect(ptxt, "^OB|^OBSTETRIC") ~ 220L,
    str_detect(ptxt, "^PSYCHIATRY NEUROLOGY") ~ 755L,
    str_detect(ptxt, "^PSYCH") ~ 400L,
    str_detect(ptxt, "^TRANSITIONAL") ~ 999L,
    str_detect(ptxt, "^EMERGENCY") ~ 110L,
    str_detect(ptxt, "^ANESTHES") ~ 40L,
    str_detect(ptxt, "^RADIOLOGY DIAG|^RADIOLOGY$") ~ 420L,
    str_detect(ptxt, "^RADIATION") ~ 430L,
    str_detect(ptxt, "^PATHOLOGY") ~ 300L,
    str_detect(ptxt, "^NEUROLOGICAL SURG|^NEUROSURG") ~ 160L,
    str_detect(ptxt, "^NEUROLOGY") ~ 180L,
    str_detect(ptxt, "^ORTHOP") ~ 260L,
    str_detect(ptxt, "^OTOLARYNG") ~ 280L,
    str_detect(ptxt, "^DERMATOL") ~ 80L,
    str_detect(ptxt, "^PHYS MEDICINE|^PHYSICAL MED") ~ 340L,
    str_detect(ptxt, "^PLASTIC") ~ 360L,
    str_detect(ptxt, "^PREVENTIVE") ~ 380L,
    str_detect(ptxt, "^NUCLEAR") ~ 200L,
    str_detect(ptxt, "^UROLOGY") ~ 480L,
    str_detect(ptxt, "^VASCULAR") ~ 451L,
    str_detect(ptxt, "^THORACIC") ~ 461L,
    TRUE ~ NA_integer_
  )
}

old <- ext %>% filter(year <= 2001) %>%
  mutate(ptxt = norm_txt(program)) %>%
  left_join(txt_map, by = "ptxt") %>%
  mutate(spec = if_else(is.na(spec), fallback_spec(ptxt), spec))

# second pass: OCR-typo variants resolved by edit distance (<=2) against the
# known modern vocabulary (e.g. RADIGLOGY DIAGNOSTIC, TRANSITIGNAL)
vocab <- txt_map$ptxt
fuzzy_one <- function(p) {
  d <- utils::adist(p, vocab, partial = FALSE)
  i <- which.min(d)
  if (d[i] <= 2) txt_map$spec[i] else NA_integer_
}
# keyword fallback searched anywhere in the text (subspecialty phrasings,
# heavy OCR damage): checked in a priority order
keyword_spec <- function(p) {
  case_when(
    str_detect(p, "ORTHO") ~ 260L,
    str_detect(p, "RAD (DIAG|IOLOGY)|RADIOLOGY") ~ 420L,
    str_detect(p, "RAD ONC|RADIATION") ~ 430L,
    str_detect(p, "TRANSITIONAL") ~ 999L,
    str_detect(p, "CHLD PSY|CHILD PSYCH") ~ 730L,
    str_detect(p, "PSYCH") ~ 400L,
    str_detect(p, "NEUROLOG SURG|NEUROSURG") ~ 160L,
    str_detect(p, "NEUROLOGY") ~ 180L,
    str_detect(p, "OTOLARYNG") ~ 280L,
    str_detect(p, "OBSTETRIC|GYNECOL") ~ 220L,
    str_detect(p, "MEDICINE PEDIATRIC") ~ 700L,
    str_detect(p, "PEDIATRIC|PEDS") ~ 320L,
    str_detect(p, "EMERGENCY") ~ 110L,
    str_detect(p, "FAMILY|FAM PRAC|AMILY") ~ 120L,
    str_detect(p, "INTERNAL MED|MEDICINE") ~ 140L,
    str_detect(p, "PATHOLOGY") ~ 300L,
    str_detect(p, "DERMATOL") ~ 80L,
    str_detect(p, "ANESTH") ~ 40L,
    str_detect(p, "PHYS MED|REHAB") ~ 340L,
    str_detect(p, "PLASTIC") ~ 360L,
    str_detect(p, "UROLOGY") ~ 480L,
    str_detect(p, "SURG") ~ 440L,
    TRUE ~ NA_integer_
  )
}
resolve_spec <- function(ptxts) {
  vapply(ptxts, function(p) {
    s <- txt_map$spec[match(p, txt_map$ptxt)]
    if (!is.na(s)) return(s)
    s <- fuzzy_one(p)
    if (!is.na(s)) return(s)
    # strip up to two leading junk tokens and retry
    q <- p
    for (i in 1:2) {
      q <- str_remove(q, "^\\S+ ")
      if (q == p || !str_detect(q, " |^\\S{4,}")) break
      s <- txt_map$spec[match(q, txt_map$ptxt)]
      if (!is.na(s)) return(s)
      s <- fuzzy_one(q)
      if (!is.na(s)) return(s)
    }
    keyword_spec(p)
  }, integer(1))
}
still <- is.na(old$spec)
if (any(still)) {
  ures <- resolve_spec(unique(old$ptxt[still]))
  old$spec[still] <- ures[old$ptxt[still]]
}

# hand-resolved residuals (heavy OCR damage / site-specific program names)
manual_map <- c(
  "FP ANDERSON OCONEE" = 120L, "FP GREENVILLE OCONEE" = 120L,
  "FP PREV MED BARRE FHC" = 120L, "INVESTIGATOR PATHWAY" = 140L,
  "MED PRELIM UCLA SFVP" = 140L, "MED PRIMARY UCLA SFVP" = 140L,
  "PEDLIATRICS BOX" = 320L, "PREV MED PUBLIC HEALTH CG" = 380L,
  "PSYC CAMB MGH MCLEAN" = 400L, "RADIATIGN ONCOLGGY S" = 430L,
  "RADIATIGN ONCOLOGY S" = 430L, "RADIATLION ONCOLOGY S" = 430L,
  "RADLATION ONCOLOGY S" = 430L, "RAOIATION GNCGLOGY" = 430L,
  "RADIGLOGY OLAGNOSTIC" = 420L, "RADIQGLOGY DIAGNGSTIC" = 420L,
  "RAPIGLOGY OLAGNOSTIC S" = 420L, "RADIO OLAG RESEARCH S" = 420L,
  "TRANSITIGNAL BOSTON U" = 999L
)
still <- is.na(old$spec) & old$ptxt %in% names(manual_map)
old$spec[still] <- manual_map[old$ptxt[still]]

unmapped <- old %>% filter(is.na(spec)) %>% count(ptxt, sort = TRUE)
if (nrow(unmapped) > 0) {
  message("Unmapped 2000-01 specialty texts (rows dropped from panel):")
  print(unmapped, n = 30)
}

prog_long <- bind_rows(
  modern %>% select(year, inst_code, spec, quota, matched, state, hospital, city),
  old %>% filter(!is.na(spec)) %>%
    select(year, inst_code, spec, quota, matched, state, hospital, city)
)

# ---- 2. aggregate to institution x specialty x year --------------------
agg <- prog_long %>%
  group_by(inst_code, spec, year) %>%
  summarise(quota = sum(quota), matched = sum(matched), .groups = "drop")

# ---- 3. name standardization crosswalk ---------------------------------
# Institution/city/state: prefer the 2010-2019 file's spelling; else the
# modal spelling from the text-layer years (2007-09, exact prints); else
# the modal OCR spelling.
base_inst <- base %>%
  mutate(inst4 = as.integer(floor(institution_code))) %>%
  group_by(inst4) %>%
  summarise(institution_name = first(institution_name),
            b_city = first(city), b_state = first(trimws(state)),
            .groups = "drop")

clean_name <- function(x) {
  x <- str_replace_all(x, "[|:;~_]+", " ")     # OCR margin junk
  x <- str_squish(x)
  # stray leading letters (typewriter margin bleed) — but keep a real
  # leading "U " (university) or "A " that is part of the name
  x <- str_remove(x, "^([IilEFTLe] )+")
  x <- str_remove(x, "^[^A-Za-z0-9]+")
  str_squish(x)
}
modal_of <- function(x) {
  x <- clean_name(x)
  x <- x[x != "" & !is.na(x)]
  if (length(x) == 0) return(NA_character_)
  names(sort(table(x), decreasing = TRUE))[1]
}
ours_inst <- prog_long %>%
  mutate(pref = year >= 2007) %>%
  group_by(inst_code) %>%
  summarise(
    o_name = if (any(pref)) modal_of(hospital[pref]) else modal_of(hospital),
    o_city = if (any(pref)) modal_of(city[pref]) else modal_of(city),
    o_state = modal_of(state),
    .groups = "drop") %>%
  mutate(inst4 = as.integer(inst_code))

xwalk <- ours_inst %>%
  left_join(base_inst, by = "inst4") %>%
  mutate(
    institution_name_std = coalesce(institution_name, str_to_title(o_name)),
    # str_to_title lowercases state suffixes / acronyms; restore them
    institution_name_std = str_replace(institution_name_std,
                                       "-([A-Z][a-z])$",
                                       function(m) toupper(m)),
    city_std = coalesce(na_if(b_city, ""), str_to_title(o_city)),
    state_std = coalesce(na_if(b_state, ""), o_state),
    name_source = if_else(is.na(institution_name), "extraction_modal", "dta2010")
  )
write_csv(xwalk %>%
            select(inst_code, institution_name_std, city_std, state_std,
                   name_source, extraction_name = o_name),
          here("data", "datasets",
               "nrmp-2000-2009-standardization-crosswalk.csv"))

# specialty names from the base vocabulary; fill gaps from text years
spec_vocab <- base %>%
  count(specialty_code, program_name_standardized) %>%
  group_by(specialty_code) %>% slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>% select(spec = specialty_code, program_name_standardized)
spec_ours <- modern %>% filter(year >= 2007) %>%
  count(spec, program) %>% group_by(spec) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>% ungroup() %>%
  transmute(spec, ours_name = program)

# ---- 4. wide reshape ---------------------------------------------------
wide <- agg %>%
  pivot_wider(names_from = year, values_from = c(quota, matched),
              names_sep = "_", values_fill = 0L) %>%
  mutate(inst4 = as.integer(inst_code))

yrs <- 2000:2009
qcols <- paste0("quota_", yrs)
mcols <- paste0("matched_", yrs)
for (cl in c(qcols, mcols)) if (!cl %in% names(wide)) wide[[cl]] <- 0L

# ---- 5. merge with the 2010-2019 baseline ------------------------------
base2 <- base %>%
  mutate(inst4 = as.integer(floor(institution_code)),
         spec = specialty_code)

# split institution codes (e.g. 1978.1/.2): historical values go to the
# split row with the same specialty and the largest total 2010-2019 quota
base2 <- base2 %>%
  group_by(inst4, spec) %>%
  mutate(.tot = rowSums(across(starts_with("quota_2")), na.rm = TRUE),
         hist_target = row_number(desc(.tot)) == 1) %>%
  ungroup() %>% select(-.tot)

merged <- base2 %>%
  left_join(wide %>% select(inst4, spec, all_of(c(qcols, mcols))),
            by = c("inst4", "spec")) %>%
  mutate(across(all_of(c(qcols, mcols)),
                ~ if_else(hist_target, coalesce(., 0L), 0L)))

new_rows <- wide %>%
  anti_join(base2 %>% distinct(inst4, spec), by = c("inst4", "spec")) %>%
  left_join(xwalk %>% mutate(inst4 = as.integer(inst_code)) %>%
              select(inst4, institution_name_std, city_std, state_std),
            by = "inst4") %>%
  left_join(spec_vocab, by = "spec") %>%
  left_join(spec_ours, by = "spec") %>%
  transmute(
    state = state_std,
    institution_code = as.numeric(inst4),
    institution_name = institution_name_std,
    city = city_std,
    specialty_code = as.numeric(spec),
    program_name_standardized = coalesce(program_name_standardized, ours_name),
    across_placeholder = NA
  ) %>% select(-across_placeholder) %>%
  bind_cols(wide %>%
              anti_join(base2 %>% distinct(inst4, spec),
                        by = c("inst4", "spec")) %>%
              select(all_of(c(qcols, mcols))))
for (cl in grep("^(quota|matched)_201", names(base), value = TRUE)) {
  new_rows[[cl]] <- 0L
}

panel <- bind_rows(
  merged %>% select(-inst4, -spec, -hist_target),
  new_rows
) %>%
  arrange(institution_code, specialty_code)

# ---- 6. missing-not-zero corrections ----------------------------------
# (a) 2002 source-PDF gap: GA/HI/ID + affected FL/IL institutions -> NA
gap_inst <- agg %>%
  group_by(inst_code) %>%
  summarise(y01 = any(year == 2001), y02 = any(year == 2002),
            y03 = any(year == 2003), .groups = "drop") %>%
  filter(y01, y03, !y02) %>% pull(inst_code) %>% as.integer()
ga_hi_id <- panel %>%
  filter(trimws(state) %in% c("GA", "HI", "ID")) %>%
  pull(institution_code)
gap_codes <- union(gap_inst, floor(ga_hi_id))
panel <- panel %>%
  mutate(quota_2002 = if_else(floor(institution_code) %in% gap_codes,
                              NA_integer_, quota_2002),
         matched_2002 = if_else(floor(institution_code) %in% gap_codes,
                                NA_integer_, matched_2002))

# (b) Puerto Rico: excluded from the panel (absent from the 2010-2019
# source and not wanted in the analysis sample)
panel <- panel %>% filter(is.na(state) | trimws(state) != "PR")

# ---- 7. labels + write -------------------------------------------------
for (y in 2000:2019) {
  qc <- paste0("quota_", y); mc <- paste0("matched_", y)
  if (qc %in% names(panel)) attr(panel[[qc]], "label") <- paste("(sum) quota", y)
  if (mc %in% names(panel)) attr(panel[[mc]], "label") <- paste("(sum) matched", y)
}
attr(panel$institution_name, "label") <- "institution_name (2010-19 spelling where available)"
attr(panel$program_name_standardized, "label") <- "program_name_standardized"

ord <- c("state", "institution_code", "institution_name", "city",
         "specialty_code", "program_name_standardized",
         paste0(rep(c("quota_", "matched_"), times = 20),
                rep(2000:2019, each = 2)))
panel <- panel %>% select(all_of(intersect(ord, names(panel))),
                          everything())

write_dta(panel, here("data", "datasets", "2000_2019_residency_programs.dta"))

# ---- 8. validation report ---------------------------------------------
cat("\n== panel summary ==\n")
cat("rows:", nrow(panel), " (base:", nrow(base),
    "+ new:", nrow(new_rows), ")\n")
cat("institutions:", n_distinct(floor(panel$institution_code)), "\n")
for (y in 2000:2019) {
  qc <- paste0("quota_", y)
  if (qc %in% names(panel))
    cat(sprintf("%d: quota=%6.0f (NA rows: %d)\n", y,
                sum(panel[[qc]], na.rm = TRUE), sum(is.na(panel[[qc]]))))
}
# reconciliation vs the long extraction
chk <- agg %>% group_by(year) %>% summarise(q = sum(quota), .groups = "drop")
cat("\nlong-file totals for comparison:\n")
print(as.data.frame(chk), row.names = FALSE)
cat("\nname sources:\n")
print(table(xwalk$name_source))
