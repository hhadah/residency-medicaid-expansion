# ==============================================================================
# 10-balance-table.R
# Baseline (2010) summary statistics and balance tables from ONE variable
# registry, so the summary-statistics table (main text) and the GME-formula
# balance table (appendix) report the same characteristics.
#
# Table 1 (sumstats): All / Expansion / Never-expansion state means, plus a
#   Welch t-test of the expansion-vs-never difference (state as the unit).
# Balance table: Volume-responsive / Fixed-none / Never-expansion means, plus a
#   Welch t-test of the volume-vs-fixed/none difference (state as the unit),
#   and the population-weighted program-level baseline row.
#
# Inputs:  data/datasets/cleaned_program_residency_medicaid.dta
#          data/datasets/cleaned_residency_medicaid.dta (specialty panel, for
#            the primary-care share)
#          data/raw/gme_formula_classification.csv
# Outputs: my_paper/tables/sumstats_main.tex        (bare tabular, INV-13)
#          my_paper/tables/balance_gme_formula.tex  (bare tabular, INV-13)
#          output/tables/sumstats-main.csv
#          output/tables/balance-gme-formula.csv
# ==============================================================================

library(here)
library(haven)
library(dplyr)
library(readr)
library(tidyr)

panel <- read_dta(here("data", "datasets", "cleaned_program_residency_medicaid.dta")) |>
  mutate(state = toupper(trimws(state)))

spec_panel <- read_dta(here("data", "datasets", "cleaned_residency_medicaid.dta")) |>
  mutate(state = toupper(trimws(state)))

gme <- read_csv(here("data", "raw", "gme_formula_classification.csv"),
                show_col_types = FALSE) |>
  mutate(state = toupper(trimws(state))) |>
  select(state, gme_formula)

# --- primary-care share of matched positions by state, 2010 ------------------
pc_share <- spec_panel |>
  filter(year == 2010) |>
  group_by(state) |>
  summarise(
    pc_share = sum(matched[gen_specialty_alt %in% c("FM", "IM", "Peds")],
                   na.rm = TRUE) / sum(matched, na.rm = TRUE),
    .groups = "drop"
  )

# --- fill share by state, 2010: measured at the SPECIALTY-PROGRAM level ------
# (the NRMP program unit the manuscript's "~86 percent fill every position"
# claim refers to; the institution-level analogue is much lower because an
# institution "fills" only if every one of its specialty programs fills)
fill_state <- spec_panel |>
  filter(year == 2010, quota > 0) |>
  group_by(state) |>
  summarise(fill_share = mean(matched == quota, na.rm = TRUE), .groups = "drop")

# --- state-level 2010 registry -----------------------------------------------
base10 <- panel |>
  filter(year == 2010) |>
  group_by(state) |>
  summarise(
    n_programs        = n(),
    matched_total     = sum(matched, na.rm = TRUE),
    matched_per_prog  = mean(matched, na.rm = TRUE),
    quota_total       = sum(quota, na.rm = TRUE),
    quota_per_prog    = mean(quota, na.rm = TRUE),
    pop_2010          = first(total_population_10),
    rural_share       = mean(rural_urban_2010 > 3, na.rm = TRUE),
    treated_state     = first(treated_state),
    year_expanded     = first(year_expanded),
    .groups = "drop"
  ) |>
  mutate(
    matched_per_100k = matched_total / pop_2010 * 100000,
    quota_per_100k   = quota_total / pop_2010 * 100000,
    pop_mil          = pop_2010 / 1e6
  ) |>
  left_join(pc_share, by = "state") |>
  left_join(fill_state, by = "state") |>
  left_join(gme, by = "state") |>
  mutate(
    exp_group = if_else(treated_state == 1, "expansion", "never"),
    gme_group = case_when(
      treated_state == 0                  ~ "never",
      gme_formula == "volume"             ~ "volume",
      gme_formula %in% c("fixed", "none") ~ "notvol",
      TRUE                                ~ NA_character_
    )
  )

dropped <- base10 |> filter(is.na(gme_group))
if (nrow(dropped) > 0) {
  message("Excluded from balance groups (unclassified GME formula): ",
          paste(dropped$state, collapse = ", "))
}

# --- shared variable registry (rows appear in BOTH tables) --------------------
vars <- tribble(
  ~var,               ~label,                                             ~fmt,
  "matched_per_prog", "Matched positions per program",                    "%.1f",
  "quota_per_prog",   "Offered positions (quota) per program",            "%.1f",
  "fill_share",       "Share of programs filling every offered position", "%.2f",
  "matched_per_100k", "Matched positions per 100{,}000 population",       "%.2f",
  "quota_per_100k",   "Offered positions (quota) per 100{,}000",          "%.2f",
  "n_programs",       "Number of programs",                               "%.1f",
  "pop_mil",          "State population (millions)",                      "%.1f",
  "rural_share",      "Rural program share",                              "%.2f",
  "pc_share",         "Primary-care share of matched positions",          "%.2f",
  "year_expanded",    "Mean expansion year",                              "%.1f"
)

grp_mean <- function(data, gvar, g, v) {
  x <- data[[v]][data[[gvar]] == g & !is.na(data[[gvar]])]
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

welch <- function(data, gvar, gA, gB, v) {
  x <- data[[v]][data[[gvar]] == gA & !is.na(data[[gvar]])]
  y <- data[[v]][data[[gvar]] == gB & !is.na(data[[gvar]])]
  x <- x[!is.na(x)]; y <- y[!is.na(y)]
  if (length(x) < 2 || length(y) < 2) return(c(diff = NA_real_, p = NA_real_))
  tt <- t.test(x, y)
  c(diff = mean(x) - mean(y), p = tt$p.value)
}

fmt_num <- function(x, fmt) ifelse(is.na(x), "--", sprintf(fmt, x))
fmt_p   <- function(p) ifelse(is.na(p), "--",
                       ifelse(p < 0.001, "$<$0.001", sprintf("%.3f", p)))

# ==============================================================================
# Table 1: summary statistics (All / Expansion / Never; diff = expansion - never)
# ==============================================================================
sum_rows <- vars |>
  rowwise() |>
  mutate(
    all_m = mean(base10[[var]], na.rm = TRUE),
    exp_m = grp_mean(base10, "exp_group", "expansion", var),
    nev_m = grp_mean(base10, "exp_group", "never", var),
    stat  = list(welch(base10, "exp_group", "expansion", "never", var))
  ) |>
  ungroup() |>
  mutate(diff = sapply(stat, `[[`, "diff"),
         p    = sapply(stat, `[[`, "p")) |>
  select(-stat)
# expansion-year row: never-expansion states have no expansion year
sum_rows <- sum_rows |>
  mutate(all_m = if_else(var == "year_expanded", exp_m, all_m))

n_exp <- sum(base10$exp_group == "expansion")
n_nev <- sum(base10$exp_group == "never")
n_prog_2010 <- sum(base10$n_programs)
n_obs_panel <- nrow(panel)

write_csv(sum_rows |> select(-fmt) |>
            rename(variable = label) |> select(-var),
          here("output", "tables", "sumstats-main.csv"))

lines <- c(
  "\\begin{tabular}{>{\\raggedright\\arraybackslash}p{2.35in}ccccc}",
  "\\toprule",
  " & All & Expansion & Never- & Difference & \\\\",
  "Baseline characteristic (2010) & states & states & expansion & (2)$-$(3) & $p$-value \\\\",
  " & (1) & (2) & (3) & & \\\\",
  "\\midrule"
)
for (i in seq_len(nrow(sum_rows))) {
  r <- sum_rows[i, ]
  lines <- c(lines, paste0(
    r$label, " & ", fmt_num(r$all_m, r$fmt), " & ", fmt_num(r$exp_m, r$fmt),
    " & ", fmt_num(r$nev_m, r$fmt), " & ", fmt_num(r$diff, r$fmt),
    " & ", fmt_p(r$p), " \\\\"))
}
lines <- c(lines,
  "\\midrule",
  paste0("Number of states & ", n_exp + n_nev, " & ", n_exp, " & ", n_nev, " & & \\\\"),
  paste0("Number of programs & ", n_prog_2010, " & & & & \\\\"),
  paste0("Hospital-year observations (2010--2019) & ",
         format(n_obs_panel, big.mark = "{,}"), " & & & & \\\\"),
  "\\bottomrule",
  "\\end{tabular}")
writeLines(lines, here("my_paper", "tables", "sumstats_main.tex"))

# ==============================================================================
# Balance table: Volume / Fixed-none / Never; diff = volume - fixed/none
# ==============================================================================
bal <- base10 |> filter(!is.na(gme_group))

# program-level, population-weighted baseline per-capita outcome (balance only)
wtd10 <- panel |>
  filter(year == 2010) |>
  left_join(base10 |> select(state, gme_group), by = "state") |>
  filter(!is.na(gme_group)) |>
  group_by(gme_group) |>
  summarise(w_mp100k = weighted.mean(matched_per_100k, total_population_10,
                                     na.rm = TRUE), .groups = "drop")
w_get <- function(g) {
  x <- wtd10$w_mp100k[wtd10$gme_group == g]
  if (length(x) == 0) NA_real_ else x
}

bal_rows <- vars |>
  rowwise() |>
  mutate(
    vol = grp_mean(bal, "gme_group", "volume", var),
    fix = grp_mean(bal, "gme_group", "notvol", var),
    nev = grp_mean(bal, "gme_group", "never", var),
    stat = list(welch(bal, "gme_group", "volume", "notvol", var))
  ) |>
  ungroup() |>
  mutate(diff = sapply(stat, `[[`, "diff"),
         p    = sapply(stat, `[[`, "p")) |>
  select(-stat)

wrow <- tibble(
  var = "w_mp100k",
  label = "\\quad Program-level mean, population-weighted",
  fmt = "%.2f",
  vol = w_get("volume"), fix = w_get("notvol"), nev = w_get("never"),
  diff = w_get("volume") - w_get("notvol"), p = NA_real_
)
i100k <- which(bal_rows$var == "matched_per_100k")
bal_rows <- bind_rows(bal_rows[1:i100k, ], wrow,
                      bal_rows[(i100k + 1):nrow(bal_rows), ])

n_vol <- sum(bal$gme_group == "volume")
n_fix <- sum(bal$gme_group == "notvol")
n_nev2 <- sum(bal$gme_group == "never")

write_csv(bal_rows |> select(-fmt) |>
            transmute(variable = label, volume = vol, fixed_none = fix,
                      never = nev, diff_vol_minus_fixed = diff, p_value = p),
          here("output", "tables", "balance-gme-formula.csv"))

lines <- c(
  "\\begin{tabular}{>{\\raggedright\\arraybackslash}p{3in}ccccc}",
  "\\toprule",
  " & Volume- & Fixed/ & Never- & Difference & \\\\",
  paste0("Baseline characteristic (2010) & responsive & none & expansion",
         " & (1)$-$(2) & $p$-value \\\\"),
  " & (1) & (2) & (3) & & \\\\",
  "\\midrule"
)
for (i in seq_len(nrow(bal_rows))) {
  r <- bal_rows[i, ]
  lines <- c(lines, paste0(
    r$label, " & ", fmt_num(r$vol, r$fmt), " & ", fmt_num(r$fix, r$fmt),
    " & ", fmt_num(r$nev, r$fmt), " & ", fmt_num(r$diff, r$fmt),
    " & ", fmt_p(r$p), " \\\\"))
}
lines <- c(lines,
  "\\midrule",
  paste0("Number of states & ", n_vol, " & ", n_fix, " & ", n_nev2, " & & \\\\"),
  "\\bottomrule",
  "\\end{tabular}")
writeLines(lines, here("my_paper", "tables", "balance_gme_formula.tex"))

message("Wrote sumstats_main.tex, balance_gme_formula.tex and CSV twins")
print(sum_rows |> select(label, all_m, exp_m, nev_m, diff, p), n = Inf)
print(bal_rows |> select(label, vol, fix, nev, diff, p), n = Inf)
