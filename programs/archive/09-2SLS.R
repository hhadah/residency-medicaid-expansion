# =============================================================================
# 2SLS / IV Regression
# First stage:  Medicaid expansion → Residency slots
# Second stage: Residency slots (instrumented) → Hospital quality scores
# =============================================================================

# author: Hussein Hadah
# first created: March 18, 2026
# last updated: March 18, 2026

# -------------------------------------------------------------------------
# Load merged residency-CMS data
# -------------------------------------------------------------------------
residency_cms <- read_dta(file.path(datasets, "residency_cms_merged.dta"))

# -------------------------------------------------------------------------
# Aggregate to institution-year level
# -------------------------------------------------------------------------
panel <- residency_cms |>
  group_by(state, institution_code, institution_name, year) |>
  summarize(
    quota      = sum(quota, na.rm = TRUE),
    matched    = sum(matched, na.rm = TRUE),
    unmatched  = sum(unmatched, na.rm = TRUE),
    total_population_10 = first(total_population_10),
    medicaid_expansion  = first(medicaid_expansion),
    ami_score       = first(ami_score),
    cabg_score      = first(cabg_score),
    copd_score      = first(copd_score),
    hf_score        = first(hf_score),
    hip_knee_score  = first(hip_knee_score),
    hosp_wide_score = first(hosp_wide_score),
    pn_score        = first(pn_score),
    stk_score       = first(stk_score),
    .groups = "drop"
  ) |>
  mutate(
    quota_per_100k   = (quota / total_population_10) * 100000,
    matched_per_100k = (matched / total_population_10) * 100000,
    inst_id  = as.factor(institution_code),
    state_fe = as.factor(state)
  )

cat("Panel:", nrow(panel), "rows,",
    n_distinct(panel$institution_code), "institutions,",
    "years", paste(range(panel$year), collapse = "-"), "\n")

# -------------------------------------------------------------------------
# Variable definitions
# -------------------------------------------------------------------------
score_outcomes <- c(
  "hosp_wide_score", "ami_score", "cabg_score", "copd_score",
  "hf_score", "hip_knee_score", "pn_score", "stk_score"
)

score_labels <- c(
  "hosp_wide_score" = "Hospital-Wide",
  "ami_score"       = "AMI",
  "cabg_score"      = "CABG",
  "copd_score"      = "COPD",
  "hf_score"        = "Heart Failure",
  "hip_knee_score"  = "Hip/Knee",
  "pn_score"        = "Pneumonia",
  "stk_score"       = "Stroke"
)

# Column headers with line breaks for LaTeX
score_headers <- c(
  "hosp_wide_score" = "\\specialcell{(1) \\\\ Hospital-Wide \\\\ Readmission}",
  "ami_score"       = "\\specialcell{(2) \\\\ AMI \\\\ Readmission}",
  "cabg_score"      = "\\specialcell{(3) \\\\ CABG \\\\ Readmission}",
  "copd_score"      = "\\specialcell{(4) \\\\ COPD \\\\ Readmission}",
  "hf_score"        = "\\specialcell{(5) \\\\ Heart Failure \\\\ Readmission}",
  "hip_knee_score"  = "\\specialcell{(6) \\\\ Hip/Knee \\\\ Readmission}",
  "pn_score"        = "\\specialcell{(7) \\\\ Pneumonia \\\\ Readmission}",
  "stk_score"       = "\\specialcell{(8) \\\\ Stroke \\\\ Readmission}"
)

# =========================================================================
# Regressions
# =========================================================================

# --- Helper: run IV for all score outcomes ---
run_iv_all <- function(endog_var, data) {
  results <- lapply(score_outcomes, function(y) {
    fml <- as.formula(paste0(
      y, " ~ 1 | inst_id + year | ", endog_var, " ~ medicaid_expansion"
    ))
    feols(fml, cluster = ~state_fe, data = data)
  })
  names(results) <- score_headers[score_outcomes]
  results
}

# --- Helper: run reduced form for all score outcomes ---
run_rf_all <- function(data) {
  results <- lapply(score_outcomes, function(y) {
    fml <- as.formula(paste0(y, " ~ medicaid_expansion | inst_id + year"))
    feols(fml, cluster = ~state_fe, data = data)
  })
  names(results) <- score_headers[score_outcomes]
  results
}

# =========================================================================
# TABLE 1: 2SLS using Quota
# Panel A: Quota (levels)
# Panel B: Quota per 100k
# =========================================================================

regression_quota <- list(
  "Panel A: Quota (levels)" = run_iv_all("quota", panel),
  "Panel B: Quota per 100,000" = run_iv_all("quota_per_100k", panel)
)

# =========================================================================
# TABLE 2: 2SLS using Matched
# Panel A: Matched (levels)
# Panel B: Matched per 100k
# =========================================================================

regression_matched <- list(
  "Panel A: Matched (levels)" = run_iv_all("matched", panel),
  "Panel B: Matched per 100,000" = run_iv_all("matched_per_100k", panel)
)

# =========================================================================
# TABLE 3: Reduced form (Medicaid expansion → Scores directly)
# =========================================================================

regression_rf <- run_rf_all(panel)

# =========================================================================
# Summary statistics rows
# =========================================================================

# Compute outcome means
calc_mean <- function(x) sprintf("%.3f", round(mean(x, na.rm = TRUE), 3))

summary_data_mean <- sapply(panel[, score_outcomes], calc_mean)

# First stage F-statistics (from Panel A quota regression)
fstat_quota <- sapply(regression_quota[["Panel A: Quota (levels)"]], function(m) {
  format(round(fitstat(m, "ivf")[[1]]$stat, digits = 2), big.mark = ",")
})

fstat_quota_100k <- sapply(regression_quota[["Panel B: Quota per 100,000"]], function(m) {
  format(round(fitstat(m, "ivf")[[1]]$stat, digits = 2), big.mark = ",")
})

fstat_matched <- sapply(regression_matched[["Panel A: Matched (levels)"]], function(m) {
  format(round(fitstat(m, "ivf")[[1]]$stat, digits = 2), big.mark = ",")
})

fstat_matched_100k <- sapply(regression_matched[["Panel B: Matched per 100,000"]], function(m) {
  format(round(fitstat(m, "ivf")[[1]]$stat, digits = 2), big.mark = ",")
})

# Build extra rows for quota table
mean_column_quota <- c("First Stage F-stat (Quota)", "First Stage F-stat (Quota/100k)",
                       "Mean Dep. Var.")
all_means_quota <- rbind(fstat_quota, fstat_quota_100k, summary_data_mean)
all_means_quota <- cbind(mean_column_quota, all_means_quota)
colnames(all_means_quota) <- NULL
rownames(all_means_quota) <- NULL
all_means_quota <- as.data.frame(all_means_quota)
attr(all_means_quota, "position") <- NULL

# Build extra rows for matched table
mean_column_matched <- c("First Stage F-stat (Matched)", "First Stage F-stat (Matched/100k)",
                         "Mean Dep. Var.")
all_means_matched <- rbind(fstat_matched, fstat_matched_100k, summary_data_mean)
all_means_matched <- cbind(mean_column_matched, all_means_matched)
colnames(all_means_matched) <- NULL
rownames(all_means_matched) <- NULL
all_means_matched <- as.data.frame(all_means_matched)
attr(all_means_matched, "position") <- NULL

# Build extra rows for reduced form table
all_means_rf <- data.frame(
  V1 = "Mean Dep. Var.",
  as.data.frame(t(summary_data_mean)),
  stringsAsFactors = FALSE
)
colnames(all_means_rf) <- NULL
rownames(all_means_rf) <- NULL
attr(all_means_rf, "position") <- NULL

# =========================================================================
# Formatting for modelsummary
# =========================================================================

cm_quota <- c(
  "fit_quota"          = "Residency Quota",
  "fit_quota_per_100k" = "Residency Quota per 100k"
)

cm_matched <- c(
  "fit_matched"          = "Matched Residents",
  "fit_matched_per_100k" = "Matched Residents per 100k"
)

cm_rf <- c(
  "medicaid_expansion" = "Medicaid Expansion"
)

f1 <- function(x) format(round(x, 3), big.mark = ".")
f2 <- function(x) format(round(x, 0), big.mark = ",")

gm <- list(
  list(raw = "nobs", clean = "Observations", fmt = f2),
  list(raw = "std.error.type", clean = "Standard Errors", fmt = 0)
)

# =========================================================================
# TABLE 1: 2SLS with Quota → LaTeX
# =========================================================================

modelsummary(
  regression_quota,
  coef_map = cm_quota,
  shape = "rbind",
  stars = c(`***` = 0.01, `**` = 0.05, `*` = 0.1),
  fmt = f1,
  add_rows = all_means_quota,
  gof_map = gm,
  escape = FALSE,
  output = "kableExtra",
  title = "2SLS: Effect of Residency Quota on 30-Day Readmission Rates \\label{tab:2sls_quota}"
) |>
  kable_styling(
    latex_options = c("HOLD_position", "scale_down")
  ) |>
  footnote(
    number = c(
      "\\\\footnotesize{This table presents 2SLS estimates where Medicaid expansion instruments for
residency quota positions. The dependent variables are hospital-level 30-day unplanned readmission
rates; lower values indicate better performance. Panel~A uses quota in levels; Panel~B uses quota
per 100,000 state population. All specifications include institution and year fixed effects.}",
      "\\\\footnotesize{Standard errors are clustered at the state level.}",
      "\\\\footnotesize{Data sources: NRMP residency match data (2010--2019) merged with CMS
Hospital Readmissions Reduction Program data.}",
      "\\\\footnotesize{\\\\textit{Variable definitions.}
Column~(1): Hospital-wide 30-day unplanned readmission rate.
Column~(2): 30-day readmission rate for acute myocardial infarction (AMI) patients.
Column~(3): 30-day readmission rate for coronary artery bypass graft (CABG) patients.
Column~(4): 30-day readmission rate for chronic obstructive pulmonary disease (COPD) patients.
Column~(5): 30-day readmission rate for heart failure (HF) patients.
Column~(6): 30-day readmission rate after elective hip/knee surgery.
Column~(7): 30-day readmission rate for pneumonia patients.
Column~(8): 30-day readmission rate for stroke patients.
All scores are reported as decimals; lower values indicate better hospital performance.}"
    ),
    footnote_as_chunk = FALSE,
    title_format = c("italic"),
    escape = FALSE,
    threeparttable = TRUE
  )

IV_quota <- modelsummary(
  regression_quota,
  coef_map = cm_quota,
  shape = "rbind",
  stars = c(`***` = 0.01, `**` = 0.05, `*` = 0.1),
  fmt = f1,
  add_rows = all_means_quota,
  gof_map = gm,
  escape = FALSE,
  output = "latex",
  title = "2SLS: Effect of Residency Quota on 30-Day Readmission Rates \\label{tab:2sls_quota}"
) |>
  kable_styling(
    latex_options = c("HOLD_position", "scale_down")
  ) |>
  footnote(
    number = c(
      "\\\\footnotesize{This table presents 2SLS estimates where Medicaid expansion instruments for
residency quota positions. The dependent variables are hospital-level 30-day unplanned readmission
rates; lower values indicate better performance. Panel~A uses quota in levels; Panel~B uses quota
per 100,000 state population. All specifications include institution and year fixed effects.}",
      "\\\\footnotesize{Standard errors are clustered at the state level.}",
      "\\\\footnotesize{Data sources: NRMP residency match data (2010--2019) merged with CMS
Hospital Readmissions Reduction Program data.}",
      "\\\\footnotesize{\\\\textit{Variable definitions.}
Column~(1): Hospital-wide 30-day unplanned readmission rate.
Column~(2): 30-day readmission rate for acute myocardial infarction (AMI) patients.
Column~(3): 30-day readmission rate for coronary artery bypass graft (CABG) patients.
Column~(4): 30-day readmission rate for chronic obstructive pulmonary disease (COPD) patients.
Column~(5): 30-day readmission rate for heart failure (HF) patients.
Column~(6): 30-day readmission rate after elective hip/knee surgery.
Column~(7): 30-day readmission rate for pneumonia patients.
Column~(8): 30-day readmission rate for stroke patients.
All scores are reported as decimals; lower values indicate better hospital performance.}"
    ),
    footnote_as_chunk = FALSE,
    title_format = c("italic"),
    escape = FALSE,
    threeparttable = TRUE
  )

IV_quota |>
  save_kable(file.path(tables_wd, "01-2sls_quota_readmission.tex"))
IV_quota |>
  save_kable(file.path(thesis_tabs, "01-2sls_quota_readmission.tex"))

# =========================================================================
# TABLE 2: 2SLS with Matched → LaTeX
# =========================================================================

modelsummary(
  regression_matched,
  coef_map = cm_matched,
  shape = "rbind",
  stars = c(`***` = 0.01, `**` = 0.05, `*` = 0.1),
  fmt = f1,
  add_rows = all_means_matched,
  gof_map = gm,
  escape = FALSE,
  output = "kableExtra",
  title = "2SLS: Effect of Matched Residents on 30-Day Readmission Rates \\label{tab:2sls_matched}"
) |>
  kable_styling(
    latex_options = c("HOLD_position", "scale_down")
  ) |>
  footnote(
    number = c(
      "\\\\footnotesize{This table presents 2SLS estimates where Medicaid expansion instruments for
the number of matched residents. The dependent variables are hospital-level 30-day unplanned
readmission rates; lower values indicate better performance. Panel~A uses matched counts in levels;
Panel~B uses matched per 100,000 state population. All specifications include institution and year
fixed effects.}",
      "\\\\footnotesize{Standard errors are clustered at the state level.}",
      "\\\\footnotesize{Data sources: NRMP residency match data (2010--2019) merged with CMS
Hospital Readmissions Reduction Program data.}",
      "\\\\footnotesize{\\\\textit{Variable definitions.}
Column~(1): Hospital-wide 30-day unplanned readmission rate.
Column~(2): 30-day readmission rate for acute myocardial infarction (AMI) patients.
Column~(3): 30-day readmission rate for coronary artery bypass graft (CABG) patients.
Column~(4): 30-day readmission rate for chronic obstructive pulmonary disease (COPD) patients.
Column~(5): 30-day readmission rate for heart failure (HF) patients.
Column~(6): 30-day readmission rate after elective hip/knee surgery.
Column~(7): 30-day readmission rate for pneumonia patients.
Column~(8): 30-day readmission rate for stroke patients.
All scores are reported as decimals; lower values indicate better hospital performance.}"
    ),
    footnote_as_chunk = FALSE,
    title_format = c("italic"),
    escape = FALSE,
    threeparttable = TRUE
  )

IV_matched <- modelsummary(
  regression_matched,
  coef_map = cm_matched,
  shape = "rbind",
  stars = c(`***` = 0.01, `**` = 0.05, `*` = 0.1),
  fmt = f1,
  add_rows = all_means_matched,
  gof_map = gm,
  escape = FALSE,
  output = "latex",
  title = "2SLS: Effect of Matched Residents on 30-Day Readmission Rates \\label{tab:2sls_matched}"
) |>
  kable_styling(
    latex_options = c("HOLD_position", "scale_down")
  ) |>
  footnote(
    number = c(
      "\\\\footnotesize{This table presents 2SLS estimates where Medicaid expansion instruments for
the number of matched residents. The dependent variables are hospital-level 30-day unplanned
readmission rates; lower values indicate better performance. Panel~A uses matched counts in levels;
Panel~B uses matched per 100,000 state population. All specifications include institution and year
fixed effects.}",
      "\\\\footnotesize{Standard errors are clustered at the state level.}",
      "\\\\footnotesize{Data sources: NRMP residency match data (2010--2019) merged with CMS
Hospital Readmissions Reduction Program data.}",
      "\\\\footnotesize{\\\\textit{Variable definitions.}
Column~(1): Hospital-wide 30-day unplanned readmission rate.
Column~(2): 30-day readmission rate for acute myocardial infarction (AMI) patients.
Column~(3): 30-day readmission rate for coronary artery bypass graft (CABG) patients.
Column~(4): 30-day readmission rate for chronic obstructive pulmonary disease (COPD) patients.
Column~(5): 30-day readmission rate for heart failure (HF) patients.
Column~(6): 30-day readmission rate after elective hip/knee surgery.
Column~(7): 30-day readmission rate for pneumonia patients.
Column~(8): 30-day readmission rate for stroke patients.
All scores are reported as decimals; lower values indicate better hospital performance.}"
    ),
    footnote_as_chunk = FALSE,
    title_format = c("italic"),
    escape = FALSE,
    threeparttable = TRUE
  )

IV_matched |>
  save_kable(file.path(tables_wd, "02-2sls_matched_readmission.tex"))
IV_matched |>
  save_kable(file.path(thesis_tabs, "02-2sls_matched_readmission.tex"))

# =========================================================================
# TABLE 3: First stage → LaTeX
# =========================================================================

first_stage <- list(
  "Panel A: Levels" = list(
    "\\specialcell{(1) \\\\ Quota}" = feols(
      quota ~ medicaid_expansion | inst_id + year,
      cluster = ~state_fe, data = panel),
    "\\specialcell{(2) \\\\ Matched}" = feols(
      matched ~ medicaid_expansion | inst_id + year,
      cluster = ~state_fe, data = panel)
  ),
  "Panel B: Per 100,000 Population" = list(
    "\\specialcell{(1) \\\\ Quota}" = feols(
      quota_per_100k ~ medicaid_expansion | inst_id + year,
      cluster = ~state_fe, data = panel),
    "\\specialcell{(2) \\\\ Matched}" = feols(
      matched_per_100k ~ medicaid_expansion | inst_id + year,
      cluster = ~state_fe, data = panel)
  )
)

cm_fs <- c("medicaid_expansion" = "Medicaid Expansion")

# Means for first-stage dependent variables
fs_means <- c(
  sprintf("%.3f", mean(panel$quota, na.rm = TRUE)),
  sprintf("%.3f", mean(panel$matched, na.rm = TRUE))
)
fs_means_100k <- c(
  sprintf("%.3f", mean(panel$quota_per_100k, na.rm = TRUE)),
  sprintf("%.3f", mean(panel$matched_per_100k, na.rm = TRUE))
)

fs_add_rows <- data.frame(
  V1 = c("Mean Dep. Var. (levels)", "Mean Dep. Var. (per 100k)"),
  V2 = c(fs_means[1], fs_means_100k[1]),
  V3 = c(fs_means[2], fs_means_100k[2])
)
colnames(fs_add_rows) <- NULL

FS_table <- modelsummary(
  first_stage,
  coef_map = cm_fs,
  shape = "rbind",
  stars = c(`***` = 0.01, `**` = 0.05, `*` = 0.1),
  fmt = f1,
  add_rows = fs_add_rows,
  gof_map = gm,
  escape = FALSE,
  output = "kableExtra",
  title = "First Stage: Effect of Medicaid Expansion on Residency Positions \\label{tab:first_stage}"
) |>
  kable_styling(
    latex_options = c("HOLD_position")
  ) |>
  footnote(
    number = c(
      "\\\\footnotesize{This table presents the first-stage estimates of Medicaid expansion on
residency positions. Panel~A reports effects on quota and matched positions in levels.
Panel~B reports effects per 100,000 state population. All specifications include institution
and year fixed effects.}",
      "\\\\footnotesize{Standard errors are clustered at the state level.}",
      "\\\\footnotesize{Data source: NRMP residency match data (2010--2019).}"
    ),
    footnote_as_chunk = FALSE,
    title_format = c("italic"),
    escape = FALSE,
    threeparttable = TRUE
  )

FS_table

# =========================================================================
# Coefficient plots
# =========================================================================

make_iv_plot <- function(iv_results, endog_label) {
  dat <- modelplot(iv_results, conf_level = .95, draw = FALSE) |>
    filter(str_detect(term, "fit_"))

  # Clean model names for plotting (remove \specialcell LaTeX)
  dat <- dat |>
    mutate(model = str_remove_all(model, "\\\\specialcell\\{|\\}") |>
             str_replace_all("\\\\\\\\", " ") |>
             str_squish())

  ggplot(dat, aes(y = model, x = estimate,
                  xmin = conf.low, xmax = conf.high)) +
    geom_vline(xintercept = 0, color = "red", linetype = "dotted", linewidth = 1) +
    geom_pointrange(color = "red", size = 1.1) +
    geom_text_repel(aes(label = round(estimate, digits = 4)),
                    size = 10, nudge_y = 0.3) +
    theme_customs() +
    theme(
      legend.position = "none",
      axis.text.y  = element_text(size = 28),
      axis.text.x  = element_text(size = 28),
      axis.title.x = element_text(size = 30),
      axis.title.y = element_text(size = 30)
    ) +
    labs(
      x = paste0("2SLS Estimate (", endog_label, ") with 95% CI"),
      y = "30-Day Readmission Rate"
    ) +
    scale_y_discrete(labels = label_wrap(40))
}

# Plots for quota specifications
p_iv_quota <- make_iv_plot(
  regression_quota[["Panel A: Quota (levels)"]], "Quota")
p_iv_quota_100k <- make_iv_plot(
  regression_quota[["Panel B: Quota per 100,000"]], "Quota/100k")

print(p_iv_quota)
print(p_iv_quota_100k)
# Plots for matched specifications
p_iv_matched <- make_iv_plot(
  regression_matched[["Panel A: Matched (levels)"]], "Matched")
p_iv_matched_100k <- make_iv_plot(
  regression_matched[["Panel B: Matched per 100,000"]], "Matched/100k")
print(p_iv_matched)
print(p_iv_matched_100k)
# Save plots
ggsave(file.path(figures_wd, "13-2sls_quota_readmission.png"),
       p_iv_quota, width = 14, height = 8)
ggsave(file.path(thesis_plots, "13-2sls_quota_readmission.png"),
       p_iv_quota, width = 14, height = 8)

ggsave(file.path(figures_wd, "14-2sls_quota_100k_readmission.png"),
       p_iv_quota_100k, width = 14, height = 8)
ggsave(file.path(thesis_plots, "14-2sls_quota_100k_readmission.png"),
       p_iv_quota_100k, width = 14, height = 8)

ggsave(file.path(figures_wd, "15-2sls_matched_readmission.png"),
       p_iv_matched, width = 14, height = 8)
ggsave(file.path(thesis_plots, "15-2sls_matched_readmission.png"),
       p_iv_matched, width = 14, height = 8)

ggsave(file.path(figures_wd, "16-2sls_matched_100k_readmission.png"),
       p_iv_matched_100k, width = 14, height = 8)
ggsave(file.path(thesis_plots, "16-2sls_matched_100k_readmission.png"),
       p_iv_matched_100k, width = 14, height = 8)

cat("\n========== Done ==========\n")
cat("Tables saved to:", file.path(tables_wd, "latex"), "\n")
cat("Figures saved to:", figures_wd, "\n")
