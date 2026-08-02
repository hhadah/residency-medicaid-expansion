# ==============================================================================
# 41-make-mechdiff-table.R
# Build the appendix cross-arm-difference sensitivity table from pipeline
# outputs. The cross-arm difference is the mechanism section's central object;
# this table collects its baseline estimate, classification and sample
# sensitivity, and inference under all reported standards.
# Inputs:  output/tables/reclassification-sensitivity.csv   (script 25)
#          output/tables/notyet-yearvarying-summary.csv     (script 22)
#          output/tables/wild-bootstrap-summary.csv         (script 35)
#          output/tables/ri-extended-summary.csv            (script 33)
# Output:  my_paper/tables/reg_mechdiff_sensitivity.tex     (bare tabular, INV-13)
# ==============================================================================

library(here)
library(readr)
library(dplyr)

tab <- function(f) read_csv(here("output", "tables", f), show_col_types = FALSE)

recl  <- tab("reclassification-sensitivity.csv") |>
  filter(classification == "c2015", spec == "mech_diff")
nyfull <- tab("notyet-yearvarying-summary.csv") |> filter(spec == "nymech_diff_c15")
wb    <- tab("wild-bootstrap-summary.csv") |> filter(spec == "mech_diff_c2015")
ri    <- tab("ri-extended-summary.csv") |> filter(spec == "mechdiff")
stopifnot(nrow(recl) == 6, nrow(nyfull) == 1, nrow(wb) == 1, nrow(ri) == 1)

fmt  <- function(x, d = 3) formatC(x, format = "f", digits = d)
fmtp <- function(p) ifelse(p < 0.001, "$<0.001$", fmt(p, 2))
pnorm2 <- function(b, se) 2 * pnorm(-abs(b / se))
row3 <- function(label, b, se, p) {
  sprintf("%s & $%s$ & (%s) & %s \\\\", label, fmt(b), fmt(se), fmtp(p))
}

base  <- recl |> filter(is.na(flipped))
flips <- recl |> filter(!is.na(flipped))
flip_names <- c(MD = "Maryland", MN = "Minnesota", MT = "Montana",
                IA = "Iowa", NM = "New Mexico")

lines <- c(
  "\\begin{tabular}{lccc}",
  "\\toprule",
  " & Estimate & (s.e.) & $p$-value \\\\",
  "\\midrule",
  "\\multicolumn{4}{l}{\\textit{Panel A: Event-study estimate and classification sensitivity}} \\\\",
  "\\midrule",
  row3("\\quad Baseline (2015 classification)", base$avg_treat, base$avg_se, base$treat_p),
  vapply(seq_len(nrow(flips)), function(i) {
    row3(sprintf("\\quad Flipping %s", flip_names[[flips$flipped[i]]]),
         flips$avg_treat[i], flips$avg_se[i], flips$treat_p[i])
  }, character(1)),
  "\\\\[0.5em]",
  "\\multicolumn{4}{l}{\\textit{Panel B: Alternative design}} \\\\",
  "\\midrule",
  row3("\\quad Timing-only design (not-yet-treated controls)",
       nyfull$avg_treat, nyfull$avg_se, nyfull$treat_p),
  "\\\\[0.5em]",
  "\\multicolumn{4}{l}{\\textit{Panel C: Inference on the baseline difference}} \\\\",
  "\\midrule",
  sprintf("\\quad Static TWFE analog, clustered & $%s$ & (%s) & %s \\\\",
          fmt(wb$b_static), fmt(wb$se_cluster), fmtp(wb$p_cluster)),
  sprintf("\\quad Static TWFE analog, wild cluster bootstrap-$t$ & $%s$ & & %s \\\\",
          fmt(wb$b_static), fmtp(wb$p_boot)),
  sprintf("\\quad Randomization inference (500 draws) & $%s$ & & %s \\\\",
          fmt(ri$obs_att), fmtp(ri$ri_p)),
  "\\bottomrule",
  "\\end{tabular}"
)

writeLines(lines, here("my_paper", "tables", "reg_mechdiff_sensitivity.tex"))
message("Wrote my_paper/tables/reg_mechdiff_sensitivity.tex")
print(lines)
