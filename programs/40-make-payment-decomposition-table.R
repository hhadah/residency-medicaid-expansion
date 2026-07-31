# ==============================================================================
# 40-make-payment-decomposition-table.R
# Build the appendix payment-decomposition table (log intensive margin on the
# positive subsample + extensive-margin LPM) from pipeline outputs.
# Inputs:  output/tables/logpositive-payments-summary.csv   (script 19)
# Output:  my_paper/tables/reg_logpositive_summary.tex      (bare tabular, INV-13)
# ==============================================================================

library(here)
library(readr)
library(dplyr)

lp <- read_csv(here("output", "tables", "logpositive-payments-summary.csv"),
               show_col_types = FALSE)

row_of <- function(o, s) {
  r <- lp |> filter(outcome == o, spec == s)
  stopifnot(nrow(r) == 1)
  r
}

fmt  <- function(x, d = 3) formatC(x, format = "f", digits = d)
cell <- function(r) sprintf("$%s$ & (%s) & %s", fmt(r$b), fmt(r$se), fmt(r$p, 2))

panel <- function(o, title) {
  c(sprintf("\\multicolumn{4}{l}{\\textit{%s}} \\\\", title),
    "\\midrule",
    sprintf("\\quad Log payments (positive subsample), post-expansion effect & %s \\\\",
            cell(row_of(o, "log_pooled"))),
    sprintf("\\quad Log payments (positive subsample), volume-arm interaction & %s \\\\",
            cell(row_of(o, "log_vol_diff"))),
    sprintf("\\quad Any payment (extensive margin), post-expansion effect & %s \\\\",
            cell(row_of(o, "ext_pooled"))),
    sprintf("\\quad Any payment (extensive margin), volume-arm interaction & %s \\\\",
            cell(row_of(o, "ext_vol_diff"))))
}

lines <- c(
  "\\begin{tabular}{lccc}",
  "\\toprule",
  " & Coefficient & (s.e.) & $p$-value \\\\",
  "\\midrule",
  panel("dgme_payment", "Panel A: Direct GME payments"),
  "\\\\[0.5em]",
  panel("ime_payment", "Panel B: Indirect medical education payments"),
  "\\bottomrule",
  "\\end{tabular}"
)

writeLines(lines, here("my_paper", "tables", "reg_logpositive_summary.tex"))
message("Wrote my_paper/tables/reg_logpositive_summary.tex")
print(lines)
