# ==============================================================================
# 42-make-specgrid-table.R
# Build the appendix weighting-by-denominator specification-grid table.
# Inputs:  output/tables/specification-grid.csv   (script 27)
# Output:  my_paper/tables/reg_specification_grid.tex  (bare tabular, INV-13)
# ==============================================================================

library(here)
library(readr)
library(dplyr)

grid <- read_csv(here("output", "tables", "specification-grid.csv"),
                 show_col_types = FALSE)

labels <- c(
  yrvar_weighted         = "Contemporary denominator, population-weighted (baseline)",
  yrvar_unweighted       = "Contemporary denominator, unweighted",
  fixed_weighted         = "Fixed 2010 denominator, population-weighted",
  fixed_unweighted       = "Fixed 2010 denominator, unweighted",
  state_level_weighted   = "State-level totals, population-weighted",
  state_level_unweighted = "State-level totals, unweighted",
  yrvar_w_noGMEcontrols  = "Excl.\\ four competing-GME control states, weighted",
  yrvar_u_noGMEcontrols  = "Excl.\\ four competing-GME control states, unweighted"
)
rows <- grid |> filter(spec %in% names(labels))
stopifnot(nrow(rows) == length(labels))

fmt  <- function(x, d = 3) formatC(x, format = "f", digits = d)
fmtp <- function(p) ifelse(p < 0.001, "$<0.001$", fmt(p, 2))

body <- vapply(names(labels), function(s) {
  r <- rows |> filter(spec == s)
  sprintf("%s & $%s$ & (%s) & $%s$ & %s & %s \\\\",
          labels[[s]], fmt(r$avg_treat), fmt(r$avg_se),
          fmt(r$pct, 1), fmtp(r$treat_p), fmtp(r$pretrend_p))
}, character(1))

lines <- c(
  "\\begin{tabular}{lccccc}",
  "\\toprule",
  " & Estimate & (s.e.) & Percent & Joint $p$ & Pre-trend $p$ \\\\",
  "\\midrule",
  body[1:4],
  "\\midrule",
  body[5:6],
  "\\midrule",
  body[7:8],
  "\\bottomrule",
  "\\end{tabular}"
)

writeLines(lines, here("my_paper", "tables", "reg_specification_grid.tex"))
message("Wrote my_paper/tables/reg_specification_grid.tex")
print(lines)
