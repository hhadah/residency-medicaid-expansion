# Pipeline README — script order

Scripts are numbered in **execution order**: if script 01 must run before
script 03, it is named 01. Run the R data build first (`95-make-all.R`,
scripts 01–12), then the Stata analysis (`99-run-all-analysis.do`, scripts
13–29 + 35–36; the RI block 30–34 is run separately — see the commented
block in 99), then the four post-steps:
`python3 37-multiple-testing-qvalues.py` (needs numpy + matplotlib, e.g.
the repo `.venv`), `Rscript 40-make-payment-decomposition-table.R`
(builds `my_paper/tables/reg_logpositive_summary.tex` from script 19's
`logpositive-payments-summary.csv`), `Rscript 41-make-mechdiff-table.R`
(builds `my_paper/tables/reg_mechdiff_sensitivity.tex` from the
reclassification, not-yet-treated, wild-bootstrap, and RI summary CSVs),
and `Rscript 42-make-specgrid-table.R` (builds
`my_paper/tables/reg_specification_grid.tex` from script 27's
`specification-grid.csv`).

**As of 2026-07-30 the Medicaid GME formula classification is the 2015
vintage (`gme_formula_2015`, Henderson 2016) throughout**: scripts 11, 19,
20, 24, 25, 30, 32, 33, 34 all merge `gme_formula_2015` from
`data/raw/gme_formula_classification.csv`. The 2012 column remains in the
CSV for provenance but no estimation or manuscript object uses it.

**As of 2026-07-25 all analysis runs on the FULL 2000–2019 panel**
(`panel_2000_2019_estimation.dta`, built by scripts 05–06: 1,234 NRMP
institutions, activity-window missing-not-zero coding, ten pre-periods by
default in every event study).

## Pipeline

| # | Script | Purpose |
|---|--------|---------|
| 01 | packages-wds.R | packages + path globals |
| 02 | data-cleaning.R | clean 2010-2019 NRMP data + geocode (needs `MAPBOX_API_KEY`); source of RUCA/expansion/crosswalk inputs |
| 03 | state-year-population.R | state population series: 2010–2019 ACS + 2000–2019 (decennial 2000, interpolation 2001–04, ACS 2005–19) |
| 04 | alternative-deflators.R | ACS 18–64 and <150% FPL deflators (2010–2019) |
| 05 | make-2000-2019-residency-panel.R | standardize the 2000–2009 OCR extraction into the 2000–2019 wide file |
| 06 | make-2000-2019-estimation-panels.R | **PRIMARY PANELS**: institution×year + institution×specialty-group×year, entry/exit table |
| 07 | append-gme-funding.R | append CMS GME funding spreadsheets |
| 08 | merge-gme-expansion.R | GME funding panel + expansion status |
| 09 | merge-residency-cms.R | residency↔CMS crosswalk (fallback layer for 10) |
| 10 | merge-residency-gme-funding.R | NRMP→CCN crosswalk (rerun 06 once after first build so provider_ccn attaches) |
| 11 | balance-table.R | sumstats + formula balance tables (full panel, 2010 baseline) |
| 12 | descriptive-figures.R | desc-timing + cohort figures (full panel) + desc-physician-growth (IPUMS) |
| 13 | dd-analysis.do | levels DiD (weighted) |
| 14 | dd-analysis-hetero.do | urban/rural levels + quota |
| 15 | did-analysis-byspecialty.do | specialty levels + quota |
| 16 | dd-analysis-popcnt.do | unweighted / population-control specs |
| 17 | dd-methods-comparison.do | estimator comparison (TWFE, BJS, dCDH, SA, …) |
| 18 | gme-funding-event-study.do | pooled GME payment first stage |
| 19 | gme-firststage-byformula.do | payment first stage by formula arm + PPML robustness |
| 20 | yearvarying-suite.do | **primary spec**: headline, location, mechanism, quota (+ figure), PPML count check |
| 21 | yearvarying-specialty.do | specialty split (hospital × specialty-group) |
| 22 | yearvarying-robustness.do | not-yet-treated (+ mechanism inside), cohort-2014, HonestDiD, **pre-ACA placebo** |
| 23 | leave-one-out.do | LOO: headline (treated+controls) + mechanism objects |
| 24 | linked-sample-reconciliation.do | NRMP intake vs cost-report FTEs, linked sample (full window) |
| 25 | mechanism-reclassification.do | 2015-vintage classification + judgment flips |
| 26 | deflator-robustness.do | log-pop outcome, log-pop control, alternative deflators (deflator rows: 2010–2019 subsample) |
| 27 | specification-grid.do | weighting × denominator 2×2, state-level, control exclusions |
| 28 | entryexit-estimation.do | headline under missing-not-zero / balanced / state totals |
| 29 | abovecap-heterogeneity.do | above- vs below-cap split (linked sample) |
| 30 | randomization-inference.do | RI, fixed-2010 outcomes (run separately; slow) |
| 31 | ri-outcome-weight-diagnostic.do | RI by outcome × weighting (run separately) |
| 32 | ri-yearvarying.do | RI, year-varying headline — coefficient AND studentized (run separately) |
| 33 | ri-extended.do | RI, extended family — coefficient AND studentized (slowest; run separately) |
| 34 | label-permutation.do | formula-label permutation placebo (run separately) |
| 35 | wild-bootstrap.do | wild cluster bootstrap-t (Webb), static TWFE analogs |
| 36 | effective-clusters.do | Carter–Schnepel–Steigerwald G* |
| 37 | multiple-testing-qvalues.py | dual-standard FDR q-values incl. mechanism arms + forest plot |
| 38 | parse-sas-transition.py | parse ACGME Single Accreditation System applicant list |
| 39 | sas-entrant-crosswalk.R | classify post-2010 NRMP entrants as migrants vs genuine (with manual overrides) |
| 40 | make-payment-decomposition-table.R | appendix table atab:logpositive from script 19's log-positive CSV |
| 41 | make-mechdiff-table.R | appendix table atab:mechdiff (cross-arm difference sensitivity) |
| 42 | make-specgrid-table.R | appendix table atab:specgrid (weighting-by-denominator grid) |
| 95 | make-all.R | R runner (01–12) |
| 99 | run-all-analysis.do | Stata runner (13–29, 35–36; RI block commented) |

Helpers: `_esplot-helpers.do` (event-study plots; default 10 pre-periods),
`_ri-avgatt.do` (RI helper; returns att/se/t for studentized RI).
Superseded scripts live in `archive/` (see its README). `nrmp-pdf-extraction/`
is the OCR pipeline feeding script 05.

Replication note: `.do` scripts resolve the repository root from the working
directory (run from repo root) or an explicitly set `global topdir`; R scripts
use `here::here()`.

## Renumbering history

2026-07-25 (second pass, full-panel migration): R side reordered so the panel
builders sit at 05–06 (old 90→05, 91→06, 05→07, 06→08, 07→09, 08→10, 10→11,
11+12→12 merged); `09-entry-exit-panel.R` retired to `archive/` (logic lives
in 06); `92-longpretrend-and-placebo.do` absorbed into 20 (10-pre default)
and 22 (placebo). Stata numbering 13–36 unchanged.

2026-07-25 (first pass) old → new:

| Old | New |
|-----|-----|
| 03-heat-map.r | 11-heat-map.R |
| 04-population-residents.r | 12-population-residents.R |
| 04b-state-year-population.R | 03-state-year-population.R |
| 04c-alternative-deflators.R | 04-alternative-deflators.R |
| 05-dd-analysis.do | 13-dd-analysis.do |
| 06-dd-analysis-hetero.do | 14-dd-analysis-hetero.do |
| 07-did-analysis-byspeciality.do | 15-did-analysis-byspecialty.do |
| 08-merge-residency-cms.R | 07-merge-residency-cms.R |
| 09-2SLS.R | archive/ (2SLS abandoned per editorial decision) |
| 10-merge-mort.R | archive/ |
| 11-dd-methods-comparison.do | 17-dd-methods-comparison.do |
| 12-dd-analysis-popcnt.do | 16-dd-analysis-popcnt.do |
| 14-append-gme-funding.R | 05-append-gme-funding.R |
| 15-merge-gme-expansion.R | 06-merge-gme-expansion.R |
| 16-gme-funding-event-study.do | 18-gme-funding-event-study.do |
| 17-merge-residency-gme-funding.R | 08-merge-residency-gme-funding.R |
| 18-randomization-inference.do | 30-randomization-inference.do |
| 18b-ri-outcome-weight-diagnostic.do | 31-ri-outcome-weight-diagnostic.do |
| 18c-ri-yearvarying.do | 32-ri-yearvarying.do |
| 18d-ri-extended.do | 33-ri-extended.do |
| 18e-label-permutation.do | 34-label-permutation.do |
| 20-gme-firststage-byformula.do | 19-gme-firststage-byformula.do |
| 22-multiple-testing-qvalues.py | 37-multiple-testing-qvalues.py |
| 23-make-2000-2019-residency-panel.R | 90-make-2000-2019-residency-panel.R |
| 24-yearvarying-suite.do | 20-yearvarying-suite.do |
| 25-yearvarying-specialty.do | 21-yearvarying-specialty.do |
| 26-yearvarying-robustness.do | 22-yearvarying-robustness.do |
| 27-balance-table.R | 10-balance-table.R |
| 28-leave-one-out.do | 23-leave-one-out.do |
| 29-effective-clusters.do | 36-effective-clusters.do |
| 30-linked-sample-reconciliation.do | 24-linked-sample-reconciliation.do |
| 31-mechanism-reclassification.do | 25-mechanism-reclassification.do |
| 32-deflator-robustness.do | 26-deflator-robustness.do |
| 33-specification-grid.do | 27-specification-grid.do |
| 34-entry-exit-panel.R | 09-entry-exit-panel.R |
| 35-entryexit-estimation.do | 28-entryexit-estimation.do |
| 36-wild-bootstrap.do | 35-wild-bootstrap.do |
| 37-abovecap-heterogeneity.do | 29-abovecap-heterogeneity.do |
