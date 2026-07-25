# Pipeline README — script order and 2026-07-25 renumbering

Scripts are numbered in **execution order**: if script 01 must run before
script 03, it is named 01. Run the R data build first (`95-make-all.R`,
scripts 01–12), then the Stata analysis (`99-run-all-analysis.do`, scripts
13–36), then `python3 37-multiple-testing-qvalues.py`.

## Pipeline

| # | Script | Purpose |
|---|--------|---------|
| 01 | packages-wds.R | packages + path globals |
| 02 | data-cleaning.R | clean NRMP panel, geocode (needs `MAPBOX_API_KEY` in `~/.Renviron`) |
| 03 | state-year-population.R | ACS year-varying state population |
| 04 | alternative-deflators.R | ACS 18–64 and <150% FPL deflators |
| 05 | append-gme-funding.R | append CMS GME funding spreadsheets |
| 06 | merge-gme-expansion.R | GME funding panel + expansion status |
| 07 | merge-residency-cms.R | residency↔CMS crosswalk (fallback layer for 08) |
| 08 | merge-residency-gme-funding.R | NRMP→CCN crosswalk + linked funding panel |
| 09 | entry-exit-panel.R | entry/exit-corrected panel |
| 10 | balance-table.R | sumstats + formula balance tables |
| 11 | heat-map.R | descriptive timing/cohort figures |
| 12 | population-residents.R | descriptive physician-growth figure |
| 13 | dd-analysis.do | levels DiD (weighted) |
| 14 | dd-analysis-hetero.do | urban/rural levels + quota |
| 15 | did-analysis-byspecialty.do | specialty levels + quota |
| 16 | dd-analysis-popcnt.do | unweighted / population-control specs |
| 17 | dd-methods-comparison.do | estimator comparison (TWFE, BJS, dCDH, SA, …) |
| 18 | gme-funding-event-study.do | pooled GME payment first stage |
| 19 | gme-firststage-byformula.do | payment first stage by formula arm |
| 20 | yearvarying-suite.do | **primary spec**: headline, location, mechanism, quota |
| 21 | yearvarying-specialty.do | specialty split (hospital × specialty-group) |
| 22 | yearvarying-robustness.do | not-yet-treated (+ mechanism inside), cohort-2014, HonestDiD |
| 23 | leave-one-out.do | LOO: headline (treated+controls) + mechanism objects |
| 24 | linked-sample-reconciliation.do | NRMP intake vs cost-report FTEs, linked sample |
| 25 | mechanism-reclassification.do | 2015-vintage classification + judgment flips |
| 26 | deflator-robustness.do | log-pop outcome, log-pop control, alternative deflators |
| 27 | specification-grid.do | weighting × denominator 2×2, state-level, control exclusions |
| 28 | entryexit-estimation.do | headline under missing-not-zero / balanced / state totals |
| 29 | abovecap-heterogeneity.do | above- vs below-cap split (linked sample) |
| 30 | randomization-inference.do | RI, fixed-2010 outcomes |
| 31 | ri-outcome-weight-diagnostic.do | RI by outcome × weighting |
| 32 | ri-yearvarying.do | RI, year-varying headline |
| 33 | ri-extended.do | RI, extended family (slowest script) |
| 34 | label-permutation.do | formula-label permutation placebo |
| 35 | wild-bootstrap.do | wild cluster bootstrap-t (Webb), static TWFE analogs |
| 36 | effective-clusters.do | Carter–Schnepel–Steigerwald G* |
| 37 | multiple-testing-qvalues.py | dual-standard FDR q-values + forest plot |
| 90 | make-2000-2019-residency-panel.R | auxiliary: 2000–2019 back-extension (pre-ACA placebo groundwork; not used by the paper yet) |
| 95 | make-all.R | R runner (01–12) |
| 99 | run-all-analysis.do | Stata runner (13–36) |

Helpers: `_esplot-helpers.do` (event-study plots), `_ri-avgatt.do` (RI helper).
Superseded scripts live in `archive/` (see its README). `nrmp-pdf-extraction/`
is the OCR pipeline feeding script 90.

## Renumbering map (2026-07-25)

Old name → new name (documents cite the old numbers; e.g., the 2026-07-24
referee reports and response ledger):

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
