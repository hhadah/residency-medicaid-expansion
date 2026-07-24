/*
Master Script: Run All Analysis
Date: January 27, 2026
Purpose: Execute all analysis scripts in sequence
*/

clear all
set more off
set varabbrev off

* -------------------------------------------------------------------------
* Define paths
* -------------------------------------------------------------------------
global topdir "/Users/hhadah/Projects/GiT/residency-medicaid-expansion"
global progdir "${topdir}/programs"
global outputdir "${topdir}/output"

* Create output directory if needed
cap mkdir "${outputdir}"

* =========================================================================
* MASTER LOG FILE
* =========================================================================
capture log close master
log using "${outputdir}/99-master-analysis.log", replace name(master)

di ""
di "========================================================================="
di "MASTER ANALYSIS SCRIPT"
di "========================================================================="
di "Running all analysis scripts sequentially"
di "Start time: " c(current_date) " " c(current_time)
di "========================================================================="
di ""

* =========================================================================
* SCRIPT 05: DID Analysis (Weighted, Program-Level)
* =========================================================================
di ""
di ">>> Running Script 05: DID Analysis (Weighted, Program-Level)"
di "    File: 05-dd-analysis.do"
di ""
capture noisily do "${progdir}/05-dd-analysis.do"
if (_rc != 0) {
    di as error "ERROR: Script 05 failed with code " _rc
    di as error "Check ${outputdir}/05-dd-analysis.log for details"
}
else {
    di as result "COMPLETED: Script 05"
}
di ""

* =========================================================================
* SCRIPT 07: Heterogeneity Analysis - Urban/Rural (Weighted, Program-Level)
* =========================================================================
di ""
di ">>> Running Script 06: Heterogeneity Analysis Urban/Rural (Weighted)"
di "    File: 06-dd-analysis-hetero.do"
di ""
capture noisily do "${progdir}/06-dd-analysis-hetero.do"
if (_rc != 0) {
    di as error "ERROR: Script 06 failed with code " _rc
    di as error "Check ${outputdir}/06-dd-analysis-hetero.log for details"
}
else {
    di as result "COMPLETED: Script 06"
}
di ""

* =========================================================================
* SCRIPT 07: DID Analysis by Specialty (Primary Care vs Non-Primary Care)
* =========================================================================
di ""
di ">>> Running Script 07: DID Analysis (Primary Care vs Non-Primary Care)"
di "    File: 07-did-analysis-byspeciality.do"
di ""
capture noisily do "${progdir}/07-did-analysis-byspeciality.do"
if (_rc != 0) {
    di as error "ERROR: Script 07 failed with code " _rc
    di as error "Check ${outputdir}/07-did-analysis-byspeciality.log for details"
}
else {
    di as result "COMPLETED: Script 07"
}
di ""

* =========================================================================
* SCRIPT 11: DID Methods Comparison (Robustness across estimators)
* =========================================================================
di ""
di ">>> Running Script 11: DID Methods Comparison"
di "    File: 11-dd-methods-comparison.do"
di ""
capture noisily do "${progdir}/11-dd-methods-comparison.do"
if (_rc != 0) {
    di as error "ERROR: Script 11 failed with code " _rc
    di as error "Check ${outputdir}/11-dd-methods-comparison.log for details"
}
else {
    di as result "COMPLETED: Script 11"
}
di ""

* =========================================================================
* SCRIPT 12: DID Analysis (Unweighted, Population Control)
* =========================================================================
di ""
di ">>> Running Script 12: DID Analysis (Unweighted, Population Control)"
di "    File: 12-dd-analysis-popcnt.do"
di ""
capture noisily do "${progdir}/12-dd-analysis-popcnt.do"
if (_rc != 0) {
    di as error "ERROR: Script 12 failed with code " _rc
    di as error "Check ${outputdir}/12-dd-analysis-popcnt.log for details"
}
else {
    di as result "COMPLETED: Script 12"
}
di ""

* =========================================================================
* SCRIPT 16: Mechanism Test (Medicaid GME Funding)
* =========================================================================
di ""
di ">>> Running Script 16: Mechanism Test (Medicaid GME Funding)"
di "    File: 16-gme-funding-event-study.do"
di ""
capture noisily do "${progdir}/16-gme-funding-event-study.do"
if (_rc != 0) {
    di as error "ERROR: Script 16 failed with code " _rc
    di as error "Check ${outputdir}/16-gme-funding-event-study.log for details"
}
else {
    di as result "COMPLETED: Script 16"
}
di ""

* =========================================================================
* ADDED ROBUSTNESS / SENSITIVITY SCRIPTS (referee-requested)
* -------------------------------------------------------------------------
* Prerequisites run OUTSIDE Stata:
*   - programs/04b-state-year-population.R  builds data/datasets/state_year_population.dta
*     (needed by scripts 24/25/26); run with Rscript before this master do-file.
*   - programs/22-multiple-testing-qvalues.py builds the FDR q-value table + forest plot
*     from the summary CSVs; run with python3 AFTER scripts 24/25 have produced their
*     yearvarying-*-summary.csv tables.
* Note: script 18 (randomization inference) defaults to 1,000 permutations and is slow
*   (~20-30 min). Pass a smaller count for a quick check: do 18-...do 200.
*   Script 18d (extended RI: urban/rural/quota/notyet/primary/nonprimary) runs
*   6 x REPS did_imputation calls (default 500) and is the slowest script;
*   smoke test with: do 18d-ri-extended.do 20.
*   - programs/27-balance-table.R builds the GME-formula balance table
*     (my_paper/tables/balance_gme_formula.tex); run with Rscript, any time.
* The year-varying per-capita suite (24/25/26) is the paper's primary specification;
* scripts 13/19/21/23 were superseded by it and moved to programs/archive/.
* =========================================================================
foreach s in ///
    "18-randomization-inference" ///
    "18c-ri-yearvarying" ///
    "18d-ri-extended" ///
    "18e-label-permutation" ///
    "20-gme-firststage-byformula" ///
    "24-yearvarying-suite" ///
    "25-yearvarying-specialty" ///
    "26-yearvarying-robustness" ///
    "28-leave-one-out" ///
    "29-effective-clusters" {
    di ""
    di ">>> Running Script: `s'.do"
    capture noisily do "${progdir}/`s'.do"
    if (_rc != 0) {
        di as error "ERROR: Script `s' failed with code " _rc
        di as error "Check ${outputdir}/`s'.log for details"
    }
    else {
        di as result "COMPLETED: Script `s'"
    }
}
di ""


* =========================================================================
* SUMMARY
* =========================================================================
di ""
di "========================================================================="
di "MASTER ANALYSIS COMPLETE"
di "========================================================================="
di "End time: " c(current_date) " " c(current_time)
di ""
di "Log files created in: ${outputdir}/"
di "  - 99-master-analysis.log (this file; script 05 logs here)"
di "  - 06-dd-analysis-hetero.log"
di "  - 07-did-analysis-byspeciality.log"
di "  - 11-dd-methods-comparison.log"
di "  - 12-dd-analysis-popcnt.log"
di "  - 16-gme-funding-event-study.log"
di "  - 18-randomization-inference.log, 18c-ri-yearvarying.log, 18d-ri-extended.log"
di "  - 20-gme-firststage-byformula.log"
di "  - 24-yearvarying-suite.log, 25-yearvarying-specialty.log, 26-yearvarying-robustness.log"
di ""
di "Output figures and tables created in: ${outputdir}/figures/ and ${outputdir}/tables/"
di "========================================================================="
di ""

capture log close master
