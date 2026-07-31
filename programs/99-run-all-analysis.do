/*
Master Script: Run All Analysis (Stata scripts 13-36)
Date: renumbered July 25, 2026 (old->new mapping in programs/00-README-pipeline.md)
Purpose: Execute all analysis scripts in numeric = execution order.

Prerequisites run OUTSIDE Stata (see programs/95-make-all.R):
  - R scripts 01-12 build the datasets (cleaned panels, state_year_population,
    deflators, GME funding merge, entry/exit panel, balance tables).
  - AFTER this master finishes, run:
      python3 programs/37-multiple-testing-qvalues.py
    to rebuild the FDR q-value table + forest plot from the summary CSVs.

Runtime notes:
  - Script 30 (randomization inference) defaults to 1,000 permutations
    (~20-30 min); pass a smaller count for a quick check: do 30-...do 200.
  - Script 33 (extended RI) runs 6 x REPS did_imputation calls (default 500)
    and is the slowest script; smoke test with: do 33-ri-extended.do 20.
  - Script 23 (leave-one-out) runs ~190 did_imputation calls (~15 min).
*/

clear all
set more off
set varabbrev off

* -------------------------------------------------------------------------
* Define paths
* -------------------------------------------------------------------------
* Replication-friendly path handling: run from the repository root, or set
* global topdir before running.
if "${topdir}" == "" global topdir "`c(pwd)'"
capture confirm file "${topdir}/programs/00-README-pipeline.md"
if _rc {
    di as error "Cannot find the repository root. Run from the repo root or set global topdir."
    exit 601
}
global progdir "${topdir}/programs"
global outputdir "${topdir}/output"

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

* Each script opens its own log in ${outputdir}/<script-name>.log.
foreach s in ///
    "13-dd-analysis" ///
    "14-dd-analysis-hetero" ///
    "15-did-analysis-byspecialty" ///
    "16-dd-analysis-popcnt" ///
    "17-dd-methods-comparison" ///
    "18-gme-funding-event-study" ///
    "19-gme-firststage-byformula" ///
    "20-yearvarying-suite" ///
    "21-yearvarying-specialty" ///
    "22-yearvarying-robustness" ///
    "23-leave-one-out" ///
    "24-linked-sample-reconciliation" ///
    "25-mechanism-reclassification" ///
    "26-deflator-robustness" ///
    "27-specification-grid" ///
    "28-entryexit-estimation" ///
    "29-abovecap-heterogeneity" ///
    "35-wild-bootstrap" ///
    "36-effective-clusters" {
    di ""
    di ">>> Running Script: `s'.do"
    capture noisily do "${progdir}/`s'.do"
    local _rc_saved = _rc
    capture log close            // close any log a failed child left open
    if (`_rc_saved' != 0) {
        di as error "ERROR: Script `s' failed with code " `_rc_saved'
        di as error "Check ${outputdir}/`s'.log for details"
    }
    else {
        di as result "COMPLETED: Script `s'"
    }
}

* =========================================================================
* RANDOMIZATION INFERENCE (scripts 30-34) -- run separately
* -------------------------------------------------------------------------
* The RI suite is the pipeline's slow block (hours at full replication
* counts on the 2000-2019 panel). Scripts 32 and 33 were last rerun
* 2026-07-30 under the 2015 formula classification (the manuscript's RI
* numbers); 30, 31, and 34 are uncited replication artifacts. To run:
  foreach s in "30-randomization-inference" "31-ri-outcome-weight-diagnostic" ///
      "32-ri-yearvarying" "33-ri-extended" "34-label-permutation" {
      do "${progdir}/`s'.do"
  }
* Smoke test with reduced reps: do "${progdir}/33-ri-extended.do" 20
* Afterwards rerun: python3 programs/37-multiple-testing-qvalues.py
*                   Rscript programs/40-make-payment-decomposition-table.R
* =========================================================================

* =========================================================================
* SUMMARY
* =========================================================================
di ""
di "========================================================================="
di "MASTER ANALYSIS COMPLETE"
di "========================================================================="
di "End time: " c(current_date) " " c(current_time)
di ""
di "Per-script logs are in ${outputdir}/<script-name>.log."
di "Output figures and tables created in: ${outputdir}/figures/ and ${outputdir}/tables/"
di "Next step: python3 programs/37-multiple-testing-qvalues.py"
di "========================================================================="
di ""

capture log close master
