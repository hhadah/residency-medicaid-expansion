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
    di as error "Check ${outputdir}/06-did-analysis-byspeciality.log for details"
}
else {
    di as result "COMPLETED: Script 07"
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
di "  - 99-master-analysis.log (this file)"
di "  - 05-dd-analysis.log"
di "  - 06-dd-analysis-hetero.log"
di "  - 06-did-analysis-byspeciality.log"
di ""
di "Output figures and tables created in: ${outputdir}/figures/ and ${outputdir}/tables/"
di "========================================================================="
di ""

capture log close master
