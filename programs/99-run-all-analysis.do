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
global topdir "/Users/hhadah/Documents/GiT/residency-medicaid-expansion"
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
* SCRIPT 06: DID Analysis by Specialty (Weighted, Specialty-Level)
* =========================================================================
di ""
di ">>> Running Script 06: DID Analysis by Specialty (Weighted)"
di "    File: 06-did-analysis-byspeciality.do"
di ""
capture noisily do "${progdir}/06-did-analysis-byspeciality.do"
if (_rc != 0) {
    di as error "ERROR: Script 06 failed with code " _rc
    di as error "Check ${outputdir}/06-did-analysis-byspeciality.log for details"
}
else {
    di as result "COMPLETED: Script 06"
}
di ""

* =========================================================================
* SCRIPT 07: DID Analysis Unweighted (Program-Level)
* =========================================================================
di ""
di ">>> Running Script 07: DID Analysis (Unweighted, Program-Level)"
di "    File: 07-dd-analysis-unweighted.do"
di ""
capture noisily do "${progdir}/07-dd-analysis-unweighted.do"
if (_rc != 0) {
    di as error "ERROR: Script 07 failed with code " _rc
    di as error "Check ${outputdir}/07-dd-analysis-unweighted.log for details"
}
else {
    di as result "COMPLETED: Script 07"
}
di ""

* =========================================================================
* SCRIPT 08: DID Analysis by Specialty Unweighted (Specialty-Level)
* =========================================================================
di ""
di ">>> Running Script 08: DID Analysis by Specialty (Unweighted)"
di "    File: 08-did-analysis-byspeciality-unweighted.do"
di ""
capture noisily do "${progdir}/08-did-analysis-byspeciality-unweighted.do"
if (_rc != 0) {
    di as error "ERROR: Script 08 failed with code " _rc
    di as error "Check ${outputdir}/08-did-analysis-byspeciality-unweighted.log for details"
}
else {
    di as result "COMPLETED: Script 08"
}
di ""

* =========================================================================
* SCRIPT 09: Heterogeneity Analysis - Urban/Rural (Weighted, Program-Level)
* =========================================================================
di ""
di ">>> Running Script 09: Heterogeneity Analysis Urban/Rural (Weighted)"
di "    File: 09-dd-analysis-hetero.do"
di ""
capture noisily do "${progdir}/09-dd-analysis-hetero.do"
if (_rc != 0) {
    di as error "ERROR: Script 09 failed with code " _rc
    di as error "Check ${outputdir}/09-dd-analysis-hetero.log for details"
}
else {
    di as result "COMPLETED: Script 09"
}
di ""

* =========================================================================
* SCRIPT 10: Heterogeneity Analysis - Urban/Rural Unweighted (Program-Level)
* =========================================================================
di ""
di ">>> Running Script 10: Heterogeneity Analysis Urban/Rural (Unweighted)"
di "    File: 10-dd-analysis-hetero-unweighted.do"
di ""
capture noisily do "${progdir}/10-dd-analysis-hetero-unweighted.do"
if (_rc != 0) {
    di as error "ERROR: Script 10 failed with code " _rc
    di as error "Check ${outputdir}/10-dd-analysis-hetero-unweighted.log for details"
}
else {
    di as result "COMPLETED: Script 10"
}
di ""

* =========================================================================
* SCRIPT 11: Heterogeneity Analysis - Urban/Rural by Specialty (Weighted)
* =========================================================================
di ""
di ">>> Running Script 11: Heterogeneity Analysis Urban/Rural by Specialty (Weighted)"
di "    File: 11-dd-analysis-hetero-byspecialty.do"
di ""
capture noisily do "${progdir}/11-dd-analysis-hetero-byspecialty.do"
if (_rc != 0) {
    di as error "ERROR: Script 11 failed with code " _rc
    di as error "Check ${outputdir}/11-dd-analysis-hetero-byspecialty.log for details"
}
else {
    di as result "COMPLETED: Script 11"
}
di ""

* =========================================================================
* SCRIPT 12: Heterogeneity Analysis - Urban/Rural by Specialty (Unweighted)
* =========================================================================
di ""
di ">>> Running Script 12: Heterogeneity Analysis Urban/Rural by Specialty (Unweighted)"
di "    File: 12-dd-analysis-hetero-byspecialty-unweighted.do"
di ""
capture noisily do "${progdir}/12-dd-analysis-hetero-byspecialty-unweighted.do"
if (_rc != 0) {
    di as error "ERROR: Script 12 failed with code " _rc
    di as error "Check ${outputdir}/12-dd-analysis-hetero-byspecialty-unweighted.log for details"
}
else {
    di as result "COMPLETED: Script 12"
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
di "  - 06-did-analysis-byspeciality.log"
di "  - 07-dd-analysis-unweighted.log"
di "  - 08-did-analysis-byspeciality-unweighted.log"
di "  - 09-dd-analysis-hetero.log"
di "  - 10-dd-analysis-hetero-unweighted.log"
di "  - 11-dd-analysis-hetero-byspecialty.log"
di "  - 12-dd-analysis-hetero-byspecialty-unweighted.log"
di ""
di "Output figures and tables created in: ${outputdir}/figures/ and ${outputdir}/tables/"
di "========================================================================="
di ""

capture log close master
