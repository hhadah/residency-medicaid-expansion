* =============================================================================
* Effective number of clusters for the headline design.
* ---------------------------------------------------------------------------
* With unequal cluster weights, the nominal number of clusters (G = 51)
* overstates the information content of the design. Following the logic of
* Carter, Schnepel, and Steigerwald (2017), we report the approximation
*   G* = G / (1 + CV^2),
* where CV is the coefficient of variation of the state-level shares of total
* estimation weight (analytic weights summed by state for the weighted design;
* program-year observation counts for the unweighted design). This quantifies
* the "identifying variation concentrates in a few large states" statement in
* the paper's inference section.
*
* Output: output/tables/effective-clusters.csv (+ console summary)
* =============================================================================

clear all
set more off

* Replication-friendly path handling: run from the repository root, or set
* global topdir before running.
if "${topdir}" == "" global topdir "`c(pwd)'"
capture confirm file "${topdir}/programs/00-README-pipeline.md"
if _rc {
    di as error "Cannot find the repository root. Run from the repo root or set global topdir."
    exit 601
}
global datadir "${topdir}/data/datasets"
global tabdir  "${topdir}/output/tables"
cap mkdir "${tabdir}"

log using "${topdir}/output/36-effective-clusters.log", replace

* FULL 2000-2019 PANEL (activity-window coding is primary; see script 06)
use "${datadir}/panel_2000_2019_estimation.dta", clear
replace matched = matched_na
replace quota   = quota_na
gen double matched_per_100k = matched / total_population_10 * 100000
gen double quota_per_100k   = quota   / total_population_10 * 100000
gen double unmatched        = quota - matched
replace state = strtrim(upper(state))
* pop_yr already in the panel (2000-2019 series from script 03)
gen double matched_per_100k_yr = matched / pop_yr * 100000
keep if !missing(matched_per_100k_yr)

tempname ec
tempfile ec_file
postfile `ec' str12 design double G cv Gstar top5_share using "`ec_file'", replace

foreach design in weighted unweighted {
    preserve
    if "`design'" == "weighted"   gen double w = total_population_10
    if "`design'" == "unweighted" gen double w = 1
    collapse (sum) w, by(state)
    quietly summarize w
    local G = r(N)
    local cv = r(sd)/r(mean)
    local Gstar = `G' / (1 + `cv'^2)
    gsort -w
    quietly summarize w
    local tot = r(sum)
    quietly summarize w in 1/5
    local top5 = r(sum)/`tot'
    post `ec' ("`design'") (`G') (`cv') (`Gstar') (`top5')
    di as result "`design': G = `G'  CV = " %6.3f `cv' ///
        "  G* = G/(1+CV^2) = " %6.1f `Gstar' ///
        "  top-5 state share of weight = " %5.1f 100*`top5' "%"
    restore
}
postclose `ec'

use "`ec_file'", clear
list, clean noobs
export delimited using "${tabdir}/effective-clusters.csv", replace

di _n "=== effective clusters complete: effective-clusters.csv ==="
log close
