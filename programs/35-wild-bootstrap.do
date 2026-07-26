* =============================================================================
* WILD CLUSTER BOOTSTRAP-t (Webb weights) for the key estimates
* ---------------------------------------------------------------------------
* Referee response (editorial decision 2026-07-24, MUST-9). With G* ~ 7
* effective clusters and a cluster-weight CV of 2.49, both referees ask for
* wild cluster bootstrap-t inference (boottest, Webb weights, state level)
* for the headline, the not-yet-treated design, and the mechanism difference.
*
* boottest cannot post-process did_imputation, so these run on the STATIC
* TWFE analog (areg: outcome on treated-post + year dummies, unit FE
* absorbed, same weights, state clusters; boottest does not support
* reghdfe with two absorbed dimensions). The paper must label these as the static-TWFE
* bootstrap analog of the BJS estimates -- the point estimates differ
* (staggered-timing bias of static TWFE), but the object of interest here is
* the p-value's robustness to few effective clusters, not the point estimate.
*
* Mechanism difference runs under both classification vintages.
* Output: output/tables/wild-bootstrap-summary.csv
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
global rawdir  "${topdir}/data/raw"
global tabdir  "${topdir}/output/tables"
cap mkdir "${tabdir}"

log using "${topdir}/output/35-wild-bootstrap.log", replace

* FULL 2000-2019 PANEL (activity-window coding is primary; see script 06)
use "${datadir}/panel_2000_2019_estimation.dta", clear
replace matched = matched_na
replace quota   = quota_na
gen double matched_per_100k = matched / total_population_10 * 100000
gen double quota_per_100k   = quota   / total_population_10 * 100000
gen double unmatched        = quota - matched
replace state = strtrim(upper(state))
egen program_numeric_id = group(state institution_code)
* pop_yr already in the panel (2000-2019 series from script 03)
quietly count if missing(pop_yr)
assert r(N) == 0
gen double matched_per_100k_yr = matched / pop_yr * 100000

preserve
    import delimited "${rawdir}/gme_formula_classification.csv", clear varnames(1) stringcols(_all)
    keep state gme_formula gme_formula_2015
    replace state = strtrim(upper(state))
    tempfile gme
    save `gme'
restore
merge m:1 state using `gme', keep(master match) nogen
gen byte vol12 = (gme_formula == "volume")
gen byte nvl12 = inlist(gme_formula, "fixed", "none")
gen byte vol15 = (gme_formula_2015 == "volume")
gen byte nvl15 = inlist(gme_formula_2015, "fixed", "none")

encode state, gen(state_id)
gen byte tp = treated_state == 1 & year >= year_expanded
label var tp "Treated x Post"

tempfile master
save `master'

tempname res
tempfile resfile
postfile `res' str24 spec double b_static se_cluster p_cluster p_boot ///
    ci_lo_boot ci_hi_boot using "`resfile'", replace

set seed 20260724

* ---------------------------------------------------------------------------
* 1) Headline (static TWFE analog)
* ---------------------------------------------------------------------------
use "`master'", clear
di _n "==================== BOOT: headline ===================="
areg matched_per_100k_yr tp i.year [aw=total_population_10], ///
    absorb(program_numeric_id) vce(cluster state_id)
local b  = _b[tp]
local se = _se[tp]
local pc = 2*ttail(e(df_r), abs(`b'/`se'))
boottest tp, weighttype(webb) reps(9999) cluster(state_id) nograph
local pb  = r(p)
matrix _ci = r(CI)
local clo = _ci[1,1]
local chi = _ci[1,2]
post `res' ("headline") (`b') (`se') (`pc') (`pb') (`clo') (`chi')
di as result "headline: b=" %9.4f `b' " cluster p=" %6.3f `pc' " boot p=" %6.3f `pb'

* ---------------------------------------------------------------------------
* 2) Not-yet-treated (treated states only)
* ---------------------------------------------------------------------------
use "`master'", clear
keep if treated_state == 1
di _n "==================== BOOT: not-yet-treated ===================="
areg matched_per_100k_yr tp i.year [aw=total_population_10], ///
    absorb(program_numeric_id) vce(cluster state_id)
local b  = _b[tp]
local se = _se[tp]
local pc = 2*ttail(e(df_r), abs(`b'/`se'))
boottest tp, weighttype(webb) reps(9999) cluster(state_id) nograph
local pb  = r(p)
matrix _ci = r(CI)
local clo = _ci[1,1]
local chi = _ci[1,2]
post `res' ("notyet") (`b') (`se') (`pc') (`pb') (`clo') (`chi')
di as result "notyet: b=" %9.4f `b' " cluster p=" %6.3f `pc' " boot p=" %6.3f `pb'

* ---------------------------------------------------------------------------
* 3) Mechanism difference (tp x volume-arm interaction), both vintages
* ---------------------------------------------------------------------------
foreach v in 12 15 {
    use "`master'", clear
    keep if treated_state == 0 | vol`v' == 1 | nvl`v' == 1
    gen byte tp_vol = tp * vol`v'
    di _n "==================== BOOT: mech diff (c20`v') ===================="
    areg matched_per_100k_yr tp tp_vol i.year [aw=total_population_10], ///
        absorb(program_numeric_id) vce(cluster state_id)
    local b  = _b[tp_vol]
    local se = _se[tp_vol]
    local pc = 2*ttail(e(df_r), abs(`b'/`se'))
    boottest tp_vol, weighttype(webb) reps(9999) cluster(state_id) nograph
    local pb  = r(p)
    matrix _ci = r(CI)
    local clo = _ci[1,1]
    local chi = _ci[1,2]
    post `res' ("mech_diff_c20`v'") (`b') (`se') (`pc') (`pb') (`clo') (`chi')
    di as result "mech_diff c20`v': b=" %9.4f `b' " cluster p=" %6.3f `pc' " boot p=" %6.3f `pb'
}

postclose `res'
use "`resfile'", clear
list, clean noobs
export delimited using "${tabdir}/wild-bootstrap-summary.csv", replace

di _n "=== wild cluster bootstrap complete ==="
log close
