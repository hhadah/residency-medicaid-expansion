* =============================================================================
* SPECIFICATION GRID: weighting x denominator 2x2, state-level spec, and
* control-group contamination checks.
* ---------------------------------------------------------------------------
* Referee response (editorial decision 2026-07-24, MUST-6 and MUST-8a /
* clusters F4' and F5).
*
* Part A (MUST-6): the paper's weighting caveat cites exhibits that differ
* from the headline in BOTH weighting and denominator; the unweighted
* contemporary-population cell does not exist in the repository. This script
* completes the full 2x2:
*     {weighted, unweighted} x {fixed-2010 denominator, year-varying denominator}
* and adds the state-level collapsed specification (51 units, population-
* weighted and unweighted) the methods referee requested.
*
* Part B (MUST-8a): the never-expansion comparison group contains states
* running the competing policy (TX 2013-2019 GME appropriations cycles, FL
* Statewide Medicaid Residency Program 2013, GA and TN GME expansions).
* Re-estimates the headline excluding TX, FL, GA, TN from the control group.
* (A state-GME-appropriations control is flagged in
* data/raw/state-gme-appropriations-README.md pending digitization of the
* Henderson/AAMC survey series.)
*
* Outputs: output/tables/specification-grid.csv
* =============================================================================

clear all
set more off

global topdir "/Users/hhadah/Projects/GiT/residency-medicaid-expansion"
global datadir "${topdir}/data/datasets"
global tabdir  "${topdir}/output/tables"
cap mkdir "${tabdir}"

log using "${topdir}/output/27-specification-grid.log", replace

use "${datadir}/cleaned_program_residency_medicaid.dta", clear
replace state = strtrim(upper(state))
egen program_numeric_id = group(state institution_code)
merge m:1 state year using "${datadir}/state_year_population.dta", keep(master match) nogen
quietly count if missing(pop_yr)
assert r(N) == 0
gen double matched_per_100k_yr = matched / pop_yr * 100000
* matched_per_100k (fixed-2010 denominator) already in the dataset
encode state, gen(state_id)
xtset program_numeric_id year
tempfile master
save `master'

tempname res
tempfile resfile
postfile `res' str28 spec double avg_treat avg_se treat_p pretrend_p baseline pct ///
    using "`resfile'", replace

capture program drop _gridrun
program define _gridrun
    args outcome wopt tag resh idvar
    if ("`idvar'" == "") local idvar program_numeric_id
    di _n "==================== `tag' ===================="
    capture noisily did_imputation `outcome' `idvar' year year_expanded `wopt', ///
        horizons(0/5) pretrend(5) fe(`idvar' year) cluster(state_id) minn(0) autosample
    if (_rc != 0) {
        di as error "`tag' failed (rc=" _rc ")"
        post `resh' ("`tag'") (.) (.) (.) (.) (.) (.)
        exit
    }
    capture lincom (tau0+tau1+tau2+tau3+tau4+tau5)/6
    local a   = cond(_rc==0, r(estimate), .)
    local ase = cond(_rc==0, r(se), .)
    local pt = .
    capture test pre1 pre2 pre3 pre4 pre5
    if _rc == 0 local pt = r(p)
    local tp = .
    capture test tau0 tau1 tau2 tau3 tau4 tau5
    if _rc == 0 local tp = r(p)
    quietly summarize `outcome' if treated_state==1 & year<year_expanded `wopt'
    local b = r(mean)
    local pct = cond(`b' < . & `b' != 0, 100*`a'/`b', .)
    post `resh' ("`tag'") (`a') (`ase') (`tp') (`pt') (`b') (`pct')
    di as result "`tag': avg=" %10.6f `a' " se=" %10.6f `ase' " pct=" %5.1f `pct' ///
        " treat_p=" %6.3f `tp' " pretrend_p=" %6.3f `pt'
end

* ---------------------------------------------------------------------------
* Part A1: the 2x2 (weighting x denominator), program level
* ---------------------------------------------------------------------------
use "`master'", clear
_gridrun matched_per_100k_yr "[aw=total_population_10]" "yrvar_weighted"   `res'
use "`master'", clear
_gridrun matched_per_100k_yr ""                         "yrvar_unweighted" `res'
use "`master'", clear
_gridrun matched_per_100k    "[aw=total_population_10]" "fixed_weighted"   `res'
use "`master'", clear
_gridrun matched_per_100k    ""                         "fixed_unweighted" `res'

* ---------------------------------------------------------------------------
* Part A2: state-level collapsed specification (51 units)
* ---------------------------------------------------------------------------
use "`master'", clear
collapse (sum) matched (first) pop_yr total_population_10 year_expanded treated_state, ///
    by(state state_id year)
gen double state_matched_per_100k = matched / pop_yr * 100000
xtset state_id year
_gridrun state_matched_per_100k "[aw=total_population_10]" "state_level_weighted"   `res' state_id
_gridrun state_matched_per_100k ""                         "state_level_unweighted" `res' state_id

* ---------------------------------------------------------------------------
* Part B: control-group contamination (drop TX, FL, GA, TN from controls)
* ---------------------------------------------------------------------------
use "`master'", clear
drop if treated_state == 0 & inlist(state, "TX", "FL", "GA", "TN")
_gridrun matched_per_100k_yr "[aw=total_population_10]" "yrvar_w_noGMEcontrols" `res'
use "`master'", clear
drop if treated_state == 0 & inlist(state, "TX", "FL", "GA", "TN")
_gridrun matched_per_100k_yr ""                         "yrvar_u_noGMEcontrols" `res'

postclose `res'
use "`resfile'", clear
list, clean noobs
export delimited using "${tabdir}/specification-grid.csv", replace

di _n "=== specification grid complete ==="
log close
