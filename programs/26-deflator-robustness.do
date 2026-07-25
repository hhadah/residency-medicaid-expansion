* =============================================================================
* DEFLATOR ROBUSTNESS: is the flat pre-trend an artifact of the denominator?
* ---------------------------------------------------------------------------
* Referee response (editorial decision 2026-07-24, MUST-5 / cluster F4).
* The methods referee's Major Comment 1: levels fail parallel trends; fixed-
* 2010 per-capita is levels rescaled and also fails; ONLY the contemporary-
* population deflator is flat -- and that flatness could be the cancellation
* of a trend in positions against a differential trend in the denominator,
* which is itself plausibly post-treatment (interstate migration).
*
* This script runs the referee's requested diagnostics:
*   1. Event study with log CONTEMPORARY STATE POPULATION as the outcome
*      (same sample, same weights): does the denominator itself trend
*      differentially around expansion?
*   2. Levels and asinh(matched) specifications with log population as an
*      estimated time-varying control (did_imputation controls()).
*   3. The headline under alternative deflators: population 18-64 and
*      population below 150% FPL (closest ACS cut to the 138% threshold),
*      from state_year_deflators.dta (script 04). A non-demographic scale (state
*      GDP / discharges) is flagged in data/raw/state-gdp-README.md.
*
* Outputs: output/tables/deflator-robustness.csv
*          figures appx-logpop-outcome, appx-deflator-{a1864,fpl150}
* =============================================================================

clear all
set more off

global topdir "/Users/hhadah/Projects/GiT/residency-medicaid-expansion"
global datadir "${topdir}/data/datasets"
global figdir  "${topdir}/output/figures"
global tabdir  "${topdir}/output/tables"
global latex_figdir "${topdir}/my_paper/figures"
cap mkdir "${figdir}"
cap mkdir "${tabdir}"
cap mkdir "${latex_figdir}"

log using "${topdir}/output/26-deflator-robustness.log", replace

use "${datadir}/cleaned_program_residency_medicaid.dta", clear
replace state = strtrim(upper(state))
egen program_numeric_id = group(state institution_code)
merge m:1 state year using "${datadir}/state_year_population.dta", keep(master match) nogen
quietly count if missing(pop_yr)
assert r(N) == 0
merge m:1 state year using "${datadir}/state_year_deflators.dta", keep(master match) nogen
quietly count if missing(pop_1864)
di as text "rows missing pop_1864: " r(N)

gen double matched_per_100k_yr = matched / pop_yr * 100000
gen double ln_pop      = ln(pop_yr)
gen double asinh_matched = asinh(matched)
gen double matched_per_100k_1864 = matched / pop_1864 * 100000 if !missing(pop_1864)
gen double matched_per_100k_fpl  = matched / pop_u150fpl * 100000 if !missing(pop_u150fpl)

encode state, gen(state_id)
xtset program_numeric_id year

do "${topdir}/programs/_esplot-helpers.do"

tempname res
tempfile resfile
postfile `res' str24 spec double avg_treat avg_se treat_p pretrend_p baseline pct ///
    using "`resfile'", replace

capture program drop _defrun
program define _defrun
    args outcome tag resh fname yti ctrls
    local copt ""
    if ("`ctrls'" != "") local copt "controls(`ctrls')"
    di _n "==================== `tag' ===================="
    capture noisily did_imputation `outcome' program_numeric_id year year_expanded ///
        [aw=total_population_10], horizons(0/5) pretrend(5) `copt' ///
        fe(program_numeric_id year) cluster(state_id) minn(0) autosample
    if (_rc != 0) {
        * SE convergence can fail with a time-varying control (rc 430);
        * retry with nose for point estimates, posted without SEs.
        di as error "`tag' failed (rc=" _rc "), retrying with nose"
        capture noisily did_imputation `outcome' program_numeric_id year year_expanded ///
            [aw=total_population_10], horizons(0/5) pretrend(5) `copt' ///
            fe(program_numeric_id year) cluster(state_id) minn(0) autosample nose
        if (_rc != 0) {
            di as error "`tag' failed with nose too (rc=" _rc ")"
            post `resh' ("`tag'") (.) (.) (.) (.) (.) (.)
            exit
        }
        local tau_sum = 0
        local tau_n = 0
        forval h = 0/5 {
            capture scalar __b = _b[tau`h']
            if (_rc == 0) {
                local tau_sum = `tau_sum' + __b
                local tau_n = `tau_n' + 1
            }
        }
        capture scalar drop __b
        local a = cond(`tau_n' > 0, `tau_sum'/`tau_n', .)
        quietly summarize `outcome' if treated_state==1 & year<year_expanded [aw=total_population_10]
        local b = r(mean)
        local pct = cond(`b' < . & `b' != 0 & `a' < ., 100*`a'/`b', .)
        post `resh' ("`tag'") (`a') (.) (.) (.) (`b') (`pct')
        di as result "`tag' (nose): avg=" %10.6f `a' " pct=" %5.1f `pct' " (no SE)"
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
    quietly summarize `outcome' if treated_state==1 & year<year_expanded [aw=total_population_10]
    local b = r(mean)
    local pct = cond(`b' < . & `b' != 0, 100*`a'/`b', .)
    post `resh' ("`tag'") (`a') (`ase') (`tp') (`pt') (`b') (`pct')
    di as result "`tag': avg=" %10.6f `a' " se=" %10.6f `ase' " pct=" %5.1f `pct' ///
        " treat_p=" %6.3f `tp' " pretrend_p=" %6.3f `pt'
    if ("`fname'" != "") {
        _fillcoef
        _esplot "`fname'" "`yti'" "" `a' `b' `pct' `tp' `pt'
    }
end

* 1) Does the denominator itself respond to expansion?
_defrun ln_pop "logpop_outcome" `res' "appx-logpop-outcome" "Treatment Effect (log state population)" ""

* 2) Numerator specs with the denominator as an estimated control
_defrun matched       "levels_lnpop_ctrl" `res' "" "" "ln_pop"
_defrun asinh_matched "asinh_lnpop_ctrl"  `res' "" "" "ln_pop"
* references without the control, for comparison
_defrun matched       "levels_nocontrol"  `res' "" "" ""
_defrun asinh_matched "asinh_nocontrol"   `res' "" "" ""

* 3) Alternative deflators
_defrun matched_per_100k_yr   "percap_totalpop" `res' "" "" ""
_defrun matched_per_100k_1864 "percap_age1864"  `res' "appx-deflator-a1864" "Treatment Effect (per 100,000 aged 18-64)" ""
_defrun matched_per_100k_fpl  "percap_u150fpl"  `res' "appx-deflator-fpl150" "Treatment Effect (per 100,000 below 150% FPL)" ""

postclose `res'
use "`resfile'", clear
list, clean noobs
export delimited using "${tabdir}/deflator-robustness.csv", replace

di _n "=== deflator robustness complete ==="
log close
