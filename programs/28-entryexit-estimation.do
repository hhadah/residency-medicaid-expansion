* =============================================================================
* ENTRY/EXIT ESTIMATION: the headline with the extensive margin handled right
* ---------------------------------------------------------------------------
* Referee response (editorial decision 2026-07-24, MUST-7 / cluster F6).
* Uses program_panel_entry_exit.dta (script 09): institution-years outside an
* institution's [first_appears, last_active] window are missing, not zero.
*
* Specifications:
*   1. na_coded        : headline with entering/exiting years coded missing
*   2. balanced_only   : institutions active in all ten years
*   3. state_total     : state-level TOTAL matched per 100k INCLUDING entrants
*                        (entry is genuine capacity, so pre-entry zeros are
*                        correct at the state level)
*   4. state_total_bal : state totals from always-active institutions only --
*                        the (3)-(4) contrast is the entrant contribution
*   5. no_sas_entrants : headline dropping Single Accreditation System-window
*                        entrant institutions (2016+ first appearance), the
*                        available proxy for the AOA->ACGME migration
*
* Output: output/tables/entryexit-estimation.csv
* =============================================================================

clear all
set more off

global topdir "/Users/hhadah/Projects/GiT/residency-medicaid-expansion"
global datadir "${topdir}/data/datasets"
global tabdir  "${topdir}/output/tables"
cap mkdir "${tabdir}"

log using "${topdir}/output/28-entryexit-estimation.log", replace

use "${datadir}/program_panel_entry_exit.dta", clear
replace state = strtrim(upper(state))
egen program_numeric_id = group(state institution_code)
merge m:1 state year using "${datadir}/state_year_population.dta", keep(master match) nogen
quietly count if missing(pop_yr)
assert r(N) == 0
gen double matched_per_100k_yr    = matched    / pop_yr * 100000
gen double matched_na_per_100k_yr = matched_na / pop_yr * 100000
encode state, gen(state_id)
xtset program_numeric_id year
tempfile master
save `master'

tempname res
tempfile resfile
postfile `res' str24 spec double avg_treat avg_se treat_p pretrend_p baseline pct ///
    using "`resfile'", replace

capture program drop _eerun
program define _eerun
    args outcome tag resh idvar
    if ("`idvar'" == "") local idvar program_numeric_id
    di _n "==================== `tag' ===================="
    capture noisily did_imputation `outcome' `idvar' year year_expanded ///
        [aw=total_population_10], horizons(0/5) pretrend(5) ///
        fe(`idvar' year) cluster(state_id) minn(0) autosample
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
    quietly summarize `outcome' if treated_state==1 & year<year_expanded [aw=total_population_10]
    local b = r(mean)
    local pct = cond(`b' < . & `b' != 0, 100*`a'/`b', .)
    post `resh' ("`tag'") (`a') (`ase') (`tp') (`pt') (`b') (`pct')
    di as result "`tag': avg=" %10.6f `a' " se=" %10.6f `ase' " pct=" %5.1f `pct' ///
        " treat_p=" %6.3f `tp' " pretrend_p=" %6.3f `pt'
end

* 1) entering/exiting institution-years coded missing
use "`master'", clear
_eerun matched_na_per_100k_yr "na_coded" `res'

* 2) always-active institutions only
use "`master'", clear
keep if balanced_full == 1
_eerun matched_per_100k_yr "balanced_only" `res'

* 5) drop SAS-window entrants entirely
use "`master'", clear
drop if sas_window_entrant == 1
_eerun matched_per_100k_yr "no_sas_entrants" `res'

* 3) state totals INCLUDING entrants
use "`master'", clear
collapse (sum) matched (first) pop_yr total_population_10 year_expanded treated_state, ///
    by(state state_id year)
gen double state_total_per_100k = matched / pop_yr * 100000
xtset state_id year
_eerun state_total_per_100k "state_total_all" `res' state_id

* 4) state totals from always-active institutions only
use "`master'", clear
keep if balanced_full == 1
collapse (sum) matched (first) pop_yr total_population_10 year_expanded treated_state, ///
    by(state state_id year)
gen double state_total_per_100k = matched / pop_yr * 100000
xtset state_id year
_eerun state_total_per_100k "state_total_balanced" `res' state_id

postclose `res'
use "`resfile'", clear
list, clean noobs
export delimited using "${tabdir}/entryexit-estimation.csv", replace

di _n "=== entry/exit estimation complete ==="
log close
