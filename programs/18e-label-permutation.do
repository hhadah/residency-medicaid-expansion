* =============================================================================
* Classification placebo: permute the GME-FORMULA LABELS across expansion states.
* ---------------------------------------------------------------------------
* The mechanism split (volume-responsive vs fixed/none) is a policy choice, not
* a random assignment. This placebo asks: is the formula split special, or would
* many arbitrary groupings of expansion states produce a cross-arm contrast as
* large as the observed one?
*
* Design: hold expansion TIMING fixed at its observed values; permute the
* volume/non-responsive labels across the classified expansion states (holding
* the arm sizes fixed at 16 volume / 18 fixed-none); recompute the cross-arm
* difference in average post ATT each draw (arms constructed exactly as in
* script 24's mechanism analysis: never-expansion controls + the permuted-label
* treated states of each class; no autosample, as 24). Placebo p = share of
* label permutations with |diff| >= |observed| (exact-test convention).
*
* This complements 18d's `mechdiff' spec, which holds labels fixed and permutes
* timing: 18d tests the sharp null of no treatment effect; 18e tests whether
* the FORMULA CLASSIFICATION specifically -- rather than any split of expansion
* states -- generates the contrast.
*
* Output: output/tables/label-permutation-summary.csv
* Runtime: 2 x REPS did_imputation calls (default 500). Smoke test: do 18e-... 20
* =============================================================================

clear all
set more off
set seed 20260725

global topdir "/Users/hhadah/Projects/GiT/residency-medicaid-expansion"
global datadir "${topdir}/data/datasets"
global rawdir  "${topdir}/data/raw"
global tabdir  "${topdir}/output/tables"
cap mkdir "${tabdir}"

log using "${topdir}/output/18e-label-permutation.log", replace

local REPS = 500
if "`1'" != "" local REPS = `1'

* -------------------------------------------------------------------------
* Program panel with GME classification (identical setup to 18d / 24)
* -------------------------------------------------------------------------
use "${datadir}/cleaned_program_residency_medicaid.dta", clear
replace state = strtrim(upper(state))
merge m:1 state year using "${datadir}/state_year_population.dta", keep(master match) nogen
gen double matched_per_100k_yr = matched / pop_yr * 100000
preserve
    import delimited "${rawdir}/gme_formula_classification.csv", clear varnames(1) stringcols(_all)
    keep state gme_formula
    replace state = strtrim(upper(state))
    tempfile gme
    save `gme'
restore
merge m:1 state using `gme', keep(master match) nogen
gen byte gme_vol    = (gme_formula == "volume")
gen byte gme_notvol = inlist(gme_formula, "fixed", "none")
egen program_numeric_id = group(state institution_code)
encode state, gen(state_id)
xtset program_numeric_id year
tempfile master
save `master'

* Shared RI estimation helper (_avgatt2)
do "${topdir}/programs/_ri-avgatt.do"

* -------------------------------------------------------------------------
* _labdiff: cross-arm difference in avg post ATT for label variable `volvar'
* (1 = volume arm, 0 = fixed/none arm among classified treated states).
* Arms mirror 24's mechanism construction; timing = observed year_expanded.
* -------------------------------------------------------------------------
capture program drop _labdiff
program define _labdiff, rclass
    args volvar
    preserve
    keep if treated_state == 0 | (treated_state == 1 & `volvar' == 1)
    _avgatt2 year_expanded matched_per_100k_yr 5 ""
    local attA = r(att)
    restore
    preserve
    keep if treated_state == 0 | (treated_state == 1 & `volvar' == 0)
    _avgatt2 year_expanded matched_per_100k_yr 5 ""
    local attB = r(att)
    restore
    return scalar diff = cond(`attA' < . & `attB' < ., `attA' - `attB', .)
end

* ---- observed statistic (classified treated states only in either arm) ----
use "`master'", clear
gen byte lab_obs = gme_vol if treated_state == 1 & (gme_vol == 1 | gme_notvol == 1)
_labdiff lab_obs
local obs = r(diff)
di as result "observed label diff (volume - nonresponsive) = " %8.4f `obs'

* ---- label pool: one row per classified expansion state ----
preserve
    keep if treated_state == 1 & (gme_vol == 1 | gme_notvol == 1)
    bysort state_id: keep if _n == 1
    keep state_id gme_vol
    count
    local Ntr = r(N)
    quietly count if gme_vol == 1
    local Nvol = r(N)
    tempfile labels0
    save `labels0'
restore
di as text "label pool: `Ntr' classified expansion states (`Nvol' volume)"

* ---- label-permutation loop (arm sizes held fixed) ----
local ge = 0
local valid = 0
forval r = 1/`REPS' {
    preserve
        use `labels0', clear
        gen double _rnd = runiform()
        sort _rnd
        gen byte lab_perm = gme_vol   // labels in random order
        keep lab_perm
        gen long _k = _n
        tempfile labperm
        save `labperm'
        use `labels0', clear
        keep state_id
        sort state_id
        gen long _k = _n
        merge 1:1 _k using `labperm', nogen
        keep state_id lab_perm
        tempfile assign
        save `assign'
    restore
    capture drop lab_perm
    merge m:1 state_id using `assign', keep(master match) nogen

    _labdiff lab_perm
    local perm = r(diff)
    capture drop lab_perm
    if (`perm' < .) {
        local valid = `valid' + 1
        if (abs(`perm') >= abs(`obs') - 1e-12) local ge = `ge' + 1
    }
}
* exact-test convention: the observed assignment counts as one permutation
local pp = cond(`valid' > 0, (`ge' + 1)/(`valid' + 1), .)
di as result "LABEL PLACEBO: obs diff = " %7.4f `obs' "  placebo p = " %6.4f `pp' ///
    "  (valid " `valid' " of `REPS', pool `Ntr' states, `Nvol' volume)"

* ---- save summary ----
clear
set obs 1
gen str24 spec = "label_placebo"
gen double obs_diff = `obs'
gen double placebo_p = `pp'
gen reps = `REPS'
gen valid = `valid'
gen n_pool = `Ntr'
gen n_volume = `Nvol'
export delimited using "${tabdir}/label-permutation-summary.csv", replace

di _n "=== label permutation complete: label-permutation-summary.csv ==="
log close
