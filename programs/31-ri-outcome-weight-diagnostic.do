* =============================================================================
* Diagnostic: randomization-inference p-values for the HEADLINE under
* alternative outcome definitions and weighting schemes.
* Answers: is "levels" and/or "unweighted" more robust to few-cluster inference
* than the fixed-2010 per-100k weighted headline?
* Same permutation engine as script 18 (permute state-level cohorts, sharp null).
* =============================================================================

clear all
set more off
set seed 20260723

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

log using "${topdir}/output/31-ri-outcome-weight-diagnostic.log", replace

local REPS = 500
if "`1'" != "" local REPS = `1'

* FULL 2000-2019 PANEL (activity-window coding is primary; see script 06)
use "${datadir}/panel_2000_2019_estimation.dta", clear
replace matched = matched_na
replace quota   = quota_na
gen double matched_per_100k = matched / total_population_10 * 100000
gen double quota_per_100k   = quota   / total_population_10 * 100000
gen double unmatched        = quota - matched
egen program_numeric_id = group(state institution_code)
encode state, gen(state_id)
xtset program_numeric_id year
tempfile master
save `master'

* _avgatt: mean tau0..5 for outcome $RIO with weight expression $RIW, cohort `1'
capture program drop _avgatt
program define _avgatt, rclass
    args cohortvar
    capture noisily did_imputation $RIO program_numeric_id year `cohortvar' ///
        $RIW, horizons(0/5) pretrend(10) fe(program_numeric_id year) ///
        cluster(state_id) minn(0) autosample
    if (_rc != 0) {
        return scalar att = .
        exit
    }
    local s = 0
    local n = 0
    forval h = 0/5 {
        capture scalar __b = _b[tau`h']
        if (_rc == 0) {
            local s = `s' + __b
            local n = `n' + 1
        }
    }
    capture scalar drop __b
    return scalar att = cond(`n' > 0, `s'/`n', .)
end

tempname ri
tempfile ri_file
postfile `ri' str28 spec double obs_att ri_p reps using "`ri_file'", replace

capture program drop _rispec
program define _rispec
    args specname master reps
    use "`master'", clear
    _avgatt year_expanded
    local obs = r(att)
    preserve
        bysort state_id: keep if _n == 1
        keep state_id year_expanded
        tempfile states0
        save `states0'
    restore
    local ge = 0
    local valid = 0
    * (2026-07-24 fix) pair cohorts with the sample's ACTUAL state_ids in
    * random order, rather than assigning to state_id = 1..Ns.
    forval r = 1/`reps' {
        preserve
            use `states0', clear
            gen double _rnd = runiform()
            sort _rnd
            gen double ye_perm = year_expanded   // cohorts in random order
            keep ye_perm
            gen long _k = _n
            tempfile cohperm
            save `cohperm'
            use `states0', clear
            keep state_id
            sort state_id
            gen long _k = _n
            merge 1:1 _k using `cohperm', nogen
            keep state_id ye_perm
            tempfile assign
            save `assign'
        restore
        capture drop ye_perm
        merge m:1 state_id using `assign', keep(master match) nogen
        _avgatt ye_perm
        local perm = r(att)
        capture drop ye_perm
        if (`perm' < .) {
            local valid = `valid' + 1
            if (abs(`perm') >= abs(`obs') - 1e-12) local ge = `ge' + 1
        }
    }
    * exact-test convention: the observed assignment counts as one permutation
    local rip = cond(`valid' > 0, (`ge' + 1)/(`valid' + 1), .)
    di as result "RI [`specname']: obs = " %8.4f `obs' "  RI p = " %6.4f `rip' " (valid `valid'/`reps')"
    global RG_obs = `obs'
    global RG_rip = `rip'
end

* Spec 1: LEVELS, population-weighted
global RIO "matched"
global RIW "[aw=total_population_10]"
_rispec "levels_weighted" "`master'" `REPS'
post `ri' ("levels_weighted") (${RG_obs}) (${RG_rip}) (`REPS')

* Spec 2: LEVELS, unweighted
global RIO "matched"
global RIW ""
_rispec "levels_unweighted" "`master'" `REPS'
post `ri' ("levels_unweighted") (${RG_obs}) (${RG_rip}) (`REPS')

* Spec 3: per-100k (fixed 2010), unweighted
global RIO "matched_per_100k"
global RIW ""
_rispec "percap_unweighted" "`master'" `REPS'
post `ri' ("percap_unweighted") (${RG_obs}) (${RG_rip}) (`REPS')

postclose `ri'
use "`ri_file'", clear
list, clean noobs
export delimited using "${tabdir}/ri-outcome-weight-diagnostic.csv", replace
log close
