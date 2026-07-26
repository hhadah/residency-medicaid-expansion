* =============================================================================
* Randomization (permutation) inference for the main DiD estimates
* ---------------------------------------------------------------------------
* Treatment is assigned at the STATE level, and the population-weighted design
* concentrates identifying variation in a handful of large expansion states.
* With few effective clusters, did_imputation's cluster-robust asymptotic
* p-values are unreliable. This script provides randomization-inference (RI)
* p-values that do NOT rely on large-cluster asymptotics: we repeatedly reassign
* the observed vector of expansion-timing cohorts across states (a sharp null of
* no effect for any unit), re-estimate the average post-expansion ATT, and
* compare the true estimate to the permutation null distribution.
*
* Specs covered (the ones the referee flagged as few-cluster-fragile):
*   - Headline: matched_per_100k_yr (year-varying per-capita), full sample
*   - Mechanism: volume-responsive expansion states + controls (Fig mech, top)
*   - Mechanism: non-responsive expansion states + controls (Fig mech, bottom)
* RI p-value = share of permutations with |avg post ATT| >= |observed|.
*
* Mirrors 05/13-dd analyses (BJS 2024, fe program+year, cluster state, aw pop).
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
global rawdir  "${topdir}/data/raw"
global tabdir  "${topdir}/output/tables"
cap mkdir "${tabdir}"

log using "${topdir}/output/32-ri-yearvarying.log", replace

local REPS = 1000   // number of permutation draws
if "`1'" != "" local REPS = `1'   // optional override: do 18-....do 20 (smoke test)

* -------------------------------------------------------------------------
* Load and set up the program panel (identical to 05/13)
* -------------------------------------------------------------------------
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
egen program_numeric_id = group(state institution_code)
encode state, gen(state_id)
xtset program_numeric_id year

* Merge the Medicaid GME formula classification (2012 baseline) for the mech specs
preserve
    import delimited "${rawdir}/gme_formula_classification.csv", clear ///
        varnames(1) stringcols(_all)
    keep state gme_formula
    replace state = strtrim(upper(state))
    tempfile gme
    save `gme'
restore
replace state = strtrim(upper(state))
merge m:1 state using `gme', keep(master match) nogen
gen byte gme_vol    = (gme_formula == "volume")
gen byte gme_notvol = inlist(gme_formula, "fixed", "none")

tempfile master
save `master'

* -------------------------------------------------------------------------
* Shared RI estimation helper (_avgatt2): returns r(att), r(se), r(t) so the
* permutation test can be run on the coefficient AND studentized (MUST-9).
* -------------------------------------------------------------------------
do "${topdir}/programs/_ri-avgatt.do"
capture program drop _avgatt
program define _avgatt, rclass
    args cohortvar
    _avgatt2 `cohortvar' matched_per_100k_yr 5 "autosample"
    return scalar att = r(att)
    return scalar se  = r(se)
    return scalar t   = r(t)
end

* -------------------------------------------------------------------------
* Results collector
* -------------------------------------------------------------------------
tempname ri
tempfile ri_file
postfile `ri' str24 spec double obs_att ri_p ri_p_student reps n_states n_treated_states ///
    using "`ri_file'", replace

* -------------------------------------------------------------------------
* _rispec: restricts `master' to the condition in global RI_SAMPLEIF, computes
* the observed ATT, then REPS permutations of the state-level cohort vector
* under the sharp null, and the two-sided RI p-value. Results -> globals.
* -------------------------------------------------------------------------
capture program drop _rispec
program define _rispec
    args specname master reps
    use "`master'", clear
    keep if $RI_SAMPLEIF

    * ---- observed statistic (coefficient AND studentized) ----
    _avgatt year_expanded
    local obs   = r(att)
    local obs_t = r(t)

    * ---- state-level cohort pool (one row per state), stored once ----
    preserve
        bysort state_id: keep if _n == 1
        keep state_id year_expanded
        count
        local Ns = r(N)
        count if !missing(year_expanded)
        local Ntr = r(N)
        tempfile states0
        save `states0'
    restore

    * ---- permutation loop ----
    * NOTE (2026-07-24 fix): the cohort vector must be reassigned to the ACTUAL
    * state_id values present in the (possibly restricted) sample. The previous
    * version assigned shuffled cohorts to state_id = 1..Ns, which is only
    * correct for the full sample; in restricted samples state_id has gaps, so
    * some permuted cohorts landed on nonexistent states and some sample states
    * silently became never-treated. We now pair the cohort list with the
    * sample's own state_ids in random order (a uniform permutation).
    local ge = 0
    local ge_t = 0
    local valid = 0
    local valid_t = 0
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
        local perm   = r(att)
        local perm_t = r(t)
        capture drop ye_perm
        if (`perm' < .) {
            local valid = `valid' + 1
            if (abs(`perm') >= abs(`obs') - 1e-12) local ge = `ge' + 1
        }
        if (`perm_t' < . & `obs_t' < .) {
            local valid_t = `valid_t' + 1
            if (abs(`perm_t') >= abs(`obs_t') - 1e-12) local ge_t = `ge_t' + 1
        }
    }
    * exact-test convention: the observed assignment counts as one permutation
    local rip   = cond(`valid' > 0, (`ge' + 1)/(`valid' + 1), .)
    local rip_t = cond(`valid_t' > 0, (`ge_t' + 1)/(`valid_t' + 1), .)
    di as result "RI [`specname']: obs ATT = " %7.4f `obs' ///
        "  RI p = " %6.4f `rip' "  (valid " `valid' " of `reps'" ///
        ", states `Ns', treated `Ntr')"
    global RIG_obs = `obs'
    global RIG_rip = `rip'
    global RIG_ript = `rip_t'
    global RIG_Ns  = `Ns'
    global RIG_Ntr = `Ntr'
end

* -------------------------------------------------------------------------
* Run the specs
* -------------------------------------------------------------------------
global RI_SAMPLEIF "1"
_rispec "headline_matched100k" "`master'" `REPS'
post `ri' ("headline_matched100k") (${RIG_obs}) (${RIG_rip}) (${RIG_ript}) (`REPS') (${RIG_Ns}) (${RIG_Ntr})

global RI_SAMPLEIF "treated_state==0 | (treated_state==1 & gme_vol==1)"
_rispec "mech_volume" "`master'" `REPS'
post `ri' ("mech_volume") (${RIG_obs}) (${RIG_rip}) (${RIG_ript}) (`REPS') (${RIG_Ns}) (${RIG_Ntr})

global RI_SAMPLEIF "treated_state==0 | (treated_state==1 & gme_notvol==1)"
_rispec "mech_nonresponsive" "`master'" `REPS'
post `ri' ("mech_nonresponsive") (${RIG_obs}) (${RIG_rip}) (${RIG_ript}) (`REPS') (${RIG_Ns}) (${RIG_Ntr})

postclose `ri'

* -------------------------------------------------------------------------
* Save summary
* -------------------------------------------------------------------------
use "`ri_file'", clear
list, clean noobs
export delimited using "${tabdir}/ri-yearvarying-summary.csv", replace

di ""
di "=================================================================="
di "Randomization inference complete (REPS = `REPS')."
di "Summary: ${tabdir}/ri-yearvarying-summary.csv"
di "=================================================================="

log close
