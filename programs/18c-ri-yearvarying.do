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

global topdir "/Users/hhadah/Projects/GiT/residency-medicaid-expansion"
global datadir "${topdir}/data/datasets"
global rawdir  "${topdir}/data/raw"
global tabdir  "${topdir}/output/tables"
cap mkdir "${tabdir}"

log using "${topdir}/output/18c-ri-yearvarying.log", replace

local REPS = 1000   // number of permutation draws
if "`1'" != "" local REPS = `1'   // optional override: do 18-....do 20 (smoke test)

* -------------------------------------------------------------------------
* Load and set up the program panel (identical to 05/13)
* -------------------------------------------------------------------------
use "${datadir}/cleaned_program_residency_medicaid.dta", clear
    replace state = strtrim(upper(state))
    merge m:1 state year using "${datadir}/state_year_population.dta", keep(master match) nogen
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
* _avgatt: mean of tau0..tau5 from did_imputation on the CURRENT data, using
* cohort variable `1' as the event-time (year_expanded) argument. r(att).
* -------------------------------------------------------------------------
capture program drop _avgatt
program define _avgatt, rclass
    args cohortvar
    capture noisily did_imputation matched_per_100k_yr program_numeric_id year `cohortvar' ///
        [aw=total_population_10], horizons(0/5) pretrend(5) ///
        fe(program_numeric_id year) cluster(state_id) minn(0)
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

* -------------------------------------------------------------------------
* Results collector
* -------------------------------------------------------------------------
tempname ri
tempfile ri_file
postfile `ri' str24 spec double obs_att ri_p reps n_states n_treated_states ///
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

    * ---- observed statistic ----
    _avgatt year_expanded
    local obs = r(att)

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
    local valid = 0
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
    di as result "RI [`specname']: obs ATT = " %7.4f `obs' ///
        "  RI p = " %6.4f `rip' "  (valid " `valid' " of `reps'" ///
        ", states `Ns', treated `Ntr')"
    global RIG_obs = `obs'
    global RIG_rip = `rip'
    global RIG_Ns  = `Ns'
    global RIG_Ntr = `Ntr'
end

* -------------------------------------------------------------------------
* Run the specs
* -------------------------------------------------------------------------
global RI_SAMPLEIF "1"
_rispec "headline_matched100k" "`master'" `REPS'
post `ri' ("headline_matched100k") (${RIG_obs}) (${RIG_rip}) (`REPS') (${RIG_Ns}) (${RIG_Ntr})

global RI_SAMPLEIF "treated_state==0 | (treated_state==1 & gme_vol==1)"
_rispec "mech_volume" "`master'" `REPS'
post `ri' ("mech_volume") (${RIG_obs}) (${RIG_rip}) (`REPS') (${RIG_Ns}) (${RIG_Ntr})

global RI_SAMPLEIF "treated_state==0 | (treated_state==1 & gme_notvol==1)"
_rispec "mech_nonresponsive" "`master'" `REPS'
post `ri' ("mech_nonresponsive") (${RIG_obs}) (${RIG_rip}) (`REPS') (${RIG_Ns}) (${RIG_Ntr})

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
