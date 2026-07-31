* =============================================================================
* Randomization (permutation) inference — EXTENDED specs (uniform standard).
* ---------------------------------------------------------------------------
* The headline year-varying estimate gets RI in script 32; this script extends the
* same permutation machinery to every remaining member of the primary FDR
* family plus the not-yet-treated robustness spec, so that ONE conservative
* inference standard applies uniformly (referee: the "firmest" results must
* not rest on the clustered p-values the paper elsewhere distrusts):
*   - urban:      matched_per_100k_yr, urban programs only (split sample, as 24)
*   - rural:      matched_per_100k_yr, rural programs only (split sample, as 24)
*   - quota:      quota_per_100k_yr, full sample (as 24)
*   - notyet:     matched_per_100k_yr, expansion states only, horizons 0/4 (as 26)
*   - primary:    matched_per_100k_yr, primary-care rows, specialty panel (as 25)
*   - nonprimary: matched_per_100k_yr, non-primary rows, specialty panel (as 25)
*   - mechdiff:   volume-minus-nonresponsive DIFFERENCE in avg post ATT (as 24's
*                 mechanism arms), cohorts permuted jointly over all states
*
* Method (identical to scripts 30/32, incl. the 2026-07-24 permutation fix): reassign
* the observed vector of state-level expansion cohorts across the ACTUAL states
* in each spec's sample under a sharp null, re-estimate the average post ATT,
* RI p = share of permutations with |avg post ATT| >= |observed|. For the
* not-yet-treated spec the sample contains only expansion states, so the same
* machinery automatically permutes expansion TIMING among adopters (holding the
* cohort-size distribution fixed) — the correct sharp null for a timing-only
* design.
*
* Output: output/tables/ri-extended-summary.csv
* Runtime: ~6 specs x REPS did_imputation calls. Smoke test: do 33-ri-extended.do 20
* =============================================================================

clear all
set more off
set seed 20260724

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

log using "${topdir}/output/33-ri-extended.log", replace

local REPS = 500   // number of permutation draws
if "`1'" != "" local REPS = `1'   // optional override: do 33-ri-extended.do 20 (smoke test)

* -------------------------------------------------------------------------
* Master 1: program panel (identical setup to 24/26)
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
capture confirm variable quota
if _rc == 0 gen double quota_per_100k_yr = quota / pop_yr * 100000
capture confirm variable rural_urban_2010
if _rc == 0 gen byte urban_rural = (rural_urban_2010 > 3) if !missing(rural_urban_2010)
* GME formula classification (for the mechanism cross-arm difference spec)
preserve
    import delimited "${rawdir}/gme_formula_classification.csv", clear varnames(1) stringcols(_all)
    keep state gme_formula_2015
    replace state = strtrim(upper(state))
    tempfile gme
    save `gme'
restore
merge m:1 state using `gme', keep(master match) nogen
gen byte gme_vol    = (gme_formula_2015 == "volume")
gen byte gme_notvol = inlist(gme_formula_2015, "fixed", "none")
egen program_numeric_id = group(state institution_code)
encode state, gen(state_id)
xtset program_numeric_id year
tempfile master_prog
save `master_prog'

* -------------------------------------------------------------------------
* Master 2: specialty panel (identical setup to 25; no xtset — the panel has
* multiple specialty rows per program-year)
* -------------------------------------------------------------------------
use "${datadir}/panel_2000_2019_specialty.dta", clear
replace matched = matched_na
gen byte primary_care = inlist(gen_specialty_alt, "FM", "IM", "Peds")
gen byte specialty_group = 2 if primary_care == 1
replace specialty_group = 1 if primary_care == 0
* collapse to institution x specialty-group x year (as script 21): one row per
* institution-year within each group, NA-aware
collapse (sum) matched (count) n_obs = matched ///
    (first) total_population_10 year_expanded treated_state pop_yr, ///
    by(state institution_code specialty_group year)
replace matched = . if n_obs == 0
drop n_obs
gen double matched_per_100k_yr = matched / pop_yr * 100000
egen program_numeric_id = group(state institution_code)
encode state, gen(state_id)
capture confirm variable treated_state
if _rc gen byte treated_state = !missing(year_expanded)
tempfile master_spec
save `master_spec'

* Shared RI estimation helper (_avgatt2)
do "${topdir}/programs/_ri-avgatt.do"

* -------------------------------------------------------------------------
* Results collector
* -------------------------------------------------------------------------
tempname ri
tempfile ri_file
postfile `ri' str24 spec double obs_att ri_p ri_p_student reps valid n_states n_treated_states ///
    using "`ri_file'", replace

* -------------------------------------------------------------------------
* _rispec2: restricts `master' to the condition in global RI_SAMPLEIF, computes
* the observed ATT, then REPS permutations of the state-level cohort vector
* under the sharp null (cohorts paired with the sample's ACTUAL state_ids in
* random order), and the two-sided RI p-value. Results -> globals.
* -------------------------------------------------------------------------
capture program drop _rispec2
program define _rispec2
    args specname master reps outcome hmax extra
    use "`master'", clear
    keep if $RI_SAMPLEIF

    * ---- observed statistic (coefficient AND studentized) ----
    _avgatt2 year_expanded `outcome' `hmax' "`extra'"
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

        _avgatt2 ye_perm `outcome' `hmax' "`extra'"
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
    global RIG_obs   = `obs'
    global RIG_rip   = `rip'
    global RIG_ript  = `rip_t'
    global RIG_valid = `valid'
    global RIG_Ns    = `Ns'
    global RIG_Ntr   = `Ntr'
end

* -------------------------------------------------------------------------
* Run the specs (estimator options mirror the reported spec exactly)
* -------------------------------------------------------------------------
global RI_SAMPLEIF "urban_rural == 0"
_rispec2 "urban" "`master_prog'" `REPS' "matched_per_100k_yr" 5 "autosample"
post `ri' ("urban") (${RIG_obs}) (${RIG_rip}) (${RIG_ript}) (`REPS') (${RIG_valid}) (${RIG_Ns}) (${RIG_Ntr})

global RI_SAMPLEIF "urban_rural == 1"
_rispec2 "rural" "`master_prog'" `REPS' "matched_per_100k_yr" 5 "autosample"
post `ri' ("rural") (${RIG_obs}) (${RIG_rip}) (${RIG_ript}) (`REPS') (${RIG_valid}) (${RIG_Ns}) (${RIG_Ntr})

global RI_SAMPLEIF "1"
_rispec2 "quota" "`master_prog'" `REPS' "quota_per_100k_yr" 5 "autosample"
post `ri' ("quota") (${RIG_obs}) (${RIG_rip}) (${RIG_ript}) (`REPS') (${RIG_valid}) (${RIG_Ns}) (${RIG_Ntr})

global RI_SAMPLEIF "treated_state == 1"
_rispec2 "notyet" "`master_prog'" `REPS' "matched_per_100k_yr" 4 "autosample"
post `ri' ("notyet") (${RIG_obs}) (${RIG_rip}) (${RIG_ript}) (`REPS') (${RIG_valid}) (${RIG_Ns}) (${RIG_Ntr})

global RI_SAMPLEIF "specialty_group == 2"
_rispec2 "primary" "`master_spec'" `REPS' "matched_per_100k_yr" 5 "autosample"
post `ri' ("primary") (${RIG_obs}) (${RIG_rip}) (${RIG_ript}) (`REPS') (${RIG_valid}) (${RIG_Ns}) (${RIG_Ntr})

global RI_SAMPLEIF "specialty_group == 1"
_rispec2 "nonprimary" "`master_spec'" `REPS' "matched_per_100k_yr" 5 "autosample"
post `ri' ("nonprimary") (${RIG_obs}) (${RIG_rip}) (${RIG_ript}) (`REPS') (${RIG_valid}) (${RIG_Ns}) (${RIG_Ntr})

* -------------------------------------------------------------------------
* Mechanism cross-arm DIFFERENCE spec: statistic = avg post ATT in the
* volume-responsive arm minus avg post ATT in the fixed/none arm, where each
* arm = (permuted-)never states + (permuted-)treated states of that formula
* class (mirroring script 20's mechanism construction; autosample required on
* the full panel). Cohorts are permuted jointly over ALL states; formula classification is a
* fixed state attribute.
* -------------------------------------------------------------------------
capture program drop _mechdiff
program define _mechdiff, rclass
    args cohortvar
    preserve
    keep if missing(`cohortvar') | gme_vol == 1
    _avgatt2 `cohortvar' matched_per_100k_yr 5 "autosample"
    local attA = r(att)
    restore
    preserve
    keep if missing(`cohortvar') | gme_notvol == 1
    _avgatt2 `cohortvar' matched_per_100k_yr 5 "autosample"
    local attB = r(att)
    restore
    return scalar diff = cond(`attA' < . & `attB' < ., `attA' - `attB', .)
end

use "`master_prog'", clear
_mechdiff year_expanded
local obs = r(diff)
di as result "observed mech diff (volume - nonresponsive) = " %8.4f `obs'
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
local ge = 0
local valid = 0
forval r = 1/`REPS' {
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
    _mechdiff ye_perm
    local perm = r(diff)
    capture drop ye_perm
    if (`perm' < .) {
        local valid = `valid' + 1
        if (abs(`perm') >= abs(`obs') - 1e-12) local ge = `ge' + 1
    }
}
* exact-test convention: the observed assignment counts as one permutation
local rip = cond(`valid' > 0, (`ge' + 1)/(`valid' + 1), .)
di as result "RI [mechdiff]: obs diff = " %7.4f `obs' "  RI p = " %6.4f `rip' ///
    "  (valid " `valid' " of `REPS', states `Ns', treated `Ntr')"
post `ri' ("mechdiff") (`obs') (`rip') (.) (`REPS') (`valid') (`Ns') (`Ntr')

postclose `ri'

* -------------------------------------------------------------------------
* Save summary
* -------------------------------------------------------------------------
use "`ri_file'", clear
list, clean noobs
export delimited using "${tabdir}/ri-extended-summary.csv", replace

di ""
di "=================================================================="
di "Extended randomization inference complete (REPS = `REPS')."
di "Summary: ${tabdir}/ri-extended-summary.csv"
di "=================================================================="

log close
