* =============================================================================
* YEAR-VARYING per-capita: specialty heterogeneity (primary vs non-primary care).
* ---------------------------------------------------------------------------
* Revised 2026-07-24 for the referee response (editorial decision MUST-10).
* The submitted version estimated on the raw specialty-level panel with
* program_numeric_id as the unit id, so a hospital contributed up to three
* rows per year (FM, IM, Peds) sharing a single unit fixed effect with no
* specialty fixed effect -- the BJS imputation is not well defined on that
* structure, and the group effects summed to a third of the aggregate.
*
* This version aggregates to HOSPITAL x SPECIALTY-GROUP (two rows per
* hospital-year), takes hospital-x-group as the unit fixed effect, and
* verifies that the two group effects aggregate to the total: because the
* headline outcome is a per-capita sum and both regressions share weights,
* avg(primary) + avg(non-primary) should equal the headline average post
* effect estimated on the summed outcome (reported as `sum_check`).
*
* Primary outcome = matched positions per contemporary 100,000 (ACS).
* Figures (semantic): main-specialty-nonprimary, main-specialty-primary.
* Summary CSV stores the SE of the average post-ATT (for the FDR forest plot).
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

log using "${topdir}/output/21-yearvarying-specialty.log", replace

use "${datadir}/cleaned_residency_medicaid.dta", clear
replace state = strtrim(upper(state))
capture confirm variable treated_state
if _rc gen byte treated_state = !missing(year_expanded)
gen byte primary_care = inlist(gen_specialty_alt, "FM", "IM", "Peds")
gen byte specialty_group = 2 if primary_care == 1
replace specialty_group = 1 if primary_care == 0

* Aggregate to hospital x specialty-group x year (referee fix: two rows per
* hospital-year, each with its own unit fixed effect)
collapse (sum) matched (first) total_population_10 year_expanded treated_state, ///
    by(state institution_code specialty_group year)

merge m:1 state year using "${datadir}/state_year_population.dta", keep(master match) nogen
quietly count if missing(pop_yr)
di as text "pop_yr merge: " r(N) " unmatched master rows (missing pop_yr)"
assert r(N) == 0
gen double matched_per_100k_yr = matched / pop_yr * 100000

egen unit_id = group(state institution_code specialty_group)
egen program_numeric_id = group(state institution_code)
encode state, gen(state_id)
xtset unit_id year
tempfile master
save `master'

tempname res
tempfile resfile
postfile `res' str16 specialty double avg_treat avg_se treat_p pretrend_p baseline pct ///
    using "`resfile'", replace

* Shared event-study plotting helpers (_esplot, _fillcoef)
do "${topdir}/programs/_esplot-helpers.do"

local sum_avg = 0
foreach spec in 1 2 {
    use "`master'", clear
    keep if specialty_group == `spec'
    di _n "==================== SPECIALTY group `spec' (hospital x group panel) ===================="
    capture noisily did_imputation matched_per_100k_yr unit_id year year_expanded ///
        [aw=total_population_10], horizons(0/5) pretrend(5) ///
        fe(unit_id year) cluster(state_id) minn(0) autosample
    if (_rc != 0) {
        local rc = _rc
        di as error "did_imputation failed for specialty `spec' (rc=`rc')."
        continue
    }
    lincom (tau0+tau1+tau2+tau3+tau4+tau5)/6
    local a = r(estimate)
    local ase = r(se)
    local sum_avg = `sum_avg' + `a'
    local pt = .
    capture test pre1 pre2 pre3 pre4 pre5
    if _rc == 0 local pt = r(p)
    local tp = .
    capture test tau0 tau1 tau2 tau3 tau4 tau5
    if _rc == 0 local tp = r(p)
    quietly summarize matched_per_100k_yr if treated_state == 1 & year < year_expanded [aw=total_population_10]
    local b = r(mean)
    local pct = cond(`b' < . & `b' != 0, 100*`a'/`b', .)
    local sname = cond(`spec'==2, "Primary Care", "Non-Primary Care")
    post `res' ("`sname'") (`a') (`ase') (`tp') (`pt') (`b') (`pct')
    di as result "`sname': avg=" %9.5f `a' " se=" %9.5f `ase' " pct=" %5.1f `pct' ///
        " treat_p=" %6.3f `tp' " pretrend_p=" %6.3f `pt'

    * no in-graph title (INV-12): the LaTeX subcaption labels the panel
    local fname = cond(`spec'==2, "main-specialty-primary", "main-specialty-nonprimary")
    _fillcoef
    _esplot "`fname'" "Treatment Effect (per 100,000, year-varying pop.)" "" `a' `b' `pct' `tp' `pt'
}

* -------------------------------------------------------------------------
* Aggregation check: the two group effects should sum to the headline effect
* estimated on the hospital-level total (same rows, summed across groups)
* -------------------------------------------------------------------------
use "`master'", clear
collapse (sum) matched (first) total_population_10 year_expanded treated_state pop_yr, ///
    by(state institution_code program_numeric_id year)
gen double matched_per_100k_yr = matched / pop_yr * 100000
encode state, gen(state_id2)
xtset program_numeric_id year
did_imputation matched_per_100k_yr program_numeric_id year year_expanded ///
    [aw=total_population_10], horizons(0/5) pretrend(5) ///
    fe(program_numeric_id year) cluster(state_id2) minn(0) autosample
lincom (tau0+tau1+tau2+tau3+tau4+tau5)/6
local tot = r(estimate)
local totse = r(se)
post `res' ("sum_of_groups") (`sum_avg') (.) (.) (.) (.) (.)
post `res' ("total_check") (`tot') (`totse') (.) (.) (.) (.)
di as result "sum of group effects = " %9.5f `sum_avg' ///
    " vs total on summed outcome = " %9.5f `tot' " (se " %9.5f `totse' ")"

postclose `res'
use "`resfile'", clear
list, clean noobs
export delimited using "${tabdir}/yearvarying-specialty-summary.csv", replace
di _n "=== year-varying specialty complete: main-specialty-nonprimary, main-specialty-primary ==="
log close
