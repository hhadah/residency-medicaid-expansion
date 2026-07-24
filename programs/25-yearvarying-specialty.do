* =============================================================================
* YEAR-VARYING per-capita: specialty heterogeneity (primary vs non-primary care).
* Primary outcome = matched positions per contemporary 100,000 (ACS).
* Figures (semantic): main-specialty-nonprimary, main-specialty-primary.
* Each carries a text box: baseline mean, avg post effect (%), treatment p, PRE-TREND p.
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

log using "${topdir}/output/25-yearvarying-specialty.log", replace

use "${datadir}/cleaned_residency_medicaid.dta", clear
egen program_numeric_id = group(state institution_code)
replace state = strtrim(upper(state))
merge m:1 state year using "${datadir}/state_year_population.dta", keep(master match) nogen
quietly count if missing(pop_yr)
di as text "pop_yr merge: " r(N) " unmatched master rows (missing pop_yr)"
assert r(N) == 0
gen double matched_per_100k_yr = matched / pop_yr * 100000

gen byte primary_care = inlist(gen_specialty_alt, "FM", "IM", "Peds")
gen byte specialty_group = 2 if primary_care == 1
replace specialty_group = 1 if primary_care == 0

encode state, gen(state_id)
capture confirm variable treated_state
if _rc gen byte treated_state = !missing(year_expanded)
* NB: no xtset -- the specialty panel has multiple specialties per program-year;
* did_imputation estimates the program/year FE on the row-level data (as in 07).

tempname res
tempfile resfile
postfile `res' str16 specialty double avg_treat avg_se treat_p pretrend_p baseline pct ///
    using "`resfile'", replace

* Shared event-study plotting helpers (_esplot, _fillcoef)
do "${topdir}/programs/_esplot-helpers.do"

foreach spec in 1 2 {
    preserve
    di _n "==================== SPECIALTY group `spec' (matched_per_100k_yr) ===================="
    capture noisily did_imputation matched_per_100k_yr program_numeric_id year year_expanded ///
        [aw=total_population_10] if specialty_group == `spec', ///
        horizons(0/5) pretrend(5) fe(program_numeric_id year) cluster(state_id) minn(0)
    if (_rc != 0) {
        local rc = _rc
        di as error "did_imputation failed for specialty `spec' (rc=`rc')."
        restore
        continue
    }
    lincom (tau0+tau1+tau2+tau3+tau4+tau5)/6
    local a = r(estimate)
    local ase = r(se)
    local pt = .
    capture test pre1 pre2 pre3 pre4 pre5
    if _rc == 0 local pt = r(p)
    local tp = .
    capture test tau0 tau1 tau2 tau3 tau4 tau5
    if _rc == 0 local tp = r(p)
    quietly summarize matched_per_100k_yr if specialty_group == `spec' & treated_state == 1 & year < year_expanded [aw=total_population_10]
    local b = r(mean)
    local pct = cond(`b' < . & `b' != 0, 100*`a'/`b', .)
    local sname = cond(`spec'==2, "Primary Care", "Non-Primary Care")
    post `res' ("`sname'") (`a') (`ase') (`tp') (`pt') (`b') (`pct')
    di as result "`sname': avg=" %9.5f `a' " pct=" %5.1f `pct' " treat_p=" %6.3f `tp' " pretrend_p=" %6.3f `pt'

    * no in-graph title (INV-12): the LaTeX subcaption labels the panel
    local fname = cond(`spec'==2, "main-specialty-primary", "main-specialty-nonprimary")
    _fillcoef
    _esplot "`fname'" "Treatment Effect (per 100,000, year-varying pop.)" "" `a' `b' `pct' `tp' `pt'
    restore
}

postclose `res'
use "`resfile'", clear
list, clean noobs
export delimited using "${tabdir}/yearvarying-specialty-summary.csv", replace
di _n "=== year-varying specialty complete: main-specialty-nonprimary, main-specialty-primary ==="
log close
