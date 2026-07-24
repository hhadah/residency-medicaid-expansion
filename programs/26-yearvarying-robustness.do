* =============================================================================
* YEAR-VARYING per-capita robustness: not-yet-treated design + HonestDiD.
* Primary outcome = matched positions per contemporary 100,000 (ACS).
* Figures (semantic): appx-notyet, appx-honestdid-headline, appx-honestdid-nonresp.
* Not-yet-treated is estimated over horizons 0..4 (the last-treated cohort has no
* valid comparison at +5 under autosample), so the event study runs -5..+4.
* The not-yet figure carries a text box with baseline, avg post effect (%),
* treatment p, and PRE-TREND p.
* =============================================================================

clear all
set more off
global topdir "/Users/hhadah/Projects/GiT/residency-medicaid-expansion"
global datadir "${topdir}/data/datasets"
global rawdir  "${topdir}/data/raw"
global figdir  "${topdir}/output/figures"
global tabdir  "${topdir}/output/tables"
global latex_figdir "${topdir}/my_paper/figures"
cap mkdir "${figdir}"
cap mkdir "${tabdir}"
cap mkdir "${latex_figdir}"
log using "${topdir}/output/26-yearvarying-robustness.log", replace

use "${datadir}/cleaned_program_residency_medicaid.dta", clear
egen program_numeric_id = group(state institution_code)
replace state = strtrim(upper(state))
merge m:1 state year using "${datadir}/state_year_population.dta", keep(master match) nogen
quietly count if missing(pop_yr)
di as text "pop_yr merge: " r(N) " unmatched master rows (missing pop_yr)"
assert r(N) == 0
gen double matched_per_100k_yr = matched / pop_yr * 100000
preserve
    import delimited "${rawdir}/gme_formula_classification.csv", clear varnames(1) stringcols(_all)
    keep state gme_formula
    replace state = strtrim(upper(state))
    tempfile gme
    save `gme'
restore
merge m:1 state using `gme', keep(master match) nogen
gen byte gme_notvol = inlist(gme_formula, "fixed", "none")
gen byte gme_vol    = (gme_formula == "volume")
* Urban/rural (as in 24/06): rural = RUCA > 3, missing RUCA excluded
capture confirm variable rural_urban_2010
if _rc == 0 gen byte urban_rural = (rural_urban_2010 > 3) if !missing(rural_urban_2010)
encode state, gen(state_id)
xtset program_numeric_id year
tempfile master
save `master'

* Shared event-study plotting helpers (_esplot, _fillcoef)
do "${topdir}/programs/_esplot-helpers.do"

* ---------------- 1) NOT-YET-TREATED (drop never-expansion), horizons 0..4 -----
use "`master'", clear
keep if treated_state == 1
di _n "=== NOT-YET-TREATED (year-varying), horizons 0/4 ==="
did_imputation matched_per_100k_yr program_numeric_id year year_expanded ///
    [aw=total_population_10], horizons(0/4) pretrend(5) ///
    fe(program_numeric_id year) cluster(state_id) minn(0) autosample
lincom (tau0+tau1+tau2+tau3+tau4)/5
local a  = r(estimate)
local ase = r(se)
local pt = .
capture test pre1 pre2 pre3 pre4 pre5
if _rc == 0 local pt = r(p)
local tp = .
capture test tau0 tau1 tau2 tau3 tau4
if _rc == 0 local tp = r(p)
quietly summarize matched_per_100k_yr if year < year_expanded [aw=total_population_10]
local b = r(mean)
local pct = cond(`b' < . & `b' != 0, 100*`a'/`b', .)
di as result "notyet-yv: avg=" %9.5f `a' " pct=" %5.1f `pct' " treat_p=" %6.3f `tp' " pretrend_p=" %6.3f `pt'
* Citable summary CSV (not-yet-treated + balanced 2014 cohort).
tempname nyres
tempfile nyfile
postfile `nyres' str24 spec double avg_treat avg_se treat_p pretrend_p baseline pct ///
    using "`nyfile'", replace
post `nyres' ("notyet") (`a') (`ase') (`tp') (`pt') (`b') (`pct')
* event-study figure over horizons -5..+4 via shared helpers
_fillcoef 4
_esplot "appx-notyet" "Treatment Effect (per 100,000, year-varying pop.)" "" `a' `b' `pct' `tp' `pt' 4

* ---------------- 1b) BALANCED COHORT: 2014 adopters only ---------------------
* Horizons +4/+5 of the pooled event study are identified only from early
* cohorts, so a monotonically widening path could reflect cohort composition.
* Restricting treatment to the (largest) 2014 cohort holds composition fixed:
* if the path still widens within this single cohort, the dynamics are genuine.
use "`master'", clear
keep if treated_state == 0 | year_expanded == 2014
di _n "=== BALANCED COHORT (2014 adopters + never-expansion), horizons 0/5 ==="
did_imputation matched_per_100k_yr program_numeric_id year year_expanded ///
    [aw=total_population_10], horizons(0/5) pretrend(5) ///
    fe(program_numeric_id year) cluster(state_id) minn(0) autosample
lincom (tau0+tau1+tau2+tau3+tau4+tau5)/6
local a  = r(estimate)
local ase = r(se)
local pt = .
capture test pre1 pre2 pre3 pre4 pre5
if _rc == 0 local pt = r(p)
local tp = .
capture test tau0 tau1 tau2 tau3 tau4 tau5
if _rc == 0 local tp = r(p)
quietly summarize matched_per_100k_yr if treated_state==1 & year < year_expanded [aw=total_population_10]
local b = r(mean)
local pct = cond(`b' < . & `b' != 0, 100*`a'/`b', .)
post `nyres' ("cohort2014") (`a') (`ase') (`tp') (`pt') (`b') (`pct')
di as result "cohort2014: avg=" %9.5f `a' " pct=" %5.1f `pct' " treat_p=" %6.3f `tp' " pretrend_p=" %6.3f `pt'
_fillcoef
_esplot "appx-cohort2014" "Treatment Effect (per 100,000, year-varying pop.)" "" `a' `b' `pct' `tp' `pt'

postclose `nyres'
preserve
use "`nyfile'", clear
list, clean noobs
export delimited using "${tabdir}/notyet-yearvarying-summary.csv", replace
restore

* ---------------- 2) HonestDiD on year-varying (headline + non-responsive) ----
honestdid _plugin_check
capture program drop _honest_yv
program define _honest_yv
    args fname tag
    did_imputation matched_per_100k_yr program_numeric_id year year_expanded ///
        [aw=total_population_10], horizons(0/5) pretrend(5) ///
        fe(program_numeric_id year) cluster(state_id) minn(0)
    local order pre5 pre4 pre3 pre2 pre1 tau0 tau1 tau2 tau3 tau4 tau5
    matrix b0 = e(b)
    matrix V0 = e(V)
    matrix bb = J(1,11,.)
    matrix VV = J(11,11,.)
    forval i = 1/11 {
        local ni : word `i' of `order'
        matrix bb[1,`i'] = b0[1, colnumb(b0,"`ni'")]
    }
    forval i = 1/11 {
        local ni : word `i' of `order'
        local ri = colnumb(V0,"`ni'")
        forval j = 1/11 {
            local nj : word `j' of `order'
            matrix VV[`i',`j'] = V0[`ri', colnumb(V0,"`nj'")]
        }
    }
    local names t_m5 t_m4 t_m3 t_m2 t_m1 t0 t1 t2 t3 t4 t5
    matrix colnames bb = `names'
    matrix colnames VV = `names'
    matrix rownames VV = `names'
    matrix lvec = J(6,1,1/6)
    di _n "=== HONESTDID [`tag'] year-varying ==="
    * no in-graph title (INV-12): the LaTeX subcaption labels the panel
    honestdid, b(bb) vcov(VV) pre(1/5) post(6/11) delta(rm) l_vec(lvec) ///
        mvec(0(0.1)2) coefplot ///
        ytitle("Average post ATT (per 100,000, year-varying)") ///
        graphregion(color(white)) plotregion(color(white))
    graph export "${figdir}/`fname'.png", as(png) replace width(1200) height(800)
    graph export "${latex_figdir}/`fname'.png", as(png) replace width(1200) height(800)
    graph export "${figdir}/`fname'.pdf", replace
    graph export "${latex_figdir}/`fname'.pdf", replace
end

use "`master'", clear
_honest_yv "appx-honestdid-headline" "headline"

use "`master'", clear
keep if treated_state == 0 | (treated_state == 1 & gme_notvol == 1)
_honest_yv "appx-honestdid-nonresp" "nonresponsive"

use "`master'", clear
keep if urban_rural == 0
_honest_yv "appx-honestdid-urban" "urban"

di _n "=== year-varying robustness complete: appx-notyet, appx-cohort2014, appx-honestdid-{headline,nonresp,urban} ==="
log close
