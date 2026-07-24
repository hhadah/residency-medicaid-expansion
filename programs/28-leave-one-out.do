* =============================================================================
* Leave-one-state-out influence diagnostics for the headline estimate.
* ---------------------------------------------------------------------------
* The population-weighted design concentrates identifying variation in a few
* large expansion states. This script drops each TREATED state in turn,
* re-estimates the headline average post effect (matched_per_100k_yr, BJS
* imputation, weighted, as script 24), and reports the distribution of
* leave-one-out estimates against the full-sample benchmark.
*
* Outputs: output/tables/leave-one-out-summary.csv
*          figures appx-loo.{png,pdf} in output/figures/ and my_paper/figures/
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

log using "${topdir}/output/28-leave-one-out.log", replace

use "${datadir}/cleaned_program_residency_medicaid.dta", clear
replace state = strtrim(upper(state))
merge m:1 state year using "${datadir}/state_year_population.dta", keep(master match) nogen
quietly count if missing(pop_yr)
di as text "pop_yr merge: " r(N) " unmatched master rows (missing pop_yr)"
assert r(N) == 0
gen double matched_per_100k_yr = matched / pop_yr * 100000
egen program_numeric_id = group(state institution_code)
encode state, gen(state_id)
xtset program_numeric_id year
tempfile master
save `master'

* ---- full-sample benchmark ----
did_imputation matched_per_100k_yr program_numeric_id year year_expanded ///
    [aw=total_population_10], horizons(0/5) pretrend(5) ///
    fe(program_numeric_id year) cluster(state_id) minn(0)
lincom (tau0+tau1+tau2+tau3+tau4+tau5)/6
local full_avg = r(estimate)
local full_se  = r(se)
di as result "full sample: avg = " %8.4f `full_avg' " (se " %8.4f `full_se' ")"

* ---- leave-one-out loop over treated states ----
tempname loo
tempfile loo_file
postfile `loo' str24 dropped_state double avg_treat avg_se using "`loo_file'", replace
post `loo' ("FULL SAMPLE") (`full_avg') (`full_se')

quietly levelsof state if treated_state == 1, local(trstates)
foreach st of local trstates {
    use "`master'", clear
    drop if state == "`st'"
    capture did_imputation matched_per_100k_yr program_numeric_id year year_expanded ///
        [aw=total_population_10], horizons(0/5) pretrend(5) ///
        fe(program_numeric_id year) cluster(state_id) minn(0) autosample
    if (_rc != 0) {
        local rc = _rc
        di as error "leave-out `st' failed (rc=`rc')"
        post `loo' ("`st'") (.) (.)
        continue
    }
    capture lincom (tau0+tau1+tau2+tau3+tau4+tau5)/6
    if (_rc == 0) {
        post `loo' ("`st'") (r(estimate)) (r(se))
        di as text "drop `st': avg = " %8.4f r(estimate)
    }
    else post `loo' ("`st'") (.) (.)
}
postclose `loo'

* ---- summary + figure ----
use "`loo_file'", clear
export delimited using "${tabdir}/leave-one-out-summary.csv", replace

quietly summarize avg_treat if dropped_state != "FULL SAMPLE"
di as result "LOO range: [" %8.4f r(min) ", " %8.4f r(max) "] across " r(N) " treated states"
gsort -avg_treat
list dropped_state avg_treat in 1/5, clean noobs
gsort avg_treat
list dropped_state avg_treat in 1/5, clean noobs

* dot plot: LOO estimates sorted, vertical line at the full-sample estimate
keep if dropped_state != "FULL SAMPLE" & !missing(avg_treat)
gsort avg_treat
gen long rank = _n
quietly count
local n = r(N)
forvalues i = 1/`n' {
    local s = dropped_state[`i']
    label define rlab `i' "`s'", add
}
label values rank rlab
twoway (scatter rank avg_treat, mcolor(navy) msymbol(circle) msize(small)) ///
    , xline(`full_avg', lcolor(maroon) lpattern(dash)) ///
    xline(0, lcolor(black) lwidth(thin)) ///
    ylabel(1(1)`n', valuelabel labsize(tiny) angle(0) nogrid) ///
    xlabel(, labsize(small) format(%9.3f)) ///
    ytitle("Dropped state", size(small)) ///
    xtitle("Leave-one-out average post effect (per 100,000, year-varying pop.)", size(small)) ///
    legend(off) graphregion(color(white)) plotregion(color(white)) ysize(7) xsize(5)
graph export "${figdir}/appx-loo.png", as(png) replace width(1000) height(1400)
graph export "${latex_figdir}/appx-loo.png", as(png) replace width(1000) height(1400)
graph export "${figdir}/appx-loo.pdf", replace
graph export "${latex_figdir}/appx-loo.pdf", replace

di _n "=== leave-one-out complete: appx-loo, leave-one-out-summary.csv ==="
log close
