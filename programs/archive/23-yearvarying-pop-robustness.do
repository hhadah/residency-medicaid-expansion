* =============================================================================
* Robustness: re-scale the outcome by YEAR-VARYING population (referee C4)
* ---------------------------------------------------------------------------
* The baseline "matched per 100,000" uses the 2010 decennial state population,
* a time-invariant denominator absorbed by the hospital fixed effects. This
* re-scales matched positions by CONTEMPORARY (year-varying) ACS state
* population, so the denominator is no longer collinear with the FE and
* differential state population growth is netted out of the outcome. If the
* contraction survives, the headline is not an artifact of the fixed 2010 base.
*
* Population panel: data/datasets/state_year_population.dta (built by
* 04b-state-year-population.R, ACS 1-year 2010-2019). Design mirrors 05
* (BJS 2024, fe program+year, cluster state); weights kept at the 2010 base so
* only the OUTCOME denominator changes. Figure 39; summary CSV.
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

log using "${topdir}/output/23-yearvarying-pop-robustness.log", replace

use "${datadir}/cleaned_program_residency_medicaid.dta", clear
egen program_numeric_id = group(state institution_code)

* Merge the year-varying ACS state population (upper/trim state to match)
replace state = strtrim(upper(state))
merge m:1 state year using "${datadir}/state_year_population.dta", keep(master match)
di as text "Merge of year-varying population:"
tab _merge
count if _merge == 3
di as text "  matched program-years: " r(N)
drop _merge

encode state, gen(state_id)
xtset program_numeric_id year

* Year-varying per-capita outcomes
gen double matched_per_100k_yr = matched / pop_yr * 100000
gen double quota_per_100k_yr   = quota   / pop_yr * 100000
label var matched_per_100k_yr "Matched positions per 100,000 (year-varying pop)"

tempname r
tempfile rfile
postfile `r' str24 outcome double avg_treat treat_p pretrend_p baseline pct ///
    n_programs n_states using "`rfile'", replace

local plotnum = 39
foreach outcome in matched_per_100k_yr quota_per_100k_yr {

    quietly count if !missing(`outcome')
    if (r(N) == 0) continue

    di ""
    di "========================================================================="
    di "YEAR-VARYING POP: `outcome'"
    di "========================================================================="

    capture noisily did_imputation `outcome' program_numeric_id year year_expanded ///
        [aw=total_population_10], horizons(0/5) pretrend(5) ///
        fe(program_numeric_id year) cluster(state_id) minn(0)
    if (_rc != 0) {
        di as error "did_imputation failed for `outcome' (rc=`_rc'). Skipping."
        continue
    }

    local tau_sum = 0
    local tau_n = 0
    forval h = 0/5 {
        capture scalar __b = _b[tau`h']
        if (_rc == 0) {
            local tau_sum = `tau_sum' + __b
            local tau_n = `tau_n' + 1
        }
    }
    local avg_treat = cond(`tau_n' > 0, `tau_sum'/`tau_n', .)
    local pretrend_p = .
    local treat_p = .
    capture test pre1 pre2 pre3 pre4 pre5
    if (_rc == 0) local pretrend_p = r(p)
    capture test tau0 tau1 tau2 tau3 tau4 tau5
    if (_rc == 0) local treat_p = r(p)

    quietly summarize `outcome' if treated_state == 1 & year < year_expanded [aw=total_population_10]
    local baseline = r(mean)
    local pct = cond(!missing(`baseline') & `baseline' != 0, 100*`avg_treat'/`baseline', .)

    quietly levelsof program_numeric_id, local(pp)
    local np : word count `pp'
    quietly levelsof state_id, local(ss)
    local ns : word count `ss'

    post `r' ("`outcome'") (`avg_treat') (`treat_p') (`pretrend_p') (`baseline') (`pct') (`np') (`ns')
    di as result "`outcome': avg post = " %9.5f `avg_treat' "  joint p = " %6.3f `treat_p' ///
        "  (pretrend p = " %5.3f `pretrend_p' ", pct = " %5.1f `pct' ")"

    if "`outcome'" == "matched_per_100k_yr" {
        matrix plot_coef = J(11, 3, .)
        matrix colnames plot_coef = period coef se
        local row = 1
        forval h = 5(-1)1 {
            matrix plot_coef[`row',1] = -`h'
            capture matrix plot_coef[`row',2] = _b[pre`h']
            capture matrix plot_coef[`row',3] = _se[pre`h']
            local ++row
        }
        forval h = 0/5 {
            matrix plot_coef[`row',1] = `h'
            capture matrix plot_coef[`row',2] = _b[tau`h']
            capture matrix plot_coef[`row',3] = _se[tau`h']
            local ++row
        }
        preserve
        clear
        svmat plot_coef, names(col)
        keep if !missing(period)
        gen ci_upper = coef + 1.96*se
        gen ci_lower = coef - 1.96*se
        gen byte pre_period  = (period < 0)
        gen byte post_period = (period >= 0)
        local prefix : display %02.0f `plotnum'
        local post_line ""
        if (`avg_treat' < .) local post_line "(scatteri `avg_treat' 0 `avg_treat' 5, recast(line) lpattern(dash) lcolor(red) lwidth(medium))"

        twoway ///
            (rarea ci_upper ci_lower period if pre_period,  fcolor(dkgreen%45) lcolor(dkgreen%45) lwidth(none)) ///
            (rarea ci_upper ci_lower period if post_period, fcolor(maroon%45)  lcolor(maroon%45)  lwidth(none)) ///
            (line coef period if pre_period,  lcolor(dkgreen) lwidth(medium)) ///
            (line coef period if post_period, lcolor(maroon)  lwidth(medium)) ///
            (scatter coef period if pre_period,  mcolor(dkgreen) msymbol(circle) msize(medlarge)) ///
            (scatter coef period if post_period, mcolor(maroon)  msymbol(circle) msize(medlarge)) ///
            `post_line' ///
            , xline(-0.5, lcolor(black) lwidth(thin)) yline(0, lcolor(black) lwidth(thin)) ///
            xlabel(-5(1)5, labsize(small)) ylabel(#8, labsize(small) format(%9.2f)) ///
            xtitle("Years relative to Medicaid expansion", size(small)) ///
            ytitle("Treatment Effect (per 100,000, year-varying pop.)", size(small)) ///
            legend(off) graphregion(color(white)) plotregion(color(white))

        graph export "${figdir}/`prefix'-did_matched_per_100k_yearvarying_event.png", as(png) replace width(1200) height(800)
        graph export "${latex_figdir}/`prefix'-did_matched_per_100k_yearvarying_event.png", as(png) replace width(1200) height(800)
        restore
    }
}

postclose `r'
use "`rfile'", clear
order outcome avg_treat pct treat_p pretrend_p baseline n_programs n_states
list, clean noobs
export delimited using "${tabdir}/yearvarying-pop-summary.csv", replace

di ""
di "=================================================================="
di "Year-varying population robustness complete. Figure 39; summary CSV."
di "=================================================================="

log close
