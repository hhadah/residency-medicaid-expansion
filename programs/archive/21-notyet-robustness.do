* =============================================================================
* Robustness: not-yet-treated controls only (drop never-expansion states)
* ---------------------------------------------------------------------------
* The baseline BJS estimates use ALL untreated observations as controls, which
* includes never-expansion states. Those states differ systematically (region,
* physician labor markets, DSH exposure, population growth). This robustness
* check drops never-expansion states entirely, so identification rests solely
* on variation in the TIMING of expansion among eventually-treated states
* (the not-yet-treated design). If the contraction survives, it is not an
* artifact of the never-expansion comparison group.
*
* Design mirrors 05 (BJS 2024, fe program+year, cluster state, aw pop).
* Outcomes: matched_per_100k (headline, figure 35), plus matched (levels) and
* quota_per_100k recorded in the summary table.
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

log using "${topdir}/output/21-notyet-robustness.log", replace

use "${datadir}/cleaned_program_residency_medicaid.dta", clear
egen program_numeric_id = group(state institution_code)
encode state, gen(state_id)
xtset program_numeric_id year

* Keep only eventually-treated (expansion) states: controls become the
* not-yet-treated pre-periods of later adopters.
keep if treated_state == 1
di as text "Eventually-treated states retained:"
quietly levelsof state_id, local(ss)
di as text "  N states = " `: word count `ss''

tempname r
tempfile rfile
postfile `r' str18 outcome double avg_treat treat_p pretrend_p baseline pct ///
    n_programs n_states using "`rfile'", replace

local plotnum = 35
foreach outcome in matched_per_100k matched quota_per_100k {

    quietly count if !missing(`outcome')
    if (r(N) == 0) continue

    di ""
    di "========================================================================="
    di "NOT-YET-TREATED design: `outcome'"
    di "========================================================================="

    * autosample: with never-expansion states dropped, the FE for programs
    * observed only post-treatment cannot be imputed; drop them automatically.
    capture noisily did_imputation `outcome' program_numeric_id year year_expanded ///
        [aw=total_population_10], horizons(0/5) pretrend(5) ///
        fe(program_numeric_id year) cluster(state_id) minn(0) autosample
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

    quietly summarize `outcome' if year < year_expanded [aw=total_population_10]
    local baseline = r(mean)
    local pct = cond(!missing(`baseline') & `baseline' != 0, 100*`avg_treat'/`baseline', .)

    quietly levelsof program_numeric_id, local(pp)
    local np : word count `pp'
    quietly levelsof state_id, local(ss2)
    local ns : word count `ss2'

    post `r' ("`outcome'") (`avg_treat') (`treat_p') (`pretrend_p') (`baseline') (`pct') (`np') (`ns')
    di as result "`outcome': avg post = " %8.4f `avg_treat' "  joint p = " %6.3f `treat_p' ///
        "  (pretrend p = " %5.3f `pretrend_p' ")"

    * Figure for the headline outcome only (matched_per_100k)
    if "`outcome'" == "matched_per_100k" {
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
            ytitle("Treatment Effect (per 100,000 population)", size(small)) ///
            legend(off) graphregion(color(white)) plotregion(color(white))

        graph export "${figdir}/`prefix'-did_matched_per_100k_notyet_event.png", as(png) replace width(1200) height(800)
        graph export "${latex_figdir}/`prefix'-did_matched_per_100k_notyet_event.png", as(png) replace width(1200) height(800)
        restore
    }
}

postclose `r'
use "`rfile'", clear
order outcome avg_treat pct treat_p pretrend_p baseline n_programs n_states
list, clean noobs
export delimited using "${tabdir}/notyet-robustness-summary.csv", replace

di ""
di "=================================================================="
di "Not-yet-treated robustness complete. Figure 35; summary CSV."
di "=================================================================="

log close
