* =============================================================================
* Difference-in-Differences Analysis: Unweighted with Population as a Control
* Mirror of 05-dd-analysis.do but:
*   - No analytic weights (no [aw=total_population_10])
*   - total_population_10 added as a control via controls() in did_imputation
* Outcomes: matched (raw), matched_per_100k, quota_per_100k
* =============================================================================

clear all
set more off

* -------------------------------------------------------------------------
* Define paths
* -------------------------------------------------------------------------
global topdir "/Users/hhadah/Projects/GiT/residency-medicaid-expansion"
global datadir "${topdir}/data/datasets"
global figdir "${topdir}/output/figures"
global tabdir "${topdir}/output/tables"
global latex_figdir "${topdir}/my_paper/figures"

cap mkdir "${figdir}"
cap mkdir "${tabdir}"
cap mkdir "${latex_figdir}"

log using "${topdir}/output/12-dd-analysis-popcnt.log", replace

* -------------------------------------------------------------------------
* Load cleaned data (produced by 02-data-cleaning.R)
* -------------------------------------------------------------------------
use "${datadir}/cleaned_program_residency_medicaid.dta", clear

* -------------------------------------------------------------------------
* Panel identifiers
* -------------------------------------------------------------------------
egen program_numeric_id = group(state institution_code)
encode state, gen(state_id)

xtset program_numeric_id year

* -------------------------------------------------------------------------
* Outcomes and labels
* -------------------------------------------------------------------------
global outcomes "matched matched_per_100k quota_per_100k"
global label_matched "Total Matched Residency Positions"
global label_quota_100k "Residency Quota Positions per 100k Population"
global label_matched_100k "Matched Residency Positions per 100k Population"
global label_quota_per_100k "Residency Quota Positions per 100k Population"
global label_matched_per_100k "Matched Residency Positions per 100k Population"

global short_matched "matched"
global short_quota_100k "quota_100k"
global short_matched_100k "matched_100k"
global short_quota_per_100k "quota_per_100k"
global short_matched_per_100k "matched_per_100k"

* -------------------------------------------------------------------------
* Pre-compute aggregate annual change in residency positions from the raw
* count regressions (matched, quota). These are reused for the per-100k
* annotations so the "Avg. annual change" number is consistent across
* specifications.
* NOTE: total_population_10 is a 2010 census state-level value that is
* time-invariant within program. With program FE it is collinear and gets
* absorbed by did_imputation; it is included to satisfy the spec request.
* -------------------------------------------------------------------------
global agg_matched ""
global agg_quota ""
foreach raw_outcome in matched quota {
    capture confirm variable `raw_outcome'
    if (_rc != 0) continue

    quietly count if treated_state == 1 & !missing(`raw_outcome')
    if (r(N) == 0) continue
    quietly count if treated_state == 0 & !missing(`raw_outcome')
    if (r(N) == 0) continue

    di "Pre-computing aggregate effect for `raw_outcome' (unweighted, pop control)..."
    capture noisily did_imputation `raw_outcome' program_numeric_id year year_expanded, ///
        horizons(0/5) pretrend(5) fe(program_numeric_id year) ///
        controls(total_population_10) cluster(state_id) minn(0)
    if (_rc != 0) continue

    local _sum = 0
    local _n = 0
    forval h = 0/5 {
        capture local _coef = _b[tau`h']
        if (_rc == 0) {
            local _sum = `_sum' + `_coef'
            local _n = `_n' + 1
        }
    }
    if (`_n' > 0) {
        local _avg = `_sum' / `_n'
        quietly levelsof program_numeric_id if treated_state == 1 & !missing(`raw_outcome'), local(_tprogs)
        local _ntreated : word count `_tprogs'
        global agg_`raw_outcome' = `_avg' * `_ntreated'
        di as result "  Aggregate annual change in `raw_outcome' positions: " %15.0fc ${agg_`raw_outcome'}
    }
}

* -------------------------------------------------------------------------
* Store DID estimates
* -------------------------------------------------------------------------
tempname did_results
tempfile did_results_file
postfile `did_results' str20 outcome double coef se tstat pvalue avg_treat ///
    pretrend_p treat_p n_programs n_states using "`did_results_file'", replace


local plotnum = 22
foreach outcome of global outcomes {
    capture confirm variable `outcome'
    if _rc != 0 {
        di as error "Outcome `outcome' not found. Skipping."
        continue
    }
    quietly count if treated_state == 1 & !missing(`outcome')
    if (r(N) == 0) {
        di as error "Outcome `outcome': no treated observations with data."
        continue
    }
    quietly count if treated_state == 0 & !missing(`outcome')
    if (r(N) == 0) {
        di as error "Outcome `outcome': no control observations with data."
        continue
    }
    di ""
    di "========================================================================="
    di "DID ANALYSIS (unweighted, pop control): ${label_`outcome'}"
    di "Outcome variable: `outcome'"
    di "========================================================================="
    di ""
    capture noisily did_imputation `outcome' program_numeric_id year year_expanded, ///
        horizons(0/5) pretrend(5) fe(program_numeric_id year) ///
        controls(total_population_10) cluster(state_id) minn(0)
    if (_rc != 0) {
        di as error "did_imputation failed for outcome `outcome'. Error code `_rc'."
        continue
    }
    local coef = _b[tau0]
    local se   = _se[tau0]
    local tstat = `coef' / `se'
    local pvalue = 2*ttail(e(df_r), abs(`tstat'))
    local tau_sum = 0
    local tau_n = 0
    forval h = 0/5 {
        capture scalar __tmp = _b[tau`h']
        if (_rc == 0) {
            local tau_sum = `tau_sum' + __tmp
            local tau_n = `tau_n' + 1
        }
    }
    if (`tau_n' > 0) local avg_treat = `tau_sum' / `tau_n'
    else local avg_treat = .
    local pretrend_p = .
    local treat_p = .
    capture noisily test pre1 pre2 pre3 pre4 pre5
    if (_rc == 0) {
        local pretrend_p = r(p)
    }
    capture noisily test tau0 tau1 tau2 tau3 tau4 tau5
    if (_rc == 0) {
        local treat_p = r(p)
    }
    * Calculate baseline mean (pre-treatment observations in treated states, unweighted)
    quietly summarize `outcome' if treated_state == 1 & year < year_expanded
    local baseline_mean = r(mean)
    if missing(`baseline_mean') | `baseline_mean' == 0 {
        local baseline_mean = 1
    }
    local pct_effect = (`avg_treat' / `baseline_mean') * 100
    if `pct_effect' < -100 {
        local pct_effect = -100
    }
    local n_programs = .
    local n_states = .
    capture levelsof program_numeric_id if !missing(`outcome'), local(active_programs)
    if (_rc == 0) {
        local n_programs : word count `active_programs'
    }
    capture levelsof state_id if !missing(`outcome'), local(active_states)
    if (_rc == 0) {
        local n_states : word count `active_states'
    }

    * National impact for annotation. Use the raw count aggregate computed
    * pre-loop so per-100k and raw-count figures show the same number.
    local national_effect = .
    local has_national = 0
    local national_label "residency positions"
    if "`outcome'" == "matched" | "`outcome'" == "matched_per_100k" {
        if "${agg_matched}" != "" {
            local national_effect = ${agg_matched}
            local has_national = 1
        }
    }
    else if "`outcome'" == "quota_per_100k" {
        if "${agg_quota}" != "" {
            local national_effect = ${agg_quota}
            local has_national = 1
            local national_label "quota positions"
        }
    }
    local national_text = cond(`has_national', string(`national_effect', "%9.0fc"), "NA")

    post `did_results' ("`outcome'") ///
        (`coef') (`se') (`tstat') (`pvalue') (`avg_treat') ///
        (`pretrend_p') (`treat_p') (`n_programs') (`n_states')
    di "ATT (tau0): " %9.3f `coef' "  SE: " %9.3f `se' "  p = " %9.3f `pvalue'
    di "Average post-treatment effect: " %9.3f `avg_treat'
    if (`has_national') di "Avg. annual aggregate effect: " %15.0fc `national_effect'
    if (`pretrend_p' < .) di "Pretrend joint p-value: " %9.3f `pretrend_p'
    if (`treat_p' < .)   di "Treatment joint p-value: " %9.3f `treat_p'
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
    gen byte pre_period = (period < 0)
    gen byte post_period = (period >= 0)
    local avg_text = cond(`avg_treat' < ., string(`avg_treat', "%9.2f"), "NA")
    local treat_text = cond(`treat_p' < ., string(`treat_p', "%9.2f"), "NA")
    local baseline_text = string(`baseline_mean', "%9.2f")
    local pct_text = string(`pct_effect', "%9.1f")
    local post_line ""
    if (`avg_treat' < .) {
        local post_line "(scatteri `avg_treat' 0 `avg_treat' 5, recast(line) lpattern(dash) lcolor(red) lwidth(medium))"
    }
    local short = "${short_`outcome'}"
    local label = "${label_`outcome'}"
    if ("`label'" == "") {
        local label = "`outcome'"
    }
    local prefix : display %02.0f `plotnum'
    local ytitle_str "Treatment Effect (difference-in-differences)"
    local plot_title "Event Study: `label'"
    quietly summarize ci_upper
    local y_annot = r(max) * 0.9
    local x_annot = -3
    if (strpos("`short'", "_100k") > 0) {
        local ytitle_str "Treatment Effect (per 100,000 population)"
        local x_annot = -3
    }
    if ("`outcome'" == "matched") {
        local ytitle_str "Treatment Effect (number of residency positions)"
    }
    local main_text `"text(`y_annot' `x_annot' `"Baseline Mean: `baseline_text'"' `"Post avg = `avg_text' (`pct_text'%)"' `"p-value = `treat_text'"', size(large))"'
    local extra_text ""
    if (`has_national') {
        local extra_text `"text(`y_annot' 3 `"Avg. annual change in"' `"`national_label':"' `"`national_text'"', size(large))"'
    }
    twoway ///
        (rarea ci_upper ci_lower period if pre_period, fcolor(dkgreen%45) lcolor(dkgreen%45) lwidth(none)) ///
        (rarea ci_upper ci_lower period if post_period, fcolor(maroon%45) lcolor(maroon%45) lwidth(none)) ///
        (line coef period if pre_period, lcolor(dkgreen) lwidth(medium)) ///
        (line coef period if post_period, lcolor(maroon) lwidth(medium)) ///
        (scatter coef period if pre_period, mcolor(dkgreen) msymbol(circle) msize(medlarge)) ///
        (scatter coef period if post_period, mcolor(maroon) msymbol(circle) msize(medlarge)) ///
        `post_line' ///
        , ///
        xline(-0.5, lcolor(black) lpattern(solid) lwidth(thin)) ///
        yline(0, lcolor(black) lpattern(solid) lwidth(thin)) ///
        xlabel(-5(1)5, labsize(small)) ///
        ylabel(#8, labsize(small) format(%9.2f)) ///
        xtitle("Years relative to Medicaid expansion", size(small)) ///
        ytitle("`ytitle_str'", size(small)) ///
        `main_text' ///
        `extra_text' ///
        legend(off) ///
        graphregion(color(white)) plotregion(color(white))
    graph export "${figdir}/`prefix'-did_`short'_popcnt_event.png", as(png) replace width(1200) height(800)
    graph export "${latex_figdir}/`prefix'-did_`short'_popcnt_event.png", as(png) replace width(1200) height(800)
    restore
    local ++plotnum
}

postclose `did_results'

use "`did_results_file'", clear
order outcome coef se tstat pvalue avg_treat pretrend_p treat_p n_programs n_states
save "${tabdir}/did_summary_residency_popcnt.dta", replace
export delimited using "${tabdir}/did_summary_residency_popcnt.csv", replace

di ""
di "=================================================================="
di "Difference-in-differences (unweighted, pop control) completed."
di "Summary table:"
di "  - ${tabdir}/did_summary_residency_popcnt.dta"
di "  - ${tabdir}/did_summary_residency_popcnt.csv"
di "Figures:"
di "  - ${figdir}/{22..}-did_*_popcnt_event.png (and LaTeX copies)"
di "=================================================================="

log close
