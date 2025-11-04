* =============================================================================
* Difference-in-Differences Analysis (no triple DDD)
* Outcomes: quota, matched, unmatched (quota - matched)
* Event-study plotting follows prior scheme (pre/post shading + bands)
* =============================================================================

clear all
set more off

* -------------------------------------------------------------------------
* Define paths
* -------------------------------------------------------------------------
global topdir "/Users/hhadah/Documents/GiT/residency-medicaid-expansion"
global datadir "${topdir}/data/datasets"
global figdir "${topdir}/output/figures"
global tabdir "${topdir}/output/tables"
global latex_figdir "${topdir}/my_paper/figures"

cap mkdir "${figdir}"
cap mkdir "${tabdir}"
cap mkdir "${latex_figdir}"

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
global outcomes "quota matched unmatched matched_per_100k quota_per_100k unmatched_per_100k"
global label_quota     "Residency Quota Positions"
global label_matched   "Matched Residency Positions"
global label_unmatched "Unmatched Residency Positions"
global label_quota_100k "Residency Quota Positions per 100k Population"
global label_matched_100k "Matched Residency Positions per 100k Population"
global label_unmatched_100k "Unmatched Residency Positions per 100k Population"
global label_quota_per_100k "Residency Quota Positions per 100k Population"
global label_matched_per_100k "Matched Residency Positions per 100k Population"
global label_unmatched_per_100k "Unmatched Residency Positions per 100k Population"

global short_quota     "quota"
global short_matched   "matched"
global short_unmatched "unmatched"
global short_quota_100k "quota_100k"
global short_matched_100k "matched_100k"
global short_unmatched_100k "unmatched_100k"
global short_quota_per_100k "quota_per_100k"
global short_matched_per_100k "matched_per_100k"
global short_unmatched_per_100k "unmatched_per_100k"

* -------------------------------------------------------------------------
* Store DID estimates
* -------------------------------------------------------------------------
tempname did_results
tempfile did_results_file
postfile `did_results' str12 outcome double coef se tstat pvalue avg_treat ///
    pretrend_p treat_p n_programs n_states using "`did_results_file'", replace


local plotnum = 4
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
    di "DID ANALYSIS: ${label_`outcome'}"
    di "Outcome variable: `outcome'"
    di "========================================================================="
    di ""
    capture noisily did_imputation `outcome' program_numeric_id year year_expanded  [aw=total_population_10], ///
        horizons(0/5) pretrend(5) fe(program_numeric_id year) ///
        cluster(state_id) minn(0)
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
    post `did_results' ("`outcome'") ///
        (`coef') (`se') (`tstat') (`pvalue') (`avg_treat') ///
        (`pretrend_p') (`treat_p') (`n_programs') (`n_states')
    di "ATT (tau0): " %9.3f `coef' "  SE: " %9.3f `se' "  p = " %9.3f `pvalue'
    di "Average post-treatment effect: " %9.3f `avg_treat'
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
    local annot_x 3
    quietly summarize ci_upper
    local y_max = r(max)
    if (missing(`y_max')) local y_max = 0
    quietly summarize ci_lower
    local y_min = r(min)
    if (missing(`y_min')) local y_min = 0
    local y_span = `y_max' - `y_min'
    if (`y_span' <= 0) local y_span = max(1, abs(`y_max'))
    local annot_y = `y_min' + 0.85*`y_span'
    * Adjust for per 100k outcomes
    if (strpos("`short'", "_100k") > 0) {
        local ytitle_str "Treatment Effect (per 100,000 population)"
        local annot_x 4
        local annot_y = `y_min' + 0.75*`y_span'
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
        title("`plot_title'", size(medium)) ///
        text(`annot_y' `annot_x' "Post avg = `avg_text'" "p-value = `treat_text'", size(small)) ///
        legend(off) ///
        graphregion(color(white)) plotregion(color(white))
    graph export "${figdir}/`prefix'-did_`short'_event.png", as(png) replace width(1200) height(800)
    graph export "${latex_figdir}/`prefix'-did_`short'_event.png", as(png) replace width(1200) height(800)
    restore
    local ++plotnum
}

postclose `did_results'

use "`did_results_file'", clear
order outcome coef se tstat pvalue avg_treat pretrend_p treat_p n_programs n_states
save "${tabdir}/did_summary_residency.dta", replace
export delimited using "${tabdir}/did_summary_residency.csv", replace

di ""
di "=================================================================="
di "Difference-in-differences estimates completed for quota/matched/unmatched."
di "Summary table:"
di "  - ${tabdir}/did_summary_residency.dta"
di "  - ${tabdir}/did_summary_residency.csv"
di "Figures:"
di "  - ${figdir}/did_*_event.png (and LaTeX copies)"
di "=================================================================="
