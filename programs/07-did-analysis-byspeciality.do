* =============================================================================
* Difference-in-Differences Analysis: Primary Care vs Non-Primary Care
* Primary care: FM, IM, Peds (general, family, general internal, geriatrics,
*               and general pediatrics medicine)
* Outcomes: quota, matched per 100k population
* Event-study plotting follows prior scheme (pre/post shading + bands)
* =============================================================================

capture cls   // 'cls' is interactive-only; guard so the script runs in batch mode
clear all
set more off

* -------------------------------------------------------------------------
* Define paths
* -------------------------------------------------------------------------
global topdir "/Users/hhadah/Projects/GiT/residency-medicaid-expansion"
global datadir "${topdir}/data/datasets"
global raw "${topdir}/data/raw"
global figdir "${topdir}/output/figures"
global tabdir "${topdir}/output/tables"
global latex_figdir "${topdir}/my_paper/figures"

* Open log file
log using "${topdir}/output/07-did-analysis-byspeciality.log", replace

cap mkdir "${figdir}"
cap mkdir "${tabdir}"
cap mkdir "${latex_figdir}"

* -------------------------------------------------------------------------
* Load cleaned data
* -------------------------------------------------------------------------
use "${datadir}/cleaned_residency_medicaid.dta", clear

* -------------------------------------------------------------------------
* Use gen_specialty_alt for specialty grouping
* -------------------------------------------------------------------------
capture confirm variable gen_specialty_alt
if _rc != 0 {
    di as error "Variable gen_specialty_alt not found. Check data."
    exit 459
}

di "Specialty distribution (gen_specialty_alt):"
tab gen_specialty_alt

* -------------------------------------------------------------------------
* Build primary care vs non-primary care grouping
* Primary care: FM (Family Medicine), IM (Internal Medicine), Peds (Pediatrics)
* These encompass general, family, general internal, geriatrics, and
* general pediatrics medicine
* -------------------------------------------------------------------------

* Check for unclassified rows (empty or missing gen_specialty_alt)
quietly count if missing(gen_specialty_alt) | gen_specialty_alt == ""
if r(N) > 0 {
    di as error "WARNING: " r(N) " rows have missing gen_specialty_alt - dropping"
    drop if missing(gen_specialty_alt) | gen_specialty_alt == ""
}

capture drop primary_care specialty_group specialty_group_name
gen byte primary_care = inlist(gen_specialty_alt, "FM", "IM", "Peds")
gen byte specialty_group = primary_care + 1   // 1 = Non-Primary Care, 2 = Primary Care
gen str30 specialty_group_name = cond(primary_care == 1, "Primary Care", "Non-Primary Care")

di "Primary care classification:"
tab gen_specialty_alt primary_care

* -------------------------------------------------------------------------
* Check specialty group distribution
* -------------------------------------------------------------------------
di "Specialty group distribution (Primary Care vs Non-Primary Care):"
tab specialty_group specialty_group_name
* -------------------------------------------------------------------------
* Recreate treatment flags
* -------------------------------------------------------------------------
capture confirm variable treated_state
if _rc != 0 {
    gen byte treated_state = (year_expanded < .)
}

capture confirm variable post_expansion
if _rc != 0 {
    gen byte post_expansion = ///
        (treated_state == 1 & year_expanded < . & year >= year_expanded)
}

capture confirm variable treated_post
if _rc != 0 {
    gen byte treated_post = treated_state * post_expansion
}

capture confirm variable year_expanded
if _rc != 0 {
    di as error "Variable year_expanded not found. Re-run 02-data-cleaning.R."
    exit 459
}

* -------------------------------------------------------------------------
* Panel identifiers
* -------------------------------------------------------------------------
egen program_numeric_id = group(state institution_code)
encode state, gen(state_id)

* -------------------------------------------------------------------------
* Outcomes and labels
* -------------------------------------------------------------------------
* matched_per_100k dropped (fixed-2010 per-capita superseded by year-varying, script 25).
global outcomes "matched quota_per_100k"
global label_matched "Total Matched Residency Positions"
global label_quota_per_100k "Residency Quota Positions per 100k Population"
global label_matched_per_100k "Matched Residency Positions per 100k Population"

global short_matched "matched"
global short_quota_per_100k "quota_per_100k"
global short_matched_per_100k "matched_per_100k"

* -------------------------------------------------------------------------
* Store DID estimates by specialty
* -------------------------------------------------------------------------
tempname did_results_spec
tempfile did_results_spec_file
postfile `did_results_spec' str30 specialty str20 outcome double coef se tstat pvalue avg_treat ///
    pretrend_p treat_p n_programs n_states using "`did_results_spec_file'", replace

local plotnum = 11

* Loop over each specialty group
levelsof specialty_group, local(spec_groups)
foreach spec of local spec_groups {
    * Get the specialty group name for this group
    levelsof specialty_group_name if specialty_group == `spec', local(spec_name) clean
    
    di ""
    di "========================================================================="
    di "SPECIALTY GROUP: `spec_name'"
    di "========================================================================="
    
    foreach outcome of global outcomes {
        capture confirm variable `outcome'
        if _rc != 0 {
            di as error "Outcome `outcome' not found. Skipping."
            continue
        }
        
        quietly count if specialty_group == `spec' & treated_state == 1 & !missing(`outcome')
        if (r(N) == 0) {
            di as error "  `outcome': no treated observations for `spec_name'."
            continue
        }
        
        quietly count if specialty_group == `spec' & treated_state == 0 & !missing(`outcome')
        if (r(N) == 0) {
            di as error "  `outcome': no control observations for `spec_name'."
            continue
        }
        
        di ""
        di "--- Outcome: `outcome' ---"
        
        capture noisily did_imputation `outcome' program_numeric_id year year_expanded ///
            [aw=total_population_10] if specialty_group == `spec', ///
            horizons(0/5) pretrend(5) fe(program_numeric_id year) ///
            cluster(state_id) minn(0)
        
        if (_rc != 0) {
            di as error "did_imputation failed. Error code `_rc'."
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
        if (_rc == 0) local pretrend_p = r(p)
        
        capture noisily test tau0 tau1 tau2 tau3 tau4 tau5
        if (_rc == 0) local treat_p = r(p)
        
        * Calculate baseline mean (pre-treatment observations for this specialty in treated states)
        quietly summarize `outcome' if specialty_group == `spec' & treated_state == 1 & year < year_expanded [aw=total_population_10]
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
        capture levelsof program_numeric_id if specialty_group == `spec' & !missing(`outcome'), local(active_programs)
        if (_rc == 0) local n_programs : word count `active_programs'
        
        capture levelsof state_id if specialty_group == `spec' & !missing(`outcome'), local(active_states)
        if (_rc == 0) local n_states : word count `active_states'

        * Compute national/total annual impact for annotation, restricted to this specialty group.
        * matched (raw): avg_treat * N treated programs in this specialty.
        * per_100k:      avg_treat * sum(state_pop across treated programs in this specialty) / 100k.
        local national_effect = .
        local has_national = 0
        if "`outcome'" == "matched" {
            quietly levelsof program_numeric_id if specialty_group == `spec' & treated_state == 1 & !missing(`outcome'), local(_tprogs)
            local n_tprogs : word count `_tprogs'
            local national_effect = `avg_treat' * `n_tprogs'
            local has_national = 1
        }
        else if strpos("`outcome'", "_per_100k") > 0 {
            preserve
            keep if specialty_group == `spec' & treated_state == 1 & !missing(`outcome')
            collapse (mean) total_population_10, by(program_numeric_id)
            quietly summarize total_population_10
            local sum_treated_prog_pop = r(sum)
            restore
            local national_effect = `avg_treat' * `sum_treated_prog_pop' / 100000
            local has_national = 1
        }
        local national_text = cond(`has_national', string(`national_effect', "%9.0fc"), "NA")

        post `did_results_spec' ("`spec_name'") ("`outcome'") ///
            (`coef') (`se') (`tstat') (`pvalue') (`avg_treat') ///
            (`pretrend_p') (`treat_p') (`n_programs') (`n_states')

        di "ATT (tau0): " %9.3f `coef' "  SE: " %9.3f `se' "  p = " %9.3f `pvalue'
        di "Average post-treatment effect: " %9.3f `avg_treat'
        if (`has_national') di "Avg. annual aggregate effect: " %15.0fc `national_effect'
        
        * =====================================================================
        * Event study plot
        * =====================================================================
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
        local plot_title "Event Study: `spec_name' - `label'"
        * Calculate annotation position dynamically
        quietly summarize ci_upper
        local y_annot = r(max) * 0.9
        local x_annot = -2
        * Adjust for per 100k outcomes
        if (strpos("`short'", "_100k") > 0) {
            local ytitle_str "Treatment Effect (per 100,000 population)"
            local x_annot = -2
        }
        if ("`outcome'" == "matched") {
            local ytitle_str "Treatment Effect (number of residency positions)"
        }
        * Build text annotations.
        * Main annotation (left): baseline mean, post avg, p-value.
        * Extra annotation at x = 3, same y-level: national/total impact (residency positions
        * for raw matched; additional doctors nationally for per-100k).
        local pretrend_text = cond(`pretrend_p' < ., string(`pretrend_p', "%9.2f"), "NA")
        local main_text `"text(`y_annot' `x_annot' `"Baseline Mean: `baseline_text'"' `"Post avg = `avg_text' (`pct_text'%)"' `"Treatment p = `treat_text'"' `"Pre-trend p = `pretrend_text'"', size(medsmall))"'
        local extra_text ""
        if ("`outcome'" == "matched") {
            local extra_text `"text(`y_annot' 3 `"Avg. annual change in"' `"residency positions:"' `"`national_text'"', size(medsmall))"'
        }
        else if (strpos("`short'", "_100k") > 0) {
            local extra_text `"text(`y_annot' 3 `"Avg. annual change in"' `"doctors nationally:"' `"`national_text'"', size(medsmall))"'
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
            ylabel(#10, labsize(small) format(%9.3f)) ///
            xtitle("Years relative to Medicaid expansion", size(small)) ///
            ytitle("`ytitle_str'", size(small)) ///
            `main_text' ///
            `extra_text' ///
            legend(off) ///
            graphregion(color(white)) plotregion(color(white))
        
        * Semantic filename: appx-{levels|quota}-specialty-{primary|nonprimary}
        local sbase = cond("`outcome'"=="matched","levels","quota")
        local sspec = cond("`spec_name'"=="Primary Care","primary","nonprimary")
        local outfname = "appx-`sbase'-specialty-`sspec'"

        graph export "${figdir}/`outfname'.png", as(png) replace width(1200) height(800)

        graph export "${figdir}/`outfname'.pdf", replace
        graph export "${latex_figdir}/`outfname'.png", as(png) replace width(1200) height(800)
        graph export "${latex_figdir}/`outfname'.pdf", replace
        restore
        local ++plotnum
    }
}

postclose `did_results_spec'

use "`did_results_spec_file'", clear
order specialty outcome coef se tstat pvalue avg_treat pretrend_p treat_p n_programs n_states
save "${tabdir}/did_summary_residency_by_specialty.dta", replace
export delimited using "${tabdir}/did_summary_residency_by_specialty.csv", replace

di ""
di "=================================================================="
di "Difference-in-differences estimates by specialty completed."
di "Summary table:"
di "  - ${tabdir}/did_summary_residency_by_specialty.dta"
di "  - ${tabdir}/did_summary_residency_by_specialty.csv"
di "Figures:"
di "  - ${figdir}/11-did_*_event.png (and LaTeX copies)"
di "=================================================================="

log close
