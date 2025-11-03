* =============================================================================
* Difference-in-Differences Analysis by Specialty Group
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
* Load cleaned data
* -------------------------------------------------------------------------
use "${datadir}/cleaned_residency_medicaid.dta", clear

drop if missing(year)

* -------------------------------------------------------------------------
* Create specialty grouping
* -------------------------------------------------------------------------
gen byte specialty_group = .
gen str30 specialty_group_name = ""

* Classify specialties in order - use "else if" logic by checking if still missing
* Primary Care
replace specialty_group = 1 if missing(specialty_group) & regexm(program_name_standardized, "^Family Medicine$|^Internal Medicine$|^Pediatrics$")
replace specialty_group_name = "Primary Care" if specialty_group == 1

* Surgery
replace specialty_group = 2 if missing(specialty_group) & regexm(program_name_standardized, ///
    "Surgery-General|Orthopae|Thoracic Surgery|Vascular Surgery|Urology|Plastic Surgery \(Integrated\)|Neurological Surgery")
replace specialty_group_name = "Surgery" if specialty_group == 2

* Emergency/Critical Care
replace specialty_group = 3 if missing(specialty_group) & regexm(program_name_standardized, "^Emergency Medicine$|^Anesthesiology$")
replace specialty_group_name = "Emergency/Critical Care" if specialty_group == 3

* Radiology/Pathology
replace specialty_group = 4 if missing(specialty_group) & regexm(program_name_standardized, ///
    "Radiology|Diagnostic Radiology|Interventional Radiology|Pathology|Nuclear Medicine")
replace specialty_group_name = "Radiology/Pathology" if specialty_group == 4

* Psychiatry/Neurology
replace specialty_group = 5 if missing(specialty_group) & regexm(program_name_standardized, "^Psychiatry$|^Neurology$|Child Neurology")
replace specialty_group_name = "Psychiatry/Neurology" if specialty_group == 5

* OB/GYN
replace specialty_group = 6 if missing(specialty_group) & regexm(program_name_standardized, "Obstetrics and Gynecology")
replace specialty_group_name = "Obstetrics and Gynecology" if specialty_group == 6

* Other Specialists
replace specialty_group = 7 if missing(specialty_group) & regexm(program_name_standardized, "Dermatology|Otolaryngology|Medical Genetics|Preventive Medicine")
replace specialty_group_name = "Other Specialists" if specialty_group == 7

* Transitional Year
replace specialty_group = 8 if missing(specialty_group) & program_name_standardized == "Transitional Year"
replace specialty_group_name = "Transitional Year" if specialty_group == 8

* Drop unclassified or rare combinations
drop if missing(specialty_group)

* -------------------------------------------------------------------------
* Check specialty group distribution
* -------------------------------------------------------------------------
di "Specialty group distribution:"
tab specialty_group specialty_group_name
tab specialty_group_name
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
global outcomes "quota matched unmatched matched_per_100k quota_per_100k unmatched_per_100k"
global label_quota     "Residency Quota Positions"
global label_matched   "Matched Residency Positions"
global label_unmatched "Unmatched Residency Positions"
global label_quota_per_100k "Residency Quota Positions per 100k Population"
global label_matched_per_100k "Matched Residency Positions per 100k Population"
global label_unmatched_per_100k "Unmatched Residency Positions per 100k Population"

global short_quota     "quota"
global short_matched   "matched"
global short_unmatched "unmatched"
global short_quota_per_100k "quota_per_100k"
global short_matched_per_100k "matched_per_100k"
global short_unmatched_per_100k "unmatched_per_100k"

* -------------------------------------------------------------------------
* Store DID estimates by specialty
* -------------------------------------------------------------------------
tempname did_results_spec
tempfile did_results_spec_file
postfile `did_results_spec' str30 specialty str12 outcome double coef se tstat pvalue avg_treat ///
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
        
        local n_programs = .
        local n_states = .
        capture levelsof program_numeric_id if specialty_group == `spec' & !missing(`outcome'), local(active_programs)
        if (_rc == 0) local n_programs : word count `active_programs'
        
        capture levelsof state_id if specialty_group == `spec' & !missing(`outcome'), local(active_states)
        if (_rc == 0) local n_states : word count `active_states'
        
        post `did_results_spec' ("`spec_name'") ("`outcome'") ///
            (`coef') (`se') (`tstat') (`pvalue') (`avg_treat') ///
            (`pretrend_p') (`treat_p') (`n_programs') (`n_states')
        
        di "ATT (tau0): " %9.3f `coef' "  SE: " %9.3f `se' "  p = " %9.3f `pvalue'
        di "Average post-treatment effect: " %9.3f `avg_treat'
        
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
        
        * Clean specialty name for filename
        local spec_clean = subinstr("`spec_name'", " ", "_", .)
        local spec_clean = subinstr("`spec_clean'", "/", "_", .)
        
        graph export "${figdir}/`prefix'-did_`short'_`spec_clean'_event.png", as(png) replace width(1200) height(800)
        graph export "${latex_figdir}/`prefix'-did_`short'_`spec_clean'_event.png", as(png) replace width(1200) height(800)
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
