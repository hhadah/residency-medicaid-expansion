/*
Heterogeneity Analysis by Urban/Rural Classification
Date: January 27th, 2026
Purpose: Investigate heterogeneity in treatment effects by county-level urban/rural classification using rural_urban_2010 variable
Urban areas: RUCA codes 1, 2, 3
Rural areas: RUCA codes > 3
*/

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

* Open log file
log using "${topdir}/output/06-dd-analysis-hetero.log", replace

cap mkdir "${figdir}"
cap mkdir "${tabdir}"
cap mkdir "${latex_figdir}"

* -------------------------------------------------------------------------
* Load cleaned data (produced by 02-data-cleaning.R)
* -------------------------------------------------------------------------
use "${datadir}/cleaned_program_residency_medicaid.dta", clear

*===============================================================================
* Data Setup
*===============================================================================

* Panel identifiers
egen program_numeric_id = group(state institution_code)
encode state, gen(state_id)

xtset program_numeric_id year

* Create urban/rural group based on rural_urban_2010
* 0 = Urban (RUCA codes 1, 2, 3), 1 = Rural (RUCA codes > 3)
gen urban_rural = (rural_urban_2010 > 3) if !missing(rural_urban_2010)
label define urban_rural_lab 0 "Urban" 1 "Rural"
label values urban_rural urban_rural_lab

* Keep only observations with non-missing group
keep if !missing(urban_rural)

* Display basic information
describe rural_urban_2010 urban_rural
tab urban_rural, missing
di "Urban/Rural coding: 0 = Urban, 1 = Rural"

* -------------------------------------------------------------------------
* Outcomes and labels
* -------------------------------------------------------------------------
* matched_per_100k dropped (fixed-2010 per-capita superseded by year-varying, script 24).
global outcomes "matched quota_per_100k"
global label_matched "Total Matched Residency Positions"
global label_quota_per_100k "Residency Quota Positions per 100k Population"

global short_matched "matched"
global short_quota_per_100k "quota_per_100k"

* Semantic output basenames.
global fname_matched        "appx-levels-location"
global fname_quota_per_100k "appx-quota-location"

*===============================================================================
* Heterogeneity Analysis - Urban vs Rural
*===============================================================================

di ""
di "========================================================================="
di "HETEROGENEITY ANALYSIS: By Urban/Rural Classification"
di "Urban vs Rural"
di "========================================================================="
di ""

local plotnum = 8

foreach outcome of global outcomes {
    
    capture confirm variable `outcome'
    if _rc != 0 {
        di as error "Outcome `outcome' not found. Skipping."
        continue
    }
    
    quietly count if urban_rural == 0 & treated_state == 1 & !missing(`outcome')
    if (r(N) == 0) {
        di as error "Outcome `outcome': no treated urban observations."
        continue
    }
    
    quietly count if urban_rural == 1 & treated_state == 1 & !missing(`outcome')
    if (r(N) == 0) {
        di as error "Outcome `outcome': no treated rural observations."
        continue
    }
    
    di ""
    di "========================================================================="
    di "HETEROGENEITY ANALYSIS: ${label_`outcome'}"
    di "Outcome variable: `outcome'"
    di "========================================================================="
    di ""
    
    * SPLIT-SAMPLE design: separate did_imputation regressions for urban and
    * rural programs. Each subsample keeps the never-expansion programs of its
    * own type as controls, so each group gets its own imputed counterfactual
    * and its OWN pre-trend test. (Replaces the pooled hetby() interaction,
    * which shared pre-trends across groups. The formal urban-rural difference
    * test lives in the primary year-varying spec, script 24.)
    matrix urban = J(11, 5, .)  // 5 pretrends + 6 treatment periods
    matrix rural = J(11, 5, .)
    matrix colnames urban = period coef se ci_upper ci_lower
    matrix colnames rural = period coef se ci_upper ci_lower

    local est_failed = 0
    local has_national = 0
    foreach g in 0 1 {
        local gname = cond(`g'==0, "urban", "rural")
        preserve
        keep if urban_rural == `g'
        quietly count if treated_state == 1 & !missing(`outcome')
        local n_tr = r(N)
        quietly count if treated_state == 0 & !missing(`outcome')
        local n_co = r(N)
        di as text "`gname' subsample (`outcome'): treated obs = `n_tr', control obs = `n_co'"

        capture noisily did_imputation `outcome' program_numeric_id year year_expanded [aw=total_population_10], ///
            horizons(0/5) pretrend(5) ///
            cluster(state_id) ///
            fe(program_numeric_id year) ///
            minn(0) autosample

        if (_rc != 0) {
            di as error "did_imputation failed for outcome `outcome', group `gname'. Error code `_rc'."
            local est_failed = 1
            restore
            continue
        }

        * Group-specific pre-trend test, average post effect, joint post p
        local pretrend_`gname' = .
        capture test pre1 pre2 pre3 pre4 pre5
        if _rc == 0 local pretrend_`gname' = r(p)
        local avg_`gname' = (_b[tau0] + _b[tau1] + _b[tau2] + _b[tau3] + _b[tau4] + _b[tau5])/6
        local p_`gname' = .
        capture test tau0 tau1 tau2 tau3 tau4 tau5
        if _rc == 0 local p_`gname' = r(p)

        * Baseline mean (pre-treatment observations in estimation sample)
        quietly summarize `outcome' if year < year_expanded & e(sample) [aw=total_population_10]
        local baseline_`gname' = r(mean)
        if missing(`baseline_`gname'') | `baseline_`gname'' == 0 {
            local baseline_`gname' = 1
        }
        local pct_`gname' = (`avg_`gname'' / `baseline_`gname'') * 100
        if `pct_`gname'' < -100 local pct_`gname' = -100

        * National/total annual impact for this group.
        * matched (raw): avg * N treated programs in subset.
        * per_100k:      avg * sum(state_pop across treated programs in subset) / 100k.
        local `gname'_national = .
        if "`outcome'" == "matched" {
            quietly levelsof program_numeric_id if treated_state == 1 & !missing(`outcome'), local(_progs)
            local n_progs : word count `_progs'
            local `gname'_national = `avg_`gname'' * `n_progs'
            local has_national = 1
        }
        else if strpos("`outcome'", "_per_100k") > 0 {
            tempvar ptag
            quietly egen byte `ptag' = tag(program_numeric_id) if treated_state == 1 & !missing(`outcome')
            quietly summarize total_population_10 if `ptag' == 1
            local `gname'_national = `avg_`gname'' * r(sum) / 100000
            local has_national = 1
        }

        * Event-study matrix for this group (own pre-trend path)
        local row = 1
        forval h = 5(-1)1 {
            matrix `gname'[`row',1] = -`h'
            capture matrix `gname'[`row',2] = _b[pre`h']
            capture matrix `gname'[`row',3] = _se[pre`h']
            capture matrix `gname'[`row',4] = _b[pre`h'] + 1.96*_se[pre`h']
            capture matrix `gname'[`row',5] = _b[pre`h'] - 1.96*_se[pre`h']
            local ++row
        }
        forval h = 0/5 {
            matrix `gname'[`row',1] = `h'
            capture matrix `gname'[`row',2] = _b[tau`h']
            capture matrix `gname'[`row',3] = _se[tau`h']
            capture matrix `gname'[`row',4] = _b[tau`h'] + 1.96*_se[tau`h']
            capture matrix `gname'[`row',5] = _b[tau`h'] - 1.96*_se[tau`h']
            local ++row
        }
        restore
    }
    if (`est_failed') {
        di as error "Skipping outcome `outcome': at least one group failed to estimate."
        continue
    }
    local urban_national_text = cond(`has_national', string(`urban_national', "%9.0fc"), "NA")
    local rural_national_text = cond(`has_national', string(`rural_national', "%9.0fc"), "NA")

    * Store values in locals for annotation
    local text_urban = string(`avg_urban', "%9.2f")
    local text_rural = string(`avg_rural', "%9.2f")
    local text_urban_p = string(`p_urban', "%9.2f")
    local text_rural_p = string(`p_rural', "%9.2f")
    local text_urban_pre_p = cond(`pretrend_urban' < ., string(`pretrend_urban', "%9.2f"), "NA")
    local text_rural_pre_p = cond(`pretrend_rural' < ., string(`pretrend_rural', "%9.2f"), "NA")
    local text_baseline_urban = string(`baseline_urban', "%9.2f")
    local text_baseline_rural = string(`baseline_rural', "%9.2f")
    local text_pct_urban = string(`pct_urban', "%9.1f")
    local text_pct_rural = string(`pct_rural', "%9.1f")

    di "Average urban effect: " %9.3f `avg_urban' " (p = " %9.3f `p_urban' ", pre-trend p = " %9.3f `pretrend_urban' ")"
    di "Average rural effect: " %9.3f `avg_rural' " (p = " %9.3f `p_rural' ", pre-trend p = " %9.3f `pretrend_rural' ")"
    local avg_diff_ss = `avg_rural' - `avg_urban'
    di "Difference (rural - urban, split samples): " %9.3f `avg_diff_ss'
    if (`has_national') {
        di "Avg. annual aggregate effect (urban): " %15.0fc `urban_national'
        di "Avg. annual aggregate effect (rural): " %15.0fc `rural_national'
    }

    * =====================
    * Plotting with annotation
    * =====================
    * Matrices `urban' and `rural' were filled inside the split-sample loop
    * (each carries its own pre-trend path).
    * Convert matrices to variables for plotting
    preserve
    clear
    svmat urban, names(urb)
    svmat rural, names(rur)
    
    * Rename variables
    rename urb1 urb_period
    rename urb2 urb_coef  
    rename urb3 urb_se
    rename urb4 urb_ci_upper
    rename urb5 urb_ci_lower
    
    rename rur1 rur_period
    rename rur2 rur_coef
    rename rur3 rur_se  
    rename rur4 rur_ci_upper
    rename rur5 rur_ci_lower
    
    * Create pre/post indicators for plotting
    gen urb_pre = (urb_period < 0)
    gen urb_post = (urb_period >= 0)
    gen rur_pre = (rur_period < 0)  
    gen rur_post = (rur_period >= 0)

    * Calculate dynamic annotation coordinates using maximum upper confidence intervals from both groups
    quietly summarize urb_ci_upper
    local max1 = r(max)
    quietly summarize rur_ci_upper
    local max2 = r(max) 
    local y_annot = max(`max1', `max2') * 0.9
    local x_annot = -2
    
    local short = "${short_`outcome'}"
    local label = "${label_`outcome'}"
    if ("`label'" == "") {
        local label = "`outcome'"
    }
    local prefix : display %02.0f `plotnum'
    local ytitle_str "Treatment Effect (difference-in-differences)"
    local plot_title "Heterogeneity: Urban vs Rural - `label'"

    * Adjust for per 100k outcomes
    if (strpos("`short'", "_100k") > 0) {
        local ytitle_str "Treatment Effect (per 100,000 population)"
        local x_annot = -2
    }
    if ("`outcome'" == "matched") {
        local ytitle_str "Treatment Effect (number of residency positions)"
    }

    * Build text annotations.
    * Main annotation (left): urban/rural averages and difference.
    * Extra annotation at x = 3, same y-level: national/total impact split by urban/rural
    * (residency positions for raw matched; additional doctors nationally for per-100k).
    local main_text `"text(`y_annot' `x_annot' `"Urban Avg: `text_urban' (p=`text_urban_p', pre p=`text_urban_pre_p')"' `"Rural Avg: `text_rural' (p=`text_rural_p', pre p=`text_rural_pre_p')"', size(medsmall))"'
    local extra_text ""
    if ("`outcome'" == "matched") {
        local extra_text `"text(`y_annot' 3 `"Avg. annual change in"' `"residency positions:"' `"Urban: `urban_national_text'"' `"Rural: `rural_national_text'"', size(medium))"'
    }
    else if (strpos("`short'", "_100k") > 0) {
        local extra_text `"text(`y_annot' 3 `"Avg. annual change in"' `"doctors nationally:"' `"Urban: `urban_national_text'"' `"Rural: `rural_national_text'"', size(medium))"'
    }

    * Create combined plot using twoway, with annotation
    twoway (rarea urb_ci_upper urb_ci_lower urb_period if urb_pre, fcolor("31 119 180%20") lcolor("31 119 180%20") lwidth(none)) ///
           (rarea urb_ci_upper urb_ci_lower urb_period if urb_post, fcolor("31 119 180%20") lcolor("31 119 180%20") lwidth(none)) ///
           (rarea rur_ci_upper rur_ci_lower rur_period if rur_pre, fcolor("255 127 14%25") lcolor("255 127 14%25") lwidth(none)) ///
           (rarea rur_ci_upper rur_ci_lower rur_period if rur_post, fcolor("255 127 14%25") lcolor("255 127 14%25") lwidth(none)) ///
           (line urb_coef urb_period if urb_pre, lcolor("31 119 180") lwidth(medium) lpattern(solid)) ///
           (line urb_coef urb_period if urb_post, lcolor("31 119 180") lwidth(medium) lpattern(solid)) ///
           (line rur_coef rur_period if rur_pre, lcolor("255 127 14") lwidth(medium) lpattern(dash)) ///
           (line rur_coef rur_period if rur_post, lcolor("255 127 14") lwidth(medium) lpattern(dash)) ///
           (scatter urb_coef urb_period if urb_pre, mcolor("31 119 180") msize(medium) msymbol(circle)) ///
           (scatter urb_coef urb_period if urb_post, mcolor("31 119 180") msize(medium) msymbol(circle)) ///
           (scatter rur_coef rur_period if rur_pre, mcolor("255 127 14") msize(medium) msymbol(triangle)) ///
           (scatter rur_coef rur_period if rur_post, mcolor("255 127 14") msize(medium) msymbol(triangle)) ///
           , ///
           xline(-0.5, lcolor(black) lpattern(solid) lwidth(thin)) ///
           yline(0, lcolor(black) lpattern(solid) lwidth(thin)) ///
           xlabel(-5(1)5, labsize(small)) ///
           ylabel(#10, labsize(small) format(%9.2f)) ///
           xtitle("Years relative to Medicaid expansion", size(small)) ///
           ytitle("`ytitle_str'", size(small)) ///
           legend(order(9 11) label(9 "Urban") label(11 "Rural") ///
               position(6) rows(1) size(small)) ///
           `main_text' ///
           `extra_text' ///
           graphregion(color(white)) plotregion(color(white))
    
    local outfname = "${fname_`outcome'}"
    graph export "${figdir}/`outfname'.png", as(png) replace width(1200) height(800)
    graph export "${figdir}/`outfname'.pdf", replace
    graph export "${latex_figdir}/`outfname'.png", as(png) replace width(1200) height(800)
    graph export "${latex_figdir}/`outfname'.pdf", replace
    
    restore
    local ++plotnum
}

di ""
di "========================================================================="
di "HETEROGENEITY ANALYSIS BY URBAN/RURAL CLASSIFICATION COMPLETED"
di "Split-sample event study plots saved to:"
di "  - ${figdir}/appx-levels-location.png, appx-quota-location.png"
di "  - ${latex_figdir}/appx-levels-location.png, appx-quota-location.png"
di "========================================================================="

log close
