/*
Heterogeneity Analysis by Urban/Rural Classification - By Specialty Group
Date: January 27th, 2026
Purpose: Investigate heterogeneity in treatment effects by urban/rural within each specialty group
Urban areas: RUCA codes 1, 2, 3
Rural areas: RUCA codes > 3
*/

clear all
set more off

* -------------------------------------------------------------------------
* Define paths
* -------------------------------------------------------------------------
global topdir "/Users/hhadah/Documents/GiT/residency-medicaid-expansion"
global datadir "${topdir}/data/datasets"
global raw "${topdir}/data/raw"
global figdir "${topdir}/output/figures"
global tabdir "${topdir}/output/tables"
global latex_figdir "${topdir}/my_paper/figures"

* Open log file
log using "${topdir}/output/11-dd-analysis-hetero-byspecialty.log", replace

cap mkdir "${figdir}"
cap mkdir "${tabdir}"
cap mkdir "${latex_figdir}"

* -------------------------------------------------------------------------
* Load cleaned data
* -------------------------------------------------------------------------
use "${datadir}/cleaned_residency_medicaid.dta", clear

*===============================================================================
* Data Setup
*===============================================================================

* Use gen_specialty_alt for specialty grouping
capture confirm variable gen_specialty_alt
if _rc != 0 {
    di as error "Variable gen_specialty_alt not found. Check data."
    exit 459
}

* Create urban/rural group based on ruca_1_2010
* 0 = Urban (RUCA codes 1, 2, 3), 1 = Rural (RUCA codes > 3)
gen urban_rural = (ruca_1_2010 > 3) if !missing(ruca_1_2010)
label define urban_rural_lab 0 "Urban" 1 "Rural"
label values urban_rural urban_rural_lab

* Build specialty_group from gen_specialty_alt
capture drop specialty_group specialty_group_name
encode gen_specialty_alt, gen(specialty_group)
gen str30 specialty_group_name = gen_specialty_alt

* Check for unclassified rows
quietly count if missing(gen_specialty_alt) | gen_specialty_alt == ""
if r(N) > 0 {
    di as error "WARNING: " r(N) " rows have missing gen_specialty_alt - dropping"
    drop if missing(gen_specialty_alt) | gen_specialty_alt == ""
}

* Keep only observations with non-missing urban_rural
keep if !missing(urban_rural)

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

di "Specialty distribution (gen_specialty_alt):"
tab gen_specialty_alt
di "Urban/Rural coding: 0 = Urban, 1 = Rural"
tab urban_rural, missing

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

*===============================================================================
* Heterogeneity Analysis - Urban vs Rural by Specialty
*===============================================================================

di ""
di "========================================================================="
di "HETEROGENEITY ANALYSIS: By Urban/Rural within Specialty Groups"
di "Urban vs Rural"
di "========================================================================="
di ""

local plotnum = 233

levelsof specialty_group, local(spec_groups)
foreach spec of local spec_groups {
    levelsof specialty_group_name if specialty_group == `spec', local(spec_name) clean
    
    di ""
    di "========================================================================="
    di "SPECIALTY GROUP: `spec_name' (Urban/Rural Heterogeneity)"
    di "========================================================================="
    di ""
    
    foreach outcome of global outcomes {
        
        capture confirm variable `outcome'
        if _rc != 0 {
            di as error "Outcome `outcome' not found. Skipping."
            continue
        }
        
        quietly count if specialty_group == `spec' & urban_rural == 0 & treated_state == 1 & !missing(`outcome')
        if (r(N) == 0) {
            di as error "Outcome `outcome': no treated urban observations for `spec_name'."
            continue
        }
        
        quietly count if specialty_group == `spec' & urban_rural == 1 & treated_state == 1 & !missing(`outcome')
        if (r(N) == 0) {
            di as error "Outcome `outcome': no treated rural observations for `spec_name'."
            continue
        }
        
        di ""
        di "--- Outcome: `outcome' (Urban/Rural) ---"
        
        * Run did_imputation with heterogeneous effects by urban/rural within specialty
        capture noisily did_imputation `outcome' program_numeric_id year year_expanded [aw=total_population_10] ///
            if specialty_group == `spec', ///
            horizons(0/5) pretrend(5) ///
            cluster(state_id) ///
            hetby(urban_rural) ///
            fe(program_numeric_id year) ///
            minn(0) autosample
        
        if (_rc != 0) {
            di as error "did_imputation failed. Error code `_rc'."
            continue
        }
        
        * =====================
        * Post-treatment averages and p-value tests
        * =====================
        test pre1 pre2 pre3 pre4 pre5
        local pretrend_p = r(p)

        * Calculate average effects for both groups
        local avg_urban = (_b[tau0_0] + _b[tau1_0] + _b[tau2_0] + _b[tau3_0] + _b[tau4_0] + _b[tau5_0])/6
        local avg_rural = (_b[tau0_1] + _b[tau1_1] + _b[tau2_1] + _b[tau3_1] + _b[tau4_1] + _b[tau5_1])/6
        local avg_diff = `avg_rural' - `avg_urban'

        testnl ((_b[tau0_1] + _b[tau1_1] + _b[tau2_1] + _b[tau3_1] + _b[tau4_1] + _b[tau5_1])/6) - ((_b[tau0_0] + _b[tau1_0] + _b[tau2_0] + _b[tau3_0] + _b[tau4_0] + _b[tau5_0])/6) = 0
        local avg_diff_p = r(p)

        test (tau0_0 = tau0_1) (tau1_0 = tau1_1) (tau2_0 = tau2_1) (tau3_0 = tau3_1) (tau4_0 = tau4_1) (tau5_0 = tau5_1)
        local joint_het_p = r(p)

        test tau0_0 tau1_0 tau2_0 tau3_0 tau4_0 tau5_0
        local urban_p = r(p)
        test tau0_1 tau1_1 tau2_1 tau3_1 tau4_1 tau5_1
        local rural_p = r(p)

        * Calculate baseline means (pre-treatment observations in estimation sample)
        quietly summarize `outcome' if specialty_group == `spec' & urban_rural == 0 & year < year_expanded & e(sample) [aw=total_population_10]
        local baseline_urban = r(mean)
        if missing(`baseline_urban') | `baseline_urban' == 0 {
            local baseline_urban = 1
        }
        
        quietly summarize `outcome' if specialty_group == `spec' & urban_rural == 1 & year < year_expanded & e(sample) [aw=total_population_10]
        local baseline_rural = r(mean)
        if missing(`baseline_rural') | `baseline_rural' == 0 {
            local baseline_rural = 1
        }
        
        local pct_urban = (`avg_urban' / `baseline_urban') * 100
        local pct_rural = (`avg_rural' / `baseline_rural') * 100
        if `pct_urban' < -100 local pct_urban = -100
        if `pct_rural' < -100 local pct_rural = -100

        * Store values for annotation
        local text_urban = string(`avg_urban', "%9.2f")
        local text_rural = string(`avg_rural', "%9.2f")
        local text_urban_p = string(`urban_p', "%9.2f")
        local text_rural_p = string(`rural_p', "%9.2f")
        local text_avg_diff = string(`avg_diff', "%9.2f")
        local text_avg_diff_p = string(`avg_diff_p', "%9.2f")
        local text_baseline_urban = string(`baseline_urban', "%9.2f")
        local text_baseline_rural = string(`baseline_rural', "%9.2f")
        local text_pct_urban = string(`pct_urban', "%9.1f")
        local text_pct_rural = string(`pct_rural', "%9.1f")
        
        di "Average urban effect: " %9.3f `avg_urban' " (p = " %9.3f `urban_p' ")"
        di "Average rural effect: " %9.3f `avg_rural' " (p = " %9.3f `rural_p' ")"
        di "Difference (rural - urban): " %9.3f `avg_diff' " (p = " %9.3f `avg_diff_p' ")"
        di "Joint heterogeneity test p-value: " %9.3f `joint_het_p'

        * =====================
        * Plotting
        * =====================
        matrix urban = J(11, 5, .)
        matrix rural = J(11, 5, .)
        matrix colnames urban = period coef se ci_upper ci_lower
        matrix colnames rural = period coef se ci_upper ci_lower
        
        local row = 1
        forval h = 5(-1)1 {
            matrix urban[`row',1] = -`h'
            matrix urban[`row',2] = _b[pre`h']
            matrix urban[`row',3] = _se[pre`h']
            matrix urban[`row',4] = _b[pre`h'] + 1.96*_se[pre`h']
            matrix urban[`row',5] = _b[pre`h'] - 1.96*_se[pre`h']
            
            matrix rural[`row',1] = -`h'
            matrix rural[`row',2] = _b[pre`h']
            matrix rural[`row',3] = _se[pre`h']  
            matrix rural[`row',4] = _b[pre`h'] + 1.96*_se[pre`h']
            matrix rural[`row',5] = _b[pre`h'] - 1.96*_se[pre`h']
            
            local ++row
        }
        
        forval h = 0/5 {
            matrix urban[`row',1] = `h'
            matrix urban[`row',2] = _b[tau`h'_0]
            matrix urban[`row',3] = _se[tau`h'_0]
            matrix urban[`row',4] = _b[tau`h'_0] + 1.96*_se[tau`h'_0]
            matrix urban[`row',5] = _b[tau`h'_0] - 1.96*_se[tau`h'_0]
            
            matrix rural[`row',1] = `h'
            matrix rural[`row',2] = _b[tau`h'_1]
            matrix rural[`row',3] = _se[tau`h'_1]
            matrix rural[`row',4] = _b[tau`h'_1] + 1.96*_se[tau`h'_1]
            matrix rural[`row',5] = _b[tau`h'_1] - 1.96*_se[tau`h'_1]
            
            local ++row
        }
        
        preserve
        clear
        svmat urban, names(urb)
        svmat rural, names(rur)
        
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
        
        gen urb_pre = (urb_period < 0)
        gen urb_post = (urb_period >= 0)
        gen rur_pre = (rur_period < 0)  
        gen rur_post = (rur_period >= 0)

        quietly summarize urb_ci_upper
        local max1 = r(max)
        quietly summarize rur_ci_upper
        local max2 = r(max) 
        local y_annot = max(`max1', `max2')
        local x_annot = -2
        
        local short = "${short_`outcome'}"
        local label = "${label_`outcome'}"
        if ("`label'" == "") {
            local label = "`outcome'"
        }
        local prefix : display %02.0f `plotnum'
        local ytitle_str "Treatment Effect (difference-in-differences)"
        local plot_title "Heterogeneity: `spec_name' - Urban vs Rural - `label'"
        
        if (strpos("`short'", "_100k") > 0) {
            local ytitle_str "Treatment Effect (per 100,000 population)"
            local x_annot = -2
        }
        
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
               title("`plot_title'", size(small)) ///
               legend(order(9 11) label(9 "Urban") label(11 "Rural") ///
                   position(6) rows(1) size(small)) ///
               text(`y_annot' `x_annot' `"Urban Avg: `text_urban' (p=`text_urban_p')"' `"Rural Avg: `text_rural' (p=`text_rural_p')"' `"Difference: `text_avg_diff' (p=`text_avg_diff_p')"', size(small)) ///
               graphregion(color(white)) plotregion(color(white)) ///
               note("Vertical line at period 0 indicates Medicaid expansion" ///
                    "Blue (solid): Urban counties, Orange (dashed): Rural counties", size(vsmall))
        
        local spec_clean = subinstr("`spec_name'", " ", "_", .)
        local spec_clean = subinstr("`spec_clean'", "/", "_", .)
        
        graph export "${figdir}/`prefix'-hetero_urbanrural_`short'_`spec_clean'_event.png", as(png) replace width(1200) height(800)
        graph export "${latex_figdir}/`prefix'-hetero_urbanrural_`short'_`spec_clean'_event.png", as(png) replace width(1200) height(800)
        
        restore
        local ++plotnum
    }
}

di ""
di "========================================================================="
di "HETEROGENEITY ANALYSIS BY URBAN/RURAL WITHIN SPECIALTY COMPLETED"
di "Event study plots saved to:"
di "  - ${figdir}/311-hetero_urbanrural_*_event.png"
di "  - ${latex_figdir}/311-hetero_urbanrural_*_event.png"
di "========================================================================="

log close
