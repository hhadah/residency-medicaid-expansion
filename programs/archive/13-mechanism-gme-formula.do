* =============================================================================
* Mechanism test: the Medicaid GME financing channel
* ---------------------------------------------------------------------------
* Splits EXPANSION states by their Medicaid GME formula design and estimates the
* main event study (matched per 100k) separately for each group, each using the
* never-expansion states as the comparison group.
*
* Prediction (Conceptual Framework / Mechanisms):
*   - VOLUME-RESPONSIVE formula states gain GME revenue from expansion  -> small/no decline
*   - NON-RESPONSIVE (fixed + none) states gain no GME revenue           -> the decline
*
* Classification: gme_formula (2012 / Henderson 2013, the pre-period EXOGENOUS
*   baseline). Figures 25 = volume-responsive, 26 = non-responsive.
*   (The gme_formula_2015 column in the CSV is retained for reference but not used.)
*
* Requires: data/raw/gme_formula_classification.csv. Rows == "TODO" for a treated
*   state are excluded and reported. Mirrors 05/12-dd-analysis (did_imputation, BJS 2024).
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

log using "${topdir}/output/13-mechanism-gme-formula.log", replace

* -------------------------------------------------------------------------
* Load data and panel identifiers (same as 05/12)
* -------------------------------------------------------------------------
use "${datadir}/cleaned_program_residency_medicaid.dta", clear
egen program_numeric_id = group(state institution_code)
encode state, gen(state_id)
xtset program_numeric_id year

* -------------------------------------------------------------------------
* Merge the Medicaid GME formula classification (2012 baseline column)
* -------------------------------------------------------------------------
preserve
    import delimited "${rawdir}/gme_formula_classification.csv", clear ///
        varnames(1) stringcols(_all)
    keep state gme_formula
    replace state = strtrim(upper(state))
    tempfile gme
    save `gme'
restore

replace state = strtrim(upper(state))
merge m:1 state using `gme', keep(master match) nogen

* -------------------------------------------------------------------------
* Validate the classification before estimating
* -------------------------------------------------------------------------
di as text "Medicaid GME formula distribution (program-years):"
tab gme_formula, missing

quietly count if treated_state == 1 & (gme_formula == "TODO" | missing(gme_formula))
if r(N) > 0 {
    di as error "NOTE: " r(N) " treated (expansion-state) program-years are unclassified"
    di as error "      (gme_formula == TODO) and EXCLUDED from both treated groups."
    quietly levelsof state if treated_state == 1 & gme_formula == "TODO", local(todo_states) clean
    di as error "      Unclassified expansion states: `todo_states'"
}

* Contrast: volume-responsive vs. NOT volume-responsive (fixed + none).
gen byte gme_vol    = (gme_formula == "volume")
gen byte gme_notvol = inlist(gme_formula, "fixed", "none")

di as text "Expansion states by volume-responsiveness (1 = volume-responsive):"
tab gme_vol if treated_state == 1 & (gme_vol == 1 | gme_notvol == 1)

* -------------------------------------------------------------------------
* Store group-level summary results
* -------------------------------------------------------------------------
tempname mech
tempfile mech_file
postfile `mech' str20 group double att_post pval_treat baseline pct n_treat_progs ///
    using "`mech_file'", replace

local plotnum = 25
foreach grp in volume notvolume {

    * Sample = this group's expansion states + ALL never-expansion controls.
    preserve
    if "`grp'" == "volume"    keep if treated_state == 0 | (treated_state == 1 & gme_vol == 1)
    if "`grp'" == "notvolume" keep if treated_state == 0 | (treated_state == 1 & gme_notvol == 1)

    quietly count if treated_state == 1
    if (r(N) == 0) {
        di as error "No treated obs for group `grp' -- skipping."
        restore
        continue
    }

    di ""
    di "========================================================================="
    di "MECHANISM SPLIT: `grp'-formula expansion states vs. non-expansion controls"
    di "========================================================================="

    capture noisily did_imputation matched_per_100k program_numeric_id year year_expanded ///
        [aw=total_population_10], horizons(0/5) pretrend(5) ///
        fe(program_numeric_id year) cluster(state_id) minn(0)
    if (_rc != 0) {
        di as error "did_imputation failed for group `grp' (rc=`_rc'). Skipping."
        restore
        continue
    }

    * Average post-treatment ATT and joint significance
    local tau_sum = 0
    local tau_n   = 0
    forval h = 0/5 {
        capture scalar __b = _b[tau`h']
        if (_rc == 0) {
            local tau_sum = `tau_sum' + __b
            local tau_n   = `tau_n' + 1
        }
    }
    local att_post = cond(`tau_n' > 0, `tau_sum'/`tau_n', .)
    local treat_p = .
    capture test tau0 tau1 tau2 tau3 tau4 tau5
    if (_rc == 0) local treat_p = r(p)

    quietly summarize matched_per_100k if treated_state == 1 & year < year_expanded [aw=total_population_10]
    local baseline = r(mean)
    local pct = cond(`baseline' > 0, 100*`att_post'/`baseline', .)

    quietly levelsof program_numeric_id if treated_state == 1, local(tp)
    local n_tp : word count `tp'

    post `mech' ("`grp'") (`att_post') (`treat_p') (`baseline') (`pct') (`n_tp')
    di as result "Group `grp': avg post ATT = " %6.3f `att_post' ///
        "  (" %4.1f `pct' "% of baseline " %5.3f `baseline' "); joint p = " %5.3f `treat_p'

    * ---- Event-study plot (same scheme as 05/12) --------------------------
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
    drop _all
    svmat plot_coef, names(col)
    keep if !missing(period)
    gen ci_upper = coef + 1.96*se
    gen ci_lower = coef - 1.96*se
    gen byte pre_period  = (period < 0)
    gen byte post_period = (period >= 0)

    local glab = cond("`grp'"=="volume", "Volume-Responsive GME States", "Non-Responsive GME States (Fixed/None)")
    local fname = cond("`grp'"=="volume", "VolumeResponsive", "NonResponsive")
    local prefix : display %02.0f `plotnum'
    local post_line ""
    if (`att_post' < .) local post_line "(scatteri `att_post' 0 `att_post' 5, recast(line) lpattern(dash) lcolor(red) lwidth(medium))"

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
        title("`glab'", size(medsmall)) ///
        legend(off) graphregion(color(white)) plotregion(color(white))

    graph export "${figdir}/`prefix'-did_matched_per_100k_`fname'_event.png", as(png) replace width(1200) height(800)
    graph export "${latex_figdir}/`prefix'-did_matched_per_100k_`fname'_event.png", as(png) replace width(1200) height(800)
    restore
    local ++plotnum
}

postclose `mech'

* -------------------------------------------------------------------------
* Comparison table
* -------------------------------------------------------------------------
use "`mech_file'", clear
order group att_post pct pval_treat baseline n_treat_progs
list, clean noobs
export delimited using "${tabdir}/mechanism-gme-formula-summary.csv", replace

di ""
di "=================================================================="
di "Mechanism (GME-formula) split complete (2012 baseline classification)."
di "Figures: 25-did_matched_per_100k_VolumeResponsive_event.png"
di "         26-did_matched_per_100k_NonResponsive_event.png"
di "Summary: ${tabdir}/mechanism-gme-formula-summary.csv"
di "=================================================================="

log close
