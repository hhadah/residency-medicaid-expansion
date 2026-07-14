* =============================================================================
* Event Study: Medicaid (ACA) expansion on GME funding
* ---------------------------------------------------------------------------
* Estimates the effect of a state's ACA Medicaid expansion on hospital-level
* Graduate Medical Education (GME) funding, using the appended CMS GME funding
* panel merged with each state's expansion status.
*
* Panel:     hospital (provider CCN) x fiscal year, 2000-2023
* Design:    did_imputation (Borusyak, Jaravel & Spiess 2024), same scheme as
*            05/12/13-dd analyses. Never-expansion states are the comparison
*            group (year_expanded missing => BJS never-treated).
* Outcomes:  total GME, direct GME (DGME) and indirect (IME) payments, in
*            asinh (inverse hyperbolic sine, handles zeros/skew) and levels.
*
* Note on weights: the residency analyses weight by population; that is not
*   meaningful at the hospital level, so this runs unweighted and clusters by
*   state (the treatment-assignment level), mirroring the FE/cluster structure.
*
* Requires: data/datasets/gme_funding_expansion.dta (built by
*           14-append-gme-funding.R and 15-merge-gme-expansion.R).
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

log using "${topdir}/output/16-gme-funding-event-study.log", replace

* -------------------------------------------------------------------------
* Load merged GME funding + expansion-status panel
* -------------------------------------------------------------------------
use "${datadir}/gme_funding_expansion.dta", clear

* Drop territories with no expansion classification (e.g. Puerto Rico):
* they are neither treated nor a valid comparison unit.
drop if missing(expansion_state)

* -------------------------------------------------------------------------
* Collapse to one observation per hospital-year
* -------------------------------------------------------------------------
* A hospital can file several cost-report segments within a fiscal year
* (distinct begin/end dates). For an annual panel we sum the dollar payments
* and resident FTEs received that year, and average the rate/stock measures
* (per-resident amounts, resident caps, beds). This removes the "repeated
* time values" that would otherwise break xtset.
egen provider_numeric_id = group(state provider_ccn)

collapse (sum)  dgme_payment ime_payment total_gme_payment ///
                primary_care_fte non_primary_care_fte dgme_ftes ime_ftes ///
        (mean) primary_care_pra non_primary_care_pra ///
                dgme_resident_cap ime_resident_cap num_beds ///
        (first) state year_expanded expanded_ever, ///
        by(provider_numeric_id fiscal_year)

* -------------------------------------------------------------------------
* Panel identifiers
* -------------------------------------------------------------------------
encode state, gen(state_id)

* Ever-expansion indicator used to define the treated group in the plots
gen byte treated_state = expanded_ever

xtset provider_numeric_id fiscal_year

* -------------------------------------------------------------------------
* Outcomes
* -------------------------------------------------------------------------
* asinh (inverse hyperbolic sine) tames the extreme right-skew of dollar
* payments and is defined at zero, so it keeps hospitals with no DGME/IME.
gen double asinh_total_gme = asinh(total_gme_payment)
gen double asinh_dgme      = asinh(dgme_payment)
gen double asinh_ime       = asinh(ime_payment)

* Total GME in millions of dollars, so the level event study reads on a
* human-scale axis (0-4) instead of scientific notation (4.00e+06).
gen double total_gme_mil = total_gme_payment / 1e6

label var asinh_total_gme "Total GME Payment (asinh $)"
label var asinh_dgme      "Direct GME (DGME) Payment (asinh $)"
label var asinh_ime       "Indirect Medical Ed. (IME) Payment (asinh $)"
label var total_gme_mil   "Total GME Payment ($ millions)"

global outcomes "asinh_total_gme asinh_dgme asinh_ime total_gme_mil"

global label_asinh_total_gme "Total GME Payment (asinh dollars)"
global label_asinh_dgme      "Direct GME (DGME) Payment (asinh dollars)"
global label_asinh_ime       "Indirect Medical Education (IME) Payment (asinh dollars)"
global label_total_gme_mil   "Total GME Payment ($ millions)"

global short_asinh_total_gme "asinh_total_gme"
global short_asinh_dgme      "asinh_dgme"
global short_asinh_ime       "asinh_ime"
global short_total_gme_mil   "total_gme_mil"

* -------------------------------------------------------------------------
* Store event-study estimates
* -------------------------------------------------------------------------
tempname did_results
tempfile did_results_file
postfile `did_results' str20 outcome double coef se tstat pvalue avg_treat ///
    pretrend_p treat_p baseline pct_effect n_hospitals n_states ///
    using "`did_results_file'", replace

local plotnum = 27
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
    di "EVENT STUDY: ${label_`outcome'}"
    di "Outcome variable: `outcome'"
    di "========================================================================="
    di ""

    * autosample drops the (few) hospital-years whose FE cannot be imputed
    * -- e.g. hospitals observed in a single period -- rather than erroring.
    capture noisily did_imputation `outcome' provider_numeric_id fiscal_year year_expanded, ///
        horizons(0/5) pretrend(5) fe(provider_numeric_id fiscal_year) ///
        cluster(state_id) minn(0) autosample
    if (_rc != 0) {
        di as error "did_imputation failed for outcome `outcome'. Error code `_rc'."
        continue
    }

    * Impact effect (tau0) and its inference
    local coef  = _b[tau0]
    local se    = _se[tau0]
    local tstat = `coef' / `se'
    * did_imputation uses large-sample (normal) inference; it does not set e(df_r)
    local pvalue = 2*(1 - normal(abs(`tstat')))

    * Average post-treatment ATT across horizons 0-5
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

    * Joint tests: pre-trends and post-treatment effects
    local pretrend_p = .
    local treat_p = .
    capture noisily test pre1 pre2 pre3 pre4 pre5
    if (_rc == 0) local pretrend_p = r(p)
    capture noisily test tau0 tau1 tau2 tau3 tau4 tau5
    if (_rc == 0) local treat_p = r(p)

    * Baseline mean (pre-treatment observations in treated states)
    quietly summarize `outcome' if treated_state == 1 & fiscal_year < year_expanded
    local baseline_mean = r(mean)
    local pct_effect = .
    if !missing(`baseline_mean') & `baseline_mean' != 0 {
        local pct_effect = (`avg_treat' / `baseline_mean') * 100
    }

    * Panel counts on the estimation sample
    local n_hospitals = .
    local n_states = .
    capture levelsof provider_numeric_id if !missing(`outcome'), local(active_h)
    if (_rc == 0) local n_hospitals : word count `active_h'
    capture levelsof state_id if !missing(`outcome'), local(active_s)
    if (_rc == 0) local n_states : word count `active_s'

    post `did_results' ("`outcome'") ///
        (`coef') (`se') (`tstat') (`pvalue') (`avg_treat') ///
        (`pretrend_p') (`treat_p') (`baseline_mean') (`pct_effect') ///
        (`n_hospitals') (`n_states')

    di "ATT (tau0): " %12.3f `coef' "  SE: " %12.3f `se' "  p = " %6.3f `pvalue'
    di "Average post-treatment effect: " %12.3f `avg_treat'
    if (`pretrend_p' < .) di "Pretrend joint p-value: " %6.3f `pretrend_p'
    if (`treat_p' < .)   di "Treatment joint p-value: " %6.3f `treat_p'

    * ---------------------------------------------------------------------
    * Collect coefficients for the event-study plot (-5 .. +5)
    * ---------------------------------------------------------------------
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

    local avg_text   = cond(`avg_treat' < ., string(`avg_treat', "%9.3f"), "NA")
    local treat_text = cond(`treat_p' < ., string(`treat_p', "%9.2f"), "NA")
    local base_text  = cond(`baseline_mean' < ., string(`baseline_mean', "%9.2f"), "NA")
    local pct_text   = cond(`pct_effect' < ., string(`pct_effect', "%9.1f"), "NA")

    local post_line ""
    if (`avg_treat' < .) {
        local post_line "(scatteri `avg_treat' 0 `avg_treat' 5, recast(line) lpattern(dash) lcolor(red) lwidth(medium))"
    }

    local short = "${short_`outcome'}"
    local label = "${label_`outcome'}"
    if ("`label'" == "") local label = "`outcome'"
    local prefix : display %02.0f `plotnum'
    local plot_title "Event Study: `label'"

    * Annotation position
    quietly summarize ci_upper
    local y_annot = r(max) * 0.9
    local x_annot = -3

    local main_text `"text(`y_annot' `x_annot' `"Baseline Mean: `base_text'"' `"Post avg = `avg_text' (`pct_text'%)"' `"Joint p-value = `treat_text'"', size(large))"'

    twoway ///
        (rarea ci_upper ci_lower period if pre_period,  fcolor(dkgreen%45) lcolor(dkgreen%45) lwidth(none)) ///
        (rarea ci_upper ci_lower period if post_period, fcolor(maroon%45)  lcolor(maroon%45)  lwidth(none)) ///
        (line coef period if pre_period,  lcolor(dkgreen) lwidth(medium)) ///
        (line coef period if post_period, lcolor(maroon)  lwidth(medium)) ///
        (scatter coef period if pre_period,  mcolor(dkgreen) msymbol(circle) msize(medlarge)) ///
        (scatter coef period if post_period, mcolor(maroon)  msymbol(circle) msize(medlarge)) ///
        `post_line' ///
        , ///
        xline(-0.5, lcolor(black) lpattern(solid) lwidth(thin)) ///
        yline(0, lcolor(black) lpattern(solid) lwidth(thin)) ///
        xlabel(-5(1)5, labsize(small)) ///
        ylabel(#8, labsize(small) format(%9.2f)) ///
        xtitle("Years relative to Medicaid expansion", size(small)) ///
        ytitle("Treatment Effect: `label'", size(small)) ///
        `main_text' ///
        legend(off) ///
        graphregion(color(white)) plotregion(color(white))

    graph export "${figdir}/`prefix'-did_`short'_event.png", as(png) replace width(1200) height(800)
    graph export "${latex_figdir}/`prefix'-did_`short'_event.png", as(png) replace width(1200) height(800)
    restore
    local ++plotnum
}

postclose `did_results'

* -------------------------------------------------------------------------
* Summary table
* -------------------------------------------------------------------------
use "`did_results_file'", clear
order outcome coef se tstat pvalue avg_treat pct_effect pretrend_p treat_p ///
    baseline n_hospitals n_states
save "${tabdir}/did_summary_gme_funding.dta", replace
export delimited using "${tabdir}/did_summary_gme_funding.csv", replace
list, clean noobs

di ""
di "=================================================================="
di "Event-study estimates of Medicaid expansion on GME funding complete."
di "Summary table:"
di "  - ${tabdir}/did_summary_gme_funding.dta"
di "  - ${tabdir}/did_summary_gme_funding.csv"
di "Figures:"
di "  - ${figdir}/27..-did_*_event.png (and LaTeX copies)"
di "=================================================================="

log close
