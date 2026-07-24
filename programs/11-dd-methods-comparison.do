* =============================================================================
* DiD Methods Comparison: Robustness of Main Results across Estimators
* -----------------------------------------------------------------------------
* Estimators (Callaway & Sant'Anna / csdid intentionally EXCLUDED):
*   TWFE (OLS), Borusyak, Jaravel and Spiess (2024), de Chaisemartin and
*   D'Haultfoeuille (2020), Sun and Abraham (2021), Cengiz, Dube, Lindner and
*   Zipperer (2019), Gardner (2022).
* Legends name the PAPER, not the Stata package.
* Outcomes:
*   matched_per_100k_yr (year-varying per-capita, HEADLINE)  -> main-estimators
*   matched              (levels)                            -> appx-estimators-levels
*   quota_per_100k       (offered positions per 100k)        -> appx-estimators-quota
* One event_plot per outcome; each carries the BJS pre-trend joint-test p.
* =============================================================================

clear all
set more off

global topdir "/Users/hhadah/Projects/GiT/residency-medicaid-expansion"
global datadir "${topdir}/data/datasets"
global figdir "${topdir}/output/figures"
global tabdir "${topdir}/output/tables"
global latex_figdir "${topdir}/my_paper/figures"

cap mkdir "${figdir}"
cap mkdir "${tabdir}"
cap mkdir "${latex_figdir}"

log using "${topdir}/output/11-dd-methods-comparison.log", replace

* -------------------------------------------------------------------------
* Load cleaned data + year-varying ACS population (headline denominator)
* -------------------------------------------------------------------------
use "${datadir}/cleaned_program_residency_medicaid.dta", clear
replace state = strtrim(upper(state))
merge m:1 state year using "${datadir}/state_year_population.dta", keep(master match) nogen
gen double matched_per_100k_yr = matched / pop_yr * 100000

* -------------------------------------------------------------------------
* Panel identifiers
* -------------------------------------------------------------------------
egen program_numeric_id = group(state institution_code)
encode state, gen(state_id)
xtset program_numeric_id year

* -------------------------------------------------------------------------
* Treatment setup shared across estimators
* -------------------------------------------------------------------------
gen byte never_treat = missing(year_expanded)

capture confirm variable post_expansion
if _rc != 0 {
    gen byte post_expansion = (treated_state == 1 & year_expanded < . & year >= year_expanded)
}
gen byte D = post_expansion

gen rel_time = year - year_expanded

local maxlag  = 5
local maxlead = 5

* Lags 0..(maxlag-1) exact, last lag binned (>= maxlag)
forval h = 0/`=`maxlag' - 1' {
    gen byte L_`h' = (rel_time == `h')
}
gen byte L_`maxlag' = (rel_time >= `maxlag' & !missing(rel_time))

* Leads 1..(maxlead-1) exact, last lead binned (<= -maxlead)
forval h = 1/`=`maxlead' - 1' {
    gen byte F_`h' = (rel_time == -`h')
}
gen byte F_`maxlead' = (rel_time <= -`maxlead' & !missing(rel_time))

gen byte ref = (rel_time == -1)

* -------------------------------------------------------------------------
* Outcomes, labels, and output basenames
* -------------------------------------------------------------------------
global outcomes "matched_per_100k_yr matched quota_per_100k"
global label_matched_per_100k_yr "Matched Residency Positions per 100,000 Population"
global label_matched             "Total Matched Residency Positions"
global label_quota_per_100k      "Residency Quota Positions per 100,000 Population"
global fname_matched_per_100k_yr "main-estimators"
global fname_matched             "appx-estimators-levels"
global fname_quota_per_100k      "appx-estimators-quota"

foreach Y of global outcomes {

    capture confirm variable `Y'
    if _rc != 0 {
        di as error "Outcome `Y' not found. Skipping."
        continue
    }

    di ""
    di "========================================================================="
    di "DiD METHODS COMPARISON: ${label_`Y'}"
    di "Outcome: `Y'"
    di "========================================================================="

    ************
    *** TWFE ***
    ************
    * F_1 omitted as reference period.
    di "--- Two-way fixed effects (OLS) ---"
    capture noisily reghdfe `Y' L_0 L_1 L_2 L_3 L_4 L_5 F_2 F_3 F_4 F_5 ///
        [aw=total_population_10], absorb(program_numeric_id year) cluster(state_id)
    if (_rc == 0) estimates store twfe

    ***********************
    *** did_imputation  ***
    ***********************
    di "--- Borusyak, Jaravel and Spiess (2024) ---"
    capture noisily did_imputation `Y' program_numeric_id year year_expanded ///
        [aw=total_population_10], horizons(0/`maxlag') pretrend(`maxlead') ///
        fe(program_numeric_id year) cluster(state_id) minn(0)
    if (_rc == 0) estimates store didimp
    * BJS pre-trend joint test, captured before other estimators overwrite e().
    local pretrend_p = .
    capture test pre1 pre2 pre3 pre4 pre5
    if (_rc == 0) local pretrend_p = r(p)

    ***********************
    *** did_multiplegt  ***
    ***********************
    di "--- de Chaisemartin and D'Haultfoeuille (2020) ---"
    capture noisily did_multiplegt_dyn `Y' program_numeric_id year D, ///
        effects(`maxlag') placebo(`maxlead') cluster(state_id)
    capture matrix didmgt_b = e(estimates)
    capture matrix didmgt_v = e(variances)

    *****************************
    ***  eventstudyinteract   ***
    *****************************
    di "--- Sun and Abraham (2021) ---"
    capture noisily eventstudyinteract `Y' L_0 L_1 L_2 L_3 L_4 L_5 F_2 F_3 F_4 F_5 ///
        [aw=total_population_10], vce(cluster state_id) ///
        absorb(program_numeric_id year) cohort(year_expanded) control_cohort(never_treat)
    capture matrix evtstint_b = e(b_iw)
    capture matrix evtstint_v = e(V_iw)

    ***************
    *** did2s   ***
    ***************
    di "--- Gardner (2022) ---"
    capture noisily did2s `Y' [aw=total_population_10], ///
        first_stage(program_numeric_id year) ///
        second_stage(F_2 F_3 F_4 F_5 L_0 L_1 L_2 L_3 L_4 L_5) ///
        treatment(D) cluster(state_id)
    capture matrix did2s_b = e(b)
    capture matrix did2s_v = e(V)

    ******************
    *** stackedev  ***
    ******************
    di "--- Cengiz, Dube, Lindner and Zipperer (2019) ---"
    capture noisily stackedev `Y' F_2 F_3 F_4 F_5 L_0 L_1 L_2 L_3 L_4 L_5 ref, ///
        cohort(year_expanded) time(year) never_treat(never_treat) ///
        unit_fe(program_numeric_id) clust_unit(state_id)
    capture matrix stackedev_b = e(b)
    capture matrix stackedev_v = e(V)

    * Color palette
    colorpalette tableau, nograph

    local label  = "${label_`Y'}"
    local fname  = "${fname_`Y'}"
    local pt_text = cond(`pretrend_p' < ., string(`pretrend_p', "%4.2f"), "NA")

    * ---------------------------------------------------------------------
    * Combined event-plot overlaying all estimators (no csdid).
    * Legend names the PAPER; BJS pre-trend p annotated top-left.
    * ---------------------------------------------------------------------
    event_plot twfe didimp didmgt_b#didmgt_v evtstint_b#evtstint_v stackedev_b#stackedev_v did2s_b#did2s_v, ///
        stub_lag(L_# tau# Effect_# L_# L_# L_#) ///
        stub_lead(F_# pre# Placebo_# F_# F_# F_#) ///
        together perturb(-0.25(0.10)0.25) trimlead(`maxlead') trimlag(`maxlag') noautolegend ///
        plottype(scatter) ciplottype(rspike) ///
        lag_opt1(msymbol(+)   msize(2.0) mlwidth(0.3) color(black))           lag_ci_opt1(color(black)        lw(0.15)) ///
        lag_opt2(msymbol(Dh)  msize(2.0) mlwidth(0.3) color("`r(p2)'"))       lag_ci_opt2(color("`r(p2)'")    lw(0.15)) ///
        lag_opt3(msymbol(Th)  msize(2.0) mlwidth(0.3) color("`r(p3)'"))       lag_ci_opt3(color("`r(p3)'")    lw(0.15)) ///
        lag_opt4(msymbol(Sh)  msize(2.0) mlwidth(0.3) color("`r(p4)'"))       lag_ci_opt4(color("`r(p4)'")    lw(0.15)) ///
        lag_opt5(msymbol(Oh)  msize(2.0) mlwidth(0.3) color("`r(p5)'"))       lag_ci_opt5(color("`r(p5)'")    lw(0.15)) ///
        lag_opt6(msymbol(V)   msize(2.0) mlwidth(0.3) color("`r(p6)'"))       lag_ci_opt6(color("`r(p6)'")    lw(0.15)) ///
        graph_opt( ///
            xtitle("Years relative to Medicaid expansion") ///
            ytitle("Average effect") xlabel(-`maxlead'(1)`maxlag') ///
            text(0 -4 "Pre-trend p = `pt_text'", size(medsmall) placement(e)) ///
            legend(order(1 "Two-way fixed effects" 3 "Borusyak, Jaravel and Spiess (2024)" ///
                         5 "de Chaisemartin and D'Haultfoeuille (2020)" 7 "Sun and Abraham (2021)" ///
                         9 "Cengiz, Dube, Lindner and Zipperer (2019)" 11 "Gardner (2022)") ///
                   pos(6) rows(3) region(style(none)) size(small)) ///
            xline(-0.5, lc(gs8) lp(dash)) ///
            yline(0,    lc(gs8) lp(dash)) ///
            graphregion(color(white)) plotregion(color(white)) ///
        )

    graph export "${figdir}/`fname'.png", as(png) replace width(1200) height(800)

    graph export "${figdir}/`fname'.pdf", replace
    graph export "${latex_figdir}/`fname'.png", as(png) replace width(1200) height(800)
    graph export "${latex_figdir}/`fname'.pdf", replace
    di as result "Exported `fname'.png  (BJS pre-trend p = `pt_text')"
}

di ""
di "=================================================================="
di "DiD methods comparison completed (csdid excluded)."
di "  main-estimators.png, appx-estimators-levels.png, appx-estimators-quota.png"
di "=================================================================="

log close
