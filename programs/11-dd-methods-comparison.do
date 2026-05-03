* =============================================================================
* DiD Methods Comparison: Robustness of Main Results across Estimators
* Outcomes: matched (raw), matched_per_100k, quota_per_100k
* Estimators: TWFE, csdid (CS 2020), did_imputation (BJS 2021),
*             did_multiplegt_dyn (CD 2020), eventstudyinteract (SA 2020),
*             stackedev (CDLZ 2019), did2s (G 2021)
* Each outcome produces one event_plot overlaying all estimators.
* =============================================================================

clear all
set more off

* -------------------------------------------------------------------------
* Required packages (silent if already installed)
* -------------------------------------------------------------------------
// cap ssc install require, replace
// cap ssc install schemepack, replace
// cap ssc install avar, replace
// cap ssc install ftools, replace
// cap ssc install reghdfe, replace
// cap ssc install event_plot, replace
// cap ssc install palettes, replace
// cap ssc install colrspace, replace
// cap ssc install drdid, replace
// cap ssc install csdid, replace
// cap ssc install did_imputation, replace
// cap ssc install eventstudyinteract, replace
// cap ssc install did_multiplegt, replace
// cap ssc install did_multiplegt_dyn, replace
// cap ssc install stackedev, replace
// cap ssc install did2s, replace

* -------------------------------------------------------------------------
* Paths
* -------------------------------------------------------------------------
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
* Load cleaned data
* -------------------------------------------------------------------------
use "${datadir}/cleaned_program_residency_medicaid.dta", clear

* -------------------------------------------------------------------------
* Panel identifiers
* -------------------------------------------------------------------------
egen program_numeric_id = group(state institution_code)
encode state, gen(state_id)
xtset program_numeric_id year

* -------------------------------------------------------------------------
* Treatment setup shared across estimators
*   gvar         = first treatment year, 0 for never-treated (csdid)
*   never_treat  = indicator for never-treated programs
*   D            = treatment dummy (1 if currently treated)
*   rel_time     = year - year_expanded
*   L_h, F_h     = relative-time dummies (5 leads, 5 lags, endpoints binned)
*   ref          = reference-period dummy (rel_time == -1) for stackedev
* -------------------------------------------------------------------------
gen gvar = year_expanded
replace gvar = 0 if missing(year_expanded)

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
* Outcomes and labels
* -------------------------------------------------------------------------
global outcomes "matched matched_per_100k quota_per_100k"
global label_matched          "Total Matched Residency Positions"
global label_matched_per_100k "Matched Residency Positions per 100k Population"
global label_quota_per_100k   "Residency Quota Positions per 100k Population"

local plotnum = 17
local nocsdid_plotnum = 20

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
    di "--- TWFE (reghdfe) ---"
    capture noisily reghdfe `Y' L_0 L_1 L_2 L_3 L_4 L_5 F_2 F_3 F_4 F_5 ///
        [aw=total_population_10], absorb(program_numeric_id year) cluster(state_id)
    if (_rc == 0) estimates store twfe

    *************
    *** csdid ***
    *************
    di "--- csdid (Callaway & Sant'Anna 2020) ---"
    capture noisily csdid `Y' [iw=total_population_10], ///
        ivar(program_numeric_id) time(year) gvar(gvar) notyet
    if (_rc == 0) {
        capture noisily estat event, window(-`maxlead' `maxlag') estore(csdd)
    }

    ***********************
    *** did_imputation  ***
    ***********************
    di "--- did_imputation (Borusyak, Jaravel & Spiess 2021) ---"
    capture noisily did_imputation `Y' program_numeric_id year year_expanded ///
        [aw=total_population_10], horizons(0/`maxlag') pretrend(`maxlead') ///
        fe(program_numeric_id year) cluster(state_id) minn(0)
    if (_rc == 0) estimates store didimp

    ***********************
    *** did_multiplegt  ***
    ***********************
    di "--- did_multiplegt_dyn (de Chaisemartin & D'Haultfoeuille 2020) ---"
    capture noisily did_multiplegt_dyn `Y' program_numeric_id year D, ///
        effects(`maxlag') placebo(`maxlead') cluster(state_id)
    capture matrix didmgt_b = e(estimates)
    capture matrix didmgt_v = e(variances)

    *****************************
    ***  eventstudyinteract   ***
    *****************************
    di "--- eventstudyinteract (Sun & Abraham 2020) ---"
    capture noisily eventstudyinteract `Y' L_0 L_1 L_2 L_3 L_4 L_5 F_2 F_3 F_4 F_5 ///
        [aw=total_population_10], vce(cluster state_id) ///
        absorb(program_numeric_id year) cohort(year_expanded) control_cohort(never_treat)
    capture matrix evtstint_b = e(b_iw)
    capture matrix evtstint_v = e(V_iw)

    ***************
    *** did2s   ***
    ***************
    di "--- did2s (Gardner 2021) ---"
    capture noisily did2s `Y' [aw=total_population_10], ///
        first_stage(program_numeric_id year) ///
        second_stage(F_2 F_3 F_4 F_5 L_0 L_1 L_2 L_3 L_4 L_5) ///
        treatment(D) cluster(state_id)
    capture matrix did2s_b = e(b)
    capture matrix did2s_v = e(V)

    ******************
    *** stackedev  ***
    ******************
    di "--- stackedev (Cengiz, Dube, Lindner & Zipperer 2019) ---"
    capture noisily stackedev `Y' F_2 F_3 F_4 F_5 L_0 L_1 L_2 L_3 L_4 L_5 ref, ///
        cohort(year_expanded) time(year) never_treat(never_treat) ///
        unit_fe(program_numeric_id) clust_unit(state_id)
    capture matrix stackedev_b = e(b)
    capture matrix stackedev_v = e(V)

    * Color palette
    colorpalette tableau, nograph

    local label  = "${label_`Y'}"
    local prefix : display %02.0f `plotnum'

    * ---------------------------------------------------------------------
    * Combined event-plot overlaying all estimators
    * ---------------------------------------------------------------------
    event_plot twfe csdd didimp didmgt_b#didmgt_v evtstint_b#evtstint_v stackedev_b#stackedev_v did2s_b#did2s_v, ///
        stub_lag(L_# Tp# tau# Effect_# L_# L_# L_#) ///
        stub_lead(F_# Tm# pre# Placebo_# F_# F_# F_#) ///
        together perturb(-0.30(0.10)0.30) trimlead(`maxlead') trimlag(`maxlag') noautolegend ///
        plottype(scatter) ciplottype(rspike) ///
        lag_opt1(msymbol(+)   msize(2.0) mlwidth(0.3) color(black))           lag_ci_opt1(color(black)        lw(0.15)) ///
        lag_opt2(msymbol(lgx) msize(2.0) mlwidth(0.3) color("`r(p1)'"))       lag_ci_opt2(color("`r(p1)'")    lw(0.15)) ///
        lag_opt3(msymbol(Dh)  msize(2.0) mlwidth(0.3) color("`r(p2)'"))       lag_ci_opt3(color("`r(p2)'")    lw(0.15)) ///
        lag_opt4(msymbol(Th)  msize(2.0) mlwidth(0.3) color("`r(p3)'"))       lag_ci_opt4(color("`r(p3)'")    lw(0.15)) ///
        lag_opt5(msymbol(Sh)  msize(2.0) mlwidth(0.3) color("`r(p4)'"))       lag_ci_opt5(color("`r(p4)'")    lw(0.15)) ///
        lag_opt6(msymbol(Oh)  msize(2.0) mlwidth(0.3) color("`r(p5)'"))       lag_ci_opt6(color("`r(p5)'")    lw(0.15)) ///
        lag_opt7(msymbol(V)   msize(2.0) mlwidth(0.3) color("`r(p6)'"))       lag_ci_opt7(color("`r(p6)'")    lw(0.15)) ///
        graph_opt( ///
            title("Event Study Robustness: `label'", size(medsmall)) ///
            xtitle("Years relative to Medicaid expansion") ///
            ytitle("Average effect") xlabel(-`maxlead'(1)`maxlag') ///
            legend(order(1 "TWFE" 3 "csdid (CS 2020)" 5 "did_imputation (BJS 2021)" ///
                         7 "did_multiplegt (CD 2020)" 9 "eventstudyinteract (SA 2020)" ///
                         11 "stackedev (CDLZ 2019)" 13 "did2s (G 2021)") ///
                   pos(6) rows(3) region(style(none)) size(small)) ///
            xline(-0.5, lc(gs8) lp(dash)) ///
            yline(0,    lc(gs8) lp(dash)) ///
            graphregion(color(white)) plotregion(color(white)) ///
        )

    graph export "${figdir}/`prefix'-dd_methods_`Y'.png", as(png) replace width(1200) height(800)
    graph export "${latex_figdir}/`prefix'-dd_methods_`Y'.png", as(png) replace width(1200) height(800)

    * ---------------------------------------------------------------------
    * For per-100k outcomes only: also produce a no-csdid version
    * (figures 20 and 21).
    * ---------------------------------------------------------------------
    if (strpos("`Y'", "_per_100k") > 0) {
        local nc_prefix : display %02.0f `nocsdid_plotnum'

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
                title("Event Study Robustness (no csdid): `label'", size(medsmall)) ///
                xtitle("Years relative to Medicaid expansion") ///
                ytitle("Average effect") xlabel(-`maxlead'(1)`maxlag') ///
                legend(order(1 "TWFE" 3 "did_imputation (BJS 2021)" ///
                             5 "did_multiplegt (CD 2020)" 7 "eventstudyinteract (SA 2020)" ///
                             9 "stackedev (CDLZ 2019)" 11 "did2s (G 2021)") ///
                       pos(6) rows(3) region(style(none)) size(small)) ///
                xline(-0.5, lc(gs8) lp(dash)) ///
                yline(0,    lc(gs8) lp(dash)) ///
                graphregion(color(white)) plotregion(color(white)) ///
            )

        graph export "${figdir}/`nc_prefix'-dd_methods_`Y'_nocsdid.png", as(png) replace width(1200) height(800)
        graph export "${latex_figdir}/`nc_prefix'-dd_methods_`Y'_nocsdid.png", as(png) replace width(1200) height(800)

        local ++nocsdid_plotnum
    }

    local ++plotnum
}

di ""
di "=================================================================="
di "DiD methods comparison completed."
di "Figures:"
di "  - ${figdir}/{05..}-dd_methods_*.png (and LaTeX copies)"
di "=================================================================="

log close
