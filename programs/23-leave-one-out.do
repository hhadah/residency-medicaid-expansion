* =============================================================================
* Leave-one-state-out influence diagnostics: headline AND mechanism objects.
* ---------------------------------------------------------------------------
* Revised 2026-07-24 for the referee response (editorial decision MUST-4 /
* cluster F3). Changes relative to the submitted version:
*   1. The full-sample benchmark now runs with AUTOSAMPLE, so the benchmark
*      and the leave-one-out runs are estimated on like-for-like samples
*      (methods referee MC3: "the single most consequential comparison in
*      the paper is between two slightly different samples").
*   2. LOO extends to CONTROL (never-expansion) states, including TX and FL.
*   3. LOO runs for all three mechanism objects -- volume arm, non-responsive
*      arm, and the pooled cross-arm difference -- under BOTH classification
*      vintages (2012 baseline and 2015), covering CA, NY, IL, OH and every
*      other state in each estimation sample.
*   4. Figures now show 95% CIs (SEs were already in the posted file).
*
* Outputs: output/tables/leave-one-out-summary.csv
*          figures appx-loo (headline, with CIs),
*                  appx-loo-mechdiff-{2012,2015} (cross-arm difference)
* =============================================================================

clear all
set more off

* Replication-friendly path handling: run from the repository root, or set
* global topdir before running.
if "${topdir}" == "" global topdir "`c(pwd)'"
capture confirm file "${topdir}/programs/00-README-pipeline.md"
if _rc {
    di as error "Cannot find the repository root. Run from the repo root or set global topdir."
    exit 601
}
global datadir "${topdir}/data/datasets"
global rawdir  "${topdir}/data/raw"
global figdir  "${topdir}/output/figures"
global tabdir  "${topdir}/output/tables"
global latex_figdir "${topdir}/my_paper/figures"
cap mkdir "${figdir}"
cap mkdir "${tabdir}"
cap mkdir "${latex_figdir}"

log using "${topdir}/output/23-leave-one-out.log", replace

* FULL 2000-2019 PANEL (activity-window coding is primary; see script 06)
use "${datadir}/panel_2000_2019_estimation.dta", clear
replace matched = matched_na
replace quota   = quota_na
gen double matched_per_100k = matched / total_population_10 * 100000
gen double quota_per_100k   = quota   / total_population_10 * 100000
gen double unmatched        = quota - matched
replace state = strtrim(upper(state))
* pop_yr already in the panel (2000-2019 series from script 03)
quietly count if missing(pop_yr)
assert r(N) == 0
gen double matched_per_100k_yr = matched / pop_yr * 100000
egen program_numeric_id = group(state institution_code)
encode state, gen(state_id)
xtset program_numeric_id year

* GME formula arms under both vintages
preserve
    import delimited "${rawdir}/gme_formula_classification.csv", clear varnames(1) stringcols(_all)
    keep state gme_formula gme_formula_2015
    replace state = strtrim(upper(state))
    tempfile gme
    save `gme'
restore
merge m:1 state using `gme', keep(master match) nogen
gen byte vol12 = (gme_formula == "volume")
gen byte nvl12 = inlist(gme_formula, "fixed", "none")
gen byte vol15 = (gme_formula_2015 == "volume")
gen byte nvl15 = inlist(gme_formula_2015, "fixed", "none")

tempfile master
save `master'

tempname loo
tempfile loo_file
postfile `loo' str16 spec str8 classification str24 dropped_state ///
    str8 dropped_type double avg_treat avg_se using "`loo_file'", replace

* ---------------------------------------------------------------------------
* Helper: headline average post ATT on the data in memory (autosample always)
* Returns r(avg), r(se); rc != 0 signalled by missing values.
* ---------------------------------------------------------------------------
capture program drop _avg_headline
program define _avg_headline, rclass
    return scalar avg = .
    return scalar se  = .
    capture did_imputation matched_per_100k_yr program_numeric_id year year_expanded ///
        [aw=total_population_10], horizons(0/5) pretrend(10) ///
        fe(program_numeric_id year) cluster(state_id) minn(0) autosample
    if (_rc != 0) exit
    capture lincom (tau0+tau1+tau2+tau3+tau4+tau5)/6
    if (_rc != 0) exit
    return scalar avg = r(estimate)
    return scalar se  = r(se)
end

* Helper: cross-arm difference via hetby on the data in memory
capture program drop _avg_mechdiff
program define _avg_mechdiff, rclass
    args volvar
    return scalar avg = .
    return scalar se  = .
    capture did_imputation matched_per_100k_yr program_numeric_id year year_expanded ///
        [aw=total_population_10], horizons(0/5) pretrend(10) cluster(state_id) ///
        hetby(`volvar') fe(program_numeric_id year) minn(0) autosample
    if (_rc != 0) exit
    capture nlcom (_b[tau0_1]+_b[tau1_1]+_b[tau2_1]+_b[tau3_1]+_b[tau4_1]+_b[tau5_1])/6 ///
                - (_b[tau0_0]+_b[tau1_0]+_b[tau2_0]+_b[tau3_0]+_b[tau4_0]+_b[tau5_0])/6
    if (_rc != 0) exit
    matrix _dnl  = r(b)
    matrix _dnlV = r(V)
    return scalar avg = _dnl[1,1]
    return scalar se  = sqrt(_dnlV[1,1])
end

* ---------------------------------------------------------------------------
* 1) HEADLINE: benchmark (autosample) + LOO over treated AND control states
* ---------------------------------------------------------------------------
use "`master'", clear
_avg_headline
local full_avg = r(avg)
local full_se  = r(se)
post `loo' ("headline") ("") ("FULL SAMPLE") ("full") (`full_avg') (`full_se')
di as result "headline full sample (autosample): avg = " %8.4f `full_avg' " (se " %8.4f `full_se' ")"

quietly levelsof state, local(allstates)
foreach st of local allstates {
    use "`master'", clear
    quietly summarize treated_state if state == "`st'"
    local dtype = cond(r(mean) > 0, "treated", "control")
    drop if state == "`st'"
    _avg_headline
    post `loo' ("headline") ("") ("`st'") ("`dtype'") (r(avg)) (r(se))
    di as text "headline drop `st' (`dtype'): avg = " %8.4f r(avg)
}

* ---------------------------------------------------------------------------
* 2) MECHANISM ARMS + DIFFERENCE: LOO under both vintages
* ---------------------------------------------------------------------------
foreach v in 12 15 {
    * -- arms --
    foreach grp in volume notvolume {
        local armvar = cond("`grp'"=="volume", "vol`v'", "nvl`v'")
        use "`master'", clear
        keep if treated_state == 0 | (treated_state == 1 & `armvar' == 1)
        tempfile armsample
        save `armsample'
        _avg_headline
        post `loo' ("mech_`grp'") ("c20`v'") ("FULL SAMPLE") ("full") (r(avg)) (r(se))
        di as result "[c20`v'] mech_`grp' full: avg = " %8.4f r(avg)
        quietly levelsof state if treated_state == 1, local(armstates)
        foreach st of local armstates {
            use "`armsample'", clear
            drop if state == "`st'"
            _avg_headline
            post `loo' ("mech_`grp'") ("c20`v'") ("`st'") ("treated") (r(avg)) (r(se))
        }
    }
    * -- cross-arm difference --
    use "`master'", clear
    keep if treated_state == 0 | vol`v' == 1 | nvl`v' == 1
    tempfile diffsample
    save `diffsample'
    _avg_mechdiff vol`v'
    post `loo' ("mech_diff") ("c20`v'") ("FULL SAMPLE") ("full") (r(avg)) (r(se))
    di as result "[c20`v'] mech_diff full: avg = " %8.4f r(avg) " (se " %8.4f r(se) ")"
    quietly levelsof state if treated_state == 1, local(trstates)
    foreach st of local trstates {
        use "`diffsample'", clear
        drop if state == "`st'"
        _avg_mechdiff vol`v'
        post `loo' ("mech_diff") ("c20`v'") ("`st'") ("treated") (r(avg)) (r(se))
        di as text "[c20`v'] mech_diff drop `st': " %8.4f r(avg)
    }
}

postclose `loo'

* ---------------------------------------------------------------------------
* Summary + figures (with 95% CIs)
* ---------------------------------------------------------------------------
use "`loo_file'", clear
export delimited using "${tabdir}/leave-one-out-summary.csv", replace

* headline figure
preserve
keep if spec == "headline"
quietly summarize avg_treat if dropped_state == "FULL SAMPLE"
local bench = r(mean)
keep if dropped_state != "FULL SAMPLE" & !missing(avg_treat)
gen double ci_lo = avg_treat - 1.96*avg_se
gen double ci_hi = avg_treat + 1.96*avg_se
gsort avg_treat
gen long rank = _n
quietly count
local n = r(N)
forvalues i = 1/`n' {
    local s = dropped_state[`i']
    local t = dropped_type[`i']
    label define rlab `i' "`s'", add
}
label values rank rlab
twoway (rcap ci_hi ci_lo rank, horizontal lcolor(gs10)) ///
    (scatter rank avg_treat if dropped_type=="treated", mcolor(navy) msymbol(circle) msize(small)) ///
    (scatter rank avg_treat if dropped_type=="control", mcolor(orange) msymbol(triangle) msize(small)) ///
    , xline(`bench', lcolor(maroon) lpattern(dash)) ///
    xline(0, lcolor(black) lwidth(thin)) ///
    ylabel(1(1)`n', valuelabel labsize(tiny) angle(0) nogrid) ///
    xlabel(, labsize(small) format(%9.3f)) ///
    ytitle("Dropped state", size(small)) ///
    xtitle("Leave-one-out average post effect, 95% CI (per 100,000, year-varying pop.)", size(small)) ///
    legend(order(2 "Treated state dropped" 3 "Control state dropped") size(small) region(lstyle(none))) ///
    graphregion(color(white)) plotregion(color(white)) ysize(9) xsize(5)
graph export "${figdir}/appx-loo.png", as(png) replace width(1000) height(1600)
graph export "${latex_figdir}/appx-loo.png", as(png) replace width(1000) height(1600)
graph export "${figdir}/appx-loo.pdf", replace
graph export "${latex_figdir}/appx-loo.pdf", replace
restore

* mechanism-difference figures, one per vintage
foreach v in 12 15 {
    preserve
    keep if spec == "mech_diff" & classification == "c20`v'"
    quietly summarize avg_treat if dropped_state == "FULL SAMPLE"
    local bench = r(mean)
    keep if dropped_state != "FULL SAMPLE" & !missing(avg_treat)
    gen double ci_lo = avg_treat - 1.96*avg_se
    gen double ci_hi = avg_treat + 1.96*avg_se
    gsort avg_treat
    gen long rank = _n
    quietly count
    local n = r(N)
    capture label drop rlab`v'
    forvalues i = 1/`n' {
        local s = dropped_state[`i']
        label define rlab`v' `i' "`s'", add
    }
    label values rank rlab`v'
    twoway (rcap ci_hi ci_lo rank, horizontal lcolor(gs10)) ///
        (scatter rank avg_treat, mcolor(navy) msymbol(circle) msize(small)) ///
        , xline(`bench', lcolor(maroon) lpattern(dash)) ///
        xline(0, lcolor(black) lwidth(thin)) ///
        ylabel(1(1)`n', valuelabel labsize(tiny) angle(0) nogrid) ///
        xlabel(, labsize(small) format(%9.3f)) ///
        ytitle("Dropped state", size(small)) ///
        xtitle("Leave-one-out cross-arm difference, 95% CI (20`v' classification)", size(small)) ///
        legend(off) graphregion(color(white)) plotregion(color(white)) ysize(7) xsize(5)
    graph export "${figdir}/appx-loo-mechdiff-20`v'.png", as(png) replace width(1000) height(1400)
    graph export "${latex_figdir}/appx-loo-mechdiff-20`v'.png", as(png) replace width(1000) height(1400)
    graph export "${figdir}/appx-loo-mechdiff-20`v'.pdf", replace
    graph export "${latex_figdir}/appx-loo-mechdiff-20`v'.pdf", replace
    restore
}

di _n "=== leave-one-out complete: appx-loo, appx-loo-mechdiff-{2012,2015}, leave-one-out-summary.csv ==="
log close
