* =============================================================================
* YEAR-VARYING per-capita robustness: not-yet-treated design + HonestDiD.
* Primary outcome = matched positions per contemporary 100,000 (ACS).
* Figures (semantic): appx-notyet, appx-honestdid-headline, appx-honestdid-nonresp.
* Not-yet-treated is estimated over horizons 0..4 (the last-treated cohort has no
* valid comparison at +5 under autosample), so the event study runs -5..+4.
* The not-yet figure carries a text box with baseline, avg post effect (%),
* treatment p, and PRE-TREND p.
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
log using "${topdir}/output/22-yearvarying-robustness.log", replace

* FULL 2000-2019 PANEL (activity-window coding is primary; see script 06)
use "${datadir}/panel_2000_2019_estimation.dta", clear
replace matched = matched_na
replace quota   = quota_na
gen double matched_per_100k = matched / total_population_10 * 100000
gen double quota_per_100k   = quota   / total_population_10 * 100000
gen double unmatched        = quota - matched
egen program_numeric_id = group(state institution_code)
replace state = strtrim(upper(state))
* pop_yr already in the panel (2000-2019 series from script 03)
quietly count if missing(pop_yr)
di as text "pop_yr merge: " r(N) " unmatched master rows (missing pop_yr)"
assert r(N) == 0
gen double matched_per_100k_yr = matched / pop_yr * 100000
preserve
    import delimited "${rawdir}/gme_formula_classification.csv", clear varnames(1) stringcols(_all)
    keep state gme_formula gme_formula_2015
    replace state = strtrim(upper(state))
    tempfile gme
    save `gme'
restore
merge m:1 state using `gme', keep(master match) nogen
gen byte gme_notvol = inlist(gme_formula, "fixed", "none")
gen byte gme_vol    = (gme_formula == "volume")
* 2015 vintage arms (referee response MUST-8: mechanism inside not-yet-treated)
gen byte gme_vol15    = (gme_formula_2015 == "volume")
gen byte gme_notvol15 = inlist(gme_formula_2015, "fixed", "none")
* Urban/rural (as in 20/14): rural = RUCA > 3, missing RUCA excluded
capture confirm variable rural_urban_2010
if _rc == 0 gen byte urban_rural = (rural_urban_2010 > 3) if !missing(rural_urban_2010)
encode state, gen(state_id)
xtset program_numeric_id year
tempfile master
save `master'

* Shared event-study plotting helpers (_esplot, _fillcoef)
do "${topdir}/programs/_esplot-helpers.do"

* ---------------- 1) NOT-YET-TREATED (drop never-expansion), horizons 0..4 -----
use "`master'", clear
keep if treated_state == 1
di _n "=== NOT-YET-TREATED (year-varying), horizons 0/4 ==="
did_imputation matched_per_100k_yr program_numeric_id year year_expanded ///
    [aw=total_population_10], horizons(0/4) pretrend(10) ///
    fe(program_numeric_id year) cluster(state_id) minn(0) autosample
lincom (tau0+tau1+tau2+tau3+tau4)/5
local a  = r(estimate)
local ase = r(se)
local pt = .
capture test pre1 pre2 pre3 pre4 pre5 pre6 pre7 pre8 pre9 pre10 pre6 pre7 pre8 pre9 pre10
if _rc == 0 local pt = r(p)
local tp = .
capture test tau0 tau1 tau2 tau3 tau4
if _rc == 0 local tp = r(p)
quietly summarize matched_per_100k_yr if year < year_expanded [aw=total_population_10]
local b = r(mean)
local pct = cond(`b' < . & `b' != 0, 100*`a'/`b', .)
di as result "notyet-yv: avg=" %9.5f `a' " pct=" %5.1f `pct' " treat_p=" %6.3f `tp' " pretrend_p=" %6.3f `pt'
* Citable summary CSV (not-yet-treated + balanced 2014 cohort).
tempname nyres
tempfile nyfile
postfile `nyres' str24 spec double avg_treat avg_se treat_p pretrend_p baseline pct ///
    using "`nyfile'", replace
post `nyres' ("notyet") (`a') (`ase') (`tp') (`pt') (`b') (`pct')
* event-study figure over horizons -5..+4 via shared helpers
_fillcoef 4
_esplot "appx-notyet" "Treatment Effect (per 100,000, year-varying pop.)" "" `a' `b' `pct' `tp' `pt' 4

* ---------------- 1a) MECHANISM SPLIT INSIDE THE NOT-YET-TREATED DESIGN -------
* Referee response (editorial decision 2026-07-24, MUST-8 and Section 2):
* "make the not-yet-treated design the primary specification and run the
* mechanism split inside it, under the 2015 classification, with its own
* leave-one-out and its own SE and CI." The not-yet-treated design drops the
* never-expansion group entirely, so it is immune to the control-group GME
* contamination concern (F5). Arms are estimated split-sample (each arm's
* later adopters serve as its controls); the cross-arm difference comes from
* the pooled hetby model. Both classification vintages are reported; the
* 2015 vintage is the post-period-correct one.
foreach v in 15 12 {
    local volvar = cond(`v'==15, "gme_vol15", "gme_vol")
    local nvlvar = cond(`v'==15, "gme_notvol15", "gme_notvol")
    foreach grp in volume notvolume {
        use "`master'", clear
        keep if treated_state == 1
        if "`grp'"=="volume"    keep if `volvar' == 1
        if "`grp'"=="notvolume" keep if `nvlvar' == 1
        di _n "=== NOT-YET mech `grp' (c20`v'), horizons 0/4 ==="
        capture noisily did_imputation matched_per_100k_yr program_numeric_id year year_expanded ///
            [aw=total_population_10], horizons(0/4) pretrend(10) ///
            fe(program_numeric_id year) cluster(state_id) minn(0) autosample
        if (_rc != 0) {
            di as error "notyet mech `grp' c20`v' failed (rc=" _rc ")"
            post `nyres' ("nymech_`grp'_c`v'") (.) (.) (.) (.) (.) (.)
            continue
        }
        capture lincom (tau0+tau1+tau2+tau3+tau4)/5
        local a   = cond(_rc==0, r(estimate), .)
        local ase = cond(_rc==0, r(se), .)
        local pt = .
        capture test pre1 pre2 pre3 pre4 pre5 pre6 pre7 pre8 pre9 pre10 pre6 pre7 pre8 pre9 pre10
        if _rc == 0 local pt = r(p)
        local tp = .
        capture test tau0 tau1 tau2 tau3 tau4
        if _rc == 0 local tp = r(p)
        quietly summarize matched_per_100k_yr if year < year_expanded [aw=total_population_10]
        local b = r(mean)
        local pct = cond(`b' < . & `b' != 0, 100*`a'/`b', .)
        post `nyres' ("nymech_`grp'_c`v'") (`a') (`ase') (`tp') (`pt') (`b') (`pct')
        di as result "notyet mech `grp' c20`v': avg=" %9.5f `a' " se=" %9.5f `ase' ///
            " pct=" %5.1f `pct' " treat_p=" %6.3f `tp' " pretrend_p=" %6.3f `pt'
        if (`v'==15) {
            local fname = cond("`grp'"=="volume", "appx-notyet-mech-volume", "appx-notyet-mech-nonresp")
            _fillcoef 4
            _esplot "`fname'" "Treatment Effect (per 100,000, year-varying pop.)" "" `a' `b' `pct' `tp' `pt' 4
        }
    }
    * cross-arm difference inside the not-yet design (pooled hetby)
    use "`master'", clear
    keep if treated_state == 1 & (`volvar' == 1 | `nvlvar' == 1)
    local mdd  = .
    local mdse = .
    local mdp  = .
    capture did_imputation matched_per_100k_yr program_numeric_id year year_expanded ///
        [aw=total_population_10], horizons(0/4) pretrend(10) cluster(state_id) ///
        hetby(`volvar') fe(program_numeric_id year) minn(0) autosample
    if (_rc == 0) {
        capture nlcom (_b[tau0_1]+_b[tau1_1]+_b[tau2_1]+_b[tau3_1]+_b[tau4_1])/5 ///
                    - (_b[tau0_0]+_b[tau1_0]+_b[tau2_0]+_b[tau3_0]+_b[tau4_0])/5
        if (_rc == 0) {
            matrix _mnl  = r(b)
            matrix _mnlV = r(V)
            local mdd  = _mnl[1,1]
            local mdse = sqrt(_mnlV[1,1])
            local mdp  = 2*normal(-abs(`mdd'/`mdse'))
        }
    }
    post `nyres' ("nymech_diff_c`v'") (`mdd') (`mdse') (`mdp') (.) (.) (.)
    di as result "notyet mech diff c20`v' (volume - nonresp) = " %9.5f `mdd' ///
        " (se=" %9.5f `mdse' ", p=" %6.3f `mdp' "; 95% CI [" ///
        %9.5f `mdd'-1.96*`mdse' ", " %9.5f `mdd'+1.96*`mdse' "])"
}

* Leave-one-state-out for the not-yet cross-arm difference (2015 vintage)
tempname nyloo
tempfile nyloofile
postfile `nyloo' str24 dropped_state double avg_treat avg_se using "`nyloofile'", replace
use "`master'", clear
keep if treated_state == 1 & (gme_vol15 == 1 | gme_notvol15 == 1)
tempfile nydiffsample
save `nydiffsample'
quietly levelsof state, local(nystates)
foreach st of local nystates {
    use "`nydiffsample'", clear
    drop if state == "`st'"
    local mdd = .
    local mdse = .
    capture did_imputation matched_per_100k_yr program_numeric_id year year_expanded ///
        [aw=total_population_10], horizons(0/4) pretrend(10) cluster(state_id) ///
        hetby(gme_vol15) fe(program_numeric_id year) minn(0) autosample
    if (_rc == 0) {
        capture nlcom (_b[tau0_1]+_b[tau1_1]+_b[tau2_1]+_b[tau3_1]+_b[tau4_1])/5 ///
                    - (_b[tau0_0]+_b[tau1_0]+_b[tau2_0]+_b[tau3_0]+_b[tau4_0])/5
        if (_rc == 0) {
            matrix _mnl  = r(b)
            matrix _mnlV = r(V)
            local mdd  = _mnl[1,1]
            local mdse = sqrt(_mnlV[1,1])
        }
    }
    post `nyloo' ("`st'") (`mdd') (`mdse')
    di as text "notyet mech diff drop `st': " %9.5f `mdd'
}
postclose `nyloo'
preserve
use "`nyloofile'", clear
export delimited using "${tabdir}/notyet-mechdiff-loo.csv", replace
restore

* ---------------- 1b) BALANCED COHORT: 2014 adopters only ---------------------
* Horizons +4/+5 of the pooled event study are identified only from early
* cohorts, so a monotonically widening path could reflect cohort composition.
* Restricting treatment to the (largest) 2014 cohort holds composition fixed:
* if the path still widens within this single cohort, the dynamics are genuine.
use "`master'", clear
keep if treated_state == 0 | year_expanded == 2014
di _n "=== BALANCED COHORT (2014 adopters + never-expansion), horizons 0/5 ==="
did_imputation matched_per_100k_yr program_numeric_id year year_expanded ///
    [aw=total_population_10], horizons(0/5) pretrend(10) ///
    fe(program_numeric_id year) cluster(state_id) minn(0) autosample
lincom (tau0+tau1+tau2+tau3+tau4+tau5)/6
local a  = r(estimate)
local ase = r(se)
local pt = .
capture test pre1 pre2 pre3 pre4 pre5 pre6 pre7 pre8 pre9 pre10 pre6 pre7 pre8 pre9 pre10
if _rc == 0 local pt = r(p)
local tp = .
capture test tau0 tau1 tau2 tau3 tau4 tau5
if _rc == 0 local tp = r(p)
quietly summarize matched_per_100k_yr if treated_state==1 & year < year_expanded [aw=total_population_10]
local b = r(mean)
local pct = cond(`b' < . & `b' != 0, 100*`a'/`b', .)
post `nyres' ("cohort2014") (`a') (`ase') (`tp') (`pt') (`b') (`pct')
di as result "cohort2014: avg=" %9.5f `a' " pct=" %5.1f `pct' " treat_p=" %6.3f `tp' " pretrend_p=" %6.3f `pt'
_fillcoef
_esplot "appx-cohort2014" "Treatment Effect (per 100,000, year-varying pop.)" "" `a' `b' `pct' `tp' `pt'

postclose `nyres'
preserve
use "`nyfile'", clear
list, clean noobs
export delimited using "${tabdir}/notyet-yearvarying-summary.csv", replace
restore

* ---------------- 2) HonestDiD on year-varying (headline + non-responsive) ----
honestdid _plugin_check
capture program drop _honest_yv
program define _honest_yv
    args fname tag
    did_imputation matched_per_100k_yr program_numeric_id year year_expanded ///
        [aw=total_population_10], horizons(0/5) pretrend(10) ///
        fe(program_numeric_id year) cluster(state_id) minn(0) autosample
    local order pre10 pre9 pre8 pre7 pre6 pre5 pre4 pre3 pre2 pre1 tau0 tau1 tau2 tau3 tau4 tau5
    matrix b0 = e(b)
    matrix V0 = e(V)
    matrix bb = J(1,16,.)
    matrix VV = J(16,16,.)
    forval i = 1/16 {
        local ni : word `i' of `order'
        matrix bb[1,`i'] = b0[1, colnumb(b0,"`ni'")]
    }
    forval i = 1/16 {
        local ni : word `i' of `order'
        local ri = colnumb(V0,"`ni'")
        forval j = 1/16 {
            local nj : word `j' of `order'
            matrix VV[`i',`j'] = V0[`ri', colnumb(V0,"`nj'")]
        }
    }
    local names t_m10 t_m9 t_m8 t_m7 t_m6 t_m5 t_m4 t_m3 t_m2 t_m1 t0 t1 t2 t3 t4 t5
    matrix colnames bb = `names'
    matrix colnames VV = `names'
    matrix rownames VV = `names'
    matrix lvec = J(6,1,1/6)
    di _n "=== HONESTDID [`tag'] year-varying ==="
    * no in-graph title (INV-12): the LaTeX subcaption labels the panel
    honestdid, b(bb) vcov(VV) pre(1/10) post(11/16) delta(rm) l_vec(lvec) ///
        mvec(0(0.1)2) coefplot ///
        ytitle("Average post ATT (per 100,000, year-varying)") ///
        graphregion(color(white)) plotregion(color(white))
    graph export "${figdir}/`fname'.png", as(png) replace width(1200) height(800)
    graph export "${latex_figdir}/`fname'.png", as(png) replace width(1200) height(800)
    graph export "${figdir}/`fname'.pdf", replace
    graph export "${latex_figdir}/`fname'.pdf", replace
end

use "`master'", clear
_honest_yv "appx-honestdid-headline" "headline"

use "`master'", clear
keep if treated_state == 0 | (treated_state == 1 & gme_notvol == 1)
_honest_yv "appx-honestdid-nonresp" "nonresponsive"

use "`master'", clear
keep if urban_rural == 0
_honest_yv "appx-honestdid-urban" "urban"


* ---------------- 3) PRE-ACA PLACEBO (expansion shifted back 10 years) --------
* Formerly the standalone long-panel placebo script. Each expansion state gets
* a placebo expansion 10 years before its real one; estimation on 2000-2013
* only (no real treatment in sample); never-expansion states as controls.
* Figures: appx-placebo-preaca (institution), appx-placebo-statetotal (state).
tempname plres
tempfile plfile
postfile `plres' str16 spec double avg_treat avg_se treat_p pretrend_p baseline pct ///
    using "`plfile'", replace

capture program drop _placebo_run
program define _placebo_run
    args outcome idvar tag resh fname yti
    capture noisily did_imputation `outcome' `idvar' year year_expanded_pl ///
        [aw=total_population_10], horizons(0/5) pretrend(4) ///
        fe(`idvar' year) cluster(state_id) minn(0) autosample
    if (_rc != 0) {
        di as error "`tag' failed (rc=" _rc ")"
        post `resh' ("`tag'") (.) (.) (.) (.) (.) (.)
        exit
    }
    capture lincom (tau0+tau1+tau2+tau3+tau4+tau5)/6
    local a   = cond(_rc==0, r(estimate), .)
    local ase = cond(_rc==0, r(se), .)
    local pt = .
    capture test pre1 pre2 pre3 pre4
    if _rc == 0 local pt = r(p)
    local tp = .
    capture test tau0 tau1 tau2 tau3 tau4 tau5
    if _rc == 0 local tp = r(p)
    quietly summarize `outcome' if !missing(year_expanded_pl) & year < year_expanded_pl [aw=total_population_10]
    local b = r(mean)
    local pct = cond(`b' < . & `b' != 0, 100*`a'/`b', .)
    post `resh' ("`tag'") (`a') (`ase') (`tp') (`pt') (`b') (`pct')
    di as result "`tag': avg=" %9.4f `a' " se=" %9.4f `ase' " pct=" %5.1f `pct' ///
        " treat_p=" %6.3f `tp' " pretrend_p=" %6.3f `pt'
    _fillcoef 5 4
    _esplot "`fname'" "`yti'" "" `a' `b' `pct' `tp' `pt' 5 4
end

use "`master'", clear
keep if year <= 2013
gen year_expanded_pl = year_expanded - 10
_placebo_run matched_per_100k_yr program_numeric_id "placebo_na" `plres' ///
    "appx-placebo-preaca" "Placebo Effect (per 100,000, year-varying pop.)"

use "`master'", clear
collapse (sum) matched_zf (first) pop_yr total_population_10 year_expanded treated_state, ///
    by(state state_id year)
gen double state_total_per_100k = matched_zf / pop_yr * 100000
keep if year <= 2013
gen year_expanded_pl = year_expanded - 10
xtset state_id year
_placebo_run state_total_per_100k state_id "placebo_state" `plres' ///
    "appx-placebo-statetotal" "Placebo Effect (state total per 100,000)"

postclose `plres'
preserve
use "`plfile'", clear
list, clean noobs
export delimited using "${tabdir}/preaca-placebo-summary.csv", replace
restore

di _n "=== year-varying robustness complete: appx-notyet, appx-cohort2014, appx-honestdid-*, appx-placebo-* ==="
log close
