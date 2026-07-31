* =============================================================================
* MECHANISM CLASSIFICATION SENSITIVITY: judgment-state flips + time-varying check
* ---------------------------------------------------------------------------
* The paper classifies states by the Medicaid GME formula rules in force
* during the 2014-2019 treatment window (gme_formula_2015; Henderson 2016
* AAMC survey). This script probes that classification:
*
*   1. Runs the three mechanism objects (volume arm, non-responsive arm,
*      cross-arm difference) on the NRMP panel under the 2015 classification.
*      TIME-VARYING NOTE: an exposure-weighted time-varying classification
*      assigns every expansion state the arm of the vintage covering >=5/6
*      of its post period, which is the 2015 classification for every state.
*      The script verifies this and reports it; the 2015 run IS the
*      time-varying run under majority-exposure assignment.
*   2. Runs the GME payment first stage (script 19 outcomes) by arm
*      (summary CSV; figures for asinh_dgme) - cross-check against script 19.
*   3. Sensitivity table: flips each recorded judgment state (MD, MN, MT,
*      IA, NM) between arms, one at a time, and reports the cross-arm
*      difference each time.
*
* Utah is resolved (fixed pool, SPA 4.19-A Sec 700) in the classification
* file; it is a non-expansion control and never enters the arms.
*
* Outputs: output/tables/reclassification-sensitivity.csv
*          output/tables/firststage-2015vintage-summary.csv
*          figures appx-mech2015-{volume,nonresp},
*                  appx-firststage2015-dgme-{volume,nonresp}
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

log using "${topdir}/output/25-mechanism-reclassification.log", replace

* ---------------------------------------------------------------------------
* Classification file (2015 payment rules)
* ---------------------------------------------------------------------------
import delimited "${rawdir}/gme_formula_classification.csv", clear varnames(1) stringcols(_all)
keep state gme_formula_2015
replace state = strtrim(upper(state))
assert gme_formula_2015 != "TODO"
tempfile gme
save `gme'

* ---------------------------------------------------------------------------
* NRMP panel prep (as in script 20)
* ---------------------------------------------------------------------------
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
assert r(N) == 0
gen double matched_per_100k_yr = matched / pop_yr * 100000
merge m:1 state using `gme', keep(master match) nogen
encode state, gen(state_id)
xtset program_numeric_id year

* Arm indicators (2015 classification)
gen byte vol15 = (gme_formula_2015 == "volume")
gen byte nvl15 = inlist(gme_formula_2015, "fixed", "none")

* Exposure-weighted time-varying assignment check: share of each expansion
* state's post window (year_expanded..2019) governed by the 2015 vintage
preserve
    keep if treated_state == 1
    bysort state: keep if _n == 1
    gen post_years  = 2019 - year_expanded + 1
    gen years_2015v = 2019 - max(year_expanded, 2015) + 1
    gen share_2015  = years_2015v / post_years
    quietly count if share_2015 < 0.5
    di as text "Expansion states with <50% of post window under the 2015 vintage: " r(N)
    assert r(N) == 0
    di as text "=> majority-exposure time-varying classification == 2015 vintage for all states"
restore

tempfile master
save `master'

do "${topdir}/programs/_esplot-helpers.do"

tempname res
tempfile resfile
postfile `res' str12 classification str24 spec str8 flipped double avg_treat ///
    avg_se treat_p pretrend_p baseline pct using "`resfile'", replace

* ---------------------------------------------------------------------------
* Program: run the three mechanism objects for a given arm pair
* (volvar/nvlvar are byte indicators over expansion states)
* ---------------------------------------------------------------------------
capture program drop _mechrun
program define _mechrun
    args volvar nvlvar cls flipped resh dofigs
    * arms
    foreach grp in volume notvolume {
        preserve
        if "`grp'"=="volume"    keep if treated_state==0 | (treated_state==1 & `volvar'==1)
        if "`grp'"=="notvolume" keep if treated_state==0 | (treated_state==1 & `nvlvar'==1)
        capture noisily did_imputation matched_per_100k_yr program_numeric_id year year_expanded ///
            [aw=total_population_10], horizons(0/5) pretrend(10) ///
            fe(program_numeric_id year) cluster(state_id) minn(0) autosample
        if (_rc != 0) {
            post `resh' ("`cls'") ("mech_`grp'") ("`flipped'") (.) (.) (.) (.) (.) (.)
            restore
            continue
        }
        capture lincom (tau0+tau1+tau2+tau3+tau4+tau5)/6
        local a   = cond(_rc==0, r(estimate), .)
        local ase = cond(_rc==0, r(se), .)
        local pt = .
        capture test pre1 pre2 pre3 pre4 pre5 pre6 pre7 pre8 pre9 pre10 pre6 pre7 pre8 pre9 pre10
        if _rc == 0 local pt = r(p)
        local tp = .
        capture test tau0 tau1 tau2 tau3 tau4 tau5
        if _rc == 0 local tp = r(p)
        quietly summarize matched_per_100k_yr if treated_state==1 & year<year_expanded [aw=total_population_10]
        local b = r(mean)
        local pct = cond(`b' < . & `b' != 0, 100*`a'/`b', .)
        post `resh' ("`cls'") ("mech_`grp'") ("`flipped'") (`a') (`ase') (`tp') (`pt') (`b') (`pct')
        di as result "[`cls'`flipped'] mech_`grp': avg=" %9.4f `a' " se=" %9.4f `ase' ///
            " pct=" %5.1f `pct' " treat_p=" %6.3f `tp' " pretrend_p=" %6.3f `pt'
        if ("`dofigs'" == "figs") {
            local fname = cond("`grp'"=="volume", "appx-mech2015-volume", "appx-mech2015-nonresp")
            _fillcoef
            _esplot "`fname'" "Treatment Effect (per 100,000, year-varying pop.)" "" `a' `b' `pct' `tp' `pt'
        }
        restore
    }
    * cross-arm difference (pooled hetby)
    preserve
    keep if treated_state == 0 | `volvar' == 1 | `nvlvar' == 1
    quietly capture did_imputation matched_per_100k_yr program_numeric_id year year_expanded ///
        [aw=total_population_10], horizons(0/5) pretrend(10) cluster(state_id) ///
        hetby(`volvar') fe(program_numeric_id year) minn(0) autosample
    local mdd  = .
    local mdse = .
    local mdp  = .
    if (_rc == 0) {
        capture nlcom (_b[tau0_1]+_b[tau1_1]+_b[tau2_1]+_b[tau3_1]+_b[tau4_1]+_b[tau5_1])/6 ///
                    - (_b[tau0_0]+_b[tau1_0]+_b[tau2_0]+_b[tau3_0]+_b[tau4_0]+_b[tau5_0])/6
        if (_rc == 0) {
            matrix _mnl  = r(b)
            matrix _mnlV = r(V)
            local mdd  = _mnl[1,1]
            local mdse = sqrt(_mnlV[1,1])
            local mdp  = 2*normal(-abs(`mdd'/`mdse'))
        }
    }
    post `resh' ("`cls'") ("mech_diff") ("`flipped'") (`mdd') (`mdse') (`mdp') (.) (.) (.)
    di as result "[`cls'`flipped'] mech_diff (volume - nonresp) = " %9.4f `mdd' ///
        " (se=" %9.4f `mdse' ", p=" %6.3f `mdp' ")"
    restore
end

* ---------------------------------------------------------------------------
* 1) Baseline run (2015 classification)
* ---------------------------------------------------------------------------
use "`master'", clear
di _n "==================== 2015 CLASSIFICATION (paper baseline) ===================="
_mechrun vol15 nvl15 "c2015" "" `res' "figs"

* ---------------------------------------------------------------------------
* 2) Judgment-state flips (MD, MN, MT, IA, NM), one at a time
* ---------------------------------------------------------------------------
foreach jst in MD MN MT IA NM {
    use "`master'", clear
    gen byte volf = vol15
    gen byte nvlf = nvl15
    quietly replace volf = 1 - volf if state == "`jst'"
    quietly replace nvlf = 1 - volf if state == "`jst'"
    di _n "==================== FLIP `jst' ===================="
    _mechrun volf nvlf "c2015" "`jst'" `res' ""
}

postclose `res'
use "`resfile'", clear
list, clean noobs
export delimited using "${tabdir}/reclassification-sensitivity.csv", replace

* ---------------------------------------------------------------------------
* 3) GME payment first stage under the 2015 vintage (hospital panel, as 20)
* ---------------------------------------------------------------------------
use "${datadir}/gme_funding_expansion.dta", clear
drop if missing(expansion_state)
egen provider_numeric_id = group(state provider_ccn)
collapse (sum)  dgme_payment ime_payment dgme_ftes months_covered ///
        (first) state year_expanded expanded_ever, ///
        by(provider_numeric_id fiscal_year)
* annualize by months covered (as in scripts 18/19/24)
foreach v in dgme_payment ime_payment dgme_ftes {
    replace `v' = `v' * 12 / months_covered if months_covered > 0 & !missing(months_covered)
}
encode state, gen(state_id)
gen byte treated_state = expanded_ever
xtset provider_numeric_id fiscal_year
gen double asinh_dgme = asinh(dgme_payment)
gen double asinh_ime  = asinh(ime_payment)
gen double asinh_dgme_ftes = asinh(dgme_ftes)
replace state = strtrim(upper(state))
merge m:1 state using `gme', keep(master match) nogen
gen byte vol15 = (gme_formula_2015 == "volume")
gen byte nvl15 = inlist(gme_formula_2015, "fixed", "none")
tempfile fsmaster
save `fsmaster'

tempname fs
tempfile fsfile
postfile `fs' str20 outcome str12 group double avg_treat avg_se treat_p ///
    pretrend_p baseline pct using "`fsfile'", replace

foreach outcome in asinh_dgme asinh_ime asinh_dgme_ftes {
    foreach grp in volume notvolume {
        use "`fsmaster'", clear
        if "`grp'"=="volume"    keep if treated_state==0 | (treated_state==1 & vol15==1)
        if "`grp'"=="notvolume" keep if treated_state==0 | (treated_state==1 & nvl15==1)
        di _n "========== FIRST STAGE (2015 vintage): `outcome' / `grp' =========="
        capture noisily did_imputation `outcome' provider_numeric_id fiscal_year year_expanded, ///
            horizons(0/5) pretrend(10) fe(provider_numeric_id fiscal_year) ///
            cluster(state_id) minn(0) autosample
        if (_rc != 0) {
            post `fs' ("`outcome'") ("`grp'") (.) (.) (.) (.) (.) (.)
            continue
        }
        capture lincom (tau0+tau1+tau2+tau3+tau4+tau5)/6
        local a   = cond(_rc==0, r(estimate), .)
        local ase = cond(_rc==0, r(se), .)
        local pt = .
        capture test pre1 pre2 pre3 pre4 pre5 pre6 pre7 pre8 pre9 pre10 pre6 pre7 pre8 pre9 pre10
        if _rc == 0 local pt = r(p)
        local tp = .
        capture test tau0 tau1 tau2 tau3 tau4 tau5
        if _rc == 0 local tp = r(p)
        quietly summarize `outcome' if treated_state==1 & fiscal_year < year_expanded
        local b = r(mean)
        local pct = cond(`b' < . & `b' != 0, 100*`a'/`b', .)
        post `fs' ("`outcome'") ("`grp'") (`a') (`ase') (`tp') (`pt') (`b') (`pct')
        di as result "[c2015] `outcome' / `grp': avg=" %8.3f `a' " se=" %8.3f `ase' ///
            " pct=" %5.1f `pct' " treat_p=" %6.3f `tp' " pretrend_p=" %7.5f `pt'
        if ("`outcome'" == "asinh_dgme") {
            local fgrp = cond("`grp'"=="volume", "volume", "nonresp")
            _fillcoef
            _esplot "appx-firststage2015-dgme-`fgrp'" "Treatment Effect (asinh dollars)" "" `a' `b' `pct' `tp' `pt'
        }
    }
    * cross-arm difference
    use "`fsmaster'", clear
    keep if treated_state == 0 | vol15 == 1 | nvl15 == 1
    capture did_imputation `outcome' provider_numeric_id fiscal_year year_expanded, ///
        horizons(0/5) pretrend(10) fe(provider_numeric_id fiscal_year) ///
        cluster(state_id) minn(0) autosample hetby(vol15)
    local mdd  = .
    local mdse = .
    local mdp  = .
    if (_rc == 0) {
        capture nlcom (_b[tau0_1]+_b[tau1_1]+_b[tau2_1]+_b[tau3_1]+_b[tau4_1]+_b[tau5_1])/6 ///
                    - (_b[tau0_0]+_b[tau1_0]+_b[tau2_0]+_b[tau3_0]+_b[tau4_0]+_b[tau5_0])/6
        if (_rc == 0) {
            matrix _mnl  = r(b)
            matrix _mnlV = r(V)
            local mdd  = _mnl[1,1]
            local mdse = sqrt(_mnlV[1,1])
            local mdp  = 2*normal(-abs(`mdd'/`mdse'))
        }
    }
    post `fs' ("`outcome'") ("diff") (`mdd') (`mdse') (`mdp') (.) (.) (.)
    di as result "[c2015] `outcome' / diff = " %8.3f `mdd' " (se=" %8.3f `mdse' ", p=" %6.3f `mdp' ")"
}

postclose `fs'
use "`fsfile'", clear
list, clean noobs
export delimited using "${tabdir}/firststage-2015vintage-summary.csv", replace

di _n "=== mechanism reclassification complete ==="
log close
