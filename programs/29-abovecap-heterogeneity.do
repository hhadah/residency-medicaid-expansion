* =============================================================================
* ABOVE-CAP HETEROGENEITY: the hospital-level dose test
* ---------------------------------------------------------------------------
* Referee response (domain referee Major 8 / methods MC6d; editor SHOULD list:
* "the single test that would most change my assessment"). The Balanced Budget
* Act cap fixes Medicare-funded residents at 1996 levels; hospitals already
* training ABOVE their cap fund the marginal resident from operating margins,
* so any financing effect of expansion should be concentrated there. Cap
* status is measured PRE-EXPANSION (2010-2013 mean annualized cost-report
* FTEs vs the DGME resident cap) on the linked NRMP-CCN sample (script 08).
*
* Runs:
*   1. matched_per_100k_yr split by baseline cap status (above vs below),
*      each subsample keeping never-expansion controls of the same status,
*      plus the pooled hetby difference.
*   2. The same cap difference within the volume-responsive and
*      non-responsive arms (2015 classification), for completeness on the
*      (now negative) mechanism reading.
*
* Output: output/tables/abovecap-heterogeneity.csv
*         figures appx-abovecap-above, appx-abovecap-below
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

log using "${topdir}/output/29-abovecap-heterogeneity.log", replace

* FULL 2000-2019 PANEL + in-script funding merge (funding data span 2000-2023,
* so the linked comparison now covers the full panel window).
preserve
    use "${datadir}/gme_funding_expansion.dta", clear
    collapse (sum) dgme_payment ime_payment dgme_ftes months_covered ///
             (mean) dgme_resident_cap, by(provider_ccn fiscal_year)
    rename fiscal_year year
    tempfile funding_year
    save `funding_year'
restore
use "${datadir}/panel_2000_2019_estimation.dta", clear
replace matched = matched_na
replace quota   = quota_na
merge m:1 provider_ccn year using `funding_year', keep(master match) nogen
egen program_numeric_id = group(state institution_code)
gen double matched_per_100k_yr = matched / pop_yr * 100000

* Annualized FTEs (as in the linked-sample reconciliation)
gen double dgme_ftes_ann = dgme_ftes * 12 / months_covered ///
    if !missing(dgme_ftes) & months_covered > 0 & !missing(months_covered)

* ---------------------------------------------------------------------------
* Baseline (2010-2013) cap status per institution
* ---------------------------------------------------------------------------
preserve
    keep if year >= 2010 & year <= 2013 & !missing(provider_ccn)
    collapse (mean) base_ftes = dgme_ftes_ann base_cap = dgme_resident_cap, ///
        by(institution_code)
    keep if !missing(base_ftes) & !missing(base_cap) & base_cap > 0
    gen byte above_cap = base_ftes >= base_cap
    gen double cap_ratio = base_ftes / base_cap
    quietly count
    di as text "Institutions with baseline cap status: " r(N)
    quietly summarize above_cap
    di as text "Share above cap at baseline: " %5.3f r(mean)
    tempfile capstatus
    save `capstatus'
restore
merge m:1 institution_code using `capstatus', keep(master match) nogen

* GME formula arms (2015 vintage)
preserve
    import delimited "${rawdir}/gme_formula_classification.csv", clear varnames(1) stringcols(_all)
    keep state gme_formula_2015
    replace state = strtrim(upper(state))
    tempfile gme
    save `gme'
restore
merge m:1 state using `gme', keep(master match) nogen
gen byte vol15 = (gme_formula_2015 == "volume")
gen byte nvl15 = inlist(gme_formula_2015, "fixed", "none")

encode state, gen(state_id)
keep if !missing(above_cap)
xtset program_numeric_id year
tempfile master
save `master'

do "${topdir}/programs/_esplot-helpers.do"

tempname res
tempfile resfile
postfile `res' str24 spec double avg_treat avg_se treat_p pretrend_p baseline pct ///
    n_inst n_states using "`resfile'", replace

capture program drop _caprun
program define _caprun
    args tag resh fname yti
    di _n "==================== `tag' ===================="
    capture noisily did_imputation matched_per_100k_yr program_numeric_id year year_expanded ///
        [aw=total_population_10], horizons(0/5) pretrend(10) ///
        fe(program_numeric_id year) cluster(state_id) minn(0) autosample
    if (_rc != 0) {
        di as error "`tag' failed (rc=" _rc ")"
        post `resh' ("`tag'") (.) (.) (.) (.) (.) (.) (.) (.)
        exit
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
    local ni = .
    local ns = .
    capture levelsof program_numeric_id, local(hh)
    if (_rc == 0) local ni : word count `hh'
    capture levelsof state_id, local(ss)
    if (_rc == 0) local ns : word count `ss'
    post `resh' ("`tag'") (`a') (`ase') (`tp') (`pt') (`b') (`pct') (`ni') (`ns')
    di as result "`tag': avg=" %9.4f `a' " se=" %9.4f `ase' " pct=" %5.1f `pct' ///
        " treat_p=" %6.3f `tp' " pretrend_p=" %6.3f `pt'
    if ("`fname'" != "") {
        _fillcoef
        _esplot "`fname'" "`yti'" "" `a' `b' `pct' `tp' `pt'
    }
end

capture program drop _capdiff
program define _capdiff
    args tag resh
    local mdd  = .
    local mdse = .
    local mdp  = .
    capture did_imputation matched_per_100k_yr program_numeric_id year year_expanded ///
        [aw=total_population_10], horizons(0/5) pretrend(10) cluster(state_id) ///
        hetby(above_cap) fe(program_numeric_id year) minn(0) autosample
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
    post `resh' ("`tag'") (`mdd') (`mdse') (`mdp') (.) (.) (.) (.) (.)
    di as result "`tag' (above - below) = " %9.4f `mdd' " (se=" %9.4f `mdse' ", p=" %6.3f `mdp' ")"
end

* 1) Cap split, full linked sample
use "`master'", clear
keep if above_cap == 1
_caprun "above_cap" `res' "appx-abovecap-above" "Treatment Effect (per 100,000, above-cap institutions)"
use "`master'", clear
keep if above_cap == 0
_caprun "below_cap" `res' "appx-abovecap-below" "Treatment Effect (per 100,000, below-cap institutions)"
use "`master'", clear
_capdiff "cap_diff" `res'

* 2) Cap difference within 2015-vintage arms
use "`master'", clear
keep if treated_state == 0 | vol15 == 1
_capdiff "cap_diff_volume15" `res'
use "`master'", clear
keep if treated_state == 0 | nvl15 == 1
_capdiff "cap_diff_nonresp15" `res'

postclose `res'
use "`resfile'", clear
list, clean noobs
export delimited using "${tabdir}/abovecap-heterogeneity.csv", replace

di _n "=== above-cap heterogeneity complete ==="
log close
