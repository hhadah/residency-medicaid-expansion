* =============================================================================
* ENTRY/EXIT ESTIMATION: the headline with the extensive margin handled right
* ---------------------------------------------------------------------------
* Referee response (editorial decision 2026-07-24, MUST-7 / cluster F6).
* Uses panel_2000_2019_estimation.dta (script 06): institution-years outside an
* institution's [first_appears, last_active] window are missing, not zero.
*
* Specifications:
*   1. na_coded        : headline with entering/exiting years coded missing
*   2. balanced_only   : institutions active in all ten years
*   3. state_total     : state-level TOTAL matched per 100k INCLUDING entrants
*                        (entry is genuine capacity, so pre-entry zeros are
*                        correct at the state level)
*   4. state_total_bal : state totals from always-active institutions only --
*                        the (3)-(4) contrast is the entrant contribution
*   5. no_sas_entrants : headline dropping Single Accreditation System-window
*                        entrant institutions (2016+ first appearance), the
*                        old timing PROXY for the AOA->ACGME migration
*
* LIST-BASED SAS classification (desk review 2026-07-26 return condition;
* scripts 38-39 build data/datasets/sas_entrant_classification.csv from the
* ACGME ADS public SAS applicant lists, plus manual overrides in
* data/raw/sas_entrant_manual_overrides.csv when present):
*   6. no_sas_migrants     : headline dropping list-classified SAS migrants
*   7. state_total_nosas   : state totals EXCLUDING SAS migrants (weighted
*                            + unweighted) -- genuine capacity only
*   8. entry_genuine       : state-level genuine-entrant capacity per 100k
*      entry_sasmigrant    : same for SAS-migrant capacity (contrast: should
*                            track national SAS timing, not expansion)
*
* Outputs: output/tables/entryexit-estimation.csv
*          output/tables/entry-classification-counts.csv
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
global tabdir  "${topdir}/output/tables"
global figdir  "${topdir}/output/figures"
global latex_figdir "${topdir}/my_paper/figures"
cap mkdir "${tabdir}"
cap mkdir "${figdir}"
cap mkdir "${latex_figdir}"

log using "${topdir}/output/28-entryexit-estimation.log", replace

* FULL 2000-2019 PANEL (script 06): activity-window and zero-filled variants
* plus balanced_full and sas_window_entrant flags are all built in.
use "${datadir}/panel_2000_2019_estimation.dta", clear
egen program_numeric_id = group(state institution_code)
quietly count if missing(pop_yr)
assert r(N) == 0
gen double matched_per_100k_yr    = matched_zf / pop_yr * 100000
gen double matched_na_per_100k_yr = matched_na / pop_yr * 100000
encode state, gen(state_id)
xtset program_numeric_id year
tempfile master
save `master'

do "${topdir}/programs/_esplot-helpers.do"

tempname res
tempfile resfile
postfile `res' str24 spec double avg_treat avg_se treat_p pretrend_p baseline pct ///
    using "`resfile'", replace

capture program drop _eerun
program define _eerun
    args outcome tag resh idvar fname yti wopt
    if ("`idvar'" == "") local idvar program_numeric_id
    local aw "[aw=total_population_10]"
    if ("`wopt'" == "uw") local aw ""
    di _n "==================== `tag' ===================="
    capture noisily did_imputation `outcome' `idvar' year year_expanded ///
        `aw', horizons(0/5) pretrend(10) ///
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
    capture test pre1 pre2 pre3 pre4 pre5 pre6 pre7 pre8 pre9 pre10 pre6 pre7 pre8 pre9 pre10
    if _rc == 0 local pt = r(p)
    local tp = .
    capture test tau0 tau1 tau2 tau3 tau4 tau5
    if _rc == 0 local tp = r(p)
    quietly summarize `outcome' if treated_state==1 & year<year_expanded `aw'
    local b = r(mean)
    local pct = cond(`b' < . & `b' != 0, 100*`a'/`b', .)
    post `resh' ("`tag'") (`a') (`ase') (`tp') (`pt') (`b') (`pct')
    di as result "`tag': avg=" %10.6f `a' " se=" %10.6f `ase' " pct=" %5.1f `pct' ///
        " treat_p=" %6.3f `tp' " pretrend_p=" %6.3f `pt'
    if ("`fname'" != "") {
        _fillcoef
        _esplot "`fname'" "`yti'" "" `a' `b' `pct' `tp' `pt'
    }
end

* 1) entering/exiting institution-years coded missing
use "`master'", clear
_eerun matched_na_per_100k_yr "na_coded" `res'

* 2) always-active institutions only
use "`master'", clear
keep if balanced_full == 1
_eerun matched_per_100k_yr "balanced_only" `res'

* 5) drop SAS-window entrants entirely
use "`master'", clear
drop if sas_window_entrant == 1
_eerun matched_per_100k_yr "no_sas_entrants" `res'

* 3) state totals INCLUDING entrants
use "`master'", clear
collapse (sum) matched_zf (first) pop_yr total_population_10 year_expanded treated_state, ///
    by(state state_id year)
gen double state_total_per_100k = matched_zf / pop_yr * 100000
xtset state_id year
_eerun state_total_per_100k "state_total_all" `res' state_id "appx-statetotal" "Treatment Effect (state total per 100,000)"

* 4) state totals from always-active institutions only
use "`master'", clear
keep if balanced_full == 1
collapse (sum) matched_zf (first) pop_yr total_population_10 year_expanded treated_state, ///
    by(state state_id year)
gen double state_total_per_100k = matched_zf / pop_yr * 100000
xtset state_id year
_eerun state_total_per_100k "state_total_balanced" `res' state_id

* --- 6-8) list-based SAS classification specs (scripts 38-39) ----------------
use "`master'", clear
preserve
import delimited using "${datadir}/sas_entrant_classification.csv", ///
    clear varnames(1)
keep institution_code state sas_migrant
tempfile sasclass
save `sasclass'
capture confirm file "${topdir}/data/raw/sas_entrant_manual_overrides.csv"
if _rc == 0 {
    import delimited using "${topdir}/data/raw/sas_entrant_manual_overrides.csv", ///
        clear varnames(1) stringcols(_all)
    keep institution_code override_sas_migrant confidence
    destring institution_code, replace
    gen byte override = .
    replace override = 1 if override_sas_migrant == "1"
    replace override = 0 if override_sas_migrant == "0"
    drop override_sas_migrant
    drop if missing(override)
    * classification-perturbation variant: every manual call NOT graded
    * high-confidence is flipped the other way (mirrors the judgment-state
    * flips in the GME formula reclassification, script 25)
    gen byte override_pert = override
    replace override_pert = 1 - override if lower(strtrim(confidence)) != "high"
    drop confidence
    duplicates drop institution_code, force
    tempfile sasover
    save `sasover'
    use `sasclass', clear
    merge m:1 institution_code using `sasover', keep(master match) nogen
    gen byte sas_migrant_pert = sas_migrant
    replace sas_migrant = override if override < .
    replace sas_migrant_pert = override_pert if override_pert < .
    drop override override_pert
    save `sasclass', replace
    di as text "manual overrides applied (with perturbation variant)"
}
restore
merge m:1 institution_code state using `sasclass', keep(master match) nogen
replace sas_migrant = 0 if missing(sas_migrant)
capture confirm variable sas_migrant_pert
if _rc != 0 gen byte sas_migrant_pert = sas_migrant
replace sas_migrant_pert = 0 if missing(sas_migrant_pert)
gen byte genuine_entrant = (first_active >= 2011) & (sas_migrant == 0)
gen byte sas_entrant     = (first_active >= 2011) & (sas_migrant == 1)
gen byte genuine_pert    = (first_active >= 2011) & (sas_migrant_pert == 0)
tempfile masterclass
save `masterclass'

* descriptive entry counts by window x treatment x classification
preserve
keep if first_active >= 2011
bysort institution_code state: keep if _n == 1
gen str9 window = cond(first_active <= 2015, "2011-2015", "2016-2019")
contract window treated_state sas_migrant
export delimited using "${tabdir}/entry-classification-counts.csv", replace
restore

* entrant matched positions in 2019 by classification (decomposes the
* "~2,200 positions at entrants in expansion states" descriptive claim)
preserve
keep if first_active >= 2011 & year == 2019
collapse (sum) matched_zf, by(treated_state sas_migrant)
export delimited using "${tabdir}/entry-capacity-2019.csv", replace
restore

* 6) headline dropping list-classified SAS migrants
use "`masterclass'", clear
drop if sas_migrant == 1
_eerun matched_na_per_100k_yr "no_sas_migrants" `res'

* 7) state totals excluding SAS migrants, weighted and unweighted
use "`masterclass'", clear
drop if sas_migrant == 1
collapse (sum) matched_zf (first) pop_yr total_population_10 year_expanded treated_state, ///
    by(state state_id year)
gen double state_total_per_100k = matched_zf / pop_yr * 100000
xtset state_id year
_eerun state_total_per_100k "state_total_nosas" `res' state_id "appx-statetotal-nosas" "Treatment Effect (state total per 100,000, excluding SAS migrants)"
_eerun state_total_per_100k "state_total_nosas_uw" `res' state_id "" "" uw

* 8) entry margin by classification: state-level entrant capacity per 100k
use "`masterclass'", clear
gen double genuine_cap = matched_zf * genuine_entrant
gen double sas_cap     = matched_zf * sas_entrant
collapse (sum) genuine_cap sas_cap (first) pop_yr total_population_10 year_expanded treated_state, ///
    by(state state_id year)
gen double genuine_per_100k = genuine_cap / pop_yr * 100000
gen double sas_per_100k     = sas_cap    / pop_yr * 100000
xtset state_id year
_eerun genuine_per_100k "entry_genuine" `res' state_id "appx-entry-genuine" "Treatment Effect (genuine-entrant capacity per 100,000)"
_eerun genuine_per_100k "entry_genuine_uw" `res' state_id "" "" uw
_eerun sas_per_100k "entry_sasmigrant" `res' state_id

* classification perturbation: all medium/low-confidence manual calls flipped
use "`masterclass'", clear
gen double genuine_cap_p = matched_zf * genuine_pert
collapse (sum) genuine_cap_p (first) pop_yr total_population_10 year_expanded treated_state, ///
    by(state state_id year)
gen double genuine_pert_per_100k = genuine_cap_p / pop_yr * 100000
xtset state_id year
_eerun genuine_pert_per_100k "entry_genuine_pert" `res' state_id
_eerun genuine_pert_per_100k "entry_gen_pert_uw" `res' state_id "" "" uw

use "`masterclass'", clear
drop if sas_migrant_pert == 1
_eerun matched_na_per_100k_yr "no_sas_migr_pert" `res'
collapse (sum) matched_zf (first) pop_yr total_population_10 year_expanded treated_state, ///
    by(state state_id year)
gen double state_total_per_100k = matched_zf / pop_yr * 100000
xtset state_id year
_eerun state_total_per_100k "state_tot_nosas_pert" `res' state_id
_eerun state_total_per_100k "st_tot_nosas_pert_uw" `res' state_id "" "" uw

postclose `res'
use "`resfile'", clear
list, clean noobs
export delimited using "${tabdir}/entryexit-estimation.csv", replace

di _n "=== entry/exit estimation complete ==="
log close
