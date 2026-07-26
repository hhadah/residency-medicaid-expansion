* =============================================================================
* LINKED-SAMPLE RECONCILIATION: NRMP matched positions vs cost-report FTEs
* ---------------------------------------------------------------------------
* Referee response (editorial decision 2026-07-24, MUST-2 / cluster F1).
* Both referees flagged that the paper compares NRMP matched positions
* (859 institutions, population-weighted, calendar years) with cost-report
* resident FTEs (a different hospital sample, unweighted, fiscal years), so
* the apparent contradiction -- FTEs flat where intake falls, FTEs +19% where
* intake rises 3% -- is measured on non-comparable objects.
*
* This script estimates BOTH outcomes on the IDENTICAL sample:
*   - panel_2000_2019_estimation.dta + CCN crosswalk (scripts 06/10),
*     restricted to institution-years where matched AND dgme_ftes are both
*     observed -- same rows, same weights [aw=total_population_10], same
*     program+year FE, same clustering, same year convention (funding merged
*     at year == fiscal_year).
*   - FTEs annualized by months_covered (also addresses methods Minor 8).
* Estimates run pooled and split by GME formula arm (2012 classification,
* as in the submitted paper; classification sensitivity is script 25).
*
* Additional outputs:
*   - crosswalk match rate by source (methods MC8b)
*   - baseline above-cap share by arm (dgme_ftes vs dgme_resident_cap)
*   - baseline stock/flow ratio (FTEs per matched position)
* Outputs: output/tables/linked-sample-reconciliation.csv
*          output/tables/crosswalk-match-rate.csv
*          output/tables/linked-cap-status.csv
*          figures appx-linked-{matched,ftes}-{pooled,volume,nonresp}
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

log using "${topdir}/output/24-linked-sample-reconciliation.log", replace

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
encode state, gen(state_id)

* ---------------------------------------------------------------------------
* Crosswalk coverage (methods MC8b): institutions linked to a CCN, by source
* ---------------------------------------------------------------------------
preserve
    bysort institution_code: keep if _n == 1
    gen byte linked = !missing(provider_ccn)
    quietly count
    local n_inst = r(N)
    quietly count if linked
    local n_link = r(N)
    di as result "Crosswalk: `n_link' / `n_inst' institutions linked to a CCN " ///
        "(" %4.1f 100*`n_link'/`n_inst' "%)"
    gen str24 source = match_source
    replace source = "unlinked" if missing(provider_ccn)
    contract source
    gen double share = _freq / `n_inst'
    export delimited using "${tabdir}/crosswalk-match-rate.csv", replace
restore

* ---------------------------------------------------------------------------
* Annualize cost-report FTEs by months covered (methods Minor 8)
* ---------------------------------------------------------------------------
* months_covered is summed across cost-report segments within a fiscal year
* in the collapse above, so scaling by 12/months_covered annualizes both short-period
* reports and double-counted overlapping segments.
gen double dgme_ftes_ann = dgme_ftes * 12 / months_covered ///
    if !missing(dgme_ftes) & months_covered > 0 & !missing(months_covered)

* GME formula arms (2012 classification, as in the submitted paper)
preserve
    import delimited "${rawdir}/gme_formula_classification.csv", clear varnames(1) stringcols(_all)
    keep state gme_formula
    replace state = strtrim(upper(state))
    tempfile gme
    save `gme'
restore
merge m:1 state using `gme', keep(master match) nogen
gen byte gme_vol    = (gme_formula == "volume")
gen byte gme_notvol = inlist(gme_formula, "fixed", "none")

* ---------------------------------------------------------------------------
* The linked estimation sample: BOTH outcomes observed on the same rows
* ---------------------------------------------------------------------------
gen byte in_linked = !missing(provider_ccn) & !missing(dgme_ftes_ann) & !missing(matched)
quietly count if in_linked
di as text "Linked estimation rows (matched AND annualized FTEs observed): " r(N)
quietly levelsof institution_code if in_linked, local(li)
di as text "Linked institutions: " `: word count `li''

keep if in_linked
xtset program_numeric_id year

* Baseline stock/flow ratio and above-cap status (pre-expansion years)
preserve
    keep if year >= 2010 & year <= 2013
    gen byte above_cap = (dgme_ftes_ann >= dgme_resident_cap) ///
        if !missing(dgme_ftes_ann) & !missing(dgme_resident_cap) & dgme_resident_cap > 0
    gen byte arm = 1 if gme_vol == 1
    replace  arm = 0 if gme_notvol == 1
    collapse (mean) above_cap_share = above_cap ///
             (sum)  ftes = dgme_ftes_ann matched_sum = matched, by(treated_state arm)
    gen double stock_flow_ratio = ftes / matched_sum
    list, clean noobs
    export delimited using "${tabdir}/linked-cap-status.csv", replace
restore

* ---------------------------------------------------------------------------
* Event studies: matched vs annualized FTEs, identical spec, identical rows
* ---------------------------------------------------------------------------
tempname res
tempfile resfile
postfile `res' str16 outcome str12 group double avg_treat avg_se treat_p ///
    pretrend_p baseline pct n_inst n_states using "`resfile'", replace

do "${topdir}/programs/_esplot-helpers.do"

gen double asinh_matched = asinh(matched)
gen double asinh_ftes    = asinh(dgme_ftes_ann)

tempfile master
save `master'

foreach outcome in matched dgme_ftes_ann asinh_matched asinh_ftes {
    foreach grp in pooled volume notvolume {

        use "`master'", clear
        if "`grp'" == "volume"    keep if treated_state == 0 | (treated_state == 1 & gme_vol == 1)
        if "`grp'" == "notvolume" keep if treated_state == 0 | (treated_state == 1 & gme_notvol == 1)

        di _n "==================== LINKED `outcome' / `grp' ===================="
        capture noisily did_imputation `outcome' program_numeric_id year year_expanded ///
            [aw=total_population_10], horizons(0/5) pretrend(10) ///
            fe(program_numeric_id year) cluster(state_id) minn(0) autosample
        if (_rc != 0) {
            di as error "did_imputation failed for `outcome' / `grp' (rc=" _rc ") -- skipping."
            post `res' ("`outcome'") ("`grp'") (.) (.) (.) (.) (.) (.) (.) (.)
            continue
        }

        capture lincom (tau0+tau1+tau2+tau3+tau4+tau5)/6
        local a   = cond(_rc == 0, r(estimate), .)
        local ase = cond(_rc == 0, r(se), .)
        local pt = .
        capture test pre1 pre2 pre3 pre4 pre5 pre6 pre7 pre8 pre9 pre10 pre6 pre7 pre8 pre9 pre10
        if _rc == 0 local pt = r(p)
        local tp = .
        capture test tau0 tau1 tau2 tau3 tau4 tau5
        if _rc == 0 local tp = r(p)
        quietly summarize `outcome' if treated_state==1 & year < year_expanded [aw=total_population_10]
        local b = r(mean)
        local pct = cond(`b' < . & `b' != 0, 100*`a'/`b', .)
        local ni = .
        local ns = .
        capture levelsof program_numeric_id, local(hh)
        if (_rc == 0) local ni : word count `hh'
        capture levelsof state_id, local(ss)
        if (_rc == 0) local ns : word count `ss'

        post `res' ("`outcome'") ("`grp'") (`a') (`ase') (`tp') (`pt') (`b') (`pct') (`ni') (`ns')
        di as result "`outcome' / `grp': avg=" %9.4f `a' " se=" %9.4f `ase' ///
            " pct=" %5.1f `pct' " treat_p=" %6.3f `tp' " pretrend_p=" %6.3f `pt'

        * Figures only for the two level outcomes (the reconciliation exhibits)
        if inlist("`outcome'", "matched", "dgme_ftes_ann") {
            local obase = cond("`outcome'"=="matched", "matched", "ftes")
            local ggrp  = cond("`grp'"=="pooled", "pooled", cond("`grp'"=="volume", "volume", "nonresp"))
            local yti = cond("`outcome'"=="matched", ///
                "Treatment Effect (matched positions, linked sample)", ///
                "Treatment Effect (annualized resident FTEs, linked sample)")
            _fillcoef
            _esplot "appx-linked-`obase'-`ggrp'" "`yti'" "" `a' `b' `pct' `tp' `pt'
        }
    }
}

postclose `res'
use "`resfile'", clear
list, clean noobs
export delimited using "${tabdir}/linked-sample-reconciliation.csv", replace

di _n "=== linked-sample reconciliation complete ==="
log close
