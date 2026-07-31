* =============================================================================
* First-stage GME funding response, SPLIT by Medicaid GME formula design
* ---------------------------------------------------------------------------
* Closes the mechanism chain. The pooled first stage (script 18) shows expansion
* raises Direct GME (DGME) payments ~14%. But the reduced-form contraction in
* residency positions is concentrated in NON-RESPONSIVE (fixed/none) GME states.
* If the financing channel is real, the DGME gain should appear in VOLUME-
* RESPONSIVE states and be absent (or much smaller) in NON-RESPONSIVE states.
*
* This re-runs the DGME and IME event studies from script 18 separately for:
*   - volume-responsive expansion states + never-expansion controls
*   - non-responsive (fixed/none) expansion states + never-expansion controls
* Classification: gme_formula_2015 (2015 payment rules / Henderson 2016 AAMC survey),
* merged from data/raw/gme_formula_classification.csv.
*
* Design mirrors 16: did_imputation (BJS 2024), hospital + fiscal-year FE,
* cluster by state, UNWEIGHTED. Figures 31-34; summary CSV.
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

log using "${topdir}/output/19-gme-firststage-byformula.log", replace

* -------------------------------------------------------------------------
* Load and collapse the GME funding panel (identical to script 18)
* -------------------------------------------------------------------------
use "${datadir}/gme_funding_expansion.dta", clear
drop if missing(expansion_state)
egen provider_numeric_id = group(state provider_ccn)
collapse (sum)  dgme_payment ime_payment total_gme_payment ///
                primary_care_fte non_primary_care_fte dgme_ftes ime_ftes ///
                months_covered ///
        (mean) primary_care_pra non_primary_care_pra ///
                dgme_resident_cap ime_resident_cap num_beds ///
        (first) state year_expanded expanded_ever, ///
        by(provider_numeric_id fiscal_year)
* Annualize by months covered (referee response, methods Minor 8): summed
* cost-report segments can cover more or fewer than 12 months; scaling by
* 12/months_covered puts every provider-year on an annual basis (as in the
* linked-sample reconciliation, script 24).
foreach v in dgme_payment ime_payment total_gme_payment ///
    primary_care_fte non_primary_care_fte dgme_ftes ime_ftes {
    replace `v' = `v' * 12 / months_covered if months_covered > 0 & !missing(months_covered)
}


encode state, gen(state_id)
gen byte treated_state = expanded_ever
xtset provider_numeric_id fiscal_year

gen double asinh_dgme = asinh(dgme_payment)
gen double asinh_ime  = asinh(ime_payment)
label var asinh_dgme "Direct GME (DGME) Payment (asinh $)"
label var asinh_ime  "Indirect Medical Ed. (IME) Payment (asinh $)"

* Decomposition outcomes: realized DGME = FTEs x payment-per-FTE. Splitting the
* payment response into a volume (FTE) and a rate (per-FTE) component shows
* whether the post-expansion DGME rise in NON-responsive states reflects more
* residents or higher payment rates (mechanical formula channels).
gen double asinh_dgme_ftes = asinh(dgme_ftes)
gen double dgme_per_fte = cond(dgme_ftes > 0, dgme_payment / dgme_ftes, .)
gen double asinh_dgme_perfte = asinh(dgme_per_fte)
label var asinh_dgme_ftes   "DGME Resident FTEs (asinh)"
label var asinh_dgme_perfte "DGME Payment per FTE (asinh $)"

* -------------------------------------------------------------------------
* Merge the Medicaid GME formula classification (2015 payment rules)
* -------------------------------------------------------------------------
preserve
    import delimited "${rawdir}/gme_formula_classification.csv", clear ///
        varnames(1) stringcols(_all)
    keep state gme_formula_2015
    replace state = strtrim(upper(state))
    tempfile gme
    save `gme'
restore
replace state = strtrim(upper(state))
merge m:1 state using `gme', keep(master match) nogen

gen byte gme_vol    = (gme_formula_2015 == "volume")
gen byte gme_notvol = inlist(gme_formula_2015, "fixed", "none")

di as text "Treated hospital-years by volume-responsiveness (1 = volume):"
tab gme_vol if treated_state == 1 & (gme_vol == 1 | gme_notvol == 1)

tempfile master
save `master'

* -------------------------------------------------------------------------
* Results collector
* -------------------------------------------------------------------------
tempname fs
tempfile fs_file
postfile `fs' str20 outcome str12 group double avg_treat avg_se treat_p pretrend_p ///
    baseline pct_effect n_hospitals n_states using "`fs_file'", replace

* Shared event-study plotting helpers (_esplot, _fillcoef)
do "${topdir}/programs/_esplot-helpers.do"

local plotnum = 31
foreach outcome in asinh_dgme asinh_ime asinh_dgme_ftes asinh_dgme_perfte {
    foreach grp in volume notvolume {

        use "`master'", clear
        if "`grp'" == "volume"    keep if treated_state == 0 | (treated_state == 1 & gme_vol == 1)
        if "`grp'" == "notvolume" keep if treated_state == 0 | (treated_state == 1 & gme_notvol == 1)

        quietly count if treated_state == 1 & !missing(`outcome')
        if (r(N) == 0) {
            di as error "No treated obs for `outcome' / `grp' -- skipping."
            local ++plotnum
            continue
        }

        di ""
        di "========================================================================="
        di "FIRST STAGE: `outcome' -- `grp'-formula expansion states vs controls"
        di "========================================================================="

        capture noisily did_imputation `outcome' provider_numeric_id fiscal_year year_expanded, ///
            horizons(0/5) pretrend(10) fe(provider_numeric_id fiscal_year) ///
            cluster(state_id) minn(0) autosample
        if (_rc != 0) {
            local rc = _rc
            di as error "did_imputation failed for `outcome' / `grp' (rc=`rc'). Skipping."
            local ++plotnum
            continue
        }

        * Average post ATT (tau0..5) with its SE (lincom); manual fallback if
        * some horizons are missing (referee response MUST-9: report the SE)
        local avg_treat = .
        local avg_se    = .
        capture lincom (tau0+tau1+tau2+tau3+tau4+tau5)/6
        if (_rc == 0) {
            local avg_treat = r(estimate)
            local avg_se    = r(se)
        }
        else {
            local tau_sum = 0
            local tau_n = 0
            forval h = 0/5 {
                capture scalar __b = _b[tau`h']
                if (_rc == 0) {
                    local tau_sum = `tau_sum' + __b
                    local tau_n = `tau_n' + 1
                }
            }
            capture scalar drop __b
            local avg_treat = cond(`tau_n' > 0, `tau_sum'/`tau_n', .)
        }
        local pretrend_p = .
        local treat_p = .
        capture test pre1 pre2 pre3 pre4 pre5 pre6 pre7 pre8 pre9 pre10 pre6 pre7 pre8 pre9 pre10
        if (_rc == 0) local pretrend_p = r(p)
        capture test tau0 tau1 tau2 tau3 tau4 tau5
        if (_rc == 0) local treat_p = r(p)

        quietly summarize `outcome' if treated_state == 1 & fiscal_year < year_expanded
        local baseline = r(mean)
        local pct = cond(!missing(`baseline') & `baseline' != 0, 100*`avg_treat'/`baseline', .)

        local nh = .
        local ns = .
        capture levelsof provider_numeric_id if !missing(`outcome'), local(hh)
        if (_rc == 0) local nh : word count `hh'
        capture levelsof state_id if !missing(`outcome'), local(ss)
        if (_rc == 0) local ns : word count `ss'

        post `fs' ("`outcome'") ("`grp'") (`avg_treat') (`avg_se') (`treat_p') (`pretrend_p') ///
            (`baseline') (`pct') (`nh') (`ns')
        di as result "`outcome' / `grp': avg post = " %7.3f `avg_treat' ///
            "  joint p = " %6.3f `treat_p' "  (pretrend p = " %5.3f `pretrend_p' ")"

        * ---- Event-study figure via shared helpers ----
        * No in-graph title (INV-12): the LaTeX subcaption labels the panel.
        * Semantic name: appx-firststage-{dgme|ime|dgmeftes|dgmeperfte}-{volume|nonresp}
        local fbase = cond("`outcome'"=="asinh_dgme", "dgme", ///
                      cond("`outcome'"=="asinh_ime", "ime", ///
                      cond("`outcome'"=="asinh_dgme_ftes", "dgmeftes", "dgmeperfte")))
        local fgrp  = cond("`grp'"=="volume", "volume", "nonresp")
        local outfname = "appx-firststage-`fbase'-`fgrp'"
        local yti = cond(strpos("`outcome'", "ftes") & !strpos("`outcome'", "perfte"), ///
            "Treatment Effect (asinh FTEs)", "Treatment Effect (asinh dollars)")
        _fillcoef
        _esplot "`outfname'" "`yti'" "" `avg_treat' `baseline' `pct' `treat_p' `pretrend_p'

        local ++plotnum
    }

    * ---- Pooled interacted model: formal volume-vs-nonresponsive DIFFERENCE
    * test for this outcome (delta method on the difference in avg post effects).
    use "`master'", clear
    keep if treated_state == 0 | gme_vol == 1 | gme_notvol == 1
    capture noisily did_imputation `outcome' provider_numeric_id fiscal_year year_expanded, ///
        horizons(0/5) pretrend(10) fe(provider_numeric_id fiscal_year) ///
        cluster(state_id) minn(0) autosample hetby(gme_vol)
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
    post `fs' ("`outcome'") ("diff") (`mdd') (`mdse') (`mdp') (.) (.) (.) (.) (.)
    di as result "`outcome' / diff (volume - nonresp) = " %7.3f `mdd' ///
        " (se=" %7.3f `mdse' ", p=" %6.3f `mdp' ")"
}


* -------------------------------------------------------------------------
* PPML robustness (referee response: Chen-Roth 2024). asinh coefficients on
* dollar outcomes with mass at zero are not unit-invariant; PPML in LEVELS
* with the same fixed effects and clustering is. Static treated-post design
* (event-study PPML is not available for the imputation estimator); the
* object of interest is the proportional post effect and the cross-arm
* interaction, not dynamics.
* -------------------------------------------------------------------------
tempname pp
tempfile pp_file
postfile `pp' str20 outcome str12 spec double b_ppml se_ppml p_ppml ///
    using "`pp_file'", replace

use "`master'", clear
gen byte tp = treated_state == 1 & fiscal_year >= year_expanded
gen byte tp_vol = tp * gme_vol
keep if treated_state == 0 | gme_vol == 1 | gme_notvol == 1
* PPML requires non-negative outcomes; a small number of cost-report years
* carry negative payment totals (accounting adjustments). Those years are
* excluded from the PPML runs and the count is reported.
foreach v in dgme_payment ime_payment {
    quietly count if `v' < 0 & !missing(`v')
    di as text "PPML: excluding " r(N) " hospital-years with negative `v'"
    replace `v' = . if `v' < 0
}

foreach outcome in dgme_payment ime_payment {
    di _n "========== PPML: `outcome' (pooled treated-post) =========="
    capture noisily ppmlhdfe `outcome' tp, absorb(provider_numeric_id fiscal_year) ///
        vce(cluster state_id)
    if (_rc == 0) {
        local b = _b[tp]
        local se = _se[tp]
        local p = 2*normal(-abs(`b'/`se'))
        post `pp' ("`outcome'") ("pooled") (`b') (`se') (`p')
        di as result "PPML `outcome' pooled: b=" %8.4f `b' " (se=" %8.4f `se' ", p=" %6.3f `p' ")"
    }
    else post `pp' ("`outcome'") ("pooled") (.) (.) (.)

    di _n "========== PPML: `outcome' (cross-arm interaction) =========="
    capture noisily ppmlhdfe `outcome' tp tp_vol, absorb(provider_numeric_id fiscal_year) ///
        vce(cluster state_id)
    if (_rc == 0) {
        local b = _b[tp_vol]
        local se = _se[tp_vol]
        local p = 2*normal(-abs(`b'/`se'))
        post `pp' ("`outcome'") ("vol_diff") (`b') (`se') (`p')
        di as result "PPML `outcome' vol-diff: b=" %8.4f `b' " (se=" %8.4f `se' ", p=" %6.3f `p' ")"
    }
    else post `pp' ("`outcome'") ("vol_diff") (.) (.) (.)
}

postclose `pp'
preserve
use "`pp_file'", clear
list, clean noobs
export delimited using "${tabdir}/ppml-payments-summary.csv", replace
restore

* -------------------------------------------------------------------------
* Log on the positive subsample + extensive-margin LPM (static treated-post).
* Decomposes the payment response into an intensive margin identified on
* hospital-years with positive payments (logs, unit-invariant) and an
* extensive margin (any positive payment). Same fixed effects, sample
* restriction, and clustering as the PPML block above.
* -------------------------------------------------------------------------
tempname lp
tempfile lp_file
postfile `lp' str20 outcome str12 spec double b se p n_obs using "`lp_file'", replace

use "`master'", clear
gen byte tp = treated_state == 1 & fiscal_year >= year_expanded
gen byte tp_vol = tp * gme_vol
keep if treated_state == 0 | gme_vol == 1 | gme_notvol == 1
foreach v in dgme_payment ime_payment {
    quietly gen double log_`v' = ln(`v') if `v' > 0
    quietly gen byte   any_`v' = (`v' > 0) if !missing(`v')
}

foreach outcome in dgme_payment ime_payment {
    di _n "========== LOG-POSITIVE: `outcome' (pooled) =========="
    capture noisily reghdfe log_`outcome' tp, absorb(provider_numeric_id fiscal_year) ///
        vce(cluster state_id)
    if (_rc == 0) post `lp' ("`outcome'") ("log_pooled") (_b[tp]) (_se[tp]) ///
        (2*normal(-abs(_b[tp]/_se[tp]))) (e(N))
    else post `lp' ("`outcome'") ("log_pooled") (.) (.) (.) (.)

    di _n "========== LOG-POSITIVE: `outcome' (cross-arm interaction) =========="
    capture noisily reghdfe log_`outcome' tp tp_vol, absorb(provider_numeric_id fiscal_year) ///
        vce(cluster state_id)
    if (_rc == 0) post `lp' ("`outcome'") ("log_vol_diff") (_b[tp_vol]) (_se[tp_vol]) ///
        (2*normal(-abs(_b[tp_vol]/_se[tp_vol]))) (e(N))
    else post `lp' ("`outcome'") ("log_vol_diff") (.) (.) (.) (.)

    di _n "========== EXTENSIVE MARGIN: any `outcome' (pooled) =========="
    capture noisily reghdfe any_`outcome' tp, absorb(provider_numeric_id fiscal_year) ///
        vce(cluster state_id)
    if (_rc == 0) post `lp' ("`outcome'") ("ext_pooled") (_b[tp]) (_se[tp]) ///
        (2*normal(-abs(_b[tp]/_se[tp]))) (e(N))
    else post `lp' ("`outcome'") ("ext_pooled") (.) (.) (.) (.)

    di _n "========== EXTENSIVE MARGIN: any `outcome' (cross-arm interaction) =========="
    capture noisily reghdfe any_`outcome' tp tp_vol, absorb(provider_numeric_id fiscal_year) ///
        vce(cluster state_id)
    if (_rc == 0) post `lp' ("`outcome'") ("ext_vol_diff") (_b[tp_vol]) (_se[tp_vol]) ///
        (2*normal(-abs(_b[tp_vol]/_se[tp_vol]))) (e(N))
    else post `lp' ("`outcome'") ("ext_vol_diff") (.) (.) (.) (.)
}

postclose `lp'
preserve
use "`lp_file'", clear
list, clean noobs
export delimited using "${tabdir}/logpositive-payments-summary.csv", replace
restore

postclose `fs'

use "`fs_file'", clear
order outcome group avg_treat avg_se pct_effect treat_p pretrend_p baseline n_hospitals n_states
list, clean noobs
export delimited using "${tabdir}/gme-firststage-byformula-summary.csv", replace

di ""
di "=================================================================="
di "Group-specific GME first stage complete."
di "Figures: 31=dgme volume, 32=dgme nonresp, 33=ime volume, 34=ime nonresp"
di "Summary: ${tabdir}/gme-firststage-byformula-summary.csv"
di "=================================================================="

log close
