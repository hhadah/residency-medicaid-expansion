* =============================================================================
* HonestDiD sensitivity analysis (Rambachan & Roth 2023)
* ---------------------------------------------------------------------------
* The event-study design rests on parallel pre-trends, which are untestable in
* the post period. HonestDiD reports how large a violation of parallel trends
* would have to be to overturn the result. Under the "relative magnitudes" (RM)
* restriction, the post-period differential trend is bounded by Mbar times the
* largest pre-period differential trend; the breakdown Mbar is the value at
* which the robust CI first includes zero.
*
* Parameter of interest: the AVERAGE post-expansion ATT (equal weights on
* horizons 0-5), matching the effect reported in the paper.
*
* Panels:
*   - headline: matched_per_100k, full sample (Figure 5)         -> figure 36
*   - mech volume-responsive expansion states + controls          -> figure 37
*   - mech non-responsive expansion states + controls             -> figure 38
* Uses the BJS (2024) event-study coefficients e(b)/e(V), reordered to event
* time (-5..-1, 0..5) and passed to honestdid.
* =============================================================================

clear all
set more off

global topdir "/Users/hhadah/Projects/GiT/residency-medicaid-expansion"
global datadir "${topdir}/data/datasets"
global rawdir  "${topdir}/data/raw"
global figdir  "${topdir}/output/figures"
global tabdir  "${topdir}/output/tables"
global latex_figdir "${topdir}/my_paper/figures"
cap mkdir "${figdir}"
cap mkdir "${tabdir}"
cap mkdir "${latex_figdir}"

log using "${topdir}/output/19-honestdid-sensitivity.log", replace

honestdid _plugin_check

* -------------------------------------------------------------------------
* Load + set up, merge GME formula classification (for the mech samples)
* -------------------------------------------------------------------------
use "${datadir}/cleaned_program_residency_medicaid.dta", clear
egen program_numeric_id = group(state institution_code)
encode state, gen(state_id)
xtset program_numeric_id year

preserve
    import delimited "${rawdir}/gme_formula_classification.csv", clear ///
        varnames(1) stringcols(_all)
    keep state gme_formula
    replace state = strtrim(upper(state))
    tempfile gme
    save `gme'
restore
replace state = strtrim(upper(state))
merge m:1 state using `gme', keep(master match) nogen
gen byte gme_vol    = (gme_formula == "volume")
gen byte gme_notvol = inlist(gme_formula, "fixed", "none")
tempfile master
save `master'

* -------------------------------------------------------------------------
* Program: run BJS event study on the current sample, reorder e(b)/e(V) to
* event time, run honestdid(RM) for the AVERAGE post ATT, save a coefplot.
* -------------------------------------------------------------------------
capture program drop _honest
program define _honest
    args tag prefix
    did_imputation matched_per_100k program_numeric_id year year_expanded ///
        [aw=total_population_10], horizons(0/5) pretrend(5) ///
        fe(program_numeric_id year) cluster(state_id) minn(0)

    local order pre5 pre4 pre3 pre2 pre1 tau0 tau1 tau2 tau3 tau4 tau5
    matrix b0 = e(b)
    matrix V0 = e(V)
    matrix bb = J(1,11,.)
    matrix VV = J(11,11,.)
    forval i = 1/11 {
        local ni : word `i' of `order'
        matrix bb[1,`i'] = b0[1, colnumb(b0,"`ni'")]
    }
    forval i = 1/11 {
        local ni : word `i' of `order'
        local ri = colnumb(V0,"`ni'")
        forval j = 1/11 {
            local nj : word `j' of `order'
            matrix VV[`i',`j'] = V0[`ri', colnumb(V0,"`nj'")]
        }
    }
    local names t_m5 t_m4 t_m3 t_m2 t_m1 t0 t1 t2 t3 t4 t5
    matrix colnames bb = `names'
    matrix colnames VV = `names'
    matrix rownames VV = `names'
    matrix lvec = J(6,1,1/6)

    di ""
    di "=== HONESTDID [`tag'] : average post ATT, Delta^RM ==="
    honestdid, b(bb) vcov(VV) pre(1/5) post(6/11) delta(rm) l_vec(lvec) ///
        mvec(0(0.1)2) coefplot ///
        title("HonestDiD sensitivity: `tag'") ///
        ytitle("Average post-expansion ATT (per 100,000)") ///
        graphregion(color(white)) plotregion(color(white))
    graph export "${figdir}/`prefix'-honestdid_`tag'.png", as(png) replace width(1200) height(800)
    graph export "${latex_figdir}/`prefix'-honestdid_`tag'.png", as(png) replace width(1200) height(800)
end

* -------------------------------------------------------------------------
* Run the three panels
* -------------------------------------------------------------------------
use "`master'", clear
_honest "headline" "36"

use "`master'", clear
keep if treated_state == 0 | (treated_state == 1 & gme_vol == 1)
_honest "volume" "37"

use "`master'", clear
keep if treated_state == 0 | (treated_state == 1 & gme_notvol == 1)
_honest "nonresponsive" "38"

di ""
di "=================================================================="
di "HonestDiD sensitivity complete. Figures 36 (headline), 37 (volume),"
di "38 (non-responsive). Breakdown Mbar = largest M with robust CI ub < 0"
di "(parsed from the M/lb/ub tables above)."
di "=================================================================="

log close
