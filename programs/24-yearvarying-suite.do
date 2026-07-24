* =============================================================================
* YEAR-VARYING per-capita suite (PRIMARY OUTCOME): headline, urban/rural, GME-
* formula mechanism, and the quota family member for the FDR correction. Outcome
* = matched positions per CONTEMPORARY 100,000 population (ACS 1-year). This is
* the paper's primary specification: a genuine rate with clean pre-trends,
* whereas the levels outcome fails parallel trends.
*
* Figures (semantic names): main-headline, main-location, main-mechanism-volume,
*   main-mechanism-nonresp. Every event-study figure carries a text box with the
*   baseline mean, average post effect (%), treatment p, and PRE-TREND p.
* Summary CSV also stores the SE of the average post-ATT (for the FDR forest plot)
*   and a quota_per_100k_yr row (folds in the retired script 23).
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

log using "${topdir}/output/24-yearvarying-suite.log", replace

use "${datadir}/cleaned_program_residency_medicaid.dta", clear
egen program_numeric_id = group(state institution_code)
replace state = strtrim(upper(state))

* Year-varying ACS population -> per-capita outcomes
merge m:1 state year using "${datadir}/state_year_population.dta", keep(master match) nogen
quietly count if missing(pop_yr)
di as text "pop_yr merge: " r(N) " unmatched master rows (missing pop_yr)"
assert r(N) == 0
gen double matched_per_100k_yr = matched / pop_yr * 100000
capture confirm variable quota
if _rc == 0 gen double quota_per_100k_yr = quota / pop_yr * 100000

* Urban/rural (as in 06): rural = RUCA > 3; missing RUCA excluded (2026-07-24
* fix: the unguarded expression classified missing RUCA as rural, since
* missing > 3 is true in Stata)
capture confirm variable rural_urban_2010
if _rc == 0 gen byte urban_rural = (rural_urban_2010 > 3) if !missing(rural_urban_2010)

* GME formula classification (as in 13)
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

encode state, gen(state_id)
xtset program_numeric_id year

tempname res
tempfile resfile
postfile `res' str24 spec double avg_treat avg_se treat_p pretrend_p baseline pct ///
    using "`resfile'", replace

* Shared event-study plotting helpers (_esplot, _fillcoef)
do "${topdir}/programs/_esplot-helpers.do"

* ------------------------------------------------------------------ 1) HEADLINE
di _n "==================== HEADLINE (matched_per_100k_yr) ===================="
did_imputation matched_per_100k_yr program_numeric_id year year_expanded ///
    [aw=total_population_10], horizons(0/5) pretrend(5) ///
    fe(program_numeric_id year) cluster(state_id) minn(0)
lincom (tau0+tau1+tau2+tau3+tau4+tau5)/6
local a  = r(estimate)
local ase = r(se)
local pt = .
capture test pre1 pre2 pre3 pre4 pre5
if _rc == 0 local pt = r(p)
local tp = .
capture test tau0 tau1 tau2 tau3 tau4 tau5
if _rc == 0 local tp = r(p)
quietly summarize matched_per_100k_yr if treated_state==1 & year<year_expanded [aw=total_population_10]
local b = r(mean)
local pct = cond(`b' < . & `b' != 0, 100*`a'/`b', .)
post `res' ("headline") (`a') (`ase') (`tp') (`pt') (`b') (`pct')
di as result "headline: avg=" %8.4f `a' " se=" %8.4f `ase' " pct=" %5.1f `pct' " treat_p=" %6.3f `tp' " pretrend_p=" %6.3f `pt'
_fillcoef
_esplot "main-headline" "Treatment Effect (per 100,000, year-varying pop.)" "" `a' `b' `pct' `tp' `pt'

* --------------------------------------------------------------- 2) URBAN/RURAL
* SPLIT-SAMPLE design: urban and rural programs are estimated in two SEPARATE
* did_imputation regressions. Each subsample keeps the never-expansion programs
* of its own type as controls, so each group gets its own imputed counterfactual
* and -- the substantive gain over hetby() -- its OWN pre-trend test. A pooled
* hetby() model is re-run quietly below solely to compute a formal test of the
* urban-rural difference in average post effects (that model shares pre-trends
* across groups, so the split-sample estimates are primary).
di _n "==================== URBAN/RURAL (matched_per_100k_yr), split samples ===================="
matrix U = J(11,3,.)
matrix R = J(11,3,.)
matrix colnames U = period coef se
matrix colnames R = period coef se
foreach g in 0 1 {
    local gname = cond(`g'==0, "urban", "rural")
    preserve
    keep if urban_rural == `g'
    quietly count if treated_state==1
    local n_tr = r(N)
    quietly count if treated_state==0
    local n_co = r(N)
    quietly tab state_id if treated_state==1
    local ns_tr = r(r)
    quietly tab state_id if treated_state==0
    local ns_co = r(r)
    di as text "`gname' subsample: treated obs=`n_tr' (`ns_tr' states), control obs=`n_co' (`ns_co' states)"
    did_imputation matched_per_100k_yr program_numeric_id year year_expanded ///
        [aw=total_population_10], horizons(0/5) pretrend(5) cluster(state_id) ///
        fe(program_numeric_id year) minn(0) autosample
    lincom (tau0+tau1+tau2+tau3+tau4+tau5)/6
    local a_`gname'  = r(estimate)
    local se_`gname' = r(se)
    local tp_`gname' = .
    capture test tau0 tau1 tau2 tau3 tau4 tau5
    if _rc == 0 local tp_`gname' = r(p)
    local pt_`gname' = .
    capture test pre1 pre2 pre3 pre4 pre5
    if _rc == 0 local pt_`gname' = r(p)
    quietly summarize matched_per_100k_yr if treated_state==1 & year<year_expanded [aw=total_population_10]
    local b_`gname' = r(mean)
    local pct_`gname' = cond(`b_`gname'' < . & `b_`gname'' != 0, 100*`a_`gname''/`b_`gname'', .)
    post `res' ("`gname'") (`a_`gname'') (`se_`gname'') (`tp_`gname'') (`pt_`gname'') (`b_`gname'') (`pct_`gname'')
    di as result "`gname' avg=" %8.4f `a_`gname'' " se=" %8.4f `se_`gname'' ///
        " pct=" %5.1f `pct_`gname'' " treat_p=" %6.3f `tp_`gname'' " pretrend_p=" %6.3f `pt_`gname''
    _fillcoef
    if (`g'==0) matrix U = plot_coef
    else        matrix R = plot_coef
    restore
}
* Pooled interacted model: formal urban-rural difference test ONLY (delta method
* on the difference in average post effects). Shares pre-trends across groups.
quietly did_imputation matched_per_100k_yr program_numeric_id year year_expanded ///
    [aw=total_population_10], horizons(0/5) pretrend(5) cluster(state_id) ///
    hetby(urban_rural) fe(program_numeric_id year) minn(0) autosample
local dd  = .
local dse = .
local dp  = .
capture nlcom (_b[tau0_1]+_b[tau1_1]+_b[tau2_1]+_b[tau3_1]+_b[tau4_1]+_b[tau5_1])/6 ///
            - (_b[tau0_0]+_b[tau1_0]+_b[tau2_0]+_b[tau3_0]+_b[tau4_0]+_b[tau5_0])/6
if _rc == 0 {
    matrix _dnl  = r(b)
    matrix _dnlV = r(V)
    local dd  = _dnl[1,1]
    local dse = sqrt(_dnlV[1,1])
    local dp  = 2*normal(-abs(`dd'/`dse'))
}
post `res' ("urban_rural_diff") (`dd') (`dse') (`dp') (.) (.) (.)
di as result "pooled diff (rural - urban) = " %8.4f `dd' " (se=" %8.4f `dse' ", p=" %6.3f `dp' ")"
* two-series figure (urban navy, rural orange); per-group pre-period paths.
preserve
clear
svmat U, names(col)
rename (period coef se) (period u_coef u_se)
gen long _i=_n
tempfile uu
save `uu'
clear
svmat R, names(col)
rename (period coef se) (r_period r_coef r_se)
gen long _i=_n
merge 1:1 _i using `uu', nogen
keep if !missing(period)
gen u_hi=u_coef+1.96*u_se
gen u_lo=u_coef-1.96*u_se
gen r_hi=r_coef+1.96*r_se
gen r_lo=r_coef-1.96*r_se
quietly summarize u_hi
local ymax = r(max)
quietly summarize r_hi
local ymax = max(`ymax', r(max))
local y_annot = `ymax'*0.92
local au_t  = string(`a_urban', "%9.3f")
local ar_t  = string(`a_rural', "%9.3f")
local up_t  = cond(`tp_urban' < ., string(`tp_urban', "%4.2f"), "NA")
local rp_t  = cond(`tp_rural' < ., string(`tp_rural', "%4.2f"), "NA")
local upt_t = cond(`pt_urban' < ., string(`pt_urban', "%4.2f"), "NA")
local rpt_t = cond(`pt_rural' < ., string(`pt_rural', "%4.2f"), "NA")
local dp_t  = cond(`dp' < ., string(`dp', "%4.2f"), "NA")
local annot `"text(`y_annot' -4.5 `"Urban avg = `au_t' (p=`up_t', pre-trend p=`upt_t')"' `"Rural avg = `ar_t' (p=`rp_t', pre-trend p=`rpt_t')"' `"Difference p = `dp_t' (pooled model)"', placement(e) size(medsmall) justification(left))"'
twoway ///
    (rarea u_hi u_lo period, fcolor(navy%25) lcolor(navy%0)) ///
    (rarea r_hi r_lo period, fcolor(orange%25) lcolor(orange%0)) ///
    (line u_coef period, lcolor(navy) lwidth(medium)) ///
    (line r_coef period, lcolor(orange) lwidth(medium)) ///
    (scatter u_coef period, mcolor(navy) msymbol(circle)) ///
    (scatter r_coef period, mcolor(orange) msymbol(triangle)) ///
    , xline(-0.5, lcolor(black) lwidth(thin)) yline(0, lcolor(black) lwidth(thin)) ///
    xlabel(-5(1)5, labsize(small)) ylabel(#8, labsize(small) format(%9.3f)) ///
    xtitle("Years relative to Medicaid expansion", size(small)) ///
    ytitle("Treatment Effect (per 100,000, year-varying pop.)", size(small)) ///
    `annot' ///
    legend(order(5 "Urban" 6 "Rural") size(small) region(lstyle(none))) ///
    graphregion(color(white)) plotregion(color(white))
graph export "${figdir}/main-location.png", as(png) replace width(1200) height(800)
graph export "${latex_figdir}/main-location.png", as(png) replace width(1200) height(800)
graph export "${figdir}/main-location.pdf", replace
graph export "${latex_figdir}/main-location.pdf", replace
restore

* ------------------------------------------------------------------ 3) MECHANISM
foreach grp in volume notvolume {
    preserve
    if "`grp'"=="volume"    keep if treated_state==0 | (treated_state==1 & gme_vol==1)
    if "`grp'"=="notvolume" keep if treated_state==0 | (treated_state==1 & gme_notvol==1)
    di _n "==================== MECHANISM `grp' (matched_per_100k_yr) ===================="
    did_imputation matched_per_100k_yr program_numeric_id year year_expanded ///
        [aw=total_population_10], horizons(0/5) pretrend(5) ///
        fe(program_numeric_id year) cluster(state_id) minn(0)
    lincom (tau0+tau1+tau2+tau3+tau4+tau5)/6
    local a = r(estimate)
    local ase = r(se)
    local pt = .
    capture test pre1 pre2 pre3 pre4 pre5
    if _rc == 0 local pt = r(p)
    local tp = .
    capture test tau0 tau1 tau2 tau3 tau4 tau5
    if _rc == 0 local tp = r(p)
    quietly summarize matched_per_100k_yr if treated_state==1 & year<year_expanded [aw=total_population_10]
    local b = r(mean)
    local pct = cond(`b' < . & `b' != 0, 100*`a'/`b', .)
    post `res' ("mech_`grp'") (`a') (`ase') (`tp') (`pt') (`b') (`pct')
    di as result "mech `grp': avg=" %8.4f `a' " pct=" %5.1f `pct' " treat_p=" %6.3f `tp' " pretrend_p=" %6.3f `pt'
    local glab = cond("`grp'"=="volume","Volume-Responsive GME States","Non-Responsive GME States (Fixed/None)")
    local fname = cond("`grp'"=="volume","main-mechanism-volume","main-mechanism-nonresp")
    _fillcoef
    * no in-graph title (INV-12): the LaTeX subcaption labels the panel
    _esplot "`fname'" "Treatment Effect (per 100,000, year-varying pop.)" "" `a' `b' `pct' `tp' `pt'
    restore
}

* Pooled interacted mechanism model: formal volume-vs-nonresponsive DIFFERENCE
* test (delta method on the difference in average post effects), mirroring the
* urban/rural pattern. Sample keeps never-expansion controls plus classified
* expansion states only; hetby group 1 = volume-responsive, 0 = fixed/none.
preserve
keep if treated_state == 0 | gme_vol == 1 | gme_notvol == 1
quietly did_imputation matched_per_100k_yr program_numeric_id year year_expanded ///
    [aw=total_population_10], horizons(0/5) pretrend(5) cluster(state_id) ///
    hetby(gme_vol) fe(program_numeric_id year) minn(0) autosample
local mdd  = .
local mdse = .
local mdp  = .
capture nlcom (_b[tau0_1]+_b[tau1_1]+_b[tau2_1]+_b[tau3_1]+_b[tau4_1]+_b[tau5_1])/6 ///
            - (_b[tau0_0]+_b[tau1_0]+_b[tau2_0]+_b[tau3_0]+_b[tau4_0]+_b[tau5_0])/6
if _rc == 0 {
    matrix _mnl  = r(b)
    matrix _mnlV = r(V)
    local mdd  = _mnl[1,1]
    local mdse = sqrt(_mnlV[1,1])
    local mdp  = 2*normal(-abs(`mdd'/`mdse'))
}
post `res' ("mech_diff") (`mdd') (`mdse') (`mdp') (.) (.) (.)
di as result "pooled mech diff (volume - nonresponsive) = " %8.4f `mdd' ///
    " (se=" %8.4f `mdse' ", p=" %6.3f `mdp' ")"
restore

* --------------------------------------------------- 4) QUOTA (FDR family only)
capture confirm variable quota_per_100k_yr
if _rc == 0 {
    di _n "==================== QUOTA (quota_per_100k_yr) [FDR family, no figure] ===================="
    did_imputation quota_per_100k_yr program_numeric_id year year_expanded ///
        [aw=total_population_10], horizons(0/5) pretrend(5) ///
        fe(program_numeric_id year) cluster(state_id) minn(0)
    lincom (tau0+tau1+tau2+tau3+tau4+tau5)/6
    local a = r(estimate)
    local ase = r(se)
    local pt = .
    capture test pre1 pre2 pre3 pre4 pre5
    if _rc == 0 local pt = r(p)
    local tp = .
    capture test tau0 tau1 tau2 tau3 tau4 tau5
    if _rc == 0 local tp = r(p)
    quietly summarize quota_per_100k_yr if treated_state==1 & year<year_expanded [aw=total_population_10]
    local b = r(mean)
    local pct = cond(`b' < . & `b' != 0, 100*`a'/`b', .)
    post `res' ("quota") (`a') (`ase') (`tp') (`pt') (`b') (`pct')
    di as result "quota: avg=" %8.4f `a' " pct=" %5.1f `pct' " treat_p=" %6.3f `tp' " pretrend_p=" %6.3f `pt'
}

postclose `res'
use "`resfile'", clear
list, clean noobs
export delimited using "${tabdir}/yearvarying-suite-summary.csv", replace
di _n "=== year-varying suite complete: main-headline, main-location, main-mechanism-{volume,nonresp} ==="
log close
