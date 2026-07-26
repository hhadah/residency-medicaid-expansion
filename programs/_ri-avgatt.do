* =============================================================================
* _ri-avgatt.do — shared randomization-inference estimation helper.
* Include with:  do "${topdir}/programs/_ri-avgatt.do"
*
* _avgatt2, rclass : args cohortvar outcome hmax extra
*   Runs did_imputation on the CURRENT data for `outcome' with cohort variable
*   `cohortvar', horizons 0/`hmax', program+year FE, state clustering,
*   population weights, plus any `extra' options (e.g. autosample), and returns
*     r(att) = average of tau0..tau`hmax' (missing on estimation failure)
*     r(se)  = its clustered standard error (via lincom; missing if the
*              average had to fall back to manual summation)
*     r(t)   = r(att)/r(se)
*   The t-statistic supports STUDENTIZED randomization inference (permute
*   |t| rather than |att|), the MacKinnon-Webb/Young-recommended standard
*   when cluster weights are highly variable (referee response, MUST-9).
*   Mirrors the reported specs' estimator options (pretrend(10) since the
*   2026-07-25 migration to the full 2000-2019 panel).
* =============================================================================

capture program drop _avgatt2
program define _avgatt2, rclass
    args cohortvar outcome hmax extra
    return scalar att = .
    return scalar se  = .
    return scalar t   = .
    capture noisily did_imputation `outcome' program_numeric_id year `cohortvar' ///
        [aw=total_population_10], horizons(0/`hmax') pretrend(10) ///
        fe(program_numeric_id year) cluster(state_id) minn(0) `extra'
    if (_rc != 0) exit

    * preferred: lincom gives both the average and its SE
    local lc "(tau0"
    forval h = 1/`hmax' {
        local lc "`lc'+tau`h'"
    }
    local lc "`lc')/`=`hmax'+1'"
    capture lincom `lc'
    if (_rc == 0) {
        return scalar att = r(estimate)
        return scalar se  = r(se)
        if (r(se) < . & r(se) > 0) return scalar t = r(estimate)/r(se)
        exit
    }

    * fallback (some horizons missing under autosample): manual average, no SE
    local s = 0
    local n = 0
    forval h = 0/`hmax' {
        capture scalar __b = _b[tau`h']
        if (_rc == 0) {
            local s = `s' + __b
            local n = `n' + 1
        }
    }
    capture scalar drop __b
    return scalar att = cond(`n' > 0, `s'/`n', .)
end
