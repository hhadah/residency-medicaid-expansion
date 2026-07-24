* =============================================================================
* _ri-avgatt.do — shared randomization-inference estimation helper.
* Include with:  do "${topdir}/programs/_ri-avgatt.do"
*
* _avgatt2, rclass : args cohortvar outcome hmax extra
*   Runs did_imputation on the CURRENT data for `outcome' with cohort variable
*   `cohortvar', horizons 0/`hmax', program+year FE, state clustering,
*   population weights, plus any `extra' options (e.g. autosample), and returns
*   r(att) = mean of the available tau0..tau`hmax' coefficients (missing on
*   estimation failure). Mirrors the reported specs' estimator options.
* =============================================================================

capture program drop _avgatt2
program define _avgatt2, rclass
    args cohortvar outcome hmax extra
    capture noisily did_imputation `outcome' program_numeric_id year `cohortvar' ///
        [aw=total_population_10], horizons(0/`hmax') pretrend(5) ///
        fe(program_numeric_id year) cluster(state_id) minn(0) `extra'
    if (_rc != 0) {
        return scalar att = .
        exit
    }
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
