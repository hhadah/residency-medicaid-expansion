* =============================================================================
* _esplot-helpers.do — shared event-study plotting helpers (single source).
* Include from analysis scripts with:  do "${topdir}/programs/_esplot-helpers.do"
* Requires globals: ${figdir}, ${latex_figdir}.
*
* 2026-07-25: generalized for the full 2000-2019 panel — the default
* pre-window is now TEN periods (pretrend(10) is the pipeline default);
* pass npre explicitly for shorter windows (e.g., the pre-ACA placebo).
*
* _fillcoef [hmax] [npre] : builds matrix plot_coef ((npre+hmax+1) x 3:
*                     period coef se) from stored pre1-pre`npre' /
*                     tau0-tau`hmax' (defaults hmax = 5, npre = 10).
* _esplot args      : fname ytitle title avg baseline pct treatp pretrendp
*                     [hmax] [npre] — single-series event study (green pre /
*                     maroon post) with annotation box; exports PNG + PDF to
*                     both ${figdir} and ${latex_figdir}.
* =============================================================================

capture program drop _fillcoef
program define _fillcoef
    args hmax npre
    if ("`hmax'" == "") local hmax = 5
    if ("`npre'" == "") local npre = 10
    local nrows = `npre' + `hmax' + 1
    matrix plot_coef = J(`nrows', 3, .)
    matrix colnames plot_coef = period coef se
    local row = 1
    forval h = `npre'(-1)1 {
        matrix plot_coef[`row',1] = -`h'
        capture matrix plot_coef[`row',2] = _b[pre`h']
        capture matrix plot_coef[`row',3] = _se[pre`h']
        local ++row
    }
    forval h = 0/`hmax' {
        matrix plot_coef[`row',1] = `h'
        capture matrix plot_coef[`row',2] = _b[tau`h']
        capture matrix plot_coef[`row',3] = _se[tau`h']
        local ++row
    }
end

capture program drop _esplot
program define _esplot
    args fname ytitle title avg baseline pct treatp pretrendp hmax npre
    if ("`hmax'" == "") local hmax = 5
    if ("`npre'" == "") local npre = 10
    preserve
    clear
    svmat plot_coef, names(col)
    keep if !missing(period) & !missing(coef)
    gen ci_upper = coef + 1.96*se
    gen ci_lower = coef - 1.96*se
    gen byte pre_period  = (period < 0)
    gen byte post_period = (period >= 0)
    quietly summarize ci_upper
    local y_annot = r(max)*0.92
    local x_annot = -`npre' + 0.5
    local post_line ""
    if (`avg' < .) local post_line "(scatteri `avg' 0 `avg' `hmax', recast(line) lpattern(dash) lcolor(red) lwidth(medium))"
    local bl_t = string(`baseline', "%9.3f")
    local av_t = string(`avg', "%9.3f")
    local pc_t = string(`pct', "%9.1f")
    local tp_t = cond(`treatp'    < ., string(`treatp',    "%4.2f"), "NA")
    local pt_t = cond(`pretrendp' < ., string(`pretrendp', "%4.2f"), "NA")
    local annot `"text(`y_annot' `x_annot' `"Baseline mean: `bl_t'"' `"Post avg = `av_t' (`pc_t'%)"' `"Treatment p = `tp_t'"' `"Pre-trend p = `pt_t'"', placement(e) size(medsmall) justification(left))"'
    twoway ///
        (rarea ci_upper ci_lower period if pre_period,  fcolor(dkgreen%45) lcolor(dkgreen%45) lwidth(none)) ///
        (rarea ci_upper ci_lower period if post_period, fcolor(maroon%45)  lcolor(maroon%45)  lwidth(none)) ///
        (line coef period if pre_period,  lcolor(dkgreen) lwidth(medium)) ///
        (line coef period if post_period, lcolor(maroon)  lwidth(medium)) ///
        (scatter coef period if pre_period,  mcolor(dkgreen) msymbol(circle) msize(medium)) ///
        (scatter coef period if post_period, mcolor(maroon)  msymbol(circle) msize(medium)) ///
        `post_line' ///
        , xline(-0.5, lcolor(black) lwidth(thin)) yline(0, lcolor(black) lwidth(thin)) ///
        xlabel(-`npre'(1)`hmax', labsize(small)) ylabel(#8, labsize(small) format(%9.3f)) ///
        xtitle("Years relative to Medicaid expansion", size(small)) ///
        ytitle("`ytitle'", size(small)) title("`title'", size(medsmall)) ///
        legend(off) graphregion(color(white)) plotregion(color(white)) ///
        `annot'
    graph export "${figdir}/`fname'.png", as(png) replace width(1400) height(800)
    graph export "${latex_figdir}/`fname'.png", as(png) replace width(1400) height(800)
    graph export "${figdir}/`fname'.pdf", replace
    graph export "${latex_figdir}/`fname'.pdf", replace
    restore
end
