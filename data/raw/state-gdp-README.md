# State GDP / non-demographic deflator — ACQUIRED 2026-07-26

**Why:** The methods referee (2026-07-24, Major Comment 1) asks that the
headline effect be shown under at least three alternative deflators, including
one **non-demographic** scale (total state inpatient discharges or state GDP),
because every demographic denominator is potentially post-treatment.

**What to download:**
- BEA regional accounts, annual state GDP (SAGDP2N, all-industry total),
  2010–2019: https://apps.bea.gov/regional/downloadzip.htm (SAGDP zip), or via
  the BEA API with a (free) API key.
- Alternative: HCUP state inpatient discharge totals (may require purchase),
  or AHA Annual Survey state totals.

**Where it plugs in:**
1. Saved as `data/raw/state_gdp.csv` with columns `state, year, gdp` (state =
   2-letter code, gdp in current $M; 51 units × 2000–2019).
2. Merged in `programs/26-deflator-robustness.do` (post-2026-07-25 numbering)
   as the `per_gdp` outcome row (positions per $billion of state GDP).

**Status:** DONE 2026-07-26. Source: BEA SAGDP bulk zip
(https://apps.bea.gov/regional/zip/SAGDP.zip), table SAGDP2 (NAICS,
current-dollar, LineCode 1 = all-industry total), downloaded 2026-07-26.
Result: +3.9% of baseline, pre-trend p = 0.14 (the only alternative deflator
that passes), CI includes zero — reported in §7's denominator item.
The two demographic alternatives (age 18–64, <150% FPL) are implemented in
`programs/04-alternative-deflators.R` + `programs/26-deflator-robustness.do`.
