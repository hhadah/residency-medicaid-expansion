# State GDP / non-demographic deflator — data still needed (referee request)

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
1. Save as `data/raw/state_gdp.csv` with columns `state, year, gdp` (state =
   2-letter code, gdp in current $M).
2. Add a merge in `programs/32-deflator-robustness.do` and a
   `matched_per_gdp` outcome row mirroring the `percap_age1864` /
   `percap_u150fpl` runs already implemented there.

**Status:** flagged 2026-07-24; the two demographic alternatives (age 18–64,
<150% FPL) are implemented in `programs/04c-alternative-deflators.R` +
`programs/32-deflator-robustness.do`.
