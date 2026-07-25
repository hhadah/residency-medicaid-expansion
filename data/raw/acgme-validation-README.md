# ACGME Data Resource Book validation — data still needed (referee request)

**Why:** The domain referee (2026-07-24 report, Major Comment 2) and the editor
(MUST-2) ask for validation of NRMP-based training counts against an
*independent* count of residents on duty. The designated source is the ACGME
**Data Resource Book** (annual), with AAMC **GME Track** as an alternative.

**What to download:**
- ACGME Data Resource Book editions covering academic years 2009-10 through
  2019-20: https://www.acgme.org/about/publications-and-resources/graduate-medical-education-data-resource-book/
- The tables needed: **residents on duty by state** (and, if available, by
  specialty) for each academic year.

**Where it plugs in:**
1. Save extracted tables as `data/raw/acgme-residents-on-duty.csv` with columns
   `state, academic_year, residents_on_duty` (state = 2-letter code;
   academic_year = starting calendar year, e.g. 2014 for AY2014-15).
2. Validation exercise (to be scripted once the file exists — see
   `programs/30-linked-sample-reconciliation.do` for the estimation pattern):
   - Compare state-year NRMP matched-position stocks (sum of matched over the
     preceding 3-4 cohorts) against ACGME residents on duty, in levels and
     growth rates, by expansion status and GME-formula arm.
   - The reconciliation question: does the cost-report FTE rise (+19% in
     volume-responsive states) or the NRMP intake path (+3.4%) track the
     independent ACGME count?

**Status:** flagged 2026-07-24 during the referee-response campaign; not yet
downloaded (the Data Resource Book PDFs may require manual navigation).
