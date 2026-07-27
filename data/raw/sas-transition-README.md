# AOA→ACGME Single Accreditation System (SAS) transition lists — ACQUIRED 2026-07-26

**Why:** Desk review 2026-07-26 (AEJ:Policy), return condition: classify every
post-2010 NRMP entrant institution as (i) a genuinely new sponsoring
institution or (ii) a pre-existing AOA-accredited sponsor migrating under the
Single Accreditation System (July 2015–June 2020), then re-estimate the entry
margin and state totals on genuine entrants only.

**Source:** ACGME Accreditation Data System (ADS) public reports,
https://apps.acgme.org/ads/public/, downloaded 2026-07-26:

- `sas-transition/sas_programs_report18.pdf` — ADS public Report 18, "All
  Programs that Applied for Accreditation Under the Single Accreditation
  System" (full historical list incl. voluntary withdrawals; 743 programs,
  75 pp., AY2026-27 snapshot of the historical applicant set).
- `sas-transition/sas_sponsors_report14.pdf` — ADS public Report 14, "List of
  Sponsors that Applied for Accreditation Under the Single Accreditation
  System" (sponsor-level, 77 sponsors, current-AY snapshot; used as a
  cross-check only).

**Pipeline:**
1. `programs/38-parse-sas-transition.py` → `data/raw/sas_transition_programs.csv`
   (program_number, program_name, city, state, specialty, status, effective
   date) and `data/raw/sas_transition_institutions.csv` (name × state rollup).
2. `programs/39-sas-entrant-crosswalk.R` → `data/datasets/sas_entrant_classification.csv`:
   within-state Jaro-Winkler + token-overlap matching of the 201 post-2010
   NRMP entrants against SAS applicant names (compound "School/Hospital"
   names split; parenthesized acronyms extracted; OPTI/osteopathic names
   auto-classified). Bands: match / match_tokens / match_city / osteo_name
   (⇒ sas_migrant = 1), review, no_match.
3. `data/raw/sas_entrant_manual_overrides.csv` — manual adjudication of the
   review/no_match rows (institution_code, override_sas_migrant ∈ {0,1,unknown},
   confidence, evidence). **Assistant-drafted from web research; user should
   audit the high-impact rows.** Applied by `programs/28-entryexit-estimation.do`.
4. `programs/28-entryexit-estimation.do` specs 6–8 consume the classification
   (no_sas_migrants, state_total_nosas ±weights, entry_genuine /
   entry_sasmigrant) → `output/tables/entryexit-estimation.csv` and
   `output/tables/entry-classification-counts.csv`.

**Caveats:** ADS public reports are current-AY snapshots of the historical
applicant list; AOA sponsors that closed before ever applying to ACGME are
not listed (they also never enter NRMP, so they do not affect entrant
classification). Matching is name-based: NRMP sponsors are often hospitals
while SAS applicants are listed under medical-school/OPTI names — hence the
token/acronym matching layers and the manual override file.
