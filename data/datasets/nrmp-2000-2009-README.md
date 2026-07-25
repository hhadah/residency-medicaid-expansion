# NRMP Residency Match Data, 2000–2009 (extracted from source PDFs)

Extracted directly from the NRMP *Results and Data* books in
`Box: Murphy Institute - Encoding Physician Data/Hospital-level, 2000 to 2014/`
(`resultsanddata2000.pdf` … `resultsanddata2009.pdf`), replacing the
RA-collected spreadsheet `data/raw/residency-spots-00to09.xlsx`, which had
systematic transcription errors in 2000–2005.

## Files

- **`2000_2019_residency_programs.dta`** — combined standardized panel, same
  structure as `data/raw/2010_2019_residency_programs.dta`: one row per
  institution × 3-digit specialty (5,470 rows = the 4,415 baseline rows
  plus 1,055 rows for programs that closed before 2010), wide columns
  `quota_2000 … matched_2019`. Institution/city names and
  `program_name_standardized` use the 2010–2019 file's spelling wherever the
  institution exists there (647 institutions); closed programs use the
  cleaned modal extraction spelling (382). 2000–01 specialties were mapped
  from program text to the standard 3-digit codes (exact → fuzzy →
  keyword → 19 hand-mapped residuals; zero rows dropped). Conventions:
  0 = no positions that year; **NA** = not observed (2002 for the
  missing-page institutions: 212 rows). **Puerto Rico is excluded** from
  this panel (absent from the 2010–2019 source and outside the analysis
  sample); PR remains in the program-level csv below.
  Built by `programs/23-make-2000-2019-residency-panel.R`; every year's
  total reconciles with the program-level file net of PR.
- **`nrmp-2000-2009-standardization-crosswalk.csv`** — institution-name
  crosswalk (extraction spelling → standardized name, with source).
- **`nrmp-programs-2000-2009.csv`** — program-level (37,159 rows): one row per
  year × program. Columns: `year, state, hospital, city, program,
  program_type, code, inst_code, quota, matched, plus_flag, flag, src`.
- **`nrmp-hospitals-2000-2009.csv`** — institution-year aggregate (6,862 rows):
  `inst_code, year, hospital_name, state, city, n_programs, quota, matched`.

## Code formats

- **2002–2009**: 9-character NRMP program code (4-digit institution +
  3-digit specialty + type letter + digit), same scheme as the
  2010–2019 file. `inst_code` = first 4 digits, links to
  `institution_code` in `data/raw/2010_2019_residency_programs.dta`
  (floor the decimal codes there first).
- **2000–2001**: the books use a different 6-digit scheme
  (4-digit institution + 2-digit sequential program number); the program
  type letter (C/P/S) is in `program_type`. `inst_code` (first 4 digits) is
  consistent with later years. `plus_flag` marks rows printed with a `+`.
  Specialty must be taken from the `program` text, not the code.

## Extraction pipeline (scripts in `programs/nrmp-pdf-extraction/`)

- 2007–2009 have text layers → parsed directly (`pdftotext`, column crops).
- 2000–2006 are scans → 300-dpi render, adaptive two-column split
  (word-box gutter detection; several pages have a printed vertical rule or
  wide intra-column gaps that defeat naive splitting), tesseract OCR (TSV),
  cross-validated against Apple Vision OCR by spatial join, with the
  RA spreadsheet as a third witness where the engines were weak.
- Rows the engines could not settle (~2,900) were transcribed from zoomed
  image crops by Claude agents; implausible values, duplicate codes, and
  matched>quota rows got a second verification pass.

## `flag` column (provenance per row)

| flag | meaning |
|---|---|
| `ok` / `clean` | tesseract high-confidence (text years are all `ok`) |
| `agree` / `vision_confirmed` | tesseract and Vision agree |
| `vision_fix` | tesseract garbled; Vision supplied numbers |
| `*_ra_diff` | engines agree but RA spreadsheet differs (RA presumed wrong) |
| `lowconf_vision_partial` | low-confidence tesseract pair, Vision confirms one number |
| `lowconf_ra_confirmed` / `conflict_ra_*` | RA witness used as tie-breaker |
| `manual` / `verified` / `manual_code_fix` | human-read from zoomed crop |
| `montage*` / `unresolved` | still unresolved (≤ 5 rows/year; excluded rows: 39 total) |

## Validation

- Printed grand totals in the books: 2000 quota 22,722 / matched 20,272 vs
  extracted 22,880 / 20,213 (+0.7% / −0.3%); 2001 printed 22,878 / 20,410 vs
  extracted 22,866 / 20,225 (−0.05% / −0.9%).
- 2009 extracted total (25,185 positions) matches NRMP's published figure
  exactly; 2007/2008 totals are internally consistent with the books.
- Agreement with the RA spreadsheet: ~100% in 2007–2009 and 98% in 2006
  (their clean years); 78–90% in 2000–2005 (their corrupted years).
- No `matched > quota` rows; no duplicate `year × code` rows (65 collisions
  resolved, mostly identical duplicates).
- 2009→2010 seam: institution-level quota correlation 0.994 with
  `2010_2019_residency_programs.dta`.

## Known gap: 2002 source PDF is missing pages

`resultsanddata2002.pdf` skips from the end of Florida (Pensacola) directly to
Illinois (Berwyn/Chicago) and contains one Indiana page twice — roughly
printed pages 12–13 of 47 were never scanned. As a result, **2002 is missing
all of Georgia, Hawaii, and Idaho, plus the tail of Florida and the head of
Illinois** — 38 institutions, ≈850 positions (e.g., Emory, Medical College of
Georgia, Morehouse, Cook County, Evanston Northwestern, Lutheran General,
Children's Memorial, U Hawaii programs). Every other year is complete.
NRMP's own posted PDF (nrmp.org) has the identical defect — the Box copy
derives from it — so the fix requires the pages from NRMP directly or a
print/library copy of the 2002 book. Until then, treat 2002 as missing (not
zero) for GA/HI/ID and for the affected FL/IL institutions.

## State column repair

The books' running state headers were unreliable to OCR (worst in 2000–2002),
so `state` was re-derived from the institution code using the 2010–2019 Stata
file, the modal 2003–2009 assignment, and the RA file's clean years
(`programs/nrmp-pdf-extraction/fix_states.py`). 479 rows (mostly 2000–2001
institutions that closed before 2003) had no authority and keep the
as-parsed state.

Extraction date: 2026-07-24. Known caveats: 2000–2001 hospital names are
OCR'd typewriter text (verify before exact-matching); the 2010–2019 file
excludes Puerto Rico while these data include it. 2000–2001 have ~810–827
distinct institutions vs ~640–675 from 2002 on — this is predominantly real
early-2000s consolidation (the independently transcribed RA file shows the
same pattern: 769/808), though a small number of residual code-digit
misreads may fragment institutions in those two years.
