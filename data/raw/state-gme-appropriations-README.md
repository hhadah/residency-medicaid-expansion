# State Medicaid GME appropriations (state-year) — data still needed

**Why:** MUST-8 asks for the headline controlled for state GME appropriations
per capita, because control-group states (TX, FL, GA, TN) ran their own GME
expansions during the window. The exclusion version is implemented in
`programs/33-specification-grid.do` (`yrvar_*_noGMEcontrols`); the
appropriations-control version needs a state-year dollars panel.

**Source to digitize:** the Henderson/AAMC Medicaid GME survey series —
survey waves covering 2012, 2015, 2018, and 2022 (published 2013, 2016, 2019
["Henderson 2013" etc. in `my_paper/bib/references.bib`], plus AAMC 2021/2023
updates). Each wave reports total Medicaid GME spending by state (FFS and
managed care separately in later waves).

**Format:** `data/raw/state_gme_appropriations.csv` with columns
`state, survey_year, medicaid_gme_total, medicaid_gme_ffs, medicaid_gme_mc`
(dollars; NA where a state did not respond). Linear interpolation between
waves for the estimation panel is acceptable and should be noted.

**Where it plugs in:** merge into `programs/33-specification-grid.do`, add
`controls(gme_approp_pc)` runs mirroring the existing grid rows.

**Status:** flagged 2026-07-24; exclusion-based checks are done, the dollars
control awaits this file.
