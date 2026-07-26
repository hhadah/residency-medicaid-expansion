#######################################################################
# Master script — R data-build pipeline (scripts 01-12)
# Scripts are numbered in execution order: 01 runs before 02, etc.
# The FULL 2000-2019 panel (scripts 05-06) is the dataset all Stata
# analysis (13-36) runs on.
# (Renumbered 2026-07-25; old->new mapping in programs/00-README-pipeline.md)
#######################################################################

## Set master directory where all sub-directories are located

### GiT directories
git_mdir <- here::here()
datasets <- paste0(git_mdir,"/data/datasets")
raw <- paste0(git_mdir,"/data/raw")
tables_wd <- paste0(git_mdir,"/output/tables")
figures_wd <- paste0(git_mdir,"/output/figures")
programs <- paste0(git_mdir,"/programs")
thesis_tabs <- paste0(git_mdir,"/my_paper/tables")
thesis_plots <- paste0(git_mdir,"/my_paper/figures")
options(modelsummary_factory_latex = "kableExtra")

### run scripts in numeric order

source(file.path(programs,"01-packages-wds.R"))            # packages + paths
source(file.path(programs,"02-data-cleaning.R"))         # clean 2010-2019 NRMP data + geocode (needs MAPBOX_API_KEY in ~/.Renviron; slow)
source(file.path(programs,"03-state-year-population.R"))   # ACS/decennial state population, 2010-2019 + 2000-2019 series
source(file.path(programs,"04-alternative-deflators.R"))   # ACS 18-64 + <150% FPL deflators -> state_year_deflators.dta
source(file.path(programs,"05-make-2000-2019-residency-panel.R"))   # standardize 2000-2009 OCR extraction into the 2000-2019 wide file
source(file.path(programs,"06-make-2000-2019-estimation-panels.R")) # PRIMARY PANELS: institution + specialty estimation panels, entry/exit table
source(file.path(programs,"07-append-gme-funding.R"))      # append annual CMS GME funding spreadsheets into one panel
source(file.path(programs,"08-merge-gme-expansion.R"))     # merge GME funding panel with Medicaid expansion status
source(file.path(programs,"09-merge-residency-cms.R"))     # residency <-> CMS readmissions crosswalk (fallback layer for 10)
source(file.path(programs,"10-merge-residency-gme-funding.R")) # NRMP institution -> CCN crosswalk + linked funding panel
source(file.path(programs,"11-balance-table.R"))           # sumstats + GME-formula balance tables (full panel, 2010 baseline)
source(file.path(programs,"12-descriptive-figures.R"))     # desc-timing, cohort figures (full panel) + desc-physician-growth (IPUMS)

# NOTE: 06 merges the CCN crosswalk built by 10 when available; after the
# first-ever run of 10, re-run 06 once so provider_ccn is attached.

# Stata analysis (scripts 13-36): run programs/99-run-all-analysis.do
# AFTER the Stata suite has produced its summary and RI CSVs, run:
#   python3 programs/37-multiple-testing-qvalues.py  # dual-standard FDR q-values + forest plot

print("All scripts executed successfully.")
