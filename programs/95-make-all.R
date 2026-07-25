#######################################################################
# Master script — R data-build pipeline (scripts 01-12)
# Scripts are numbered in execution order: 01 runs before 02, etc.
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
# source(file.path(programs,"02-data-cleaning.R"))         # clean NRMP data + geocode (needs MAPBOX_API_KEY in ~/.Renviron; slow)
source(file.path(programs,"03-state-year-population.R"))   # ACS year-varying state population -> state_year_population.dta
source(file.path(programs,"04-alternative-deflators.R"))   # ACS 18-64 + <150% FPL deflators -> state_year_deflators.dta
source(file.path(programs,"05-append-gme-funding.R"))      # append annual CMS GME funding spreadsheets into one panel
source(file.path(programs,"06-merge-gme-expansion.R"))     # merge GME funding panel with Medicaid expansion status
source(file.path(programs,"07-merge-residency-cms.R"))     # residency <-> CMS readmissions crosswalk (fallback layer for 08)
source(file.path(programs,"08-merge-residency-gme-funding.R")) # NRMP institution -> CCN crosswalk + linked funding panel
source(file.path(programs,"09-entry-exit-panel.R"))        # entry/exit-corrected panel -> program_panel_entry_exit.dta
source(file.path(programs,"10-balance-table.R"))           # sumstats + GME-formula balance tables
source(file.path(programs,"11-heat-map.R"))                # desc-timing + cohort figures
source(file.path(programs,"12-population-residents.R"))    # desc-physician-growth (reads raw IPUMS extracts; ~minutes)

# Stata analysis (scripts 13-36): run programs/99-run-all-analysis.do
# AFTER the Stata suite has produced its summary and RI CSVs, run:
#   python3 programs/37-multiple-testing-qvalues.py  # dual-standard FDR q-values + forest plot
# Auxiliary (not part of the paper pipeline):
#   programs/90-make-2000-2019-residency-panel.R     # 2000-2019 back-extension (pre-ACA placebo groundwork)

print("All scripts executed successfully.")
