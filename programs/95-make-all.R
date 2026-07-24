#######################################################################
# Master script
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

### run do files and scripts

# main scripts
source(file.path(programs,"01-packages-wds.r")) # set up package
# source(file.path(programs,"02-data-cleaning.R")) # clean and merge data
source(file.path(programs,"03-heat-map.r")) # desc-timing + cohort figures (PNG+PDF)
source(file.path(programs,"04-population-residents.r")) # desc-physician-growth (reads raw IPUMS extracts; ~minutes)
source(file.path(programs,"04b-state-year-population.R")) # year-varying ACS state population -> state_year_population.dta (needed by Stata 24/25/26, 18c/18d)
# GME funding pipeline
source(file.path(programs,"14-append-gme-funding.R")) # append annual CMS GME funding spreadsheets into one panel
source(file.path(programs,"15-merge-gme-expansion.R")) # merge GME funding panel with Medicaid expansion status
source(file.path(programs,"17-merge-residency-gme-funding.R")) # merge residency program panel with GME funding via POS-anchored CCN crosswalk
# balance table (2010 baseline across GME formula groups; needs only cleaned dta + raw CSV)
source(file.path(programs,"27-balance-table.R")) # -> my_paper/tables/balance_gme_formula.tex + output/tables/balance-gme-formula.csv
# stata do files
# run sperately (programs/99-run-all-analysis.do drives 05/06/07/11/12/16 + 18/18c/18d/20/24/25/26)
# AFTER the Stata suite has produced its summary CSVs (24/25) and the RI CSVs (18c/18d), run:
#   python3 programs/22-multiple-testing-qvalues.py   # dual-standard FDR q-values + forest plot

# scripts done
print("All scripts executed successfully.")