#######################################################################
# Master script
#######################################################################

## Clear out existing environment
gc()
rm(list = ls()) 
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
# source(file.path(programs,"03-heat-map.r")) # generate heat map
# source(file.path(programs,"04-population-residents.r")) # generate population and residents data
# GME funding pipeline
source(file.path(programs,"14-append-gme-funding.R")) # append annual CMS GME funding spreadsheets into one panel
source(file.path(programs,"15-merge-gme-expansion.R")) # merge GME funding panel with Medicaid expansion status
source(file.path(programs,"17-merge-residency-gme-funding.R")) # merge residency program panel with GME funding via POS-anchored CCN crosswalk
# stata do files
# run sperately
# using stata
# file.path(programs, "05-dd-analysis.do") # event study analysis at the hospital level
# file.path(programs, "06-event-study-analysis.do") # event study analysis at Hospital-specialty level
# file.path(programs, "16-gme-funding-event-study.do") # event study of Medicaid expansion on GME funding

# scripts done
print("All scripts executed successfully.")