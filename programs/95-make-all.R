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

### run do files and scripts

# main scripts
source(file.path(programs,"01-packages-wds.r")) # set up package

### summary stats

# Send Message

textme(message = "👹 Back to work! You're not paid to run around and drink ☕ all day!")