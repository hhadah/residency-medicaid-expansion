#######################################################################
# Master script
#######################################################################

## Clear out existing environment
gc()
rm(list = ls()) 
## Set master directory where all sub-directories are located
GSS_path <- "~/Dropbox/Research/GSS/Data/Datasets/GSS_Clean.csv"
GSS_bystate_path <- "~/Dropbox/Research/GSS/Data/Datasets/Prejeduice_index_by_stateyear.csv"
GSS_byregion_path <- "~/Dropbox/Research/GSS/Data/Datasets/Prejeduice_index_by_regionyear.csv"
GSS_bystate_new_path <- "~/Dropbox/Research/GSS/Data/Datasets/Prejeduice_index_by_stateyear_newest.csv"
GSS_bystate_new_path_iat <- "~/Dropbox/Research/GSS/Data/Datasets/GSS_Clean_IAT_reg.csv"

CPS_path <- "~/Dropbox/Research/My Research Data and Ideas/CPS_4GenData/CPS_DataTable.csv"
CPS_all_path <- "~/Dropbox/Research/My Research Data and Ideas/CPS_4GenData/CPS_DataTable_All.csv"
CPS_hispanic_mean <- "~/Dropbox/Research/My Research Data and Ideas/CPS_4GenData/CPS_hispanic_merge.csv"
CPS_hispanic_mean_county <- "~/Dropbox/Research/My Research Data and Ideas/CPS_4GenData/CPS_mean_reg_merge_county.csv"
CPS_asian <- "~/Dropbox/Research/My Research Data and Ideas/CPS_4GenData/CPS_DataTable_Asians.csv"
CPS_hispanic_mean_hh <- "~/Dropbox/Research/My Research Data and Ideas/CPS_4GenData/CPS_hispanic_merge_hh.csv"
CPS_interracial <- "~/Dropbox/Research/My Research Data and Ideas/CPS_4GenData/CPS_DataTable_Blacks.csv"
ACS_path <- "~/Dropbox/Research/My Research Data and Ideas/ACS-migration/ACS_DataTable.csv"

Implicity_Harvard <- "~/Dropbox/Research/My Research Data and Ideas/ProjectImplicit/skin_tone_iat/Skin IAT.public.2004-2021.csv"
Implicit_Race_Harvard <- "~/Documents/GiT/Project-Implicit-Data/data/datasets"
Implicit_asian_Harvard <- "~/Dropbox/Research/My Research Data and Ideas/ProjectImplicit/asian_iat/Asian_IAT.public.2004-2021.csv"
Implicit_arab_Harvard <- "~/Dropbox/Research/My Research Data and Ideas/ProjectImplicit/arab_muslim_iat/Arab IAT.public.2004-2021.csv"
Implicit_sexuality_Harvard <- "~/Dropbox/Research/My Research Data and Ideas/ProjectImplicit/sexuality_iat/Sexuality IAT.public.2004-2021.csv"

Identity_paper_tab <- "~/Documents/GiT/IdentificationProject/my_paper/tables"
Identity_paper_plots <- "~/Documents/GiT/IdentificationProject/my_paper/figure"

### GiT directories
git_mdir <- "~/Documents/GiT/Attitudes-and-Identity"
datasets <- paste0(git_mdir,"/data/datasets")
raw <- paste0(git_mdir,"/data/raw")
tables_wd <- paste0(git_mdir,"/output/tables")
figures_wd <- paste0(git_mdir,"/output/figures")
programs <- paste0(git_mdir,"/programs")
thesis_tabs <- paste0(git_mdir,"/my_paper/tables")
thesis_plots <- paste0(git_mdir,"/my_paper/figure")
pres_tabs <- paste0(git_mdir,"/presentations/jmp-presentation/tables")
pres_plots <- paste0(git_mdir,"/presentations/jmp-presentation/figures")

jmp_pres <- "~/Documents/GiT/jmp-presentation/xaringan" 

### run do files and scripts

# main scripts
source(file.path(programs,"01-packages-wds.r")) # set up package
source(file.path(programs,"02-clean-implicit-project.R")) # clean skin iat
source(file.path(programs,"03-merge-skin-iat-cps.R")) # merge cps and skin iat
source(file.path(programs,"04-table-one-summary-stats.R")) # table one summary stats
source(file.path(programs,"05-table-two-observations-by-gen.R")) # table two observations
source(file.path(programs,"06-figure-two-skin-iat-plots.R")) 
source(file.path(programs,"07-figure-three-skin-map.R")) 
source(file.path(programs,"08-table-three-summary-stats-iat-and-cps.R")) 
source(file.path(programs,"09-figure-five-skin-iat-regression-by-gen-plot.R")) 
source(file.path(programs,"10-figure-six-regressions-skin-iat-byparent-plot.R")) 
source(file.path(programs,"11-table-four-skin-iat-regression-by-gen.R")) 
source(file.path(programs,"12-table-five-regressions-skin-iat-byparent.R")) 
source(file.path(programs,"13-figure-seven-skin-iat-regression-interaction-bygen-plot.R")) 
source(file.path(programs,"14-figure-eight-skin-iat-regression-interaction-bygen-plot.R")) 
source(file.path(programs,"15-table-six-regressions-skin-iat-thirdgens-grandparents.R"))   
source(file.path(programs,"16-clean-skin-iat-level.R"))                                    
source(file.path(programs,"17-table-seven-regressions-acs-skin-iat-migration.R"))          
source(file.path(programs,"18-table-eight-endo-interethnic-reg.R"))                        
source(file.path(programs,"19-table-nine-proxy-main-effect-table.R"))                      
source(file.path(programs,"20-table-ten-skin-iat-regression-by-prox.R"))                   
source(file.path(programs,"21-table-eleven-regressions-cps-skin-iat.R"))                   
source(file.path(programs,"22-merge-gss-cps.R"))                                           
source(file.path(programs,"23-table-twelve-gss-bygen-regressions.R"))                      
source(file.path(programs,"24-table-thirteen-gss-regression-by-parent-type.R"))            
source(file.path(programs,"25-figure-nine-regressions-skin-iat-thirdgens-grandparents-interactions.R")) 
source(file.path(programs,"26-figure-nine-regressions-skin-iat-thirdgens-grandparents-interactions.R")) 
source(file.path(programs,"27-figure-10-cps-hispanic-plot.R")) 
source(file.path(programs,"28-figure-13-14-15-merge-iat-gss.R"))
source(file.path(programs,"29-figure-eleven-cps-hispanic-plot-hh.R"))
source(file.path(programs,"30-figure-sixteen-hh-skin-iat-cps-hispanic-plot.R"))

# other stuff
source(file.path(programs,"31-table-hispanic-identity-by-parent.R"))
source(file.path(programs,"32-table-observations-by-gen-high-bias.R")) 
source(file.path(programs,"33-table-observations-by-gen-low-bias.R")) 
source(file.path(programs,"34-skin-iat-regression-interaction-bygen-byparent-second-gen.R")) 
source(file.path(programs,"35-skin-iat-regression-interaction-bygen-bygrandparent-third-gen.R")) 
source(file.path(programs,"36-regressions-skin-iat-byparent-interethnic.R"))                
source(file.path(programs,"37-crosstab.R"))                                                 
stata(file.path(programs, "38-scatter-plot-with-fe.do"))                                    
source(file.path(programs,"39-multi-logit-reg.R"))                                          
stata(file.path(programs, "40-probit-regression-stata.do"))                                 
source(file.path(programs,"41-merge-skin-iat-cps-3-years.R"))                               
stata(file.path(programs, "42-probit-regression-stata.do"))                                 
source(file.path(programs,"43-regressions-skin-iat-byparent-three-years-ints.R"))           
source(file.path(programs,"44-regressions-skin-iat-on-parents-type.R"))                     
source(file.path(programs,"45-regressions-hispanic-on-skin-iat.R"))                         
source(file.path(programs,"46-regressions-skin-iat-byparent-specifications.R"))             
source(file.path(programs,"47-merge-gss-cps-1990s.R"))                                      
source(file.path(programs,"48-gss-bygen-regressions-1990s.R"))                              
source(file.path(programs,"49-gss-regression-by-parent-type-1990s.R"))                      
source(file.path(programs,"50-regressions-skin-iat-thirdgens-grandparents-cg-index-90s.R")) 
source(file.path(programs,"51-marginal-effect-bias.R")) 
source(file.path(programs,"52-some-graphs.R"))          
source(file.path(programs,"53-hispanic-bubble-plot.R")) 
source(file.path(programs,"54-skin-iat-regression-interaction-bygen-byparent.R"))     
source(file.path(programs,"55-skin-iat-regression-interaction-bygen-byparent-dad.R")) 
source(file.path(programs,"56-skin-iat-regression-interaction-bygen-byparent-mom.R")) 
source(file.path(programs,"57-binscatter.R"))
source(file.path(programs,"58-skin-iat-regression-by-gen-msa.R"))
source(file.path(programs,"59-regressions-skin-iat-byparent-msa.R"))
source(file.path(programs,"60-skin-iat-regression-interaction-bygen-byparent-msa.R"))
source(file.path(programs,"61-merge-skin-iat-cps-on-county-level.R"))
source(file.path(programs,"62-regression-cps-skin-iat-county.R"))
source(file.path(programs,"63-skin-iat-regression-by-gen-county.R"))
source(file.path(programs,"64-regressions-skin-iat-byparent-county.R"))
source(file.path(programs,"65-skin-iat-regression-interaction-bygen-byparent-county.R"))
source(file.path(programs,"66-skin-iat-regression-interaction-bygen-plot-msa.R"))
source(file.path(programs,"67-skin-iat-regression-interaction-bygen-plot-msa.R"))
source(file.path(programs,"68-merge-skin-iat-cps-on-msa-level.R"))
source(file.path(programs,"69-regression-cps-skin-iat-msa.R"))

### summary stats

# Send Message

textme(message = "👹 Back to work! You're not paid to run around and drink ☕ all day!")