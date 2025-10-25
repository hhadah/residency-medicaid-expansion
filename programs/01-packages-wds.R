# This a script to downlaod
# packages and set working
# directories

# date: May 18th, 2022

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tictoc, parallel, pbapply, future, 
               future.apply, furrr, RhpcBLASctl, memoise, 
               here, foreign, mfx, tidyverse, hrbrthemes, 
               estimatr, ivreg, fixest, sandwich, lmtest, 
               margins, vtable, broom, modelsummary, 
               stargazer, fastDummies, recipes, dummy, 
               gplots, haven, huxtable, kableExtra, 
               gmodels, survey, gtsummary, data.table, 
               tidyfast, dtplyr, microbenchmark, ggpubr, 
               tibble, viridis, wesanderson, censReg, 
               rstatix, srvyr, formatR, sysfonts, 
               showtextdb, showtext, thematic, 
               sampleSelection, textme, paletteer, 
               wesanderson, patchwork, RStata, car,
               #textme, lodown,
               BiocManager, Polychrome, effects,
               maps, sf, multcomp, cdlTools,
               finalfit, ggtext, glue, scales, 
               gganimate, ggrepel, MetBrewer, fs,
               marginaleffects, gghighlight, ggview,
               camcorder, rnaturalearth, rnaturalearthdata,
               latex2exp)
options("RStata.StataPath" = "/Applications/Stata/StataSE.app/Contents/MacOS/stata-se")
options("RStata.StataVersion" = 17)
            
# devtools::install_github("thomasp85/patchwork")
# devtools::install_github("ajdamico/lodown")
## My preferred ggplot2 plotting theme (optional)
## https://github.com/hrbrmstr/hrbrthemes
# scale_fill_ipsum()
# scale_color_ipsum()
font_add_google("Fira Sans", "firasans")
font_add_google("Fira Code", "firasans")

showtext_auto()

showtext_opts(dpi = 300)
camcorder::gg_record(
  dir = figures_wd,
  device = "png",
  width = 10,
  height = 10 + 9 / 16, #16:9 ratio
  units = 'cm',
  dpi = 300
)
theme_customs <- function() {
  theme_minimal(base_family = "IBM Plex Sans Condensed") +
    theme(panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "white", color = NA),
          plot.title = element_text(face = "bold"),
          axis.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold"),
          strip.background = element_rect(fill = "grey80", color = NA),
          legend.title = element_text(face = "bold", size = rel(1)),
          legend.text = element_text(size = rel(1)))
}

theme_customs_map <- function() {
  theme_minimal(base_family = "IBM Plex Sans Condensed") +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_blank(),
          plot.background = element_rect(fill = "white", color = NA),
          plot.title = element_text(face = "bold"),
          axis.title = element_blank(),
          axis.text = element_blank(),
          strip.text = element_blank(),
          strip.background = element_blank(),
          legend.title = element_text(face = "bold", size = rel(1.5)),
          legend.text = element_text(size = rel(1.2)))
}
# Make labels use IBM Plex Sans by default
update_geom_defaults("label", 
                     list(family = "IBM Plex Sans Condensed"))
update_geom_defaults(ggtext::GeomRichText, 
                     list(family = "IBM Plex Sans Condensed"))
update_geom_defaults("label_repel", 
                     list(family = "IBM Plex Sans Condensed"))

# Use the Johnson color palette
clrs <- met.brewer("Johnson")

# theme_set(theme_minimal() + theme_customs)
# theme_set(hrbrthemes::theme_ipsum() + theme_customs)
## Set master directory where all sub-directories are located
# mdir <- "/Users/hhadah/Dropbox/Research/My Research Data and Ideas/"
# mdir <- "C:\\Users\\16023\\Dropbox\\Research\\My Research Data and Ideas\\Identification_Paper"
## Set working directories
# workdir  <- paste0(mdir,"/Data/Datasets")
# rawdatadir  <- paste0(mdir,"/Data/Raw")
# tables_plots_dir <- paste0(mdir, "/Users/hhadah/Documents/GiT/Depression_Idea/outputs")

## Set working directory

# COLOR PALLETES
library(paletteer) 
# paletteer_d("awtools::a_palette")
# paletteer_d("suffrager::CarolMan")

### COLOR BLIND PALLETES
paletteer_d("colorblindr::OkabeIto")
paletteer_d("colorblindr::OkabeIto_black")
paletteer_d("colorBlindness::paletteMartin")
paletteer_d("colorBlindness::Blue2DarkRed18Steps")
paletteer_d("colorBlindness::SteppedSequential5Steps")
paletteer_d("colorBlindness::PairedColor12Steps")
paletteer_d("colorBlindness::ModifiedSpectralScheme11Steps")
colorBlindness <- paletteer_d("colorBlindness::Blue2Orange12Steps")
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# scale_colour_paletteer_d("colorBlindness::ModifiedSpectralScheme11Steps", dynamic = FALSE)
# To use for fills, add
scale_fill_manual(values="colorBlindness::Blue2Orange12Steps")
scale_color_paletteer_d("nord::aurora")
scale_color_paletteer_d("colorBlindness::Blue2Orange12Steps")

# To use for line and point colors, add
scale_colour_manual(values="colorBlindness::Blue2Orange12Steps")
