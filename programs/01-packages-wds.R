# This a script to downlaod
# packages and set working
# directories

# date: June 28th, 2022

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
               BiocManager, maps, sf, Polychrome, cdlTools, tools,
               ggtext, glue, scales, gganimate, ggrepel, MetBrewer, didimputation,
               tidyverse, AER, lubridate, dplyr, readr, MASS, rticles, did,
               bacondecomp, TwoWayFEWeights, fixest, glue, 
               DIDmultiplegt, panelView, did2s,
               tidygeocoder, zipcodeR,
               tigris, fips, stringr, etwfe)

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

theme_customs <- function() {
  theme_minimal(base_family = "serif") +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          plot.background = element_rect(fill = "white", color = NA),
          plot.title = element_text(face = "bold", size = 32),
          plot.subtitle = element_text(size = 26),
          axis.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold"),
          strip.background = element_rect(
          color="black", fill="white", linewidth=1.5
          ),
          legend.title = element_text(size = 28),
          axis.text.y  = element_text(size = 28),
          axis.text.x  = element_text(size = 28),
          axis.title.x = element_text(size = 30),
          axis.title.y = element_text(size = 30),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.text = element_text(size = 30))
}
theme_customs_map <- function() {
  theme_minimal(base_family = "IBM Plex Sans Condensed") +
    theme(
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      panel.background = element_blank(), 
      axis.line = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(face = "bold", size = 40),
      plot.subtitle = element_text(size = 30),
      axis.title = element_blank(),
      axis.text = element_blank(),
      strip.text = element_blank(),
      strip.background = element_blank(),
      legend.title = element_text(face = "bold", size = 30),  # Increased font size for the legend title
      legend.text = element_text(size = rel(2))
    )
}
# Theme of the plots
#theme_set(theme_clean() + theme(plot.background = element_blank()))
library(ggthemes)

theme_set(theme_clean() + 
            theme(#panel.background = element_rect(fill = "transparent"), # bg of the panel
              plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
              #panel.grid.major = element_blank(), # get rid of major grid
              panel.grid.minor = element_blank(), # get rid of minor grid
              legend.background = element_rect(fill = "transparent"), # get rid of legend bg
              legend.box.background = element_rect(fill = "transparent"), # get rid of legend panel bg
              #axis.line = element_line(size = 1, colour = "black"),
              #legend.title = element_blank(),
              legend.title=element_text(size=9),
              legend.text=element_text(size=8),
              legend.key.size = unit(.5, "cm")
            ))


p.style <- theme(
  panel.background = element_rect(fill = "transparent"), # bg of the panel
  plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
  #panel.grid.major = element_blank(), # get rid of major grid
  panel.grid.minor = element_blank(), # get rid of minor grid
  legend.background = element_rect(fill = "transparent"), # get rid of legend bg
  legend.box.background = element_rect(fill = "transparent"), # get rid of legend panel bg
  axis.line = element_line(linewidth = 1, colour = "black"),
  #legend.title = element_blank(),
  legend.title=element_text(size=9),
  legend.text=element_text(size=8),
  legend.key.size = unit(.5, "cm"))


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
