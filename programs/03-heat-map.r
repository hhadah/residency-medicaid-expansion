# This is a script
# to plot heatmaps
# and some outcome
# plots by treatment
# status

# Date: April 12th, 2025

# open data
long_data <- read_dta(file.path(datasets,"cleaned_residency_medicaid.dta"))

# create data by state, year, program by summing
# all specialties within institution
long_data <- long_data |> 
  group_by(state, institution_code, year) |>
  summarize(
    matched = sum(matched, na.rm = TRUE),
    quota = sum(quota, na.rm = TRUE),
    unmatched = sum(unmatched, na.rm = TRUE),
    city = first(city),
    year_expanded = first(year_expanded),
    medicaid_expansion = first(medicaid_expansion)
  ) |> 
  ungroup()

long_data |> 
  glimpse()

## make sure output dirs exist
dir.create(figures_wd,   recursive = TRUE, showWarnings = FALSE)
dir.create(thesis_plots, recursive = TRUE, showWarnings = FALSE)

## ----------------------------------------------------
## 1. Generate the plot with panelview()
## ----------------------------------------------------
panelview(
  matched ~ medicaid_expansion,
  data = long_data,
  index = c("institution_code","year"),
  by.timing = TRUE,
  pre.post = TRUE,
  display.all = TRUE,
  xlab = "Year",
  ylab = "Number of Programs",
  background = "white",
  collapse.history = TRUE,
  cex.main = 12,
  cex.axis = 12,
  cex.lab = 12,
  cex.legend = 10,
  axis.lab.gap = c(1,0),
  gridOff = FALSE,
  main = ""
)

## ----------------------------------------------------
## 2. Grab the most recently created PNG in figures_wd
## ----------------------------------------------------
pngs   <- list.files(figures_wd, pattern = "\\.png$", full.names = TRUE)
info   <- file.info(pngs)
latest <- rownames(info)[which.max(info$mtime)]
cat("Latest panelview file was: ", latest, "\n")

## ----------------------------------------------------
## 3. Re-save it at width=10, height=6, res=300
##    into BOTH output dirs with a stable name
## ----------------------------------------------------

final_name <- "01-staggarred_sample.png"
final_fig_path    <- file.path(figures_wd,   final_name)
final_thesis_path <- file.path(thesis_plots, final_name)

# read the auto-generated PNG as a raster
img <- png::readPNG(latest)

# function to redraw that raster into a new device
# function to redraw that raster into a new device
resave_png <- function(target_path) {
  png(
    filename = target_path,
    width = 18,    # wider
    height = 10,   # longer / taller
    units = "in",
    res = 300
  )
  
  # set up an empty plot that fills the device
  par(mar = c(0,0,0,0))
  plot(
    0, 0,
    type = "n",
    xlab = "",
    ylab = "",
    axes = FALSE,
    xlim = c(0,1),
    ylim = c(0,1),
    xaxs = "i",
    yaxs = "i"
  )
  
  rasterImage(img, 0, 0, 1, 1)
  dev.off()
}

# write standardized copies
resave_png(final_fig_path)
resave_png(final_thesis_path)

cat("Saved final figure(s) to:\n",
    final_fig_path, "\n",
    final_thesis_path, "\n")


#-----------------------------------
# Plot the evolution of average 
# outcomes across cohorts
# matched positions
#-----------------------------------

# Sum of matched positions by cohort (year_expanded) and year
library(dplyr)
matched_by_cohort <- long_data %>%
  mutate(cohort = as.character(year_expanded)) %>%
  group_by(cohort, year) %>%
  summarize(total_matched = sum(matched, na.rm = TRUE), .groups = 'drop')

# Replace Inf or NA with 'Never Treated'
matched_by_cohort$cohort[is.na(matched_by_cohort$cohort) | matched_by_cohort$cohort == "Inf"] <- "Never Treated"

library(ggplot2)
library(Polychrome)
num_groups <- length(unique(matched_by_cohort$cohort))
color_palette <- unname(createPalette(num_groups,  c("#ff0000", "#00ff00", "#0000ff")))
line_types <- rep(1:6, times = 2)
pch_types <- c(16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 1)

matched_by_cohort$cohort <- factor(matched_by_cohort$cohort, levels = unique(matched_by_cohort$cohort))

library(scales)

p <- ggplot(matched_by_cohort, aes(x = year, y = total_matched, group = cohort, color = cohort, linetype = cohort, shape = cohort)) +
  geom_line() +
  geom_point(size = 3) +
  scale_color_manual(values = color_palette, name = "Cohort") +
  scale_shape_manual(values = pch_types, name = "Cohort") +
  scale_linetype_manual(values = line_types, name = "Cohort") +
  labs(
       x = "Year", y = "Matched Positions") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = seq(min(matched_by_cohort$year), max(matched_by_cohort$year), by = 1)) +
  theme_customs() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(path = figures_wd, filename = "02-matched-positions-by-cohort.png", width = 10, height = 6, units = "in")
ggsave(path = thesis_plots, filename = "02-matched-positions-by-cohort.png", width = 10, height = 6, units = "in")
