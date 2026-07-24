# ==============================================================================
# 03-heat-map.r
# Descriptive figures: panelview treatment-timing plot (desc-timing) and
# matched-positions-by-cohort plots in levels and per 100,000
# (desc-cohort-levels, desc-cohort-percapita), each saved as PNG + PDF to
# output/figures/ and my_paper/figures/.
# Input: data/datasets/cleaned_residency_medicaid.dta
# Normally sourced from 95-make-all.R (which defines path globals); the guard
# below makes it standalone-runnable.
# Date: April 12th, 2025 (revised 2026-07-24)
# ==============================================================================

library(scales)

if (!exists("datasets")) {
  git_mdir     <- here::here()
  datasets     <- file.path(git_mdir, "data", "datasets")
  figures_wd   <- file.path(git_mdir, "output", "figures")
  thesis_plots <- file.path(git_mdir, "my_paper", "figures")
  source(file.path(git_mdir, "programs", "01-packages-wds.r"))
}

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
    medicaid_expansion = first(medicaid_expansion),
    total_population_10 = first(total_population_10)
  ) |> 
  ungroup()

long_data |> 
  glimpse()

## make sure output dirs exist
dir.create(figures_wd,   recursive = TRUE, showWarnings = FALSE)
dir.create(thesis_plots, recursive = TRUE, showWarnings = FALSE)

## ----------------------------------------------------
## 1. Generate the plot with panelview(), saving to a known file
## ----------------------------------------------------
## ----------------------------------------------------
## 1. Generate the plot with panelview(), saving to a known file
## ----------------------------------------------------


# Build the panelview plot once, then save PNG (raster) + PDF (vector) to both
# output locations.
timing_plot <- panelview(
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
  cex.main = 28,
  cex.axis = 24,
  cex.lab = 24,
  cex.legend = 28,
  axis.lab.gap = c(1,0),
  gridOff = FALSE,
  main = ""
)
for (dir_out in c(figures_wd, thesis_plots)) {
  ggsave(file.path(dir_out, "desc-timing.png"), plot = timing_plot,
         width = 18, height = 10, units = "in", dpi = 100)
  ggsave(file.path(dir_out, "desc-timing.pdf"), plot = timing_plot,
         width = 18, height = 10, units = "in")
}

#-----------------------------------
# Plot the evolution of average 
# outcomes across cohorts
# matched positions
#-----------------------------------

# Sum of matched positions by cohort (year_expanded) and year

matched_by_cohort <- long_data |>
  mutate(cohort = as.character(year_expanded)) |>
  group_by(cohort, state, year) |>
  summarize(total_matched = sum(matched, na.rm = TRUE), .groups = 'drop')

# Replace Inf or NA with 'Never Treated'
matched_by_cohort$cohort[is.na(matched_by_cohort$cohort) | matched_by_cohort$cohort == "Inf"] <- "Never Treated"

# Get unique cohort years (excluding Never Treated)
cohort_years <- unique(na.omit(as.numeric(matched_by_cohort$cohort[matched_by_cohort$cohort != "Never Treated"])))

# Calculate matched_per_100k before summarizing
long_data <- long_data |>
  mutate(matched_per_100k = ifelse(matched > 0, matched / total_population_10 * 100000, NA_real_))

# Sum of matched positions by cohort (year_expanded) and year
matched_by_cohort <- long_data |>
  mutate(cohort = as.character(year_expanded)) |>
  group_by(cohort, state, year) |>
  summarize(total_matched = sum(matched, na.rm = TRUE), .groups = 'drop')
matched_by_cohort$cohort[is.na(matched_by_cohort$cohort) | matched_by_cohort$cohort == "Inf"] <- "Never Treated"
matched_by_cohort$cohort <- factor(matched_by_cohort$cohort, levels = unique(matched_by_cohort$cohort))

matched_by_cohort <- matched_by_cohort |>
  group_by(cohort, year) |>
  summarize(total_matched = sum(total_matched, na.rm = TRUE), .groups = 'drop')

# Correctly summarize matched_per_100k by cohort and year (aggregate then compute per 100k)
matched_per_100k_by_cohort <- long_data |>
  mutate(cohort = as.character(year_expanded)) |>
  group_by(cohort, state, year) |>
  summarize(
    total_matched = sum(matched, na.rm = TRUE),
    total_quota = sum(quota, na.rm = TRUE),
    total_population_10 = first(total_population_10),
    .groups = 'drop'
  )

# Replace NA/Inf cohorts with "Never Treated" before final aggregation
matched_per_100k_by_cohort$cohort[is.na(matched_per_100k_by_cohort$cohort) | 
                                   matched_per_100k_by_cohort$cohort == "Inf"] <- "Never Treated"
matched_per_100k_by_cohort$cohort <- factor(matched_per_100k_by_cohort$cohort, 
                                            levels = unique(matched_per_100k_by_cohort$cohort))

#  aggregate across states, then calculate per 100k
matched_per_100k_by_cohort <- matched_per_100k_by_cohort |>
  group_by(cohort, year) |>
  summarize(
    total_matched = sum(total_matched, na.rm = TRUE),
    total_population_10 = sum(total_population_10, na.rm = TRUE),
    .groups = 'drop'
  ) |>
  mutate(mean_matched_per_100k = total_matched / total_population_10 * 100000)

# Get unique cohort years (excluding Never Treated)
cohort_years <- unique(na.omit(as.numeric(c(
  as.character(matched_by_cohort$cohort[matched_by_cohort$cohort != "Never Treated"]),
  as.character(matched_per_100k_by_cohort$cohort[matched_per_100k_by_cohort$cohort != "Never Treated"])
))))


num_groups <- length(unique(matched_by_cohort$cohort))
color_palette <- unname(createPalette(num_groups,  c("#ff0000", "#00ff00", "#0000ff")))
line_types <- rep(1:6, times = 2)
pch_types <- c(16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 1)

matched_by_cohort$cohort <- factor(matched_by_cohort$cohort, levels = unique(matched_by_cohort$cohort))


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
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_vline(xintercept = cohort_years, linetype = "dashed", color = "grey40")
p
ggsave(path = figures_wd, filename = "desc-cohort-levels.png", plot = p, width = 10, height = 6, units = "in")
ggsave(path = thesis_plots, filename = "desc-cohort-levels.png", plot = p, width = 10, height = 6, units = "in")
ggsave(path = figures_wd, filename = "desc-cohort-levels.pdf", plot = p, width = 10, height = 6, units = "in")
ggsave(path = thesis_plots, filename = "desc-cohort-levels.pdf", plot = p, width = 10, height = 6, units = "in")

#-----------------------------------
# Calculate matched_per_100k
#-----------------------------------
# Assume you have a population variable; if not, replace with the correct one
# For demonstration, I'll use 'quota' as a placeholder for population
# Replace 'quota' with your actual population variable if available

#-----------------------------------
# Plot matched_per_100k by cohort (with vlines)
#-----------------------------------
p_per_100k <- ggplot(matched_per_100k_by_cohort, aes(x = year, y = mean_matched_per_100k, group = cohort, color = cohort, linetype = cohort, shape = cohort)) +
  geom_line() +
  geom_point(size = 3) +
  scale_color_manual(values = color_palette, name = "Cohort") +
  scale_shape_manual(values = pch_types, name = "Cohort") +
  scale_linetype_manual(values = line_types, name = "Cohort") +
  labs(x = "Year", y = "Matched Positions per 100,000") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = seq(min(matched_per_100k_by_cohort$year), max(matched_per_100k_by_cohort$year), by = 1)) +
  theme_customs() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_vline(xintercept = cohort_years, linetype = "dashed", color = "grey40")

ggsave(path = figures_wd, filename = "desc-cohort-percapita.png", plot = p_per_100k, width = 10, height = 6, units = "in")
ggsave(path = thesis_plots, filename = "desc-cohort-percapita.png", plot = p_per_100k, width = 10, height = 6, units = "in")
ggsave(path = figures_wd, filename = "desc-cohort-percapita.pdf", plot = p_per_100k, width = 10, height = 6, units = "in")
ggsave(path = thesis_plots, filename = "desc-cohort-percapita.pdf", plot = p_per_100k, width = 10, height = 6, units = "in")
