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


# Save panelview plot to a known file using png() device
panelview_file <- file.path(figures_wd, "01-staggarred_sample_raw.png")
png(filename = panelview_file, width = 1800, height = 1000, res = 100)
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
dev.off()

panelview_file <- file.path(thesis_plots, "01-staggarred_sample_raw.png")
png(filename = panelview_file, width = 1800, height = 1000, res = 100)
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
dev.off()

#-----------------------------------
# Plot the evolution of average 
# outcomes across cohorts
# matched positions
#-----------------------------------

# Sum of matched positions by cohort (year_expanded) and year
library(dplyr)
matched_by_cohort <- long_data |>
  mutate(cohort = as.character(year_expanded)) |>
  group_by(cohort, year) |>
  summarize(total_matched = sum(matched, na.rm = TRUE), .groups = 'drop')

# Replace Inf or NA with 'Never Treated'
matched_by_cohort$cohort[is.na(matched_by_cohort$cohort) | matched_by_cohort$cohort == "Inf"] <- "Never Treated"

# Get unique cohort years (excluding Never Treated)
cohort_years <- unique(na.omit(as.numeric(matched_by_cohort$cohort[matched_by_cohort$cohort != "Never Treated"])))

# Calculate matched_per_100k before summarizing
long_data <- long_data |>
  mutate(matched_per_100k = ifelse(quota > 0, matched / quota * 100000, NA_real_))

# Sum of matched positions by cohort (year_expanded) and year
matched_by_cohort <- long_data |>
  mutate(cohort = as.character(year_expanded)) |>
  group_by(cohort, year) |>
  summarize(total_matched = sum(matched, na.rm = TRUE), .groups = 'drop')
matched_by_cohort$cohort[is.na(matched_by_cohort$cohort) | matched_by_cohort$cohort == "Inf"] <- "Never Treated"
matched_by_cohort$cohort <- factor(matched_by_cohort$cohort, levels = unique(matched_by_cohort$cohort))


# Correctly summarize matched_per_100k by cohort and year (aggregate then compute per 100k)
matched_per_100k_by_cohort <- long_data |>
  mutate(cohort = as.character(year_expanded)) |>
  group_by(cohort, year) |>
  summarize(
    total_matched = sum(matched, na.rm = TRUE),
    total_quota = sum(quota, na.rm = TRUE),
    total_population_10 = first(total_population_10),
    .groups = 'drop'
  )
  
matched_per_100k_by_cohort <- matched_per_100k_by_cohort |>
  mutate(mean_matched_per_100k = total_matched / total_population_10 * 100000)

matched_per_100k_by_cohort$cohort[is.na(matched_per_100k_by_cohort$cohort) | matched_per_100k_by_cohort$cohort == "Inf"] <- "Never Treated"
matched_per_100k_by_cohort$cohort <- factor(matched_per_100k_by_cohort$cohort, levels = unique(matched_per_100k_by_cohort$cohort))

# Get unique cohort years (excluding Never Treated)
cohort_years <- unique(na.omit(as.numeric(c(
  as.character(matched_by_cohort$cohort[matched_by_cohort$cohort != "Never Treated"]),
  as.character(matched_per_100k_by_cohort$cohort[matched_per_100k_by_cohort$cohort != "Never Treated"])
))))


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
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_vline(xintercept = cohort_years, linetype = "dashed", color = "grey40")

ggsave(path = figures_wd, filename = "02-matched-positions-by-cohort.png", width = 10, height = 6, units = "in")
ggsave(path = thesis_plots, filename = "02-matched-positions-by-cohort.png", width = 10, height = 6, units = "in")

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

ggsave(path = figures_wd, filename = "03-matched-per-100k-by-cohort.png", width = 10, height = 6, units = "in")
ggsave(path = thesis_plots, filename = "03-matched-per-100k-by-cohort.png", width = 10, height = 6, units = "in")
