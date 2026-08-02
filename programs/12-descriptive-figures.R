# ==============================================================================
# 12-descriptive-figures.R
# Part A: state-by-state staggered-adoption heatmap (desc-timing) and matched-
#   positions-by-cohort plots in levels and per 100,000 (desc-cohort-levels,
#   desc-cohort-percapita), on the FULL 2000-2019 panel (activity-window
#   coding; per-capita uses contemporary population).
# Part B: physician-growth descriptive (IPUMS; formerly 12-population-residents.R).
# Each figure saved as PNG + PDF to output/figures/ and my_paper/figures/.
# Input: data/datasets/panel_2000_2019_estimation.dta (script 06)
# Normally sourced from 95-make-all.R (which defines path globals); the guard
# below makes it standalone-runnable.
# ==============================================================================

library(scales)

if (!exists("datasets")) {
  git_mdir     <- here::here()
  datasets     <- file.path(git_mdir, "data", "datasets")
  figures_wd   <- file.path(git_mdir, "output", "figures")
  thesis_plots <- file.path(git_mdir, "my_paper", "figures")
  source(file.path(git_mdir, "programs", "01-packages-wds.r"))
}

# open the full estimation panel (already institution x year)
long_data <- read_dta(file.path(datasets, "panel_2000_2019_estimation.dta")) |>
  mutate(
    matched = matched_na,        # activity-window coding is primary
    quota   = quota_na,
    medicaid_expansion = as.integer(!is.na(year_expanded) & year >= year_expanded)
  )

long_data |> 
  glimpse()

## make sure output dirs exist
dir.create(figures_wd,   recursive = TRUE, showWarnings = FALSE)
dir.create(thesis_plots, recursive = TRUE, showWarnings = FALSE)

## ----------------------------------------------------
## 1. State-by-state staggered adoption heatmap (desc-timing)
## ----------------------------------------------------

# Expansion is a state-level policy, so the timing display plots the full
# 50-state + DC grid (not the estimation sample): one row per state, one
# column per year, cells colored by expansion status.
# exists() alone is not enough: base R has a raw() function
if (!exists("raw", mode = "character")) raw <- file.path(here::here(), "data", "raw")

expansion_status <- read_dta(file.path(raw, "expansion_status.dta")) |>
  mutate(state = toupper(str_trim(state)))

# earliest adopters at the top; never-expansion states grouped at the bottom
state_order <- expansion_status |>
  arrange(is.na(year_expanded), year_expanded, state) |>
  pull(state)

adoption_grid <- tidyr::crossing(expansion_status, year = 2000:2019) |>
  mutate(
    status = case_when(
      is.na(year_expanded)  ~ "Never expanded",
      year >= year_expanded ~ "Expanded",
      TRUE                  ~ "Pre-expansion"
    ),
    status = factor(status,
                    levels = c("Pre-expansion", "Expanded", "Never expanded")),
    state  = factor(state, levels = rev(state_order))
  )

timing_plot <- ggplot(adoption_grid, aes(x = year, y = state, fill = status)) +
  geom_tile(color = "white", linewidth = 0.4) +
  scale_x_continuous(breaks = 2000:2019, expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  # light/dark separation keeps the three groups readable in grayscale
  scale_fill_manual(values = c("Pre-expansion"  = "#9ECAE1",
                               "Expanded"       = "#08519C",
                               "Never expanded" = "grey85")) +
  labs(x = "Year", y = NULL, fill = NULL) +
  theme_customs() +
  theme(axis.text.y     = element_text(size = 23),
        axis.text.x     = element_text(size = 23, angle = 45, hjust = 1),
        axis.title.x    = element_text(size = 28),
        legend.text     = element_text(size = 27),
        legend.key.size = unit(1.4, "lines"),
        legend.position = "bottom",
        axis.line       = element_blank())

for (dir_out in c(figures_wd, thesis_plots)) {
  ggsave(file.path(dir_out, "desc-timing.png"), plot = timing_plot,
         width = 10, height = 12, units = "in", dpi = 320)
  ggsave(file.path(dir_out, "desc-timing.pdf"), plot = timing_plot,
         width = 10, height = 12, units = "in")
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
    pop_yr = first(pop_yr),
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
    pop_yr = sum(pop_yr, na.rm = TRUE),
    .groups = 'drop'
  ) |>
  mutate(mean_matched_per_100k = total_matched / pop_yr * 100000)

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


# =====================================================================
# Part B: physician-growth descriptive (formerly 12-population-residents.R)
# =====================================================================

# ==============================================================================
# 04-population-residents.r
# Descriptive figure: growth in physicians per 100,000 (ACS) vs total population
# (ACS), each indexed to 100 in 2001 -> desc-physician-growth.{png,pdf}.
# Inputs: data/raw/cps_00078.csv.gz (IPUMS-CPS), data/raw/usa_00064.csv.gz
# (IPUMS-USA). Caches the combined year panel to
# data/datasets/combined_physician_pop.rds for fast re-plots.
# Normally sourced from 95-make-all.R (which defines path globals); the guard
# below makes it standalone-runnable.
# ==============================================================================

library(grid)

if (!exists("raw", mode = "character")) {
  git_mdir    <- here::here()
  datasets    <- file.path(git_mdir, "data", "datasets")
  raw         <- file.path(git_mdir, "data", "raw")
  figures_wd  <- file.path(git_mdir, "output", "figures")
  thesis_plots <- file.path(git_mdir, "my_paper", "figures")
  source(file.path(git_mdir, "programs", "01-packages-wds.r"))
}

# open the data
cps_file <- file.path(raw, "cps_00078.csv.gz")
acs_file <- file.path(raw, "usa_00064.csv.gz")

#---------------------------
# 1. Read data
#---------------------------
cps <- read_csv(cps_file)
acs <- read_csv(acs_file)

#---------------------------
# 2. CPS: physicians and total CPS population by YEAR
#---------------------------

# 2a. CPS physicians per (YEAR, MONTH)
#     - keep employed or has job
#     - physicians/surgeons using harmonized OCC1950 == 75
cps_physicians_monthly <- cps |>
  filter(EMPSTAT %in% c(10, 12)) |>         # employed
  filter(OCC1950 == 75) |>                  # physicians/surgeons
  group_by(YEAR, MONTH) |>
  summarise(
    monthly_physicians = sum(WTFINL, na.rm = TRUE),
    .groups = "drop"
  )

# 2b. CPS total pop per (YEAR, MONTH)
#     - everyone in CPS universe (civilian non-institutional 16+)
cps_totalpop_monthly <- cps |>
  group_by(YEAR, MONTH) |>
  summarise(
    monthly_total_pop = sum(WTFINL, na.rm = TRUE),
    .groups = "drop"
  )

# 2c. Collapse CPS to YEAR level by averaging monthly totals
#     This gives a "typical month" for that year instead of summing all 12 months.
cps_yearly <- cps_physicians_monthly |>
  left_join(cps_totalpop_monthly,
            by = c("YEAR", "MONTH")) |>
  group_by(YEAR) |>
  summarise(
    cps_physicians = mean(monthly_physicians, na.rm = TRUE),
    cps_total_pop  = mean(monthly_total_pop, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    cps_physicians_per_100k =
      (cps_physicians / cps_total_pop) * 100000
  )

# cps_yearly now has, for each YEAR:
#   - cps_physicians (estimated count of physicians)
#   - cps_total_pop  (CPS population universe)
#   - cps_physicians_per_100k (rate)

#---------------------------
# 3. ACS: total population and physicians
#---------------------------

# 3a. ACS total population by YEAR
acs_pop <- acs |>
  filter(!(SAMPLE %in% c(200007, 200004, 200003))) |>
  group_by(YEAR) |>
  summarise(
    acs_total_pop = sum(PERWT, na.rm = TRUE),
    .groups = "drop"
  )

# 3b. ACS physicians by YEAR (same OCC1950 == 75 definition)
acs_physicians <- acs |>
  filter(OCC1950 == 75) |>
  filter(!(SAMPLE %in% c(200007, 200004, 200003))) |>
  group_by(YEAR) |>
  summarise(
    acs_physicians = sum(PERWT, na.rm = TRUE),
    .groups = "drop"
  )

# 3c. Merge and compute physicians per 100k in ACS
acs_yearly <- acs_physicians |>
  left_join(acs_pop, by = "YEAR") |>
  mutate(
    acs_physicians_per_100k =
      (acs_physicians / acs_total_pop) * 100000
  )

#---------------------------
# 4. Combine CPS + ACS for overlap years (raw values)
#---------------------------

combined <- acs_yearly |>
  inner_join(cps_yearly, by = "YEAR")

#---------------------------
# 5. Plot percent change in per 100k physicians and population
#---------------------------
# (Exploratory raw-count and percent plots removed 2026-07-24: they carried
# in-graph titles (INV-12) and were never saved.)

# Two series only: total population (ACS) and physicians per 100,000 (ACS), each
# indexed to 100 in the first year -- matching the figure caption ("physicians per
# 100,000 relative to total population"). CSP series dropped.
# Cache `combined` so the figure can be regenerated without re-reading the raw data.
if (exists("datasets")) saveRDS(combined, file.path(datasets, "combined_physician_pop.rds"))

plot_data_growth <- combined |>
  select(YEAR, acs_total_pop, acs_physicians_per_100k) |>
  pivot_longer(
    cols = c(acs_total_pop, acs_physicians_per_100k),
    names_to = "series",
    values_to = "value"
  ) |>
  mutate(
    series = dplyr::recode(
      series,
      acs_total_pop           = "Total population (ACS)",
      acs_physicians_per_100k = "Physicians per 100k (ACS)"
    )
  ) |>
  filter(YEAR > 2000 & YEAR < 2020) |>
  group_by(series) |>
  arrange(YEAR, .by_group = TRUE) |>
  mutate(
    pct_of_start = 100 * value / first(value)
  ) |>
  ungroup()

# End-of-series labels at the last year; nudge apart vertically if they collide.
label_data <- plot_data_growth %>%
  group_by(series) %>%
  filter(YEAR == max(YEAR)) %>%
  ungroup() %>%
  arrange(series)   # "Physicians per 100k (ACS)" then "Total population (ACS)"
if (abs(diff(label_data$pct_of_start)) < 2.5) {
  mid <- mean(label_data$pct_of_start)
  label_data$pct_of_start <- ifelse(
    label_data$pct_of_start >= mid, mid + 1.6, mid - 1.6)
}

growth_plot <- ggplot(plot_data_growth, aes(x = YEAR, y = pct_of_start, color = series)) +
  geom_line(linewidth = 1.1, alpha = 0.6) +
  geom_point(size = 2, alpha = 0.8) +
  annotate(
    "text",
    x = label_data$YEAR + 0.15,
    y = label_data$pct_of_start,
    label = label_data$series,
    color = c("#D81B60", "#FFC107"),
    hjust = 0, size = 10
  ) +
  labs(
    title = NULL,
    x = "Year",
    y = "Percent of Starting Value (first year = 100)"
  ) +
  scale_color_manual(
    values = c(
      "Physicians per 100k (ACS)" = "#D81B60",
      "Total population (ACS)"    = "#FFC107"
    )
  ) +
  scale_x_continuous(
    breaks = seq(min(plot_data_growth$YEAR), max(plot_data_growth$YEAR), by = 1),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), breaks = scales::pretty_breaks()) +
  coord_cartesian(clip = "off") +
  theme_customs() +
  theme(legend.position = "none", plot.margin = unit(c(1, 18, 1, 1), "lines"))

ggsave(path = figures_wd, filename = "desc-physician-growth.png", plot = growth_plot, width = 10, height = 6, units = "in", dpi = 300)
ggsave(path = thesis_plots, filename = "desc-physician-growth.png", plot = growth_plot, width = 10, height = 6, units = "in", dpi = 300)
ggsave(path = figures_wd, filename = "desc-physician-growth.pdf", plot = growth_plot, width = 10, height = 6, units = "in")
ggsave(path = thesis_plots, filename = "desc-physician-growth.pdf", plot = growth_plot, width = 10, height = 6, units = "in")
