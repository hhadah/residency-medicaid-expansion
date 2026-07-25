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

if (!exists("raw")) {
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
    hjust = 0, size = 7
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
