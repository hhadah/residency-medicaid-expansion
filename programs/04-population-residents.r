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
  filter(!(SAMPLE %in% c(200007, 200004, 200003, 200003))) |>
  group_by(YEAR) |>
  summarise(
    acs_total_pop = sum(PERWT, na.rm = TRUE),
    .groups = "drop"
  )

# 3b. ACS physicians by YEAR (same OCC1950 == 75 definition)
acs_physicians <- acs |>
  filter(OCC1950 == 75) |>
  filter(!(SAMPLE %in% c(200007, 200004, 200003, 200003))) |>
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
# 5. Reshape to long format for ggplot (raw values)
#---------------------------

plot_data <- combined |>
  filter(YEAR >= 1990) |>
  select(YEAR, acs_total_pop, acs_physicians, cps_physicians) |>
  pivot_longer(
    cols = c(acs_total_pop, acs_physicians, cps_physicians),
    names_to = "series",
    values_to = "count"
  ) |>
  mutate(
    series = dplyr::recode(
      series,
      acs_total_pop   = "Total population (ACS)",
      acs_physicians  = "Physicians (ACS)",
      cps_physicians  = "Physicians (CPS)"
    )
  ) |>
  group_by(series) |>
  arrange(YEAR, .by_group = TRUE) |>
  mutate(
    pct_of_start = 100 * count / first(count)
  ) |>
  ungroup()
  

#---------------------------
# 6. Plot raw values (original plot)
#---------------------------

totals_plot <- ggplot(plot_data |> filter(series != "Total population (ACS)"), aes(x = YEAR, y = count, color = series)) +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  labs(
    title = "Number of Physicians (Raw Values)",
    x = "Year",
    y = "Count",
    color = ""
  ) +
  scale_color_manual(
    values = c(
      "Physicians (ACS)" = "#0072B2",   # Blue
      "Physicians (CPS)" = "#D55E00"    # Vermillion
    )
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_customs()
totals_plot
#---------------------------
# 7. Plot percent change since first year (first year = 100)
#---------------------------

plot_data_pct <- plot_data |>
  group_by(series) |>
  arrange(YEAR, .by_group = TRUE) |>
  mutate(
    pct_of_start = 100 * count / first(count)
  ) |>
  ungroup()

percent_plot <- ggplot(plot_data_pct, aes(x = YEAR, y = pct_of_start, color = series)) +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  labs(
    title = "Percent Change Since First Year (First Year = 100)",
    x = "Year",
    y = "Percent of Starting Value",
    color = ""
  ) +
  scale_color_manual(
    values = c(
      "Physicians (ACS)" = "#D81B60",
      "Physicians (CPS)" = "#1E88E5",
      "Total population (ACS)" = "#FFC107"
    )
  ) +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), breaks = scales::pretty_breaks()) +
  theme_customs()
percent_plot

#---------------------------
# 8. Plot percent change in per 100k physicians and population
#---------------------------

plot_data_per100k <- combined |>
  select(YEAR, acs_total_pop, acs_physicians_per_100k, cps_physicians_per_100k) |>
  pivot_longer(
    cols = c(acs_total_pop, acs_physicians_per_100k, cps_physicians_per_100k),
    names_to = "series",
    values_to = "value"
  ) |>
  mutate(
    series = dplyr::recode(
      series,
      acs_total_pop = "Total population (ACS)",
      acs_physicians_per_100k = "Physicians per 100k (ACS)",
      cps_physicians_per_100k = "Physicians per 100k (CPS)"
    )
  ) |>
  filter(YEAR > 2000 & YEAR < 2020) |>
  group_by(series) |>
  arrange(YEAR, .by_group = TRUE) |>
  mutate(
    pct_of_start = 100 * value / first(value)
  ) |>
  ungroup()



library(grid)


# Get the last value for each series for labeling, and nudge x to the right
label_data <- plot_data_per100k %>%
  group_by(series) %>%
  filter(YEAR == max(YEAR)) %>%
  ungroup()

per100k_plot <- ggplot(plot_data_per100k, aes(x = YEAR, y = pct_of_start, color = series)) +
  geom_line(size = 1.1, alpha = 0.5) +
  geom_point(size = 2, alpha = 0.7) +
  annotate(
    "text",
    x = label_data$YEAR,
    y = label_data$pct_of_start,
    label = label_data$series,
    color = c("#D81B60", "#1E88E5", "#FFC107"),
    hjust = 0, size = 16
  ) +
  labs(
    title = "Percent Change: Physicians per 100k and Population (First Year = 100)",
    x = "Year",
    y = "Percent of Starting Value"
  ) +
  scale_color_manual(
    values = c(
      "Physicians per 100k (ACS)" = "#D81B60",
      "Physicians per 100k (CPS)" = "#1E88E5",
      "Total population (ACS)" = "#FFC107"
    )
  ) +
  scale_x_continuous(
    breaks = seq(min(plot_data_per100k$YEAR), max(plot_data_per100k$YEAR), by = 1),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), breaks = scales::pretty_breaks()) +
  coord_cartesian(clip = "off") +  # This allows drawing outside plot area
  theme_customs() +
  theme(legend.position = "none", plot.margin = unit(c(1, 15, 1, 1), "lines"))  # Increased right margin to 15

ggsave(path = figures_wd, filename = "10-doctors-vs-pop.png", plot = per100k_plot, width = 10, height = 6, units = "in", dpi = 300)
ggsave(path = thesis_plots, filename = "10-doctors-vs-pop.png", plot = per100k_plot, width = 10, height = 6, units = "in", dpi = 300)

#---------------------------
# 9. Plot actual physicians per 100k over time
#---------------------------

plot_per100k_actual <- combined |>
  select(YEAR, acs_physicians_per_100k, cps_physicians_per_100k) |>
  pivot_longer(
    cols = c(acs_physicians_per_100k, cps_physicians_per_100k),
    names_to = "series",
    values_to = "per100k"
  ) |>
  mutate(
    series = dplyr::recode(
      series,
      acs_physicians_per_100k = "Physicians per 100k (ACS)",
      cps_physicians_per_100k = "Physicians per 100k (CPS)"
    )
  ) |>
  filter(YEAR > 2000)

per100k_actual_plot <- ggplot(plot_per100k_actual, aes(x = YEAR, y = per100k, color = series)) +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  labs(
    title = "Physicians per 100,000 Population Over Time",
    x = "Year",
    y = "Physicians per 100,000",
    color = ""
  ) +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), breaks = scales::pretty_breaks()) +
  theme_customs()
per100k_actual_plot
