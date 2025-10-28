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
cps_physicians_monthly <- cps %>%
  filter(EMPSTAT %in% c(10, 12)) %>%         # employed
  filter(OCC1950 == 75) %>%                  # physicians/surgeons
  group_by(YEAR, MONTH) %>%
  summarise(
    monthly_physicians = sum(WTFINL, na.rm = TRUE),
    .groups = "drop"
  )

# 2b. CPS total pop per (YEAR, MONTH)
#     - everyone in CPS universe (civilian non-institutional 16+)
cps_totalpop_monthly <- cps %>%
  group_by(YEAR, MONTH) %>%
  summarise(
    monthly_total_pop = sum(WTFINL, na.rm = TRUE),
    .groups = "drop"
  )

# 2c. Collapse CPS to YEAR level by averaging monthly totals
#     This gives a "typical month" for that year instead of summing all 12 months.
cps_yearly <- cps_physicians_monthly %>%
  left_join(cps_totalpop_monthly,
            by = c("YEAR", "MONTH")) %>%
  group_by(YEAR) %>%
  summarise(
    cps_physicians = mean(monthly_physicians, na.rm = TRUE),
    cps_total_pop  = mean(monthly_total_pop, na.rm = TRUE),
    .groups = "drop"
  ) %>%
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
acs_pop <- acs %>%
  group_by(YEAR) %>%
  summarise(
    acs_total_pop = sum(PERWT, na.rm = TRUE),
    .groups = "drop"
  )

# 3b. ACS physicians by YEAR (same OCC1950 == 75 definition)
acs_physicians <- acs %>%
  filter(OCC1950 == 75) %>%
  group_by(YEAR) %>%
  summarise(
    acs_physicians = sum(PERWT, na.rm = TRUE),
    .groups = "drop"
  )

# 3c. Merge and compute physicians per 100k in ACS
acs_yearly <- acs_physicians %>%
  left_join(acs_pop, by = "YEAR") %>%
  mutate(
    acs_physicians_per_100k =
      (acs_physicians / acs_total_pop) * 100000
  )

#---------------------------
# 4. Combine CPS + ACS + pop for overlap years
#---------------------------

combined <- acs_yearly %>%
  # attach CPS series (inner join keeps overlapping years only)
  inner_join(cps_yearly, by = "YEAR") %>%
  # keep only what's needed for plotting
  select(
    YEAR,
    acs_total_pop,
    acs_physicians_per_100k,
    cps_physicians_per_100k
  )

#---------------------------
# 5. Compute percent change relative to baseline year
#    - For population (ACS): percent change in total pop
#    - For physicians: percent change in physicians per 100k
#---------------------------

baseline_year <- min(combined$YEAR)

combined <- combined %>%
  mutate(
    pop_pctchg = (
      acs_total_pop /
      acs_total_pop[YEAR == baseline_year] - 1
    ) * 100,

    acs_phys_pctchg = (
      acs_physicians_per_100k /
      acs_physicians_per_100k[YEAR == baseline_year] - 1
    ) * 100,

    cps_phys_pctchg = (
      cps_physicians_per_100k /
      cps_physicians_per_100k[YEAR == baseline_year] - 1
    ) * 100
  )

#---------------------------
# 6. Reshape to long format for ggplot
#---------------------------

plot_data <- combined %>%
  select(YEAR, pop_pctchg, acs_phys_pctchg, cps_phys_pctchg) %>%
  pivot_longer(
    cols = c(pop_pctchg, acs_phys_pctchg, cps_phys_pctchg),
    names_to = "series",
    values_to = "pct_change"
  ) %>%
  mutate(
    series = dplyr::recode(
      series,
      pop_pctchg       = "Total population (ACS)",
      acs_phys_pctchg  = "Physicians per 100k (ACS)",
      cps_phys_pctchg  = "Physicians per 100k (CPS)"
    )
  )

#---------------------------
# 7. Plot
#---------------------------

ggplot(plot_data, aes(x = YEAR, y = pct_change, color = series)) +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  labs(
    title = "Growth in Physicians per Capita vs Population Growth",
    subtitle = paste0("Percent change since ", baseline_year),
    x = "Year",
    y = "Percent change (%)",
    color = ""
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )
