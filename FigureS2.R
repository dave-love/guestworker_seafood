# ==================================================
# Figure S2
# Monthly seasonality of employment spells by category
# ==================================================

library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(RColorBrewer)

# --------------------------------------------------
# 1. Compute mean annual workers by CATEGORY and WORKSITE_STATE
# --------------------------------------------------
top_states_summary <- visa_seafood %>%
  filter(
    as.integer(YEAR) >= 2017,
    as.integer(YEAR) <= 2025,
    CASE_STATUS_CLEAN == "certification",
    TYPE != "Permanent"
  ) %>%
  group_by(CATEGORY, WORKSITE_STATE, YEAR) %>%
  summarize(
    yearly_workers = sum(as.numeric(TOTAL_WORKERS_CERTIFIED), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(CATEGORY, WORKSITE_STATE) %>%
  summarize(
    mean_workers = mean(yearly_workers, na.rm = TRUE),
    .groups = "drop"
  )

# --------------------------------------------------
# 2. Select top N states within each category
# --------------------------------------------------
top_n <- 6

top_states_codes <- top_states_summary %>%
  filter(mean_workers > 100) %>%
  group_by(CATEGORY) %>%
  slice_max(order_by = mean_workers, n = top_n, with_ties = FALSE) %>%
  ungroup() %>%
  pull(WORKSITE_STATE) %>%
  unique()

# Fixed state color palette across all radar plots
state_levels <- sort(unique(top_states_codes))

state_colors <- setNames(
  colorRampPalette(brewer.pal(8, "Set2"))(length(state_levels)),
  state_levels
)

# --------------------------------------------------
# 3. Expand each record to one row per month employed
# --------------------------------------------------
visa_seafood2 <- visa_seafood %>%
  filter(
    as.integer(YEAR) >= 2017,
    as.integer(YEAR) <= 2025,
    CASE_STATUS_CLEAN == "certification",
    TYPE != "Permanent",
    WORKSITE_STATE %in% top_states_codes
  ) %>%
  mutate(
    EMPLOYMENT_BEGIN_DATE = as.Date(EMPLOYMENT_BEGIN_DATE),
    EMPLOYMENT_END_DATE   = as.Date(EMPLOYMENT_END_DATE)
  )

get_months_between <- function(start_date, end_date) {
  seq(
    floor_date(start_date, "month"),
    floor_date(end_date, "month"),
    by = "month"
  )
}

visa_long <- visa_seafood2 %>%
  filter(
    !is.na(EMPLOYMENT_BEGIN_DATE),
    !is.na(EMPLOYMENT_END_DATE),
    EMPLOYMENT_END_DATE >= EMPLOYMENT_BEGIN_DATE
  ) %>%
  rowwise() %>%
  mutate(months_employed = list(get_months_between(EMPLOYMENT_BEGIN_DATE, EMPLOYMENT_END_DATE))) %>%
  unnest(months_employed) %>%
  mutate(
    month = month(months_employed, label = TRUE, abbr = TRUE),
    month_num = month(months_employed)
  ) %>%
  ungroup()

# --------------------------------------------------
# 4. Calculate monthly frequencies by CATEGORY and state
# --------------------------------------------------
monthly_counts <- visa_long %>%
  group_by(CATEGORY, WORKSITE_STATE, month, month_num) %>%
  summarize(total = n(), .groups = "drop") %>%
  group_by(CATEGORY, WORKSITE_STATE) %>%
  mutate(frequency = total / sum(total, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(month = factor(month, levels = month.abb, ordered = FALSE))

# --------------------------------------------------
# 5. Add a "13th month" to close the radar plot
# --------------------------------------------------
monthly_counts2 <- monthly_counts %>%
  arrange(CATEGORY, WORKSITE_STATE, month_num) %>%
  group_by(CATEGORY, WORKSITE_STATE) %>%
  group_modify(~ bind_rows(
    .x,
    tibble(
      month = factor(NA, levels = month.abb, ordered = FALSE),
      month_num = 13,
      total = NA_real_,
      frequency = NA_real_
    )
  )) %>%
  ungroup() %>%
  mutate(WORKSITE_STATE = factor(WORKSITE_STATE, levels = state_levels))

# --------------------------------------------------
# 6. Plot one radar chart per CATEGORY
# --------------------------------------------------
plot_combos <- monthly_counts2 %>%
  distinct(CATEGORY)

dir.create("figures", showWarnings = FALSE)

for (i in seq_len(nrow(plot_combos))) {
  catg <- plot_combos$CATEGORY[i]
  
  plot_data <- monthly_counts2 %>%
    filter(CATEGORY == catg) %>%
    arrange(WORKSITE_STATE, month_num)
  
  if (nrow(plot_data) == 0) next
  
  radar <- ggplot(
    plot_data,
    aes(
      x = month_num,
      y = frequency,
      group = WORKSITE_STATE,
      color = WORKSITE_STATE
    )
  ) +
    geom_polygon(fill = NA, linewidth = 0.7) +
    geom_point(size = 2) +
    scale_x_continuous(
      breaks = 1:13,
      labels = c(month.abb, ""),
      limits = c(1, 13)
    ) +
    scale_color_manual(values = state_colors, drop = FALSE) +
    coord_polar(start = 0) +
    labs(
      x = NULL,
      y = "Fraction of Employment Spells",
      title = paste0(catg, " (Top States, 2017–2025)"),
      color = "State"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 9),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
  
  ggsave(
    filename = paste0(
      "figures/radar_",
      gsub(" ", "_", tolower(catg)),
      ".png"
    ),
    plot = radar,
    width = 7,
    height = 6,
    dpi = 300,
    bg = "white"
  )
}