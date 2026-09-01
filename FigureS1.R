# ==================================================
# Figure S1
# Mean employment duration by state and category
# ==================================================

library(dplyr)
library(ggplot2)
library(scales)

# --------------------------------------------------
# 1. Restrict to certified non-Permanent records
# --------------------------------------------------
visa_seafood2 <- visa_seafood %>%
  filter(
    as.integer(YEAR) >= 2017,
    as.integer(YEAR) <= 2025,
    CASE_STATUS_CLEAN == "certification",
    TYPE != "Permanent"
  ) %>%
  mutate(
    EMPLOYMENT_BEGIN_DATE = as.Date(EMPLOYMENT_BEGIN_DATE),
    EMPLOYMENT_END_DATE   = as.Date(EMPLOYMENT_END_DATE),
    duration_months = as.numeric(
      difftime(EMPLOYMENT_END_DATE, EMPLOYMENT_BEGIN_DATE, units = "days")
    ) / 30.44
  )

# --------------------------------------------------
# 2. Total workers by category and state
#    Used to exclude very small cells
# --------------------------------------------------
worker_totals <- visa_seafood2 %>%
  group_by(CATEGORY, WORKSITE_STATE) %>%
  summarize(
    total_workers = sum(as.numeric(TOTAL_WORKERS_CERTIFIED), na.rm = TRUE),
    .groups = "drop"
  )

# --------------------------------------------------
# 3. Mean duration by category and state
# --------------------------------------------------
plot_data <- visa_seafood2 %>%
  group_by(CATEGORY, WORKSITE_STATE) %>%
  summarize(
    mean_duration = mean(duration_months, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(worker_totals, by = c("CATEGORY", "WORKSITE_STATE")) %>%
  filter(total_workers >= 100) %>%
  mutate(
    WORKSITE_STATE = factor(WORKSITE_STATE, levels = sort(unique(WORKSITE_STATE)))
  )

# --------------------------------------------------
# 4. Plot
# --------------------------------------------------
p <- ggplot(
  plot_data,
  aes(x = WORKSITE_STATE, y = mean_duration, fill = CATEGORY)
) +
  geom_col(position = position_dodge(width = 0.9), width = 0.8) +
  geom_text(
    aes(label = round(mean_duration, 1)),
    position = position_dodge(width = 0.9),
    vjust = -0.35,
    size = 3,
    color = "black"
  ) +
  facet_wrap(~CATEGORY, nrow = 3) +
  labs(
    x = "State",
    y = "Mean Duration (Months)"
  ) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(colour = "grey", fill = NA),
    strip.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# --------------------------------------------------
# 5. Save figure
# --------------------------------------------------
dir.create("figures", showWarnings = FALSE)

ggsave(
  filename = "figures/visas_duration.png",
  plot = p,
  width = 6,
  height = 8,
  dpi = 300,
  bg = "white"
)