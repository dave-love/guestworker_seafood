# ==================================================
# Figure 1
# Visa certifications by year, category, and visa type
# ==================================================

library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(scales)
library(patchwork)

# --------------------------------------------------
# 1. Prepare data for left panel: case status by year and category
# --------------------------------------------------
plot_case_status <- visa_seafood %>%
  filter(as.integer(YEAR) >= 2017, as.integer(YEAR) <= 2025) %>%
  mutate(TOTAL_WORKERS_CERTIFIED = as.numeric(TOTAL_WORKERS_CERTIFIED)) %>%
  group_by(YEAR, CASE_STATUS_CLEAN, CATEGORY) %>%
  summarize(
    sum_workers = sum(TOTAL_WORKERS_CERTIFIED, na.rm = TRUE),
    .groups = "drop"
  )

p_case_status <- ggplot(
  plot_case_status,
  aes(x = factor(YEAR), y = sum_workers, fill = CASE_STATUS_CLEAN)
) +
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = ifelse(sum_workers < 10, NA, scales::comma(sum_workers))),
    position = position_stack(vjust = 0.5),
    size = 2.7
  ) +
  facet_wrap(~CATEGORY, nrow = 3, scales = "free_y") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    x = "Year",
    y = "Workers",
    fill = "Case status"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "grey", fill = NA),
    strip.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.box.just = "left"
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

# --------------------------------------------------
# 2. Prepare data for right panel: visa type by year and category
# --------------------------------------------------
plot_workers <- visa_seafood %>%
  filter(
    as.integer(YEAR) >= 2017,
    as.integer(YEAR) <= 2025,
    CASE_STATUS_CLEAN == "certification"
  ) %>%
  mutate(TOTAL_WORKERS_CERTIFIED = as.numeric(TOTAL_WORKERS_CERTIFIED)) %>%
  group_by(YEAR, TYPE, CATEGORY) %>%
  summarize(
    sum_workers = sum(TOTAL_WORKERS_CERTIFIED, na.rm = TRUE),
    .groups = "drop"
  )

p_workers <- ggplot(
  plot_workers,
  aes(x = factor(YEAR), y = sum_workers, fill = TYPE)
) +
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = ifelse(sum_workers < 10, NA, scales::comma(sum_workers))),
    position = position_stack(vjust = 0.5),
    size = 2.7
  ) +
  facet_wrap(~CATEGORY, nrow = 3, scales = "free_y") +
  scale_fill_brewer(palette = "Paired") +
  labs(
    x = "Year",
    y = "Workers Certified",
    fill = "Visa type"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "grey", fill = NA),
    strip.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

# --------------------------------------------------
# 3. Combine panels into Figure 1
# --------------------------------------------------
p <- (p_case_status + p_workers) +
  plot_layout(
    ncol = 2,
    guides = "collect"
  ) +
  plot_annotation(
    tag_levels = "A",
    tag_suffix = ")"
  ) &
  theme(
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.justification = c(0.4, 0.5)
  )

# --------------------------------------------------
# 4. Save figure
# --------------------------------------------------
dir.create("figures", showWarnings = FALSE)

ggsave(
  filename = "figures/figure_1.png",
  plot = p,
  width = 8,
  height = 7,
  dpi = 300,
  bg = "white"
)