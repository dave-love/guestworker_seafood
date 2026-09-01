# ==================================================
# Figure S4
# Distribution of anticipated hours by seafood category
# ==================================================

library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(scales)

# --------------------------------------------------
# 1. Clean hours data and apply category/type filters
# --------------------------------------------------
hours_clean <- visa_seafood %>%
  mutate(
    ANTICIPATED_HOURS = suppressWarnings(as.numeric(ANTICIPATED_NUMBER_OF_HOURS))
  ) %>%
  filter(
    !is.na(ANTICIPATED_HOURS),
    !is.na(TYPE),
    !is.na(CATEGORY),
    ANTICIPATED_HOURS >= 10
  ) %>%
  filter(
    (CATEGORY == "Aquaculture" & TYPE == "H-2A") |
      (CATEGORY == "Fishing" & TYPE == "H-2B") |
      (CATEGORY == "Seafood processing" & TYPE == "H-2B")
  )

# --------------------------------------------------
# 2. Calculate summary statistics by category
# --------------------------------------------------
hours_stats <- hours_clean %>%
  group_by(CATEGORY) %>%
  summarize(
    mean_hours = mean(ANTICIPATED_HOURS, na.rm = TRUE),
    n = n(),
    max_y = max(ANTICIPATED_HOURS, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    label_y = max_y * 1.05
  )

# --------------------------------------------------
# 3. Category colors
# --------------------------------------------------
category_colors <- setNames(
  brewer.pal(3, "Set2"),
  c("Aquaculture", "Fishing", "Seafood processing")
)

# --------------------------------------------------
# 4. Plot
# --------------------------------------------------
p <- ggplot(
  hours_clean,
  aes(x = CATEGORY, y = ANTICIPATED_HOURS, color = CATEGORY, fill = CATEGORY)
) +
  geom_violin(alpha = 0.3, show.legend = FALSE) +
  geom_point(
    alpha = 0.5,
    position = position_jitter(width = 0.2, height = 0),
    show.legend = FALSE
  ) +
  geom_text(
    data = hours_stats,
    aes(
      x = CATEGORY,
      y = label_y,
      label = paste0(
        "Mean: ", comma(round(mean_hours, 0)),
        " hrs\n(n=", comma(n), ")"
      )
    ),
    vjust = 0,
    size = 3,
    color = "black",
    inherit.aes = FALSE
  ) +
  scale_color_manual(values = category_colors) +
  scale_fill_manual(values = category_colors) +
  scale_y_continuous(labels = comma_format()) +
  labs(
    x = "Category",
    y = "Anticipated Hours"
  ) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(colour = "grey", fill = NA),
    strip.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

# --------------------------------------------------
# 5. Save figure
# --------------------------------------------------
dir.create("figures", showWarnings = FALSE)

ggsave(
  filename = "figures/hours_by_category.png",
  plot = p,
  width = 8,
  height = 6,
  dpi = 300,
  bg = "white"
)