# ==================================================
# Figure S3
# Hourly wages by seafood category
# ==================================================

library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(scales)

# --------------------------------------------------
# 1. Compute average anticipated hours for missing values
# --------------------------------------------------
avg_hours <- visa_seafood %>%
  filter(
    !is.na(ANTICIPATED_NUMBER_OF_HOURS),
    CASE_STATUS_CLEAN == "certification"
  ) %>%
  mutate(ANTICIPATED_NUMBER_OF_HOURS = as.numeric(ANTICIPATED_NUMBER_OF_HOURS)) %>%
  summarize(avg_hours = mean(ANTICIPATED_NUMBER_OF_HOURS, na.rm = TRUE)) %>%
  pull(avg_hours)

# --------------------------------------------------
# 2. Clean wage fields and convert to hourly wages
# --------------------------------------------------
visa_seafood_clean <- visa_seafood %>%
  filter(CASE_STATUS_CLEAN == "certification") %>%
  mutate(
    # Fix the specific PER error first
    PER = if_else(CASE_NUMBER == "G-200-24116-924997", "Year", PER),
    
    WAGE_OFFER_NUM = suppressWarnings(as.numeric(WAGE_OFFER)),
    ANTICIPATED_HOURS = suppressWarnings(as.numeric(ANTICIPATED_NUMBER_OF_HOURS)),
    ANTICIPATED_HOURS = if_else(is.na(ANTICIPATED_HOURS), avg_hours, ANTICIPATED_HOURS),
    
    EMPLOYMENT_BEGIN_DATE = as.Date(EMPLOYMENT_BEGIN_DATE),
    EMPLOYMENT_END_DATE   = as.Date(EMPLOYMENT_END_DATE),
    duration_days = as.numeric(difftime(EMPLOYMENT_END_DATE, EMPLOYMENT_BEGIN_DATE, units = "days")),
    duration_weeks = duration_days / 7,
    hours_per_week = ANTICIPATED_HOURS / duration_weeks,
    
    # If missing or invalid duration, assume 40 hours/week
    hours_per_week = if_else(is.na(hours_per_week) | hours_per_week <= 0, 40, hours_per_week),
    
    # Recode PER
    PER_CLEAN = case_when(
      WAGE_OFFER_NUM < 100 & (is.na(PER) | PER %in% c("Bi-Weekly", "Year")) ~ "Hour",
      TRUE ~ PER
    ),
    
    # Convert all wages to hourly
    WAGE_HOURLY = case_when(
      PER_CLEAN == "Hour" ~ WAGE_OFFER_NUM,
      PER_CLEAN == "Week" ~ WAGE_OFFER_NUM / hours_per_week,
      PER_CLEAN == "Bi-Weekly" ~ WAGE_OFFER_NUM / (hours_per_week * 2),
      PER_CLEAN == "Year" ~ WAGE_OFFER_NUM / (hours_per_week * 52),
      TRUE ~ NA_real_
    )
  ) %>%
  # Filter out implausibly high hourly wages
  filter(is.na(WAGE_HOURLY) | WAGE_HOURLY <= 100) %>%
  # Keep only the category-type combinations used in the analysis
  filter(
    (CATEGORY == "Aquaculture" & TYPE == "H-2A") |
      (CATEGORY == "Fishing" & TYPE == "H-2B") |
      (CATEGORY == "Seafood processing" & TYPE == "H-2B")
  )

# --------------------------------------------------
# 3. Calculate means and sample sizes by category
# --------------------------------------------------
wage_stats <- visa_seafood_clean %>%
  filter(!is.na(WAGE_HOURLY), !is.na(CATEGORY)) %>%
  group_by(CATEGORY) %>%
  summarize(
    mean_wage = mean(WAGE_HOURLY, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

# --------------------------------------------------
# 4. Category colors
# --------------------------------------------------
category_colors <- setNames(
  brewer.pal(3, "Set2"),
  c("Aquaculture", "Fishing", "Seafood processing")
)

# --------------------------------------------------
# 5. Plot
# --------------------------------------------------
p <- ggplot(
  visa_seafood_clean %>% filter(!is.na(WAGE_HOURLY), !is.na(CATEGORY)),
  aes(x = CATEGORY, y = WAGE_HOURLY, color = CATEGORY, fill = CATEGORY)
) +
  geom_violin(alpha = 0.3, show.legend = FALSE) +
  geom_point(
    alpha = 0.5,
    position = position_jitter(width = 0.2, height = 0),
    show.legend = FALSE
  ) +
  geom_text(
    data = wage_stats,
    aes(
      x = CATEGORY,
      y = mean_wage,
      label = paste0("Mean: $", round(mean_wage, 2), "\n(n=", comma(n), ")")
    ),
    vjust = -0.5,
    size = 3,
    color = "black",
    inherit.aes = FALSE
  ) +
  scale_color_manual(values = category_colors) +
  scale_fill_manual(values = category_colors) +
  scale_y_continuous(labels = dollar_format()) +
  labs(
    x = "Category",
    y = "Hourly Wage ($)"
  ) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(colour = "grey", fill = NA),
    strip.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

# --------------------------------------------------
# 6. Save figure
# --------------------------------------------------
dir.create("figures", showWarnings = FALSE)

ggsave(
  filename = "figures/wage_by_category.png",
  plot = p,
  width = 8,
  height = 6,
  dpi = 300,
  bg = "white"
)