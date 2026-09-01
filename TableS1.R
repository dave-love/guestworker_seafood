# ==================================================
# Table S1
# Seafood visa workers by state, type, and category
# ==================================================

library(dplyr)
library(tidyr)
library(gt)
library(scales)

# --------------------------------------------------
# 1. Summarize total workers certified, 2017–2025
# --------------------------------------------------
top_states <- visa_seafood %>%
  filter(
    as.integer(YEAR) >= 2017,
    as.integer(YEAR) <= 2025,
    CASE_STATUS_CLEAN == "certification"
  ) %>%
  group_by(TYPE, CATEGORY, WORKSITE_STATE) %>%
  summarize(
    total_workers = sum(as.numeric(TOTAL_WORKERS_CERTIFIED), na.rm = TRUE),
    .groups = "drop"
  )

# --------------------------------------------------
# 2. Convert to wide format by TYPE and CATEGORY
# --------------------------------------------------
top_states_wide <- top_states %>%
  unite("type_category", TYPE, CATEGORY, sep = "_") %>%
  pivot_wider(
    names_from = type_category,
    values_from = total_workers,
    values_fill = 0
  )

# --------------------------------------------------
# 3. Identify category columns
# --------------------------------------------------
aquaculture_cols <- names(top_states_wide)[grepl("_Aquaculture$", names(top_states_wide))]
fishing_cols     <- names(top_states_wide)[grepl("_Fishing$", names(top_states_wide))]
processing_cols  <- names(top_states_wide)[grepl("_Seafood processing$", names(top_states_wide))]

category_cols <- c(aquaculture_cols, fishing_cols, processing_cols)

# --------------------------------------------------
# 4. Add total workers across all category columns
# --------------------------------------------------
top_states_wide <- top_states_wide %>%
  mutate(
    total_workers_2017_2025 = rowSums(across(all_of(category_cols)), na.rm = TRUE)
  ) %>%
  arrange(desc(total_workers_2017_2025))

# --------------------------------------------------
# 5. Create summary row using only category columns
# --------------------------------------------------
summary_row <- top_states_wide %>%
  summarize(
    WORKSITE_STATE = "Total",
    across(all_of(category_cols), ~ sum(.x, na.rm = TRUE)),
    total_workers_2017_2025 = sum(total_workers_2017_2025, na.rm = TRUE)
  )

top_states_wide_with_total <- bind_rows(top_states_wide, summary_row)

# --------------------------------------------------
# 6. Build gt table
# --------------------------------------------------
gt_table <- top_states_wide_with_total %>%
  gt(rowname_col = "WORKSITE_STATE") %>%
  tab_spanner(
    label = "Aquaculture",
    columns = all_of(aquaculture_cols)
  ) %>%
  tab_spanner(
    label = "Fishing",
    columns = all_of(fishing_cols)
  ) %>%
  tab_spanner(
    label = "Seafood processing",
    columns = all_of(processing_cols)
  ) %>%
  fmt_number(
    columns = where(is.numeric),
    decimals = 0,
    use_seps = TRUE
  ) %>%
  cols_label(
    WORKSITE_STATE = "State",
    total_workers_2017_2025 = "Total workers 2017–2025"
  ) %>%
  tab_options(
    table.font.size = "small"
  )

# --------------------------------------------------
# 7. Save table
# --------------------------------------------------
dir.create("tables", showWarnings = FALSE)

gtsave(
  gt_table,
  "tables/visa_seafood_state_table_total_2017to2025.docx"
)