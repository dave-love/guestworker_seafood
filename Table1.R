# ==================================================
# Table 1
# Seafood workers by year, type, and category
# One table per TYPE
# ==================================================

library(dplyr)
library(tidyr)
library(gt)
library(scales)

# --------------------------------------------------
# 1. Prepare base data
# --------------------------------------------------
table_base <- visa_seafood %>%
  filter(CASE_STATUS_CLEAN == "certification") %>%
  group_by(YEAR, TYPE, CATEGORY) %>%
  summarize(
    seafood_total = sum(as.numeric(TOTAL_WORKERS_CERTIFIED), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    TYPE = case_when(
      TYPE == "H-2A" ~ "H2A",
      TYPE == "H-2B" ~ "H2B",
      TYPE == "Permanent" ~ "Permanent",
      TRUE ~ TYPE
    ),
    YEAR = as.character(YEAR)
  )

# --------------------------------------------------
# 2. Function to make one table for one TYPE
# --------------------------------------------------
make_type_table <- function(df, type_name, type_label, out_file) {
  
  type_table <- df %>%
    filter(TYPE == type_name) %>%
    select(-TYPE) %>%
    pivot_wider(
      names_from = CATEGORY,
      values_from = seafood_total,
      values_fill = 0,
      names_glue = "{CATEGORY}"
    ) %>%
    arrange(as.integer(YEAR))
  
  # Add Total column using all numeric columns except YEAR
  type_table <- type_table %>%
    mutate(
      Total = rowSums(across(where(is.numeric)), na.rm = TRUE)
    )
  
  # Add total row
  total_row <- type_table %>%
    summarize(
      YEAR = "Total",
      across(where(is.numeric), ~ sum(.x, na.rm = TRUE))
    )
  
  type_table_final <- bind_rows(type_table, total_row)
  
  # Format numeric columns
  type_table_final <- type_table_final %>%
    mutate(across(where(is.numeric), ~ scales::comma(.x)))
  
  # Build gt table
  gt_tbl <- type_table_final %>%
    gt(rowname_col = "YEAR") %>%
    tab_header(
      title = type_label
    ) %>%
    tab_options(
      table.font.size = "small"
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(rows = YEAR == "Total")
    ) %>%
    tab_style(
      style = cell_borders(sides = "top", color = "black", weight = px(2)),
      locations = cells_body(rows = YEAR == "Total")
    )
  
  gtsave(gt_tbl, out_file)
}

# --------------------------------------------------
# 3. Create one table per TYPE
# --------------------------------------------------
dir.create("tables", showWarnings = FALSE)

make_type_table(
  df = table_base,
  type_name = "H2A",
  type_label = "H-2A",
  out_file = "tables/table1_H2A.png"
)

make_type_table(
  df = table_base,
  type_name = "H2B",
  type_label = "H-2B",
  out_file = "tables/table1_H2B.png"
)

make_type_table(
  df = table_base,
  type_name = "Permanent",
  type_label = "Permanent",
  out_file = "tables/table1_Permanent.png"
)