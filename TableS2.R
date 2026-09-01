# ==================================================
# Table S2
# Seafood visa workers by species, state, sector, and visa type
# ==================================================

library(dplyr)
library(tidyr)
library(tibble)
library(gt)
library(scales)

# --------------------------------------------------
# 0. Recode TYPE for cleaner column names
# --------------------------------------------------
visa_seafood2 <- visa_seafood %>%
  mutate(
    TYPE = recode(TYPE,
                  "H-2A" = "H2A",
                  "H-2B" = "H2B",
                  "Permanent" = "Permanent"
    )
  )

# --------------------------------------------------
# 1. Summaries by species × state × sector × type
# --------------------------------------------------
species_state_summary <- visa_seafood2 %>%
  filter(
    as.integer(YEAR) >= 2017,
    as.integer(YEAR) <= 2025,
    CASE_STATUS_CLEAN == "certification",
    !is.na(SPECIES),
    !is.na(TYPE)
  ) %>%
  group_by(SPECIES, CATEGORY, WORKSITE_STATE, TYPE) %>%
  summarize(
    total_workers = sum(as.numeric(TOTAL_WORKERS_CERTIFIED), na.rm = TRUE),
    .groups = "drop"
  )

# --------------------------------------------------
# 2. Identify sector × state combinations with enough workers
#    Threshold based on all TYPEs combined
# --------------------------------------------------
sector_totals <- species_state_summary %>%
  group_by(CATEGORY, WORKSITE_STATE) %>%
  summarize(
    total_workers_sector = sum(total_workers, na.rm = TRUE),
    .groups = "drop"
  )

overall_sector_totals <- visa_seafood2 %>%
  filter(
    as.integer(YEAR) >= 2017,
    as.integer(YEAR) <= 2025,
    CASE_STATUS_CLEAN == "certification",
    !is.na(TYPE)
  ) %>%
  group_by(CATEGORY, WORKSITE_STATE) %>%
  summarize(
    overall_total_workers = sum(as.numeric(TOTAL_WORKERS_CERTIFIED), na.rm = TRUE),
    .groups = "drop"
  )

species_state_summary <- species_state_summary %>%
  left_join(sector_totals, by = c("CATEGORY", "WORKSITE_STATE")) %>%
  left_join(overall_sector_totals, by = c("CATEGORY", "WORKSITE_STATE")) %>%
  mutate(percent_coverage = total_workers_sector / overall_total_workers) %>%
  filter(total_workers_sector >= 20)

pct <- species_state_summary %>%
  distinct(CATEGORY, WORKSITE_STATE, percent_coverage)

species_state_summary <- species_state_summary %>%
  select(-total_workers_sector, -overall_total_workers, -percent_coverage)

# --------------------------------------------------
# 3. Wide table: rows = species, columns = category_state_type
# --------------------------------------------------
species_state_wide <- species_state_summary %>%
  unite("cat_state_type", CATEGORY, WORKSITE_STATE, TYPE, sep = "__") %>%
  pivot_wider(
    names_from = cat_state_type,
    values_from = total_workers,
    values_fill = 0
  )

# --------------------------------------------------
# 4. Add Total column and Total row
# --------------------------------------------------
species_state_wide <- species_state_wide %>%
  mutate(
    Total = rowSums(across(where(is.numeric)), na.rm = TRUE)
  )

total_row <- species_state_wide %>%
  summarize(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
  mutate(SPECIES = "Total")

species_state_wide_tot <- bind_rows(species_state_wide, total_row)

# --------------------------------------------------
# 5. Transpose so rows are sector-state-type and columns are species
# --------------------------------------------------
species_state_wide_transposed <- species_state_wide_tot %>%
  column_to_rownames("SPECIES") %>%
  t() %>%
  as.data.frame(check.names = FALSE) %>%
  rownames_to_column("cat_state_type")

# --------------------------------------------------
# 6. Separate cat_state_type back into CATEGORY, WORKSITE_STATE, TYPE
# --------------------------------------------------
species_state_wide_transposed <- species_state_wide_transposed %>%
  separate(
    col = cat_state_type,
    into = c("CATEGORY", "WORKSITE_STATE", "TYPE"),
    sep = "__",
    fill = "right",
    extra = "merge"
  ) %>%
  left_join(pct, by = c("CATEGORY", "WORKSITE_STATE")) %>%
  rename(
    State = WORKSITE_STATE,
    Sector = CATEGORY
  ) %>%
  distinct()

# --------------------------------------------------
# 7. Helper function for sector-specific gt tables
# --------------------------------------------------
make_sector_gt <- function(data, sector_name) {
  data %>%
    filter(Sector == sector_name) %>%
    arrange(State, desc(as.numeric(Total))) %>%
    select(-Sector) %>%
    gt() %>%
    tab_header(
      title = sector_name
    ) %>%
    fmt_number(
      columns = Total,
      decimals = 0,
      use_seps = TRUE
    ) %>%
    fmt_percent(
      columns = percent_coverage,
      decimals = 1
    ) %>%
    tab_options(
      table.font.size = "small"
    )
}

# --------------------------------------------------
# 8. Create gt tables
# --------------------------------------------------
gt_fishing <- make_sector_gt(species_state_wide_transposed, "Fishing")
gt_aquaculture <- make_sector_gt(species_state_wide_transposed, "Aquaculture")
gt_processing <- make_sector_gt(species_state_wide_transposed, "Seafood processing")

# --------------------------------------------------
# 9. Save to file
# --------------------------------------------------
dir.create("tables", showWarnings = FALSE)

gtsave(
  gt_fishing,
  "tables/visa_seafood_species_state_fishing_alt.png",
  vwidth = 1600,
  vheight = 900
)

gtsave(
  gt_aquaculture,
  "tables/visa_seafood_species_state_aquaculture_alt.png",
  vwidth = 1600,
  vheight = 900
)

gtsave(
  gt_processing,
  "tables/visa_seafood_species_state_processing_alt.png",
  vwidth = 1600,
  vheight = 900
)