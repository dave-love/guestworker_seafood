# ==================================================
# 02_clean_classify_seafood.R
# Clean, classify, and subset seafood-related records
# ==================================================

library(tidyverse)
library(stringr)
library(lubridate)

# --------------------------------------------------
# 1. Load raw imported data
# --------------------------------------------------
visa_raw <- readRDS("data_raw/visa_raw.rds")

# --------------------------------------------------
# 2. Helper function to parse dates safely
# --------------------------------------------------
parse_any_date <- function(x) {
  suppressWarnings(
    parse_date_time(
      x,
      orders = c("mdy", "ymd", "dmy", "mdy HMS", "ymd HMS", "dmy HMS")
    )
  )
}

# --------------------------------------------------
# 3. Ensure date columns are parsed correctly
# --------------------------------------------------
visa_raw <- visa_raw %>%
  mutate(
    DECISION_DATE = parse_any_date(DECISION_DATE),
    EMPLOYMENT_BEGIN_DATE = parse_any_date(EMPLOYMENT_BEGIN_DATE),
    EMPLOYMENT_END_DATE = parse_any_date(EMPLOYMENT_END_DATE)
  )

# --------------------------------------------------
# 4. Restrict years of interest
# --------------------------------------------------
H2A_2017_2025 <- visa_raw %>%
  filter(TYPE == "H-2A", as.integer(YEAR) %in% 2017:2025)

H2B_2017_2025 <- visa_raw %>%
  filter(TYPE == "H-2B", as.integer(YEAR) %in% 2017:2025)

PERM_2017_2025 <- visa_raw %>%
  filter(TYPE == "PERM", as.integer(YEAR) %in% 2017:2025)

# --------------------------------------------------
# 5. Shared seafood keyword pattern
# --------------------------------------------------
target <- c(
  "fish", "fishing", "fishers", "fisherman", "shrimp", "aquaculture",
  "roe", "herring", "seafood", "crab", "oyster", "fingerling",
  "crawfish", "hatchery"
)

pattern <- regex(
  paste0("\\b(", paste(target, collapse = "|"), ")\\b"),
  ignore_case = TRUE
)

# --------------------------------------------------
# 6. H-2A seafood sample
# --------------------------------------------------
H2A_seafood_1 <- H2A_2017_2025 %>%
  filter(str_starts(as.character(NAICS_CODE), "1125")) %>%
  mutate(
    TOTAL_WORKERS_CERTIFIED = suppressWarnings(as.numeric(TOTAL_WORKERS_CERTIFIED)),
    WAGE_OFFER = suppressWarnings(as.numeric(WAGE_OFFER)),
    TYPE = "H-2A"
  )

H2A_seafood_2 <- H2A_2017_2025 %>%
  filter(
    (str_detect(JOB_TITLE, pattern) | str_detect(EMPLOYER_NAME, pattern)) &
      (!str_detect(as.character(NAICS_CODE), "^(1123|1151|3116)") | is.na(NAICS_CODE)) &
      !str_detect(JOB_TITLE, regex("poultry", ignore_case = TRUE))
  ) %>%
  filter(
    !EMPLOYER_NAME %in% c(
      "CY FARMS, LLC", "JEFFREY MANUEL", "THOMAS ROE", "VEGETABLE GROWER",
      "ROE Farms", "Roe Farms, Inc.", "THOMAS S ROE", "Pecos Crossing Headquarters",
      "ROE ORCHARDS", "THOMAS S. ROE", "S M Cattle Co. LLC.", "Roe Farms"
    )
  ) %>%
  filter(
    !JOB_TITLE %in% c(
      "FARM WORKER; VEGETABLE II",
      "FARMWORKER: VEGETABLE",
      "VEGETABLE GROWER",
      "FARMWORKER: VEGETABLE",
      "VEGETABLE GROWER\n"
    )
  ) %>%
  mutate(
    TOTAL_WORKERS_CERTIFIED = suppressWarnings(as.numeric(TOTAL_WORKERS_CERTIFIED)),
    WAGE_OFFER = suppressWarnings(as.numeric(WAGE_OFFER)),
    TYPE = "H-2A"
  )

H2A_seafood <- bind_rows(H2A_seafood_1, H2A_seafood_2) %>%
  distinct()

rm(H2A_seafood_1, H2A_seafood_2)

# --------------------------------------------------
# 7. H-2B seafood sample
# --------------------------------------------------
H2B_seafood_1 <- H2B_2017_2025 %>%
  filter(
    str_starts(as.character(NAICS_CODE), "1141") |
      str_starts(as.character(NAICS_CODE), "42446") |
      str_starts(as.character(NAICS_CODE), "3117")
  ) %>%
  mutate(
    TOTAL_WORKERS_CERTIFIED = suppressWarnings(as.numeric(TOTAL_WORKERS_CERTIFIED)),
    WAGE_OFFER = suppressWarnings(as.numeric(WAGE_OFFER)),
    TYPE = "H-2B"
  )

H2B_seafood_2 <- H2B_2017_2025 %>%
  filter(
    (str_detect(JOB_TITLE, pattern) | str_detect(EMPLOYER_NAME, pattern)) &
      (!str_detect(as.character(NAICS_CODE), "^(1123|1151|3116|4511|4451|4452|4872|5617|5613|71|72)") | is.na(NAICS_CODE)) &
      !str_detect(JOB_TITLE, regex("poultry|cook|coach|counter|amusement", ignore_case = TRUE)) &
      !str_detect(
        JOB_TITLE,
        regex(
          "casheir|cleaner|dishwasher|dining room|fast|food assembler|fry cooks|guide, hunting and fishing|housekeeper|hotel clerk|kitchen helper|washer, machine|waiter|restaurant|carnival",
          ignore_case = TRUE
        )
      )
  ) %>%
  filter(
    !EMPLOYER_NAME %in% c(
      "VELA POULTRY, INC", "PHILLIPS SEAFOOD RESTAURANTS", "ROE GORDAN",
      "ROE GORDAN RACING STABLES", "ROE ORCHARDS",
      "ROE ORCHARDS / VALLEY GROWERS CO-OP", "ROE'S ORCHARDS", "THOMAS ROE",
      "THOMAS S. ROE", "TRAVIS SMITH - ROE FARMS", "Mud City Crab House"
    )
  ) %>%
  mutate(
    TOTAL_WORKERS_CERTIFIED = suppressWarnings(as.numeric(TOTAL_WORKERS_CERTIFIED)),
    WAGE_OFFER = suppressWarnings(as.numeric(WAGE_OFFER)),
    TYPE = "H-2B"
  )

H2B_seafood <- bind_rows(H2B_seafood_1, H2B_seafood_2) %>%
  distinct()

rm(H2B_seafood_1, H2B_seafood_2)

# --------------------------------------------------
# 8. PERM seafood sample
# --------------------------------------------------
PERM_seafood_1 <- PERM_2017_2025 %>%
  filter(
    str_starts(as.character(NAICS_CODE), "1125") |
      str_starts(as.character(NAICS_CODE), "1141") |
      str_starts(as.character(NAICS_CODE), "42446") |
      str_starts(as.character(NAICS_CODE), "3117")
  ) %>%
  mutate(
    WAGE_OFFER = suppressWarnings(as.numeric(WAGE_OFFER)),
    NAICS_CODE = as.character(NAICS_CODE),
    TYPE = "Permanent",
    TOTAL_WORKERS_CERTIFIED = 1
  )

PERM_seafood_2 <- PERM_2017_2025 %>%
  filter(
    (str_detect(JOB_TITLE, pattern) | str_detect(EMPLOYER_NAME, pattern)) &
      (!str_detect(as.character(NAICS_CODE), "^(1123|1151|3116|4511|4451|4452|4872|5617|5613|71|72)") | is.na(NAICS_CODE)) &
      !str_detect(JOB_TITLE, regex("nanny|patent|venture|poultry|cook|chef|forest|cashier|counter|amusement", ignore_case = TRUE)) &
      !str_detect(
        JOB_TITLE,
        regex(
          "attorney|legal|restaurant|casheir|cleaner|dishwasher|dining room|fast food worker|food assembler|fry cooks|guide, hunting and fishing|housekeeper|hotel clerk|kitchen helper|washer, machine|window|retail",
          ignore_case = TRUE
        )
      )
  ) %>%
  filter(
    !EMPLOYER_NAME %in% c(
      "ROE FARMS JTV",
      "FISH & RICHARDSON P.C.\n",
      "ROE DEVELOPMENT CORPORATION",
      "ZALMENS FISH MARKET INC.",
      "FLORIDA HOSPITAL FISH MEMORIAL",
      "OXENDINE FARMS",
      "ANTONIUS J. SCHILDERINK DBA SCHILDERINK DAIRY",
      "KURT WEISS GREENHOUSES, INC.",
      "BIG FISH GAMES",
      "PFFJ, LLC",
      "HUNGRY FISH MEDIA LLC",
      "\tFISH & RICHARDSON P.C.",
      "CRAVENS NURSERY INC.",
      "Delray Plants Co.",
      "SAINTS, INC. & PIEDMONT DAIRIES, INC. D/B/A ALLIANCE DAIRIES",
      "SEKEL MANAGEMENT GROUP",
      "Engemann Farms",
      "Owen Roe LLC.",
      "LB PORK, INC.",
      "ENCHANTMENT FARM, LLC",
      "IMPERIAL FORESTRY INC",
      "B & J Ag Inc",
      "INGURAN LLC DBA SEXING TECHNOLOGIES",
      "Marc Lippens d/b/a Lippens-Agria Dairy",
      "frio river ranch ltd.",
      "Dr. Fish, Inc.",
      "ROE Visual US, Inc.",
      "BURNS AND ROE ENTERPRISES",
      "ROE Dental Laboratory, Inc.",
      "Meadow Lane Beef, LLC",
      "JOE'S CRAB SHACK HOLDINGS, INC.",
      "RICKY AND LUCY'S COUNTRY GREENHOUSE",
      "Bradley J. Fish, Inc. , dba Sullair of Houston",
      "DONALD D. KNIER JR.",
      "NELSON DEVELOPMENT LLC",
      "NEIL DRYSDALE RACING STABLES",
      "vanilla fish, inc.",
      "FISH & RICHARDSON P.C.",
      "National Fish and Wildlife Foundation",
      "FISHERS MASTER YOOS TAE KWON DO AND MARTIAL ARTS",
      "BURNS AND ROE ENTERPRISES",
      "NATIONAL OILWELL VARCO, L.P.",
      "EL 7 MARES SEAFOOD INC.",
      "FISH  RICHARDSON P.C.",
      "SCIENTIFIC CERTIFICATION SYSTEMS",
      "Pure Fishing Inc.",
      "Crowder Custom Rods, Inc.",
      "ROCKET FISH, INC.",
      "Oyster Ventures LLC",
      "BRITTANY FISH",
      "GO FISH CARGO INC.",
      "Blue Ocean Fish Corp",
      "Global Aquaculture Alliance",
      "wellfleet harbor seafood company",
      "Wellfleet Harbor Seafood Company",
      "Siam Seafood Products Inc.",
      "Seafood Wholesalers Ltd",
      "NEW GARDENS FISH MARKET CORP",
      "Virgin Fish Inc"
    )
  ) %>%
  filter(
    !JOB_TITLE %in% c(
      "Food Service Manager",
      "FOOD SERVICE MANAGER",
      "Food Service Managers",
      "Furniture Finishers"
    )
  ) %>%
  mutate(
    NAICS_CODE = as.character(NAICS_CODE),
    TYPE = "Permanent",
    WAGE_OFFER = suppressWarnings(as.numeric(WAGE_OFFER)),
    TOTAL_WORKERS_CERTIFIED = 1
  )

PERM_seafood <- bind_rows(PERM_seafood_1, PERM_seafood_2) %>%
  distinct() %>%
  mutate(
    CASE_STATUS = recode(CASE_STATUS, "Certified - Expired" = "Certified-Expired")
  )

rm(PERM_seafood_1, PERM_seafood_2)

# --------------------------------------------------
# 9. Combine all visa data
# --------------------------------------------------
visa_seafood <- bind_rows(H2A_seafood, H2B_seafood, PERM_seafood) %>%
  mutate(
    NAICS_CODE = as.character(NAICS_CODE),
    JOB_TITLE = tolower(as.character(JOB_TITLE)),
    EMPLOYER_NAME = tolower(as.character(EMPLOYER_NAME))
  ) %>%
  mutate(
    CATEGORY = case_when(
      NAICS_CODE %in% c("1", "11", "311") ~ "Aquaculture",
      str_starts(NAICS_CODE, "111") ~ "Aquaculture",
      str_starts(NAICS_CODE, "112") ~ "Aquaculture",
      str_starts(NAICS_CODE, "1125") ~ "Aquaculture",
      str_starts(NAICS_CODE, "114") ~ "Fishing",
      str_starts(NAICS_CODE, "332") ~ "Seafood processing",
      str_starts(NAICS_CODE, "311991") ~ "Seafood processing",
      str_starts(NAICS_CODE, "3114") ~ "Seafood processing",
      str_starts(NAICS_CODE, "3117") ~ "Seafood processing",
      str_starts(NAICS_CODE, "42") ~ "Seafood processing",
      str_starts(NAICS_CODE, "43") ~ "Seafood processing",
      TRUE ~ NA_character_
    ),
    CATEGORY = if_else(
      NAICS_CODE == "114112" &
        str_detect(JOB_TITLE, regex("crawfish|farm|aquaculture", ignore_case = TRUE)),
      "Aquaculture",
      CATEGORY
    ),
    CATEGORY = if_else(
      NAICS_CODE == "114111" &
        str_detect(JOB_TITLE, regex("crawfish|farm|aquaculture", ignore_case = TRUE)),
      "Aquaculture",
      CATEGORY
    )
  ) %>%
  mutate(
    JOB_TITLE = tolower(JOB_TITLE),
    EMPLOYER_NAME = tolower(EMPLOYER_NAME)
  )

# --------------------------------------------------
# 10. Standardize state abbreviations
# --------------------------------------------------
state_lookup <- tibble::tibble(
  full = c(state.name, "District of Columbia"),
  abbr = c(state.abb, "DC")
) %>%
  mutate(full = toupper(full))

visa_seafood <- visa_seafood %>%
  mutate(
    WORKSITE_STATE = toupper(as.character(WORKSITE_STATE)),
    EMPLOYER_STATE = toupper(as.character(EMPLOYER_STATE))
  ) %>%
  left_join(state_lookup, by = c("WORKSITE_STATE" = "full")) %>%
  mutate(WORKSITE_STATE = coalesce(abbr, WORKSITE_STATE)) %>%
  select(-abbr) %>%
  left_join(state_lookup, by = c("EMPLOYER_STATE" = "full")) %>%
  mutate(EMPLOYER_STATE = coalesce(abbr, EMPLOYER_STATE)) %>%
  select(-abbr)

# --------------------------------------------------
# 11. Clean case status
# --------------------------------------------------
visa_seafood <- visa_seafood %>%
  mutate(
    CASE_STATUS_CLEAN = case_when(
      str_detect(CASE_STATUS, regex("certification.*expired|certified.?-expired|expired", ignore_case = TRUE)) ~ "certification expired",
      str_detect(CASE_STATUS, regex("certification|certified", ignore_case = TRUE)) ~ "certification",
      str_detect(CASE_STATUS, regex("withdrawn", ignore_case = TRUE)) ~ "withdrawn",
      str_detect(CASE_STATUS, regex("denied|rejected", ignore_case = TRUE)) ~ "denied",
      TRUE ~ "other"
    )
  )

# --------------------------------------------------
# 12. Add category for H-2A 2019 cases with missing NAICS
# --------------------------------------------------
visa_seafood <- visa_seafood %>%
  mutate(
    CATEGORY = if_else(
      as.integer(YEAR) == 2019 &
        TYPE == "H-2A" &
        is.na(NAICS_CODE) &
        is.na(CATEGORY),
      "Aquaculture",
      CATEGORY
    )
  )

# --------------------------------------------------
# 13. Normalize numeric and postal code fields
# --------------------------------------------------
visa_seafood <- visa_seafood %>%
  mutate(
    EMPLOYER_POSTAL_CODE = str_extract(as.character(EMPLOYER_POSTAL_CODE), "^[0-9]{5}"),
    WORKSITE_POSTAL_CODE = str_extract(as.character(WORKSITE_POSTAL_CODE), "^[0-9]{5}"),
    ANTICIPATED_NUMBER_OF_HOURS = suppressWarnings(as.numeric(ANTICIPATED_NUMBER_OF_HOURS)),
    WAGE_OFFER = suppressWarnings(as.numeric(WAGE_OFFER)),
    OVERTIME_RATE = suppressWarnings(as.numeric(OVERTIME_RATE))
  )

# --------------------------------------------------
# 14. Make PERM worksite address equal employer address
# --------------------------------------------------
visa_seafood <- visa_seafood %>%
  mutate(
    WORKSITE_CITY = if_else(TYPE == "Permanent", EMPLOYER_CITY, WORKSITE_CITY),
    WORKSITE_STATE = if_else(TYPE == "Permanent", EMPLOYER_STATE, WORKSITE_STATE),
    WORKSITE_POSTAL_CODE = if_else(TYPE == "Permanent", EMPLOYER_POSTAL_CODE, WORKSITE_POSTAL_CODE)
  )

visa_seafood <- visa_seafood %>%
  mutate(
    WORKSITE_POSTAL_CODE = str_pad(WORKSITE_POSTAL_CODE, width = 5, side = "left", pad = "0")
  )

# --------------------------------------------------
# 15. Fill missing worksite states using ZIP lookup
# --------------------------------------------------
zip_lookup <- readr::read_csv("data_raw/zip_state_lookup.csv", show_col_types = FALSE) %>%
  mutate(
    zip = str_pad(as.character(zip), width = 5, side = "left", pad = "0"),
    state = toupper(as.character(state))
  ) %>%
  distinct(zip, .keep_all = TRUE)

visa_seafood <- visa_seafood %>%
  mutate(
    WORKSITE_POSTAL_CODE = str_pad(as.character(WORKSITE_POSTAL_CODE), width = 5, side = "left", pad = "0")
  ) %>%
  left_join(zip_lookup, by = c("WORKSITE_POSTAL_CODE" = "zip")) %>%
  mutate(
    WORKSITE_STATE = coalesce(WORKSITE_STATE, state)
  ) %>%
  select(-state)

# --------------------------------------------------
# 16. Secondary category assignment for uncoded records
# --------------------------------------------------
missing <- visa_seafood %>%
  filter(as.integer(YEAR) >= 2017, as.integer(YEAR) <= 2025) %>%
  filter(CASE_STATUS_CLEAN == "certification") %>%
  filter(is.na(CATEGORY)) %>%
  mutate(
    CATEGORY = case_when(
      str_detect(
        JOB_TITLE,
        regex(
          "butcher|chopper|tester|filleter|trimmer|separator|boiler|header|scientist|technician|drier|cannery|pick|peel|cut|process|grade|plant|shuck|pack|machine|maintenance supervisor|market research analyst|network and computer systems administrators|production supervisor|store clerks and orders fillers",
          ignore_case = TRUE
        )
      ) ~ "Seafood processing",
      str_detect(JOB_TITLE, regex("farm|labor|aquacultur|hatchery", ignore_case = TRUE)) ~ "Aquaculture",
      str_detect(JOB_TITLE, regex("deck|diver|dock|man|boat|fishers", ignore_case = TRUE)) ~ "Fishing",
      TRUE ~ NA_character_
    )
  )

visa_seafood <- visa_seafood %>%
  left_join(
    missing %>% select(CASE_NUMBER, CATEGORY_missing = CATEGORY),
    by = "CASE_NUMBER"
  ) %>%
  mutate(
    CATEGORY = coalesce(CATEGORY, CATEGORY_missing)
  ) %>%
  select(-CATEGORY_missing)

# --------------------------------------------------
# 17. Manual recodes
# --------------------------------------------------
recode_df <- tribble(
  ~EMPLOYER_NAME,                   ~JOB_TITLE,                      ~NEW_CATEGORY,
  "deshotels crawfish farms llc",   "farmworker aquacultural",       "Aquaculture",
  "sea kirk co., inc.",             "shrimp headers / deckhands",    "Fishing",
  "woods & waters enterprises inc.", "fish farm",                    "Aquaculture",
  "starkist co.",                   "associate seafood procurement",  "Seafood processing"
)

visa_seafood <- visa_seafood %>%
  left_join(recode_df, by = c("EMPLOYER_NAME", "JOB_TITLE")) %>%
  mutate(
    CATEGORY = case_when(
      is.na(CATEGORY) & !is.na(NEW_CATEGORY) ~ NEW_CATEGORY,
      TRUE ~ CATEGORY
    )
  ) %>%
  select(-NEW_CATEGORY) %>%
  filter(!is.na(CATEGORY))

# --------------------------------------------------
# 18. Species extraction
# --------------------------------------------------
species_terms_1 <- c("crawfish", "crab", "oyster", "salmon", "pollock", "shrimp")
species_pattern_1 <- regex(paste(species_terms_1, collapse = "|"), ignore_case = TRUE)

extract_species <- function(job, employer, pattern) {
  x <- paste(job, employer, sep = " ")
  m <- str_extract(x, pattern)
  ifelse(is.na(m), NA_character_, tolower(m))
}

visa_seafood <- visa_seafood %>%
  mutate(
    SPECIES = extract_species(JOB_TITLE, EMPLOYER_NAME, species_pattern_1)
  )

species_terms_2 <- c("fish", "shellfish")
species_pattern_2 <- regex(paste(species_terms_2, collapse = "|"), ignore_case = TRUE)

visa_seafood <- visa_seafood %>%
  mutate(
    SPECIES = if_else(
      is.na(SPECIES),
      extract_species(JOB_TITLE, EMPLOYER_NAME, species_pattern_2),
      SPECIES
    )
  )

# --------------------------------------------------
# 19. Keep only final objects and rename datasets
# --------------------------------------------------
H2A <- H2A_2017_2025
H2B <- H2B_2017_2025
PERM <- PERM_2017_2025

rm(
  H2A_2017_2025,
  H2B_2017_2025,
  PERM_2017_2025
)

# Remove everything except final objects
keep_objects <- c("visa_seafood", "H2A", "H2B", "PERM")
rm(list = setdiff(ls(), keep_objects))

# --------------------------------------------------
# 20. Save cleaned dataset
# --------------------------------------------------
dir.create("data_clean", showWarnings = FALSE)
saveRDS(visa_seafood, "data_clean/visa_seafood.rds")
saveRDS(H2A, "data_clean/H2A.rds")
saveRDS(H2B, "data_clean/H2B.rds")
saveRDS(PERM, "data_clean/PERM.rds")