# ==================================================
# 01_download_import.R
# Download and import DOL foreign labor disclosure files
# ==================================================

# --------------------------------------------------
# 0. Packages
# --------------------------------------------------
library(tidyverse)
library(stringr)
library(lubridate)
library(readxl)

# --------------------------------------------------
# 1. File index: DOL download URLs
# --------------------------------------------------
file_index <- tribble(
  ~TYPE,   ~YEAR, ~DOWNLOAD_URL,
  "H-2A",  2025,  "https://www.dol.gov/sites/dolgov/files/ETA/oflc/pdfs/H-2A_Disclosure_Data_FY2025_Q4.xlsx",
  "H-2A",  2024,  "https://www.dol.gov/sites/dolgov/files/ETA/oflc/pdfs/H-2A_Disclosure_Data_FY2024_Q4.xlsx",
  "H-2A",  2023,  "https://www.dol.gov/sites/dolgov/files/ETA/oflc/pdfs/H-2A_Disclosure_Data_FY2023_Q4.xlsx",
  "H-2A",  2022,  "https://www.dol.gov/sites/dolgov/files/ETA/oflc/pdfs/H-2A_Disclosure_Data_FY2022_Q4.xlsx",
  "H-2A",  2021,  "https://www.dol.gov/sites/dolgov/files/ETA/oflc/pdfs/H-2A_Disclosure_Data_FY2021.xlsx",
  "H-2A",  2020,  "https://www.dol.gov/sites/dolgov/files/ETA/oflc/pdfs/H-2A_Disclosure_Data_FY2020.xlsx",
  "H-2A",  2019,  "https://www.dol.gov/sites/dolgov/files/ETA/oflc/pdfs/H-2A_Disclosure_Data_FY2019.xlsx",
  "H-2A",  2018,  "https://www.dol.gov/sites/dolgov/files/ETA/oflc/pdfs/H-2A_Disclosure_Data_FY2018_EOY.xlsx",
  "H-2A",  2017,  "https://www.dol.gov/sites/dolgov/files/ETA/oflc/pdfs/H-2A_Disclosure_Data_FY17.xlsx",
  
  "H-2B",  2025,  "https://www.dol.gov/sites/dolgov/files/ETA/oflc/pdfs/H-2B_Disclosure_Data_FY2025_Q4.xlsx",
  "H-2B",  2024,  "https://www.dol.gov/sites/dolgov/files/ETA/oflc/pdfs/H-2B_Disclosure_FY2024_Q4.xlsx",
  "H-2B",  2023,  "https://www.dol.gov/sites/dolgov/files/ETA/oflc/pdfs/H-2B_Disclosure_Data_FY2023_Q4.xlsx",
  "H-2B",  2022,  "https://www.dol.gov/sites/dolgov/files/ETA/oflc/pdfs/H-2B_Disclosure_Data_FY2022_Q4.xlsx",
  "H-2B",  2021,  "https://www.dol.gov/sites/dolgov/files/ETA/oflc/pdfs/H-2B_Disclosure_Data_FY2021.xlsx",
  "H-2B",  2020,  "https://www.dol.gov/sites/dolgov/files/ETA/oflc/pdfs/H2B_Disclosure_Data_FY2020.xlsx",
  "H-2B",  2019,  "https://www.dol.gov/sites/dolgov/files/ETA/oflc/pdfs/H-2B_Disclosure_Data_FY2019.xlsx",
  "H-2B",  2018,  "https://www.dol.gov/sites/dolgov/files/ETA/oflc/pdfs/H-2B_Disclosure_Data_FY2018_EOY.xlsx",
  "H-2B",  2017,  "https://www.dol.gov/sites/dolgov/files/ETA/oflc/pdfs/H-2B_FY2017.xlsx",
  
  "PERM",  2025,  "https://www.dol.gov/sites/dolgov/files/ETA/oflc/pdfs/PERM_Disclosure_Data_FY2025_Q4.xlsx",
  "PERM",  2024,  "https://www.dol.gov/sites/dolgov/files/ETA/oflc/pdfs/PERM_Disclosure_Data_FY2024_Q4.xlsx",
  "PERM",  2023,  "https://www.dol.gov/sites/dolgov/files/ETA/oflc/pdfs/PERM_Disclosure_Data_FY2023_Q4.xlsx",
  "PERM",  2022,  "https://www.dol.gov/sites/dolgov/files/ETA/oflc/pdfs/PERM_Disclosure_Data_FY2022_Q4.xlsx",
  "PERM",  2021,  "https://www.dol.gov/sites/dolgov/files/ETA/oflc/pdfs/PERM_Disclosure_Data_FY2021.xlsx",
  "PERM",  2020,  "https://www.dol.gov/sites/dolgov/files/ETA/oflc/pdfs/PERM_Disclosure_Data_FY2020.xlsx",
  "PERM",  2019,  "https://www.dol.gov/sites/dolgov/files/ETA/oflc/pdfs/PERM_FY2019.xlsx",
  "PERM",  2018,  "https://www.dol.gov/sites/dolgov/files/ETA/oflc/pdfs/PERM_Disclosure_Data_FY2018_EOY.xlsx",
  "PERM",  2017,  "https://www.dol.gov/sites/dolgov/files/ETA/oflc/pdfs/PERM_Disclosure_Data_FY17.xlsx"
)

# --------------------------------------------------
# 2. Helper: parse dates
# --------------------------------------------------
parse_any_date <- function(x) {
  x <- as.character(x)
  suppressWarnings(
    parse_date_time(
      x,
      orders = c("mdy", "ymd", "dmy", "mdy HMS", "ymd HMS", "dmy HMS")
    )
  )
}

# --------------------------------------------------
# 3. Helper: read one file
# --------------------------------------------------
read_government_file <- function(url) {
  tmp <- tempfile(fileext = ".xlsx")
  download.file(url, destfile = tmp, mode = "wb", quiet = TRUE)
  read_excel(tmp)
}

# --------------------------------------------------
# 4. Helper: coalesce only existing columns
# --------------------------------------------------
coalesce_any <- function(df, vars) {
  present <- vars[vars %in% names(df)]
  if (length(present) == 0) return(rep(NA_character_, nrow(df)))
  
  tmp <- df[present] %>%
    mutate(across(everything(), as.character))
  
  dplyr::coalesce(!!!tmp)
}

# --------------------------------------------------
# 5. Read and bind files
# --------------------------------------------------
read_and_bind <- function(file_table) {
  
  keep_vars <- c(
    "YEAR", "TYPE", "CASE_NUMBER", "CASE_STATUS", "DECISION_DATE",
    "EMPLOYMENT_BEGIN_DATE", "EMPLOYMENT_END_DATE", "EMPLOYER_NAME",
    "EMPLOYER_CITY", "EMPLOYER_STATE", "EMPLOYER_POSTAL_CODE",
    "JOB_TITLE", "TOTAL_WORKERS_CERTIFIED", "ANTICIPATED_NUMBER_OF_HOURS",
    "WAGE_OFFER", "OVERTIME_RATE", "PER", "WORKSITE_CITY", "WORKSITE_STATE",
    "WORKSITE_POSTAL_CODE", "NAICS_CODE", "NATURE_OF_TEMPORARY_NEED"
  )
  
  map_dfr(seq_len(nrow(file_table)), function(i) {
    row <- file_table[i, ]
    message("Downloading and reading: ", row$TYPE, " FY", row$YEAR)
    
    df <- read_government_file(row$DOWNLOAD_URL)
    
    # Harmonize common variables
    df <- df %>%
      mutate(
        EMPLOYER_NAME = coalesce_any(df, c("EMPLOYER_NAME", "EMP_BUSINESS_NAME")),
        EMPLOYER_CITY = coalesce_any(df, c("EMP_CITY", "EMPLOYER_CITY")),
        EMPLOYER_STATE = coalesce_any(df, c("EMP_STATE", "EMPLOYER_STATE")),
        EMPLOYER_POSTAL_CODE = coalesce_any(df, c("EMP_POSTCODE", "EMPLOYER_POSTAL_CODE")),
        NAICS_CODE = coalesce_any(df, c("NAICS_CODE", "EMP_NAICS", "NAICS_US_CODE")),
        EMPLOYMENT_BEGIN_DATE = coalesce_any(df, c("EMPLOYMENT_BEGIN_DATE", "CERTIFICATION_BEGIN_DATE", "REQUESTED_START_DATE_OF_NEED")),
        EMPLOYMENT_END_DATE = coalesce_any(df, c("EMPLOYMENT_END_DATE", "CERTIFICATION_END_DATE", "REQUESTED_END_DATE_OF_NEED")),
        TOTAL_WORKERS_CERTIFIED = coalesce_any(df, c("TOTAL_WORKERS_CERTIFIED", "NBR_WORKERS_CERTIFIED", "TOTAL_WORKERS_H2A_CERTIFIED")),
        ANTICIPATED_NUMBER_OF_HOURS = coalesce_any(df, c("ANTICIPATED_NUMBER_OF_HOURS", "BASIC_NUMBER_OF_HOURS", "NUMBER_OF_HOURS")),
        WAGE_OFFER = coalesce_any(df, c("WAGE_OFFER", "BASIC_WAGE_RATE_FROM", "BASIC_RATE_OF_PAY", "JOB_OPP_WAGE_FROM", "WAGE_OFFER_FROM_9089", "WAGE_OFFERED_FROM_9089", "WAGE_OFFER_FROM", "PW_WAGE")),
        OVERTIME_RATE = coalesce_any(df, c("OVERTIME_RATE", "OVERTIME_RATE_FROM")),
        PER = coalesce_any(df, c("PER", "BASIC_UNIT_OF_PAY", "JOB_OPP_WAGE_PER", "WAGE_OFFER_UNIT_OF_PAY_9089", "PW_UNIT_OF_PAY")),
        WORKSITE_CITY = coalesce_any(df, c("PRIMARY_WORKSITE_CITY", "WORKSITE_CITY", "ALIEN_WORK_CITY", "WORKSITE_LOCATION_CITY", "EMPLOYEE_WORKSITE_CITY", "JOB_INFO_WORK_CITY")),
        WORKSITE_STATE = coalesce_any(df, c("PRIMARY_WORKSITE_STATE", "WORKSITE_STATE", "ALIEN_WORK_STATE", "WORKSITE_LOCATION_STATE", "EMPLOYEE_WORK_STATE", "JOB_INFO_WORK_STATE")),
        WORKSITE_POSTAL_CODE = coalesce_any(df, c("WORKSITE_POSTAL_CODE", "EMPLOYEE_POSTAL_CODE", "PRIMARY_WORKSITE_POSTAL_CODE", "JOB_INFO_WORK_POSTAL_CODE")),
        JOB_TITLE = coalesce_any(df, c("JOB_TITLE", "PW_JOB_TITLE_9089", "PW_Job_Title_9089", "PW_JOB_TITLE"))
      ) %>%
      mutate(
        DECISION_DATE = parse_any_date(DECISION_DATE),
        EMPLOYMENT_BEGIN_DATE = parse_any_date(EMPLOYMENT_BEGIN_DATE),
        EMPLOYMENT_END_DATE = parse_any_date(EMPLOYMENT_END_DATE)
      )
    
    # Add year and type
    df$YEAR <- row$YEAR
    df$TYPE <- row$TYPE
    
    # Keep selected variables only
    df %>% select(any_of(keep_vars))
  })
}

# --------------------------------------------------
# 6. Import all records
# --------------------------------------------------
visa_raw <- read_and_bind(file_index)

# --------------------------------------------------
# 7. Save imported dataset
# --------------------------------------------------
dir.create("data_raw", showWarnings = FALSE)
saveRDS(visa_raw, "data_raw/visa_raw.rds")

# Optional CSV export
# write_csv(visa_raw, "data_raw/visa_raw.csv")