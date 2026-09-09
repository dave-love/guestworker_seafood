# create_zip_lookup.R
# One-time script to create a ZIP -> state lookup file from zipcodeR

library(tidyverse)
library(zipcodeR)

# ------------------------------------------------------------------
# 1. Pull ZIP database from zipcodeR
# ------------------------------------------------------------------
# zip_code_db should be available from the package.
# If this errors, run:
# data(package = "zipcodeR")
# or
# ls("package:zipcodeR")
#
# to confirm the object name in your installed version.
zip_df <- zip_code_db %>%
  as_tibble()

# ------------------------------------------------------------------
# 2. Inspect columns if needed
# ------------------------------------------------------------------
# Uncomment the line below the first time you run the script:
# print(names(zip_df))

# ------------------------------------------------------------------
# 3. Standardize to a 5-digit ZIP and 2-letter state code
# ------------------------------------------------------------------
# This assumes the columns are named 'zipcode' and 'state'.
# If your version uses different names, rename them here.
zip_lookup <- zip_df %>%
  transmute(
    zip = str_pad(as.character(zipcode), width = 5, side = "left", pad = "0"),
    state = toupper(as.character(state))
  ) %>%
  filter(
    !is.na(zip),
    !is.na(state),
    zip != "",
    state != ""
  ) %>%
  distinct(zip, .keep_all = TRUE) %>%
  arrange(zip)

# ------------------------------------------------------------------
# 4. Save the file
# ------------------------------------------------------------------
dir.create("data_raw", showWarnings = FALSE)
write_csv(zip_lookup, "data_raw/zip_state_lookup.csv")

# Optional: confirm it saved
message("Saved lookup file to data_raw/zip_state_lookup.csv")