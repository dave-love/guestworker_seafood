# ==================================================
# 03_analysis.R
# Run all figure and table scripts for the manuscript
# ==================================================

run_script <- function(script_path) {
  if (!file.exists(script_path)) {
    stop("Script not found: ", script_path)
  }
  message("Running: ", script_path)
  source(script_path)
}

if (!exists("visa_seafood")) {
  stop("visa_seafood object not found. Run 02_clean_classify_seafood.R first.")
}

# Main figure
run_script("Figure1.R")

# Supplementary figures
run_script("FigureS1.R")
run_script("FigureS2.R")
run_script("FigureS3.R")
run_script("FigureS4.R")

# Main table
run_script("Table1.R")

# Supplementary tables
run_script("TableS1.R")
run_script("TableS2.R")

message("All analysis scripts completed successfully.")