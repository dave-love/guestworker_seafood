# Seafood Labor Visa Dataset

This repository contains R code used to download, import, clean, classify, and analyze U.S. foreign labor disclosure data for seafood-related employment.

## Repository contents

- `01_download_import.R`  
  Downloads H-2A, H-2B, and PERM disclosure files from the U.S. Department of Labor and imports them into R.

- `02_clean_classify_seafood.R`  
  Cleans the imported data, classifies seafood-related records, assigns industry categories, and constructs the final analytic dataset.

- `03_analysis.R`  
  Contains the full analysis workflow used in the manuscript.

- `Figure1.R`, `FigureS1.R`, `FigureS2.R`, `FigureS3.R`, `FigureS4.R`  
  Scripts used to generate the figures in the manuscript and supplement.

- `Table1.R`, `TableS1.R`, `TableS2.R`  
  Scripts used to generate the tables in the manuscript and supplement.

## Data source

Data were obtained from the U.S. Department of Labor, Employment and Training Administration, Foreign Labor Certification disclosure files:  
https://www.dol.gov/agencies/eta/foreign-labor/performance

## Workflow

1. Run `01_download_import.R`
   - downloads source Excel files from the DOL website
   - imports them into R
   - saves the combined raw dataset to `data_raw/visa_raw.rds`

2. Run `02_clean_classify_seafood.R`
   - loads the raw dataset
   - applies seafood classification rules
   - creates the final cleaned dataset
   - saves it to `data_clean/visa_seafood.rds`

3. Run `03_analysis.R` or the individual figure/table scripts
   - generates the manuscript figures and tables

## Output folders

- `data_raw/`  
  Contains imported raw data saved from Script 1

- `data_clean/`  
  Contains cleaned analytical data saved from Script 2

- `figures/`  
  Contains figures generated for the manuscript and supplement

- `tables/`  
  Contains tables generated for the manuscript and supplement

## How to reproduce

To reproduce the analysis:

1. Clone or download this repository.
2. Open the project in R or RStudio with the repository root as the working directory.
3. Install any required R packages if they are not already available.
4. Run `01_download_import.R` to download and import the raw disclosure files.
5. Run `02_clean_classify_seafood.R` to create the cleaned seafood analytic dataset.
6. Run `03_analysis.R` or the individual scripts (`Figure1.R`, `Table1.R`, etc.) to generate the figures and tables.

Because the scripts write outputs to project-relative folders (`data_raw/`, `data_clean/`, `figures/`, and `tables/`), they should run correctly as long as the repository root is the working directory.

## Notes

- The cleaning script uses a combination of NAICS codes, keyword searches of job titles and employer names, and manual recodes to classify records into aquaculture, fishing, and seafood processing.
- Some outputs rely on a small number of manual exclusions and recodes to address false positives from keyword matching.
- The scripts were written to support reproducibility and transparency.

## Contact for more information

Dave Love, PhD, MSPH  
Research Professor  
Johns Hopkins Center for a Livable Future  
Department of Environmental Health and Engineering  
Johns Hopkins Bloomberg School of Public Health  
dlove8@jhu.edu

## Software

The code was written in R and uses packages from the tidyverse ecosystem, along with packages for tables, formatting, and data import.
