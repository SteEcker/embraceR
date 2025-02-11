# embraceR <img src="man/figures/logo.png" align="right" width="120" height="139" alt="embraceR logo" />

[![CRAN status](https://www.r-pkg.org/badges/version/embraceR)](https://CRAN.R-project.org/package=embraceR)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Data: Restricted](https://img.shields.io/badge/Data-Restricted_Access-red.svg)](https://www.embracestudy.dk/)

A comprehensive R package for processing and analyzing data from [EMBRACE-I and EMBRACE-II](https://www.embracestudy.dk/) studies in gynecological oncology.

## Overview

embraceR streamlines the analysis of clinical outcomes data from the EMBRACE studies (Image-guided intensity-modulated External beam radiochemotherapy and MRI-based adaptive BRAchytherapy in locally advanced CErvical cancer). It provides standardized workflows and ensures consistent analysis approaches across research projects.

## ⚠️ Data Access

This package contains **analysis code only** - no study data is included. Data access provided to participating centers and researchers upon request.


## Installation

```r
# Install from GitHub
if (!require("devtools")) install.packages("devtools")
devtools::install_github("steecker/embraceR")
```

## Key Features

### Data Processing
- Standardized data loading for EMBRACE I & II
- Automated cleaning and transformation
- Consistent variable naming conventions

### Clinical Analysis
- Disease control endpoints
- Survival analysis tools
- Treatment response assessment
- Morbidity tracking

### Quality of Life
- EORTC questionnaire analysis
- Side effects monitoring
- Long-term follow-up tools

### Reporting
- Summary statistics generation
- Excel report creation
- Comparative outcome tables

## Usage

```r
library(embraceR)

# Load and process EMBRACE II data
data <- load_embrace_ii(
  file_path = "path/to/emii.xlsx",
  file_path_eqd2 = "path/to/emii_eqd2.xlsx"
)

```



## Dependencies

### Core
- [dplyr](https://dplyr.tidyverse.org/) - Data manipulation
- [tidyr](https://tidyr.tidyverse.org/) - Data tidying
- [purrr](https://purrr.tidyverse.org/) - Functional programming

### Data Import/Export
- [readxl](https://readxl.tidyverse.org/) - Excel file reading
- [openxlsx](https://ycphs.github.io/openxlsx/) - Excel file writing
- [haven](https://haven.tidyverse.org/) - SPSS file reading

### Analysis & Utilities
- [gtsummary](https://www.danieldsjoberg.com/gtsummary/) - Summary tables
- [lubridate](https://lubridate.tidyverse.org/) - Date handling
- [stringr](https://stringr.tidyverse.org/) - String manipulation
- [glue](https://glue.tidyverse.org/) - String interpolation
- [janitor](https://sfirke.github.io/janitor/) - Data cleaning
- [here](https://here.r-lib.org/) - File path handling
- [sjlabelled](https://strengejacke.github.io/sjlabelled/) - Variable labeling
