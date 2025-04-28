# Outsourced workers in the UK: Joseph Rowntree Foundation [UPDATE WITH REPORT NAME]


[Click here for the data cleaning script](https://justknowledge-uk.github.io/jrf_nat_rep/R/data_cleaning)

[Click here for the most recent analysis](https://justknowledge-uk.github.io/jrf_nat_rep/R/key_findings_report_structure)

[Click here for the methodology document](https://justknowledge-uk.github.io/jrf_nat_rep/R/methodology)

[Click here for analysis code presented matching the report structure](https://justknowledge-uk.github.io/jrf_nat_rep/R/analysis_report_matched)

## Study 1 - Nationally Representative Survey

This repository contains the data and analysis underlying JRF's Nationally Representative survey of workers in the UK. The survey covered personal demographics, employment demographics (e.g. occupation, hours worked, pay), and the outsourced diagnostic questions. The main objectives of the analysis were to produce an accurate estimate of the size and demographic makeup of the outsourced worker population in the UK.

### Getting started

1. Clone this repo.
2. Open the .Rproj file to start RStudio using the repository's project structure.

### Repository structure

- `Data/`: Contains input data, including the original survey data files and cleaned data files.
- `R/`: Contains scripts used for data cleaning and analysis:
  - data_cleaning.qmd is the data cleaning script
  - key_findings_report_structure.qmd is the analysis script
  - methodology.qmd is the script for producing the methodology document
- `outputs`: Contains the various outputs (plots, datafiles) from the cleaning and analysis
- `py`: Contains python script for plotting regional distribution of outsourced workers.

### Usage

To reproduce the data cleaning and analysis pipeline, 

1. Start by running the data_cleaning.qmd script in the project in RStudio. Look out for a prompt in the console to asking to confirm save of the cleaned file. Type 'y' and Enter in console to confirm. This script can also be knitted to html for easier viewing. The data cleaning script 
    - applies labels to coded variables
    - calculates pay metrics such as income over different periods
    - classifies respondents as low income or not based on regional medians from the [Annual Survey of Hours and Earnings](https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/allemployeesashetable1)
    - removes outliers based on income figures
    - applies different category groupings to some categorical variables such as ethnicity and migration
    - classifies respondents as outsourced or not
2. Open key_findings_report_structure.qmd and run. We recommend knitting this file (Ctrl+Shift+K) to produce an html document for easier reading
3. The methodology.qmd can be run and knitted (recommended) in the same way as above.

Note all knitted files are already present in the repo, they are the .html files in the R folder.
## Study 2 - Experiential Survey of Outsourced Workers

This repository contains the data and analysis scripts for analysis of outsourced workers data. The analysis explores various metrics related to outsourced work experiences, compensation, and other relevant factors as detailed in the research report.

### Getting Started

To access the experiential data and analysis:

1. Clone this repository
2. Navigate to the 'experiential' folder

### Repository Structure

The 'experiential' folder contains:
- `data/`: Raw and processed datasets
- `img/`: Generated charts, graphs, and visualizations
- `scripts/`: Analysis scripts written in R and Quarto documents (.qmd)

### Usage Instructions

#### Data Cleaning

Start by running the data cleaning script:
```R
Rscript scripts/data_cleaning.R
```

This script:
- Cleans the raw data
- Calculates pay metrics
- Removes outliers
- Prepares the data for further analysis

#### Analysis Scripts

After cleaning the data, you can run any of the following analysis scripts to reproduce the results reported in the report [NAME OF REPORT TBC]:

```R
Rscript scripts/Linear Modelling.R
```

Or for Quarto documents:
```
quarto render scripts/Crosstabulation.qmd
```

Available analysis scripts include various examinations of the outsourced workers dataset.

## Requirements

- R 4.0.0+
- Required R packages: tidyverse, ggplot2, dplyr, knitr (for Quarto documents)
- Quarto (for .qmd files)

## Citation

If you use this data or analysis in your research, please cite:
[Citation information to be added]

## License

[License information]

## Contact

For questions or issues regarding this repository, please [open an issue](link) or contact Just Knowledge at hello@justknowledge.org.uk
